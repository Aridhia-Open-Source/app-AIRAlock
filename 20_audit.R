# Audit log and fingerprint functions
# Auto-split from app.R — do not edit the monolithic file


# ============================================================
# AUDIT LOG & FINGERPRINT FUNCTIONS
# ============================================================

log_decision <- function(result, final_outcome, reviewer_note="", batch_risk_score=NA_integer_) {
  rules_fired   <- paste(sapply(result$hits, `[[`, "rule"),  collapse="|")
  outcomes_fired <- paste(sapply(result$hits, `[[`, "outcome"), collapse="|")
  entry <- data.frame(
    timestamp          = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    file               = result$file,
    file_type          = result$file_type,
    size_bytes         = result$size_bytes,
    dte_class          = result$classification,
    dte_score          = result$score,
    dte_batch_risk     = batch_risk_score,
    rules_fired        = ifelse(nchar(rules_fired)>0, rules_fired, "NONE"),
    outcomes_fired     = ifelse(nchar(outcomes_fired)>0, outcomes_fired, "NONE"),
    n_rules            = length(result$hits),
    final_outcome      = final_outcome,
    overridden         = final_outcome != result$classification,
    reviewer_note      = gsub(",", ";", reviewer_note),  # escape for CSV
    stringsAsFactors = FALSE
  )
  ok <- tryCatch({
    dir.create(OUT_DIR, recursive=TRUE, showWarnings=FALSE)
    write.table(entry, LOG_PATH, append=file.exists(LOG_PATH), sep=",",
                row.names=FALSE, col.names=!file.exists(LOG_PATH), quote=TRUE)
    TRUE
  }, error=function(e) { warning("Could not write audit log: ", e$message); FALSE })
  invisible(ok)
}

load_log <- function() {
  if (!file.exists(LOG_PATH)) return(NULL)
  tryCatch({
    df <- read.csv(LOG_PATH, stringsAsFactors=FALSE)
    if (nrow(df) == 0 || ncol(df) == 0) return(NULL)
    # Parse timestamp robustly — try multiple formats, fall back to character
    df$timestamp <- tryCatch({
      ts <- as.POSIXct(df$timestamp, tz="UTC")
      if (all(is.na(ts)))
        as.POSIXct(df$timestamp, format="%Y-%m-%d %H:%M:%S", tz="UTC")
      else ts
    }, error=function(e) {
      as.POSIXct(df$timestamp, format="%Y-%m-%d %H:%M:%S", tz="UTC")
    })
    # read.csv reads TRUE/FALSE as character — coerce back to correct types
    if ("overridden"      %in% names(df)) df$overridden      <- as.logical(df$overridden)
    if ("dte_score"       %in% names(df)) df$dte_score       <- suppressWarnings(as.numeric(df$dte_score))
    if ("dte_batch_risk"  %in% names(df)) df$dte_batch_risk  <- suppressWarnings(as.numeric(df$dte_batch_risk))
    if ("size_bytes"      %in% names(df)) df$size_bytes      <- suppressWarnings(as.numeric(df$size_bytes))
    if ("n_rules"         %in% names(df)) df$n_rules         <- suppressWarnings(as.integer(df$n_rules))
    df
  }, error=function(e) NULL)
}

log_summary <- function(log) {
  if (is.null(log) || nrow(log)==0) return(NULL)
  # Per-rule stats
  rules_long <- do.call(rbind, lapply(seq_len(nrow(log)), function(i) {
    rules <- strsplit(log$rules_fired[i], "\\|")[[1]]
    if (length(rules)==0 || rules[1]=="NONE") return(NULL)
    data.frame(rule=rules, overridden=log$overridden[i],
               dte_class=log$dte_class[i], final=log$final_outcome[i],
               stringsAsFactors=FALSE)
  }))
  if (is.null(rules_long)) return(NULL)
  rules_long %>%
    group_by(rule) %>%
    summarise(
      times_fired   = n(),
      times_overridden = sum(overridden),
      fp_rate       = round(mean(overridden)*100, 1),
      confirm_rate  = round(mean(!overridden)*100, 1),
      .groups="drop"
    ) %>%
    arrange(desc(fp_rate))
}

# Structural fingerprint for tabular files
compute_fingerprint <- function(filepath, result) {
  if (!(result$file_type %in% c("tabular"))) return(NULL)
  tryCatch({
    df_head <- readr::read_csv(filepath, n_max=0, show_col_types=FALSE)
    fp <- list(
      col_sig   = paste(sort(tolower(names(df_head))), collapse="|"),
      col_count = ncol(df_head),
      file_type = result$file_type,
      rules_sig = paste(sort(sapply(result$hits, `[[`, "rule")), collapse="|")
    )
    paste(fp, collapse="||")
  }, error=function(e) NULL)
}

save_fingerprint <- function(filepath, result, final_outcome) {
  fp_str <- compute_fingerprint(filepath, result)
  if (is.null(fp_str)) return(invisible(NULL))
  entry <- data.frame(
    timestamp    = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    file         = result$file,
    fingerprint  = fp_str,
    final_outcome = final_outcome,
    approved     = final_outcome == "GREEN",
    stringsAsFactors=FALSE
  )
  tryCatch({
    write.table(entry, FP_PATH, append=file.exists(FP_PATH), sep=",",
                row.names=FALSE, col.names=!file.exists(FP_PATH), quote=TRUE)
  }, error=function(e) NULL)
  invisible(entry)
}

load_fingerprints <- function() {
  if (!file.exists(FP_PATH)) return(NULL)
  tryCatch(read.csv(FP_PATH, stringsAsFactors=FALSE), error=function(e) NULL)
}

check_fingerprint <- function(filepath, result) {
  fps <- load_fingerprints()
  if (is.null(fps)) return(NULL)
  fp_str <- compute_fingerprint(filepath, result)
  if (is.null(fp_str)) return(NULL)
  matches <- fps[fps$fingerprint == fp_str, ]
  if (nrow(matches)==0) return(NULL)
  matches[order(matches$timestamp, decreasing=TRUE), ][1,]
}
