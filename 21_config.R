# Configuration persistence (JSON save/load)
# Auto-split from app.R — do not edit the monolithic file

# ============================================================
# CONFIG PERSISTENCE  (base-R only, no external packages)
# ============================================================

cfg_to_json <- function(cfg) {
  pairs <- lapply(names(cfg), function(k) {
    v <- cfg[[k]]
    json_val <- if (is.logical(v) && length(v) == 1) {
      tolower(as.character(v))
    } else if (is.numeric(v) && length(v) == 1) {
      as.character(v)
    } else if (is.character(v) && length(v) == 0) {
      "[]"
    } else if (is.character(v)) {
      paste0("[", paste(sprintf('"%s"', gsub('"', '\\\\"'  , v)), collapse=", "), "]")
    } else {
      paste0('"', gsub('"', '\\\\"'  , as.character(v)), '"')
    }
    sprintf('  "%s": %s', k, json_val)
  })
  paste0("{\n", paste(unlist(pairs), collapse=",\n"), "\n}")
}

cfg_from_json <- function(txt) {
  out   <- list()
  lines <- strsplit(txt, "\n")[[1]]
  lines <- grep('^[[:space:]]*"', lines, value=TRUE)
  for (ln in lines) {
    km <- regmatches(ln, regexpr('^[[:space:]]*"([^"]+)"', ln))
    if (length(km) == 0) next
    key     <- sub('^[[:space:]]*"([^"]+)"', "\\1", km)
    val_str <- trimws(sub('^[[:space:]]*"[^"]+"[[:space:]]*:[[:space:]]*', "", ln))
    val_str <- sub(",[[:space:]]*$", "", val_str)
    val <- if (val_str == "true") {
      TRUE
    } else if (val_str == "false") {
      FALSE
    } else if (grepl("^\\[\\s*\\]$", val_str)) {
      character(0)
    } else if (grepl("^\\[", val_str)) {
      items <- regmatches(val_str, gregexpr('"([^"]*)"', val_str))[[1]]
      gsub('^"|"$', "", items)
    } else if (grepl('^"', val_str)) {
      gsub('^"|"$', "", val_str)
    } else {
      suppressWarnings(as.numeric(val_str))
    }
    out[[key]] <- val
  }
  out
}

save_cfg <- function(cfg) {
  tryCatch({
    dir.create(OUT_DIR, recursive=TRUE, showWarnings=FALSE)
    writeLines(cfg_to_json(cfg), CFG_PATH)
    TRUE
  }, error=function(e) { warning("Could not save config: ", e$message); FALSE })
}

load_cfg <- function(defaults) {
  if (!file.exists(CFG_PATH)) return(defaults)
  tryCatch({
    saved <- cfg_from_json(paste(readLines(CFG_PATH, warn=FALSE), collapse="\n"))
    for (k in names(saved)) defaults[[k]] <- saved[[k]]
    defaults
  }, error=function(e) { warning("Could not read config: ", e$message); defaults })
}

delete_cfg <- function() {
  if (file.exists(CFG_PATH)) file.remove(CFG_PATH)
}
