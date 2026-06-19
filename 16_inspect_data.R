# Data format inspectors (serialised, database, columnar)
# Auto-split from app.R — do not edit the monolithic file

# ── SER: Serialised R objects (.rds, .RData, .rda) ──────────────────────────
inspect_serialised <- function(filepath, cfg=list()) {
  size_thr_mb <- as.numeric(cfg$robj_large_mb %||% 100L)
  model_names <- cfg$robj_model_names %||%
    c("model","fit","lm","glm","cox","surv","rf","xgb","result","summary","output")

  sz_mb <- file.info(filepath)$size / 1024^2
  name  <- tolower(tools::file_path_sans_ext(basename(filepath)))
  ext   <- tolower(tools::file_ext(filepath))

  # Heuristic: filename contains any model/result term (split on _ - . and check each part)
  name_parts <- unlist(strsplit(name, "[_\\-\\.[:space:]]+"))
  looks_like_model <- any(model_names %in% name_parts)

  if (!is.na(sz_mb) && sz_mb >= size_thr_mb) {
    detail <- paste0(
      round(sz_mb, 1), " MB — exceeds the ", size_thr_mb,
      " MB threshold for serialised objects. Likely contains a full data structure ",
      "(data frame, list of records, or large model with embedded data). ",
      "Manual inspection required before egress.")
    return(list(list(rule="SER-002", outcome="RED", detail=detail)))
  }

  # Below size threshold — AMBER with context
  size_note <- if (!is.na(sz_mb)) paste0(round(sz_mb,2)," MB") else "size unknown"
  type_hint <- if (ext == "rdata" || ext == "rda")
    ".RData workspace — may contain multiple objects of any type"
  else if (looks_like_model)
    paste0("Filename suggests a model or result object ('", basename(filepath), "')")
  else
    paste0("Filename does not clearly indicate content type ('", basename(filepath), "')")

  list(list(rule="SER-001", outcome="AMBER",
    detail=paste0(
      "Serialised R object (", size_note, "). ", type_hint, ". ",
      "Content cannot be verified without deserialisation — manual review required.")))
}

# ── DAT: SQLite databases ────────────────────────────────────────────────────
inspect_database <- function(filepath) {
  # SQLite magic bytes: "SQLite format 3\000" (first 16 bytes)
  magic <- tryCatch({
    raw_bytes <- readBin(filepath, "raw", n=16)
    rawToChar(raw_bytes[1:min(15, length(raw_bytes))], multiple=FALSE)
  }, error=function(e) "")
  magic <- tryCatch(rawToChar(readBin(filepath,"raw",n=15)), error=function(e) "")

  confirmed <- grepl("SQLite format", magic, fixed=TRUE)
  ext        <- tolower(tools::file_ext(filepath))
  sz_mb      <- file.info(filepath)$size / 1024^2

  if (confirmed) {
    list(list(rule="DAT-001", outcome="RED",
      detail=paste0(
        "SQLite database confirmed by magic bytes (",
        if(!is.na(sz_mb)) paste0(round(sz_mb,1)," MB") else "size unknown",
        "). Database files can contain multiple participant-level tables. ",
        "Cannot be automatically assessed — egress not permitted without manual inspection.")))
  } else {
    # Extension match but magic bytes unclear (may be empty or non-standard)
    list(list(rule="DAT-001", outcome="RED",
      detail=paste0(
        "File has database extension (.", ext, ") but magic bytes could not be confirmed. ",
        "Treat as RED pending manual inspection.")))
  }
}

# ── COL: Columnar formats (Parquet, Feather / Arrow IPC) ─────────────────────
inspect_columnar <- function(filepath, cfg=list()) {
  outcome_cfg <- cfg$columnar_outcome %||% "AMBER"
  sz_mb       <- file.info(filepath)$size / 1024^2
  size_thr_mb <- (cfg$size_threshold_gb %||% 5) * 1024

  # Magic byte detection
  magic4  <- tryCatch(readBin(filepath, "raw", n=8), error=function(e) raw(0))
  magic_s <- tryCatch(rawToChar(magic4[1:min(4,length(magic4))]), error=function(e) "")

  format <- if (magic_s == "PAR1") "Parquet"
            else if (magic_s == "FEA1") "Feather v1"
            else if (grepl("ARROW", rawToChar(magic4), fixed=TRUE)) "Arrow IPC / Feather v2"
            else {
              ext <- tolower(tools::file_ext(filepath))
              paste0(toupper(ext), " (magic bytes unconfirmed)")
            }

  size_note <- if (!is.na(sz_mb)) paste0(round(sz_mb,1)," MB") else "size unknown"

  hits <- list()

  # Size check — RED if over threshold
  if (!is.na(sz_mb) && sz_mb > size_thr_mb) {
    hits <- append(hits, list(list(rule="TAB-008", outcome="AMBER",
      detail=paste0(format, " file size (", round(sz_mb/1024,2),
        " GB) exceeds the 5 GB threshold"))))
    outcome_cfg <- "RED"
  }

  hits <- append(hits, list(list(rule="COL-001",
    outcome=outcome_cfg,
    detail=paste0(
      format, " format detected (", size_note, "). ",
      "Columnar formats commonly store large research datasets. ",
      "Content cannot be assessed without the arrow package — manual review required."))))

  hits
}
