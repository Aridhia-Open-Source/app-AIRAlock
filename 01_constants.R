# Constants: brand colours, paths, application mode
# Auto-split from app.R - do not edit the monolithic file

# ── Aridhia brand colours ───────────────────────────────────
ARIDHIA_BLUE  <- "#0066A1"
ARIDHIA_DARK  <- "#003366"
GREEN_C       <- "#2E7D32"
GREEN_BG      <- "#E8F5E9"
AMBER_C       <- "#E65100"
AMBER_BG      <- "#FFF3CD"
RED_C         <- "#C62828"
RED_BG        <- "#FFEBEE"
WORKSPACE_FILES  <- "/home/workspace"
FILE_DIR_DEFAULT <- "/home/workspace/files"
# OUT_DIR is the base for persistent app state: config, audit log,
# fingerprints, reports, downloads. It derives from APP_HOME (set in app.R)
# so all state and logs share one base and follow the app per workspace.
# Falls back to a fixed location if APP_HOME is not set (e.g. a module
# sourced standalone in a test harness).
OUT_DIR       <- if (exists("APP_HOME", inherits = TRUE) &&
                     is.character(APP_HOME) && length(APP_HOME) == 1L &&
                     nzchar(APP_HOME)) {
                   APP_HOME
                 } else {
                   "/home/workspace/files/airlockcheck"
                 }
LOG_PATH      <- file.path(OUT_DIR, ".dte_audit_log.csv")
FP_PATH       <- file.path(OUT_DIR, ".dte_fingerprints.csv")
CFG_PATH      <- file.path(OUT_DIR, ".dte_config.json")
# Diagnostic JSONL log lives alongside the other debug logs (stderr,
# stdout, inspection_trace) in LOG_DIR, which app.R sets at startup.
# If LOG_DIR isn't set (e.g. someone sources this file standalone), fall
# back to the legacy airlockcheck/logs/ location so behaviour is sane.
DIAG_DIR      <- if (exists("LOG_DIR", inherits = TRUE) &&
                     is.character(LOG_DIR) && length(LOG_DIR) == 1L &&
                     nzchar(LOG_DIR)) {
                   LOG_DIR
                 } else {
                   file.path(OUT_DIR, "logs")
                 }
DIAG_LOG      <- file.path(DIAG_DIR, "airlock.jsonl")
DIAG_LEVEL    <- "INFO"    # DEBUG | INFO | WARN | ERROR
APP_DIR        <- getwd()  # directory containing app.R (set by Shiny at startup)

# ── Application mode ────────────────────────────────────────
# Read from app_mode.txt in the app folder.
# Valid values: "researcher" (default) | "reviewer"
# Absent or unrecognised file → defaults to researcher (safe).
APP_MODE <- tryCatch({
  raw <- trimws(tolower(suppressWarnings(readLines(
    file.path(APP_DIR, "app_mode.txt"), warn=FALSE))[1]))
  if (!is.na(raw) && raw %in% c("researcher","reviewer")) raw else "reviewer"
}, error=function(e) "reviewer")

# Ensure output directory exists at startup
dir.create(OUT_DIR,  recursive=TRUE, showWarnings=FALSE)
dir.create(DIAG_DIR, recursive=TRUE, showWarnings=FALSE)