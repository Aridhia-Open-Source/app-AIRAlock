# ============================================================
# Airlock Checker
# Aridhia Informatics | Airlock Checker
# ============================================================
# Statistical disclosure control tool for Trusted Research
# Environments. Inspects research output files against SDC
# rules before egress.
#
# Default source directory: /home/workspace/files
# ============================================================


# ────────────────────────────────────────────────────────────────────────────
# DEBUG LOGGING BOOTSTRAP
# ────────────────────────────────────────────────────────────────────────────
# This block runs FIRST so we can capture R startup errors, package-load
# warnings, and uncaught Shiny errors. Three log files are produced per
# session, each date-stamped:
#
#   <LOG_DIR>/airlock.jsonl                      structured app log
#   <LOG_DIR>/r_stderr_<YYYY-MM-DD_HH-MM-SS>.log uncaught R errors + warnings
#   <LOG_DIR>/r_stdout_<YYYY-MM-DD_HH-MM-SS>.log R stdout (informational)
#
# APP_HOME is the single base directory for ALL AIRAlock persistent state:
# config, audit log, fingerprints, reports, downloads, and the debug logs.
# It is pinned to a fixed location under /home/workspace/files/, which is the
# invariant persistent store in the workspace. The app's working directory
# (getwd()) is the deployment location, which can vary per workspace and may
# not persist across redeploys, so it is NOT used as the base; it is only a
# last-resort fallback. To relocate all state, change the one line below.
# OUT_DIR (01_constants.R) and LOG_DIR (below) both derive from APP_HOME.
#
# If APP_HOME is not writable we fall back, in order, to the working
# directory and then to tempdir(), emitting a startup warning so the operator
# notices. No log rotation; old logs accumulate until deleted.
APP_HOME <- "/home/workspace/files/AIRAlock"

.dir_writable <- function(d) {
  tryCatch({
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
    .tf <- file.path(d, ".write_test")
    writeLines("ok", .tf)
    unlink(.tf)
    TRUE
  }, error = function(e) FALSE, warning = function(w) FALSE)
}

if (!.dir_writable(APP_HOME)) {
  .alt <- getwd()
  if (.dir_writable(.alt)) {
    message(sprintf(
      "[AIRAlock] WARNING: %s not writable; using working dir %s",
      APP_HOME, .alt))
    APP_HOME <- .alt
  } else {
    .alt <- tempdir()
    message(sprintf(
      "[AIRAlock] WARNING: no persistent writable dir; using %s", .alt))
    APP_HOME <- .alt
  }
}

# Debug logs live in a subdirectory of APP_HOME. APP_HOME is already
# confirmed writable above, so this just ensures the subdir exists.
LOG_DIR <- file.path(APP_HOME, "debuglogs")
.dir_writable(LOG_DIR)

# Date-stamped paths so each session gets its own files. Stamping uses the
# session start time at the second granularity. If two sessions start in
# the same second (unlikely but possible) the second one will append to
# the first, which is acceptable for debugging.
.log_stamp     <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
STDERR_LOG_PATH <- file.path(LOG_DIR, sprintf("r_stderr_%s.log", .log_stamp))
STDOUT_LOG_PATH <- file.path(LOG_DIR, sprintf("r_stdout_%s.log", .log_stamp))

# Open file connections for the sinks. Connections must stay open for the
# lifetime of the R process; we deliberately do NOT close them at end-of-
# script. When the process exits (cleanly or via crash) R will flush and
# close them.
.stderr_con <- tryCatch(file(STDERR_LOG_PATH, open = "wt"),
                        error = function(e) NULL)
.stdout_con <- tryCatch(file(STDOUT_LOG_PATH, open = "wt"),
                        error = function(e) NULL)

# sink(type = "message") captures stderr including warnings, message()
# output, and the text R prints when an uncaught error reaches the top
# level. This is the file most likely to contain the cause of a server
# crash.
if (!is.null(.stderr_con)) {
  sink(.stderr_con, type = "message", split = FALSE)
}
# sink(type = "output") captures stdout (cat, print, etc). split = TRUE
# would also send to console, but Shiny console output isn't usually
# visible in the DRE workspace launcher anyway, so split = FALSE keeps
# the file clean.
if (!is.null(.stdout_con)) {
  sink(.stdout_con, type = "output", split = FALSE)
}

# Header lines so the operator can tell sessions apart when grepping.
message(sprintf("[AIRAlock] === Session start: %s ===", Sys.time()))
message(sprintf("[AIRAlock] R version: %s", R.version.string))
message(sprintf("[AIRAlock] Working dir: %s", getwd()))
cat(sprintf("[AIRAlock] === Session start: %s ===\n", Sys.time()))

# options(shiny.error = ...) is fired whenever a Shiny reactive throws
# an unhandled error. Capturing the traceback at that point is more
# informative than R's default (which just prints the error text). We
# write it to the stderr log AND let R's default handler also run, so
# Shiny's own machinery still gets to mark the session as errored.
options(shiny.error = function() {
  err <- geterrmessage()
  tb  <- tryCatch(
    paste(capture.output(traceback(max.lines = 50L)), collapse = "\n"),
    error = function(e) "(traceback unavailable)")
  message(sprintf("\n[AIRAlock SHINY ERROR] %s", Sys.time()))
  message(sprintf("Error: %s", err))
  message(sprintf("Traceback:\n%s", tb))
})

# Also install a global option to keep warnings as warnings rather than
# silently swallowing them - useful when debugging crashes downstream of
# unhandled warnings (e.g. coercion warnings that became errors via some
# upstream tryCatch).
options(warn = 1L)
# ────────────────────────────────────────────────────────────────────────────
# END DEBUG LOGGING BOOTSTRAP
# ────────────────────────────────────────────────────────────────────────────


# ── Core packages ─────────────────────────────────────────────
library(shiny)
library(bslib)
library(DT)
library(dplyr)
library(stringr)
library(readr)
library(readxl)
library(base64enc)

# ── Optional packages (guarded) ──────────────────────────────
# pdftools - PDF text extraction
PDFTOOLS_OK <- requireNamespace("pdftools", quietly = TRUE)
if (PDFTOOLS_OK) suppressPackageStartupMessages(library(pdftools))

# oro.dicom - DICOM metadata inspection
ORODICOM_OK <- requireNamespace("oro.dicom", quietly = TRUE)
if (ORODICOM_OK) suppressPackageStartupMessages(library(oro.dicom))

# oro.nifti - NIfTI pixel data reading
ORONIFTI_OK <- requireNamespace("oro.nifti", quietly = TRUE)
if (ORONIFTI_OK) suppressPackageStartupMessages(library(oro.nifti))

# haven - Stata/SAS/SPSS statistical file reading
HAVEN_OK <- requireNamespace("haven", quietly = TRUE)
if (HAVEN_OK) suppressPackageStartupMessages(library(haven))

# ── Source all modules in explicit order ──────────────────────
# Modules are flat alongside app.R (v4 layout, per R-U-07).
# Numeric prefixes indicate intended order, but we source explicitly
# so ui/server are visible to shinyApp() and so 27_aira.R lands
# BEFORE 26_server.R (the server references AIRA symbols at
# construction time).
source("01_constants.R",          local = TRUE)

# 01_constants.R derives OUT_DIR from APP_HOME (set above), so the audit CSV
# (LOG_PATH), fingerprints, config (CFG_PATH) and the JSONL diagnostic log
# (DIAG_LOG, under LOG_DIR) all share one base directory and follow the app
# per workspace. The startup messages below print the resolved locations.
message(sprintf("[AIRAlock] APP_HOME = %s", APP_HOME))
message(sprintf("[AIRAlock] DIAG_LOG = %s",
        if (exists("DIAG_LOG")) DIAG_LOG else "(not set)"))
message(sprintf("[AIRAlock] LOG_PATH (audit CSV) = %s",
        if (exists("LOG_PATH")) LOG_PATH else "(not set)"))

source("02_rules.R",              local = TRUE)
source("03_vocabularies.R",       local = TRUE)
source("04_remediation.R",        local = TRUE)
source("05_helpers.R",            local = TRUE)
source("06_file_detection.R",     local = TRUE)
# 28_inspect_acro.R must be sourced AFTER 05_helpers.R (it uses %||%) and
# BEFORE 17_engine.R (run_dte dispatches to inspect_acro / acro_batch_integrate).
# Sourcing it here, right after 06_file_detection.R, also means
# is_acro_results_file() exists before detect_file_type() is ever called,
# so the ACRO detection branch in 06 resolves to a real function rather
# than falling through its exists() guard.
source("28_inspect_acro.R",       local = TRUE)
source("07_inspect_tabular.R",    local = TRUE)
source("08_inspect_script.R",     local = TRUE)
source("09_inspect_genomic.R",    local = TRUE)
source("10_inspect_archive.R",    local = TRUE)
source("11_inspect_image.R",      local = TRUE)
source("12_inspect_document.R",   local = TRUE)
source("13_inspect_text.R",       local = TRUE)
source("14_inspect_medical.R",    local = TRUE)
source("15_inspect_statistical.R",local = TRUE)
source("16_inspect_data.R",       local = TRUE)
source("17_engine.R",             local = TRUE)
source("18_preview.R",            local = TRUE)
source("19_preview_ui.R",         local = TRUE)
source("20_audit.R",              local = TRUE)
source("21_config.R",             local = TRUE)
source("22_pdf_report.R",         local = TRUE)
source("23_evidence_ui.R",        local = TRUE)
source("24_css.R",                local = TRUE)
source("25_ui.R",                 local = TRUE)
source("27_aira.R",               local = TRUE)   # before 26 - server references its symbols
source("26_server.R",             local = TRUE)

# ── Launch ────────────────────────────────────────────────────
shinyApp(ui = ui, server = server)