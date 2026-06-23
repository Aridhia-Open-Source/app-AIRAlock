#!/usr/bin/env Rscript
# ============================================================
# dependencies.R
# Airlock Checker - dependency installer and status check.
#
# Run this once per workspace after the app files are deployed,
# and again whenever new dependencies are introduced. It installs
# every package the app needs, then prints a status report.
#
# Usage (from a workspace Terminal, in the app directory):
#
#   Rscript dependencies.R              # install + report
#   Rscript dependencies.R --check      # report only, no install
#   Rscript dependencies.R --strict     # exit 1 if ANY package is missing,
#                                       # including optional and AIRA
#
# Environment:
#
#   DRE_CRAN_MIRROR   optional; overrides the default CRAN mirror.
#                     Useful where the workspace has a local Posit
#                     Package Manager or a CRAN proxy. Default:
#                     https://cloud.r-project.org
#
# Exit codes:
#
#   0   all required packages installed
#   1   one or more required packages missing (or, with --strict,
#       any package at all missing)
#
# Notes on R-U-04 (no install.packages in the app):
#   This script is the sanctioned exception. The app itself never
#   calls install.packages; it uses requireNamespace() and degrades
#   gracefully. This script is where package provisioning lives.
# ============================================================


# ── Argument parsing ────────────────────────────────────────────────────────

args <- commandArgs(trailingOnly = TRUE)
MODE_CHECK_ONLY <- any(args %in% c("--check", "-c"))
MODE_STRICT     <- any(args %in% c("--strict", "-s"))


# ── Repository ──────────────────────────────────────────────────────────────

cran_mirror <- Sys.getenv("DRE_CRAN_MIRROR", unset = "")
if (!nzchar(cran_mirror)) cran_mirror <- "https://cloud.r-project.org"
options(repos = c(CRAN = cran_mirror))


# ── Package manifests ───────────────────────────────────────────────────────
#
# Three groups. Each entry: name, reason, what-breaks-without-it, optional
# system-dependency note.

REQUIRED <- list(
  list(name = "shiny",      reason = "Web framework",
       breaks = "App will not start"),
  list(name = "bslib",      reason = "Bootstrap theming",
       breaks = "App will not start"),
  list(name = "DT",         reason = "Interactive data tables",
       breaks = "App will not start"),
  list(name = "dplyr",      reason = "Data manipulation",
       breaks = "App will not start"),
  list(name = "stringr",    reason = "String handling",
       breaks = "App will not start"),
  list(name = "readr",      reason = "CSV/TSV parsing",
       breaks = "App will not start"),
  list(name = "readxl",     reason = "XLSX parsing",
       breaks = "App will not start"),
  list(name = "base64enc",  reason = "File fingerprinting and base64 encoding",
       breaks = "App will not start")
)

OPTIONAL_INSPECTORS <- list(
  list(name = "pdftools",   reason = "PDF text extraction",
       breaks = "PDF files return UNCERTAIN; content-based PDF rules disabled",
       sysdep = "libpoppler-cpp-dev (Linux) - must be preinstalled by workspace admin"),
  list(name = "arrow",      reason = "Parquet, Feather and Arrow IPC inspection",
       breaks = "Columnar files return UNCERTAIN; schema inspection and preview disabled",
       sysdep = "Arrow C++ libraries may be required when installing from source; binary/RSPM packages recommended"),
  list(name = "oro.dicom",  reason = "DICOM tag inspection",
       breaks = "DICOM files return UNCERTAIN",
       sysdep = NULL),
  list(name = "oro.nifti",  reason = "NIfTI pixel data reading",
       breaks = "NIfTI pixel-data rules disabled",
       sysdep = NULL),
  list(name = "haven",      reason = "SAS/Stata/SPSS file reading",
       breaks = "Statistical files return UNCERTAIN",
       sysdep = NULL)
)

AIRA <- list(
  list(name = "ellmer",     reason = "AIRA LLM client (OpenAI-compatible)",
       breaks = "AIRA features disabled; app falls back to rule-engine-only output",
       sysdep = "libcurl4-openssl-dev, libssl-dev (Linux) - present on most DRE workspaces with binary package repositories, but may need installing on source-build workspaces"),
  list(name = "future",     reason = "Async execution for non-blocking AIRA calls",
       breaks = "AIRA features disabled (async wrapper cannot run)",
       sysdep = NULL),
  list(name = "promises",   reason = "Promise-based reactive integration for AIRA",
       breaks = "AIRA features disabled (async wrapper cannot run)",
       sysdep = NULL),
  list(name = "R.utils",    reason = "Per-call timeout enforcement for AIRA",
       breaks = "AIRA features disabled (timeouts cannot be enforced)",
       sysdep = NULL)
)


# ── Helpers ─────────────────────────────────────────────────────────────────

is_installed <- function(pkg) requireNamespace(pkg, quietly = TRUE)

pkg_version_safe <- function(pkg) {
  if (!is_installed(pkg)) return(NA_character_)
  tryCatch(
    as.character(utils::packageVersion(pkg)),
    error = function(e) "unknown"
  )
}

# Attempt installation. Returns TRUE on success, FALSE on any failure.
# Never throws. Captures output so the top-level report stays tidy, but
# surfaces error and warning messages to the operator so they can diagnose
# CRAN-access or system-library problems.
try_install <- function(pkg) {
  tryCatch({
    # quiet=TRUE suppresses progress bars but keeps errors visible
    utils::install.packages(pkg, quiet = TRUE, verbose = FALSE)
    is_installed(pkg)
  }, error = function(e) {
    cat(sprintf("    install error for %s: %s\n", pkg, conditionMessage(e)))
    FALSE
  }, warning = function(w) {
    # install.packages often warns on legitimate non-failures (masking,
    # dep chains) as well as on real failures (unreachable repo, missing
    # system library). Print the warning so the operator can tell which
    # kind this was, then fall back to loadability as the ground truth.
    cat(sprintf("    install warning for %s: %s\n", pkg, conditionMessage(w)))
    is_installed(pkg)
  })
}


# ── Header ──────────────────────────────────────────────────────────────────

sep <- function() cat(paste(rep("-", 72), collapse = ""), "\n")

cat("\n")
sep()
cat(" Airlock Checker - dependency check\n")
sep()
cat(sprintf("  Mode       : %s\n",
            if (MODE_CHECK_ONLY) "CHECK ONLY (no installation)" else "INSTALL + REPORT"))
cat(sprintf("  Strict     : %s\n",
            if (MODE_STRICT) "yes (any missing -> exit 1)" else "no (only required missing -> exit 1)"))
cat(sprintf("  R version  : %s.%s (%s)\n",
            R.version$major, R.version$minor, R.version$platform))
cat(sprintf("  CRAN mirror: %s\n", getOption("repos")[["CRAN"]]))
cat("\n")


# ── Installation phase ──────────────────────────────────────────────────────

install_group <- function(group_label, group) {
  cat(sprintf(" Installing %s packages...\n", group_label))
  for (entry in group) {
    pkg <- entry$name
    if (is_installed(pkg)) {
      cat(sprintf("  [already installed] %-12s %s\n",
                  pkg, pkg_version_safe(pkg)))
    } else {
      cat(sprintf("  [installing]        %-12s ... ", pkg))
      ok <- try_install(pkg)
      if (ok) {
        cat(sprintf("OK (%s)\n", pkg_version_safe(pkg)))
      } else {
        cat("FAILED\n")
      }
    }
  }
  cat("\n")
}

if (!MODE_CHECK_ONLY) {
  install_group("REQUIRED",            REQUIRED)
  install_group("OPTIONAL (inspectors)", OPTIONAL_INSPECTORS)
  install_group("AIRA",                AIRA)
}


# ── Status report ───────────────────────────────────────────────────────────

report_group <- function(group_label, group) {
  sep()
  cat(sprintf(" %s\n", group_label))
  sep()
  n_ok <- 0L
  n_missing <- 0L
  for (entry in group) {
    pkg <- entry$name
    if (is_installed(pkg)) {
      cat(sprintf("  [OK]      %-12s v%-10s  %s\n",
                  pkg, pkg_version_safe(pkg), entry$reason))
      n_ok <- n_ok + 1L
    } else {
      cat(sprintf("  [MISSING] %-12s %-11s  %s\n",
                  pkg, "", entry$reason))
      cat(sprintf("              -> %s\n", entry$breaks))
      if (!is.null(entry$sysdep))
        cat(sprintf("              -> system dep: %s\n", entry$sysdep))
      n_missing <- n_missing + 1L
    }
  }
  cat(sprintf("  Subtotal: %d installed, %d missing\n\n", n_ok, n_missing))
  list(ok = n_ok, missing = n_missing)
}

cat("\n")
r1 <- report_group("Required packages",           REQUIRED)
r2 <- report_group("Optional inspector packages", OPTIONAL_INSPECTORS)
r3 <- report_group("AIRA packages",               AIRA)


# ── Summary and exit ────────────────────────────────────────────────────────

sep()
cat(" Summary\n")
sep()
cat(sprintf("  Required : %d / %d installed\n",
            r1$ok, r1$ok + r1$missing))
cat(sprintf("  Optional : %d / %d installed\n",
            r2$ok, r2$ok + r2$missing))
cat(sprintf("  AIRA     : %d / %d installed\n",
            r3$ok, r3$ok + r3$missing))

# AIRA is all-or-nothing. Report the feature-level state explicitly.
aira_ready <- r3$missing == 0L
cat(sprintf("  AIRA features: %s\n",
            if (aira_ready) "available (all AIRA packages present)"
            else            "DISABLED (one or more AIRA packages missing)"))

# WORKSPACE_API_KEY is independent of package installation - a workspace
# can have the packages but no key, or a key but not the packages. Both
# are needed for AIRA to actually run. Report key presence so operators
# don't get a false sense of readiness from "packages: OK".
api_key_present <- nzchar(Sys.getenv("WORKSPACE_API_KEY"))
cat(sprintf("  WORKSPACE_API_KEY: %s\n",
            if (api_key_present) "set"
            else                  "NOT SET (AIRA calls will be disabled at runtime)"))
if (aira_ready && !api_key_present) {
  cat("    -> install is complete but AIRA will not activate until the\n")
  cat("       workspace provides a WORKSPACE_API_KEY in the environment.\n")
}

# Exit status
required_missing <- r1$missing > 0L
any_missing      <- required_missing || r2$missing > 0L || r3$missing > 0L

status_code <- {
  if (MODE_STRICT && any_missing) 1L
  else if (required_missing)      1L
  else                             0L
}

cat("\n")
if (status_code == 0L) {
  cat(" Result: OK\n")
} else {
  cat(" Result: FAILED\n")
  if (required_missing) {
    cat("   One or more REQUIRED packages are missing. The app will not start.\n")
    cat("   Install them manually or re-run without --check.\n")
  } else if (MODE_STRICT) {
    cat("   One or more OPTIONAL or AIRA packages are missing (--strict mode).\n")
  }
}
sep()
cat("\n")

quit(status = status_code)
