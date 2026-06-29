# ============================================================
# Airlock Checker — Test Runner
# ============================================================
# Runs the test suite against the inspector functions without
# starting Shiny. Produces an HTML report and a CSV log.
#
# Usage:
#   cd tests
#   Rscript run_tests.R
#
# Or source() this file from an R session inside the tests/ dir.
# ============================================================

# ── Paths — override if your layout differs ───────────────────
# Defaults assume this script lives in <app_dir>/tests/
# and app modules live in <app_dir>/
# Variables prefixed with TEST_ to avoid collision with app constants
# (01_constants.R defines TEST_APP_DIR at global scope).
TEST_APP_DIR     <- normalizePath("..", mustWork = FALSE)
TEST_TESTS_DIR   <- normalizePath(".",  mustWork = FALSE)
TEST_FIXTURE_DIR <- file.path(TEST_TESTS_DIR, "fixtures")
TEST_REPORT_DIR  <- file.path(TEST_TESTS_DIR, "reports")

# ── Sanity checks ─────────────────────────────────────────────
if (!file.exists(file.path(TEST_APP_DIR, "app.R")))
  stop("TEST_APP_DIR does not contain app.R: ", TEST_APP_DIR,
       "\nEdit the paths at the top of run_tests.R.")

dir.create(TEST_FIXTURE_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(TEST_REPORT_DIR,  recursive = TRUE, showWarnings = FALSE)

# ── Core package loading (minimum for inspectors) ─────────────
suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(readr)
  library(readxl)
})

# Optional packages — inspectors that need these skip cleanly if missing
PDFTOOLS_OK <- requireNamespace("pdftools",  quietly = TRUE)
ORODICOM_OK <- requireNamespace("oro.dicom", quietly = TRUE)
ORONIFTI_OK <- requireNamespace("oro.nifti", quietly = TRUE)
HAVEN_OK    <- requireNamespace("haven",     quietly = TRUE)
if (PDFTOOLS_OK) suppressPackageStartupMessages(library(pdftools))
if (ORODICOM_OK) suppressPackageStartupMessages(library(oro.dicom))
if (ORONIFTI_OK) suppressPackageStartupMessages(library(oro.nifti))
if (HAVEN_OK)    suppressPackageStartupMessages(library(haven))

# ── Source inspector modules (not UI/server — we test pure functions) ──
.inspector_modules <- c(
  "01_constants.R",      "02_rules.R",          "03_vocabularies.R",
  "04_remediation.R",    "05_helpers.R",        "06_file_detection.R",
  "07_inspect_tabular.R","08_inspect_script.R", "09_inspect_genomic.R",
  "10_inspect_archive.R","11_inspect_image.R",  "12_inspect_document.R",
  "13_inspect_text.R",   "14_inspect_medical.R","15_inspect_statistical.R",
  "16_inspect_data.R",   "17_engine.R"
)
for (.m in .inspector_modules) {
  .path <- file.path(TEST_APP_DIR, .m)
  if (!file.exists(.path))
    stop("Missing module: ", .path)
  source(.path, local = FALSE)
}
rm(.m, .path, .inspector_modules)

# ── Build fixtures and load test definitions ──────────────────
source(file.path(TEST_TESTS_DIR, "build_fixtures.R"), local = TRUE)
build_fixtures(TEST_FIXTURE_DIR)

# Robustness fixtures + definitions (pathological-input tests)
if (file.exists(file.path(TEST_TESTS_DIR, "build_robustness_fixtures.R"))) {
  source(file.path(TEST_TESTS_DIR, "build_robustness_fixtures.R"), local = TRUE)
  build_robustness_fixtures(TEST_FIXTURE_DIR)
}

source(file.path(TEST_TESTS_DIR, "definitions.R"), local = TRUE)

if (file.exists(file.path(TEST_TESTS_DIR, "robustness_definitions.R"))) {
  source(file.path(TEST_TESTS_DIR, "robustness_definitions.R"), local = TRUE)
  test_cases <- c(test_cases, ROBUSTNESS_CASES)
}

cat(sprintf("Airlock Checker test suite — %d tests\n", length(test_cases)))
cat(sprintf("Fixture dir : %s\n", TEST_FIXTURE_DIR))
cat(sprintf("Report dir  : %s\n\n", TEST_REPORT_DIR))

# ── Utility: NULL-safe scalar coercion ────────────────────────
# Some test result fields (rule, fixture, expect_fire) are absent
# for robustness-category tests. `.scalar(x, default)` ensures a
# length-1 value so report builders don't crash on mismatched rows.
.scalar <- function(x, default = NA) {
  if (is.null(x) || length(x) == 0) return(default)
  x[[1]]
}

# ============================================================
# TEST EXECUTION
# ============================================================

# Map inspector name strings to the actual function objects
inspector_fn <- function(name) {
  fn <- tryCatch(get(name, mode = "function"), error = function(e) NULL)
  if (is.null(fn)) stop("Unknown inspector: ", name)
  fn
}

# Check which optional packages a given inspector needs
required_pkgs_for <- function(inspector_name, test_required = NULL) {
  explicit <- if (!is.null(test_required)) test_required else character(0)
  implicit <- switch(inspector_name,
    inspect_document    = "pdftools",
    inspect_dicom       = "oro.dicom",
    inspect_nifti       = character(0),     # pure R header reader
    inspect_statistical = "haven",
    character(0))
  unique(c(explicit, implicit))
}

# Check if a package is available (mirrors the _OK flags)
pkg_available <- function(pkg) {
  switch(pkg,
    pdftools  = PDFTOOLS_OK,
    oro.dicom = ORODICOM_OK,
    oro.nifti = ORONIFTI_OK,
    haven     = HAVEN_OK,
    requireNamespace(pkg, quietly = TRUE))
}

# Run a single test case — returns a result list
run_test <- function(tc) {
  t0 <- Sys.time()
  result <- list(
    id             = tc$id,
    rule           = tc$rule,
    fixture        = tc$fixture,
    inspector      = tc$inspector,
    expect_fire    = tc$expect_fire,
    expect_outcome = tc$expect_outcome %||% NA_character_,
    status         = NA_character_,   # PASS / FAIL / SKIP / ERROR
    message        = "",
    actual_outcome = NA_character_,
    fire           = NA,
    duration_ms    = NA_real_
  )

  # Check required packages
  required <- required_pkgs_for(tc$inspector, tc$required_pkgs)
  missing  <- required[!sapply(required, pkg_available)]
  if (length(missing) > 0) {
    result$status  <- "SKIP"
    result$message <- paste0("missing package(s): ", paste(missing, collapse = ", "))
    result$duration_ms <- as.numeric(difftime(Sys.time(), t0, units = "secs")) * 1000
    return(result)
  }

  # Resolve fixture path
  fp <- file.path(TEST_FIXTURE_DIR, tc$fixture)
  if (!file.exists(fp)) {
    result$status  <- "ERROR"
    result$message <- paste0("fixture not found: ", fp)
    result$duration_ms <- as.numeric(difftime(Sys.time(), t0, units = "secs")) * 1000
    return(result)
  }

  # Call the inspector
  fn <- tryCatch(inspector_fn(tc$inspector), error = function(e) NULL)
  if (is.null(fn)) {
    result$status  <- "ERROR"
    result$message <- paste0("inspector not found: ", tc$inspector)
    result$duration_ms <- as.numeric(difftime(Sys.time(), t0, units = "secs")) * 1000
    return(result)
  }

  run_dte_classif <- NA_character_
  if (tc$inspector == "run_dte") {
    res <- tryCatch(fn(fp, cfg = tc$cfg %||% list()),
                    error = function(e) {
                      result$status  <<- "ERROR"
                      result$message <<- paste0("inspector threw: ", conditionMessage(e))
                      NULL
                    })
    if (!is.null(res)) {
      run_dte_classif <- res$classification %||% NA_character_
      hits <- res$hits %||% list()
    } else {
      hits <- NULL
    }
  } else {
    hits <- tryCatch({
      if (tc$inspector == "inspect_binary") {
        fn(fp)
      } else if (tc$inspector == "inspect_database") {
        fn(fp)
      } else {
        fn(fp, cfg = tc$cfg %||% list())
      }
    }, error = function(e) {
      result$status  <<- "ERROR"
      result$message <<- paste0("inspector threw: ", conditionMessage(e))
      NULL
    })
  }

  if (is.null(hits) && result$status == "ERROR") {
    # For robustness tests, ERROR means FAIL — the whole point is no throwing
    if (identical(tc$category, "robustness")) {
      result$status <- "FAIL"
      # message already has "inspector threw: ..." from the tryCatch handler
    }
    result$duration_ms <- as.numeric(difftime(Sys.time(), t0, units = "secs")) * 1000
    return(result)
  }

  # Robustness tests: pass if inspector didn't throw, returned a list,
  # and finished within max_duration_ms. No rule-specific assertions.
  if (identical(tc$category, "robustness")) {
    duration_ms <- as.numeric(difftime(Sys.time(), t0, units = "secs")) * 1000
    budget <- tc$max_duration_ms %||% 30000
    if (!is.list(hits)) {
      result$status  <- "FAIL"
      result$message <- sprintf("inspector returned non-list: %s", class(hits)[1])
    } else if (duration_ms > budget) {
      result$status  <- "FAIL"
      result$message <- sprintf("exceeded budget: %.0fms > %dms",
                                duration_ms, as.integer(budget))
    } else {
      result$status  <- "PASS"
      n_uncertain <- sum(vapply(hits, function(h)
        identical(h$outcome, "UNCERTAIN"), logical(1)))
      result$message <- sprintf("survived (%.0fms, %d hits, %d UNCERTAIN)",
                                duration_ms, length(hits), n_uncertain)
    }
    result$duration_ms <- duration_ms
    return(result)
  }

  # For run_dte, the "fire" check is whether the classification matches expect_outcome
  if (tc$inspector == "run_dte") {
    result$actual_outcome <- run_dte_classif
    # Fire = classification matches what we expected
    result$fire <- !is.na(run_dte_classif) &&
                   (!is.na(tc$expect_outcome) &&
                    identical(run_dte_classif, tc$expect_outcome))
  } else {
    # Find hits for the expected rule
    rule_hits <- Filter(function(h) identical(h$rule, tc$rule), hits)
    result$fire <- length(rule_hits) > 0
    if (result$fire) {
      # If multiple, report the first non-GREEN if possible, else the first
      non_green <- Filter(function(h) !identical(h$outcome, "GREEN"), rule_hits)
      chosen    <- if (length(non_green) > 0) non_green[[1]] else rule_hits[[1]]
      result$actual_outcome <- chosen$outcome
      # Check detail substring if specified
      if (!is.null(tc$expect_detail) && nzchar(tc$expect_detail)) {
        det <- chosen$detail %||% ""
        if (!grepl(tc$expect_detail, det, fixed = FALSE, ignore.case = TRUE)) {
          result$status  <- "FAIL"
          result$message <- sprintf("detail mismatch: expected substring '%s' not in '%s'",
            tc$expect_detail, substr(det, 1, 100))
        }
      }
    }
  }

  # Decide pass/fail if not already set
  if (is.na(result$status)) {
    if (tc$expect_fire != result$fire) {
      result$status  <- "FAIL"
      result$message <- sprintf("expected fire=%s, got fire=%s",
        tc$expect_fire, result$fire)
    } else if (!is.na(result$expect_outcome) && result$fire) {
      if (!identical(result$actual_outcome, tc$expect_outcome)) {
        result$status  <- "FAIL"
        result$message <- sprintf("expected outcome=%s, got %s",
          tc$expect_outcome, result$actual_outcome %||% "NULL")
      } else {
        result$status  <- "PASS"
      }
    } else {
      result$status  <- "PASS"
    }
  }

  # XFAIL: a test marked as expected-to-fail flips FAIL -> XFAIL, or
  # unexpectedly-pass -> XPASS (which is a failure, "fix was reverted?").
  if (isTRUE(tc$xfail)) {
    if (result$status == "FAIL") {
      result$status  <- "XFAIL"
      result$message <- sprintf("expected failure: %s",
        tc$xfail_reason %||% "(no reason)")
    } else if (result$status == "PASS") {
      result$status  <- "XPASS"
      result$message <- "unexpectedly passed — remove xfail marker?"
    }
  }

  result$duration_ms <- as.numeric(difftime(Sys.time(), t0, units = "secs")) * 1000
  result
}

# ── Run all tests ─────────────────────────────────────────────
results <- vector("list", length(test_cases))
by_rule <- new.env()

for (i in seq_along(test_cases)) {
  tc  <- test_cases[[i]]
  res <- run_test(tc)
  results[[i]] <- res

  status_sym <- switch(res$status,
    PASS = "\u2713",  FAIL = "\u2717",
    SKIP = "\u25CB", ERROR = "\u2718",
    "?")
  cat(sprintf("  %s %-40s  %s%s\n",
    status_sym, res$id, res$status,
    if (nzchar(res$message)) paste0("  \u2014  ", res$message) else ""))
}

# ── Summary ───────────────────────────────────────────────────
n_pass  <- sum(sapply(results, function(r) r$status == "PASS"))
n_fail  <- sum(sapply(results, function(r) r$status == "FAIL"))
n_skip  <- sum(sapply(results, function(r) r$status == "SKIP"))
n_error <- sum(sapply(results, function(r) r$status == "ERROR"))
n_xfail <- sum(sapply(results, function(r) r$status == "XFAIL"))
n_xpass <- sum(sapply(results, function(r) r$status == "XPASS"))
n_total <- length(results)

cat(sprintf("\n%s\n", strrep("=", 60)))
cat(sprintf("  %d tests: %d pass, %d fail, %d skip, %d error\n",
  n_total, n_pass, n_fail, n_skip, n_error))
cat(sprintf("%s\n", strrep("=", 60)))

# ── Coverage: which rules have tests? ─────────────────────────
# Get rule IDs from the RULES list, compare against test_cases
all_rule_ids  <- unique(sapply(RULES, function(r) r$id))
tested_rules  <- unique(vapply(results,
  function(r) .scalar(r$rule, NA_character_), character(1)))
tested_rules  <- tested_rules[!is.na(tested_rules) & tested_rules != "E2E"]
untested      <- sort(setdiff(all_rule_ids, tested_rules))
coverage_pct  <- round(100 * length(tested_rules) / length(all_rule_ids), 1)

cat(sprintf("  Rule coverage: %d / %d rules tested (%s%%)\n",
  length(tested_rules), length(all_rule_ids), coverage_pct))
if (length(untested) > 0)
  cat(sprintf("  Untested rules: %s%s\n",
    paste(head(untested, 10), collapse = ", "),
    if (length(untested) > 10) sprintf(" (+%d more)", length(untested) - 10) else ""))

# ============================================================
# REPORTS
# ============================================================
# One HTML and one CSV per run — each run overwrites the previous.
# The run timestamp is embedded inside the report itself.
timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
csv_path  <- file.path(TEST_REPORT_DIR, "test_results.csv")
html_path <- file.path(TEST_REPORT_DIR, "test_report.html")

# ── CSV output ────────────────────────────────────────────────
# Build CSV data frame. Use `.scalar()` for each field — robustness
# tests omit rule/fixture/expect_fire/expect_outcome, which would
# otherwise break data.frame() with "differing number of rows".
csv_df <- do.call(rbind, lapply(results, function(r) data.frame(
  id             = .scalar(r$id,             NA_character_),
  rule           = .scalar(r$rule,           NA_character_),
  fixture        = .scalar(r$fixture,        NA_character_),
  inspector      = .scalar(r$inspector,      NA_character_),
  category       = .scalar(r$category,       "unit"),
  expect_fire    = .scalar(r$expect_fire,    NA),
  expect_outcome = .scalar(r$expect_outcome, NA_character_),
  actual_fire    = .scalar(r$fire,           NA),
  actual_outcome = .scalar(r$actual_outcome, NA_character_),
  status         = .scalar(r$status,         NA_character_),
  message        = .scalar(r$message,        ""),
  duration_ms    = round(.scalar(r$duration_ms, 0), 1),
  stringsAsFactors = FALSE
)))
write.csv(csv_df, csv_path, row.names = FALSE)

# ── HTML output ───────────────────────────────────────────────
html_escape <- function(x) {
  x <- as.character(x)
  x <- gsub("&", "&amp;",  x, fixed = TRUE)
  x <- gsub("<", "&lt;",   x, fixed = TRUE)
  x <- gsub(">", "&gt;",   x, fixed = TRUE)
  x
}

status_colour <- function(s) switch(s,
  PASS  = "#2E7D32", FAIL  = "#C62828",
  SKIP  = "#F9A825", ERROR = "#6A1B9A", "#666")

# Per-rule summary. Robustness tests are excluded — they have no
# rule ID to summarise against.
rule_summary <- list()
for (r in results) {
  k <- .scalar(r$rule, NA_character_)
  if (is.na(k) || !nzchar(k)) next
  if (is.null(rule_summary[[k]]))
    rule_summary[[k]] <- list(total = 0L, pass = 0L, fail = 0L,
                              skip = 0L, error = 0L)
  rule_summary[[k]]$total <- rule_summary[[k]]$total + 1L
  key <- tolower(r$status)
  # Some statuses (XFAIL, XPASS) aren't in the initial skeleton
  if (is.null(rule_summary[[k]][[key]])) rule_summary[[k]][[key]] <- 0L
  rule_summary[[k]][[key]] <- rule_summary[[k]][[key]] + 1L
}

rule_rows <- sapply(names(rule_summary)[order(names(rule_summary))], function(rid) {
  s <- rule_summary[[rid]]
  status_cls <- if (s$fail > 0 || s$error > 0) "row-fail"
                else if (s$skip > 0)            "row-skip"
                else                            "row-pass"
  sprintf('<tr class="%s"><td><code>%s</code></td><td>%d</td><td>%d</td><td>%d</td><td>%d</td><td>%d</td></tr>',
    status_cls, html_escape(rid), s$total, s$pass, s$fail, s$skip, s$error)
})

# Failure details
failures <- Filter(function(r) r$status %in% c("FAIL","ERROR"), results)
failure_rows <- if (length(failures) == 0) {
  '<tr><td colspan="5" style="text-align:center; color:#2E7D32; padding:1.5rem;">\u2713  No failures</td></tr>'
} else {
  sapply(failures, function(r) sprintf(
    '<tr><td><code>%s</code></td><td><code>%s</code></td><td>%s</td><td>%s</td><td>%s</td></tr>',
    html_escape(.scalar(r$id,      "")),
    html_escape(.scalar(r$rule,    "")),
    html_escape(.scalar(r$fixture, "")),
    sprintf('<span style="color:%s; font-weight:700;">%s</span>',
            status_colour(r$status), r$status),
    html_escape(.scalar(r$message, ""))))
}

# Untested rules block
untested_html <- if (length(untested) == 0) {
  '<p style="color:#2E7D32;">All rules in RULES have at least one test.</p>'
} else {
  paste0(
    sprintf('<p><strong>%d rules without tests</strong> (%s%% coverage):</p>',
            length(untested), coverage_pct),
    '<div class="rule-chips">',
    paste(sprintf('<code class="chip">%s</code>', html_escape(untested)),
          collapse = ""),
    '</div>')
}

html <- sprintf('<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<title>Airlock Checker Test Report %s</title>
<style>
  body { font-family: "Segoe UI", Arial, sans-serif; background: #EFF3F7;
         color: #1A1A1A; margin: 0; padding: 1.5rem 2rem; }
  h1 { color: #003366; margin: 0 0 0.3rem; font-size: 1.5rem; }
  h2 { color: #003366; font-size: 1.05rem; margin: 1.5rem 0 0.5rem;
       border-bottom: 2px solid #0066A1; padding-bottom: 0.2rem; }
  .sub { color: #666; font-size: 0.85rem; margin: 0 0 1.2rem; }
  .card { background: white; border: 1px solid #D0DCE8; border-radius: 8px;
          box-shadow: 0 1px 6px rgba(0,0,0,0.06); padding: 1rem 1.2rem;
          margin-bottom: 1rem; }
  .summary { display: flex; gap: 1rem; flex-wrap: wrap; }
  .stat { flex: 1; min-width: 120px; background: #F0F4F8;
          border-left: 4px solid #0066A1; border-radius: 5px;
          padding: 0.6rem 0.9rem; }
  .stat-val { font-size: 1.6rem; font-weight: 800; line-height: 1; }
  .stat-lbl { font-size: 0.72rem; text-transform: uppercase;
              letter-spacing: 0.07em; color: #666; margin-top: 0.2rem; }
  .stat-pass  { border-left-color: #2E7D32; } .stat-pass  .stat-val { color: #2E7D32; }
  .stat-fail  { border-left-color: #C62828; } .stat-fail  .stat-val { color: #C62828; }
  .stat-skip  { border-left-color: #F9A825; } .stat-skip  .stat-val { color: #F9A825; }
  .stat-error { border-left-color: #6A1B9A; } .stat-error .stat-val { color: #6A1B9A; }
  table { width: 100%%; border-collapse: collapse; font-size: 0.85rem; }
  th, td { text-align: left; padding: 0.4rem 0.6rem; border-bottom: 1px solid #E2EAF0; }
  th { background: #003366; color: white; font-weight: 700; font-size: 0.78rem;
       text-transform: uppercase; letter-spacing: 0.05em; }
  tr.row-pass { background: white; }
  tr.row-fail { background: #FFEBEE; }
  tr.row-skip { background: #FFF8E1; }
  code { font-family: "Consolas", "Monaco", monospace; font-size: 0.82rem;
         background: #F0F4F8; padding: 0.05rem 0.35rem; border-radius: 3px;
         color: #003366; }
  .rule-chips { display: flex; flex-wrap: wrap; gap: 0.3rem; margin-top: 0.5rem; }
  .rule-chips .chip { background: #FFF3E0; color: #E65100; }
  .foot { color: #888; font-size: 0.75rem; margin-top: 2rem; text-align: center; }
</style>
</head>
<body>
  <h1>Airlock Checker \u2014 Test Report</h1>
  <p class="sub">Generated %s \u00b7 R %s \u00b7 Host %s</p>

  <div class="card">
    <div class="summary">
      <div class="stat stat-pass">  <div class="stat-val">%d</div><div class="stat-lbl">Passed</div></div>
      <div class="stat stat-fail">  <div class="stat-val">%d</div><div class="stat-lbl">Failed</div></div>
      <div class="stat stat-skip">  <div class="stat-val">%d</div><div class="stat-lbl">Skipped</div></div>
      <div class="stat stat-error"> <div class="stat-val">%d</div><div class="stat-lbl">Errored</div></div>
      <div class="stat">            <div class="stat-val">%d</div><div class="stat-lbl">Total</div></div>
    </div>
  </div>

  <h2>Results by Rule</h2>
  <div class="card">
    <table>
      <thead><tr><th>Rule</th><th>Total</th><th>Pass</th><th>Fail</th><th>Skip</th><th>Error</th></tr></thead>
      <tbody>%s</tbody>
    </table>
  </div>

  <h2>Failures</h2>
  <div class="card">
    <table>
      <thead><tr><th>Test</th><th>Rule</th><th>Fixture</th><th>Status</th><th>Message</th></tr></thead>
      <tbody>%s</tbody>
    </table>
  </div>

  <h2>Rule Coverage</h2>
  <div class="card">%s</div>

  <div class="foot">Airlock Checker test runner \u00b7 Aridhia Informatics</div>
</body>
</html>',
  timestamp,
  format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  paste(R.version$major, R.version$minor, sep="."),
  Sys.info()[["nodename"]],
  n_pass, n_fail, n_skip, n_error, n_total,
  paste(rule_rows, collapse = "\n      "),
  paste(failure_rows, collapse = "\n      "),
  untested_html
)

writeLines(html, html_path)

cat(sprintf("\nReports written:\n  %s\n  %s\n", html_path, csv_path))

# Exit code reflects failure state (useful if invoked from a wrapper script)
if (n_fail > 0 || n_error > 0) {
  invisible(quit(status = 1, save = "no", runLast = FALSE))
}
