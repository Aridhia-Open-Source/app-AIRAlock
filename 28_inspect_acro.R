# ============================================================
# 28_inspect_acro.R - ACRO/SACRO results inspector
# ============================================================
# Parses ACRO results.json files and emits structured hits for each
# output in the session. Returns both:
#   (a) hits for the results.json file itself (session metadata)
#   (b) a correlation map linking ACRO output UIDs to batch filenames
#
# The engine/server layer uses the correlation map to inject ACRO hits
# into the result cards of matched output files.
#
# Inspector contract (per PRS §6.3):
#   - Input:  filepath + cfg
#   - Output: list of hits (possibly empty)
#   - Never throws; parse failures -> UNCERTAIN hit
#   - No side effects except logging
#   - Deterministic
#
# ACRO schema detection: top-level keys "version", "config", "results"
# where config contains "safe_threshold" and results is a named list
# of output objects each having "uid", "status", "type", "files".
#
# Dependencies: jsonlite (requireNamespace-guarded per R-FILE-01)
# ============================================================


# ── Schema fingerprint constants ─────────────────────────────────────────
ACRO_REQUIRED_TOP_KEYS    <- c("version", "config", "results")
ACRO_REQUIRED_CONFIG_KEYS <- c("safe_threshold")
ACRO_REQUIRED_OUTPUT_KEYS <- c("uid", "status", "type", "files")
ACRO_VALID_STATUSES       <- c("pass", "fail", "review")

# SDC test names used in ACRO's sdc.cells structure
ACRO_SDC_TESTS <- c("threshold", "p-ratio", "nk-rule",
                     "negative", "missing", "all-values-are-same")


# ── Schema recognition ──────────────────────────────────────────────────
# Returns TRUE if the parsed JSON matches the ACRO results schema.
# Called by 06_file_detection.R to distinguish ACRO JSON from generic JSON.
# Lightweight: checks structure only, does not validate content.
#
# ACRO's acro.finalise() can write config either embedded in results.json
# (under a "config" key) or as a sidecar config.json alongside it. Detection
# therefore keys on the reliable fingerprint: "version" + "results" where
# "results" is a named list of output objects with the ACRO output shape.
# "config" is treated as optional here; when absent from results.json, the
# inspector looks for a sidecar config.json (see .acro_load_sidecar_config).
is_acro_results_json <- function(parsed) {
  if (!is.list(parsed)) return(FALSE)
  # Reliable fingerprint: version + results (config may be a sidecar file)
  if (!("version" %in% names(parsed))) return(FALSE)
  if (!("results" %in% names(parsed))) return(FALSE)
  res <- parsed$results
  if (!is.list(res) || length(res) == 0L) return(FALSE)
  if (is.null(names(res))) return(FALSE)
  # Spot-check first output has the ACRO output shape
  first <- res[[1]]
  if (!is.list(first)) return(FALSE)
  if (!all(ACRO_REQUIRED_OUTPUT_KEYS %in% names(first))) return(FALSE)
  TRUE
}

# Variant that reads from file path. Used by detect_file_type when the
# extension is .json and we need to distinguish ACRO from generic JSON.
# Returns TRUE/FALSE, never throws.
#
# Reads the whole file because ACRO results.json is a single JSON object
# and a 100-line head read can truncate mid-structure for large sessions,
# making jsonlite::fromJSON fail and a real ACRO file look like plain JSON.
is_acro_results_file <- function(filepath) {
  tryCatch({
    if (!requireNamespace("jsonlite", quietly = TRUE)) return(FALSE)
    text <- paste(readLines(filepath, warn = FALSE, encoding = "UTF-8"),
                  collapse = "\n")
    if (!nzchar(text)) return(FALSE)
    parsed <- jsonlite::fromJSON(text, simplifyVector = FALSE)
    is_acro_results_json(parsed)
  }, error = function(e) FALSE)
}

# Load a sidecar config.json if present alongside results.json. ACRO can
# write config separately from results. Returns the parsed config list, or
# NULL if no sidecar exists or it cannot be read. Never throws.
.acro_load_sidecar_config <- function(results_filepath) {
  tryCatch({
    if (!requireNamespace("jsonlite", quietly = TRUE)) return(NULL)
    dir <- dirname(results_filepath)
    cfg_path <- file.path(dir, "config.json")
    if (!file.exists(cfg_path)) return(NULL)
    text <- paste(readLines(cfg_path, warn = FALSE, encoding = "UTF-8"),
                  collapse = "\n")
    if (!nzchar(text)) return(NULL)
    parsed <- jsonlite::fromJSON(text, simplifyVector = FALSE)
    if (!is.list(parsed)) return(NULL)
    # The sidecar may itself be {version, config, ...} or a bare config
    # object. Prefer an embedded $config; otherwise treat the whole file
    # as the config if it carries a recognisable ACRO config key.
    if (is.list(parsed$config)) return(parsed$config)
    if ("safe_threshold" %in% names(parsed)) return(parsed)
    NULL
  }, error = function(e) NULL)
}


# ── Main inspector ───────────────────────────────────────────────────────
# Returns a list with two elements:
#   $hits        - list of hit objects (for the results.json file itself)
#   $acro_data   - parsed session data for engine/server consumption:
#       $version          - ACRO version string
#       $config           - ACRO session config (safe_threshold etc.)
#       $outputs          - list of per-output parsed structures
#       $file_map         - named list: basename -> list of ACRO output UIDs
#       $session_summary  - list(n_pass, n_fail, n_review, n_total, title)
#       $checklist        - researcher's pre-submission checklist (if present)
#
# The $hits list always contains at least one ACR-007 (session metadata) hit.
# Additional ACR-004 hits for config discrepancies, ACR-005 for unmatched
# files, etc. are added as appropriate.
#
# Per-output hits (ACR-001/002/003/006) are returned in $acro_data$outputs
# keyed by output UID, each with a $hits sublist. The server layer injects
# these into the corresponding file's result card.

inspect_acro <- function(filepath, cfg = list(), batch_basenames = character(0)) {
  # Defensive: never throw
  tryCatch({
    .inspect_acro_inner(filepath, cfg, batch_basenames)
  }, error = function(e) {
    list(
      hits = list(list(
        rule    = "PARSE",
        outcome = "UNCERTAIN",
        detail  = paste0("ACRO results inspector failed: ", conditionMessage(e))
      )),
      acro_data = NULL
    )
  })
}

.inspect_acro_inner <- function(filepath, cfg, batch_basenames) {
  hits <- list()

  # ── Parse JSON ──────────────────────────────────────────────────────
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    return(list(
      hits = list(list(
        rule    = "PARSE",
        outcome = "UNCERTAIN",
        detail  = "jsonlite not available - cannot parse ACRO results"
      )),
      acro_data = NULL
    ))
  }

  text <- tryCatch(
    paste(readLines(filepath, warn = FALSE, encoding = "UTF-8"), collapse = "\n"),
    error = function(e) NULL
  )
  if (is.null(text) || !nzchar(text)) {
    return(list(
      hits = list(list(
        rule    = "PARSE",
        outcome = "UNCERTAIN",
        detail  = "ACRO results file is empty or unreadable"
      )),
      acro_data = NULL
    ))
  }

  parsed <- tryCatch(
    jsonlite::fromJSON(text, simplifyVector = FALSE),
    error = function(e) NULL
  )
  if (is.null(parsed) || !is_acro_results_json(parsed)) {
    return(list(
      hits = list(list(
        rule    = "PARSE",
        outcome = "UNCERTAIN",
        detail  = "File looks like JSON but does not match ACRO results schema"
      )),
      acro_data = NULL
    ))
  }

  # ── Session-level data ──────────────────────────────────────────────
  acro_version <- as.character(parsed$version %||% "unknown")[1L]
  acro_config  <- parsed$config
  # ACRO may write config as a sidecar config.json rather than embedding
  # it in results.json. If embedded config is absent, look for the sidecar
  # alongside the results file.
  if (!is.list(acro_config)) {
    acro_config <- .acro_load_sidecar_config(filepath)
  }
  acro_title   <- as.character(parsed$title %||% "")[1L]
  outputs      <- parsed$results
  n_total      <- length(outputs)

  n_pass   <- 0L
  n_fail   <- 0L
  n_review <- 0L

  # ── Session metadata hit (ACR-007) ─────────────────────────────────
  for (uid in names(outputs)) {
    st <- tolower(as.character(outputs[[uid]]$status %||% "")[1L])
    if (identical(st, "pass"))   n_pass   <- n_pass + 1L
    else if (identical(st, "fail"))   n_fail   <- n_fail + 1L
    else if (identical(st, "review")) n_review <- n_review + 1L
  }

  session_summary <- list(
    n_pass    = n_pass,
    n_fail    = n_fail,
    n_review  = n_review,
    n_total   = n_total,
    title     = acro_title,
    version   = acro_version
  )

  summary_detail <- sprintf(
    "ACRO v%s session%s: %d output(s) - %d passed, %d failed, %d for review",
    acro_version,
    if (nzchar(acro_title)) paste0(" '", acro_title, "'") else "",
    n_total, n_pass, n_fail, n_review
  )

  hits[[length(hits) + 1L]] <- list(
    rule     = "ACR-007",
    outcome  = "GREEN",
    detail   = summary_detail,
    evidence = .acro_session_evidence(session_summary, acro_config)
  )

  # ── Config discrepancy check (ACR-004) ─────────────────────────────
  acro_safe_thr <- as.integer(acro_config$safe_threshold %||% NA_integer_)
  airalck_count_thr <- as.integer(cfg$count_threshold %||% NA_integer_)

  if (!is.na(acro_safe_thr) && !is.na(airalck_count_thr) &&
      acro_safe_thr != airalck_count_thr) {
    hits[[length(hits) + 1L]] <- list(
      rule    = "ACR-004",
      outcome = "AMBER",
      detail  = sprintf(
        "ACRO safe_threshold=%d, AIRAlock count_threshold=%d - different risk appetites applied at analysis vs egress time",
        acro_safe_thr, airalck_count_thr)
    )
  }

  # ── Build file correlation map ─────────────────────────────────────
  # file_map: basename -> list of output UIDs that reference it
  # output_file_map: uid -> character vector of basenames
  file_map        <- list()
  output_file_map <- list()
  all_acro_files  <- character(0)

  for (uid in names(outputs)) {
    out <- outputs[[uid]]
    files_list <- out$files
    if (!is.list(files_list)) next
    basenames <- character(0)
    for (f in files_list) {
      fn <- as.character(f$name %||% "")[1L]
      if (!nzchar(fn)) next
      basenames <- c(basenames, fn)
      all_acro_files <- c(all_acro_files, fn)
      if (is.null(file_map[[fn]])) file_map[[fn]] <- character(0)
      file_map[[fn]] <- c(file_map[[fn]], uid)
    }
    output_file_map[[uid]] <- basenames
  }

  # ── Unmatched files (ACR-005) ──────────────────────────────────────
  if (length(batch_basenames) > 0L) {
    unmatched <- setdiff(unique(all_acro_files), batch_basenames)
    if (length(unmatched) > 0L) {
      hits[[length(hits) + 1L]] <- list(
        rule    = "ACR-005",
        outcome = "AMBER",
        detail  = sprintf(
          "%d ACRO output file(s) not found in batch: %s",
          length(unmatched),
          paste(head(unmatched, 5L), collapse = ", "))
      )
    }
  }

  # ── Per-output analysis ────────────────────────────────────────────
  # Build a list of per-output structures, each with $hits for injection
  # into the matched file's result card.
  output_data <- list()

  for (uid in names(outputs)) {
    out <- outputs[[uid]]
    status   <- tolower(as.character(out$status %||% "")[1L])
    out_type <- as.character(out$type %||% "unknown")[1L]
    method   <- as.character(out$properties$method %||% "")[1L]
    command  <- as.character(out$command %||% "")[1L]
    summary  <- as.character(out$summary %||% "")[1L]
    # Comments: ACRO writes an array. An empty string entry ("") means the
    # researcher left the comment blank - keep only non-empty, trimmed
    # comments so [""] becomes zero real comments. The distinction between
    # "review with comments" and "review with NO comment" is surfaced to
    # the reviewer (an unexplained review/exception is itself a gap).
    comments <- if (is.list(out$comments)) {
      raw <- vapply(out$comments, function(c) as.character(c)[1L], character(1))
      raw[nzchar(trimws(raw))]
    } else character(0)
    exception <- as.character(out$exception %||% "")[1L]
    exception <- if (nzchar(trimws(exception))) exception else ""
    timestamp <- as.character(out$timestamp %||% "")[1L]

    out_hits <- list()

    # ── Status-based rule hit ─────────────────────────────────────
    if (identical(status, "fail")) {
      # Parse SDC failures from the files' sdc blocks
      sdc_detail <- .acro_sdc_summary(out)
      out_hits[[length(out_hits) + 1L]] <- list(
        rule     = "ACR-001",
        outcome  = "RED",
        detail   = sprintf(
          "ACRO %s '%s' failed SDC checks: %s",
          out_type, uid, sdc_detail$summary_text),
        evidence = .acro_sdc_evidence(out, sdc_detail)
      )
    } else if (identical(status, "review")) {
      out_hits[[length(out_hits) + 1L]] <- list(
        rule     = "ACR-002",
        outcome  = "AMBER",
        detail   = sprintf(
          "ACRO %s '%s' flagged for manual review%s",
          out_type, uid,
          if (length(comments) > 0L)
            paste0(" \u2014 researcher: ", comments[1L])
          else " \u2014 no researcher comment provided")
      )
    } else if (identical(status, "pass")) {
      # Positive signal
      dof_info <- ""
      if (!is.null(out$properties$dof)) {
        dof_info <- sprintf(" (dof=%s)", as.character(out$properties$dof)[1L])
      }
      out_hits[[length(out_hits) + 1L]] <- list(
        rule    = "ACR-003",
        outcome = "GREEN",
        detail  = sprintf(
          "ACRO %s '%s' passed all SDC checks%s",
          out_type, uid, dof_info)
      )
    }

    # ── Checksum validity (ACR-006) ───────────────────────────────
    files_list <- out$files
    if (is.list(files_list)) {
      for (f in files_list) {
        fn <- as.character(f$name %||% "")[1L]
        cv <- f$checksum_valid
        if (!is.null(cv) && identical(cv, FALSE)) {
          out_hits[[length(out_hits) + 1L]] <- list(
            rule    = "ACR-006",
            outcome = "AMBER",
            detail  = sprintf(
              "ACRO checksum invalid for '%s' - file may have been modified since ACRO session",
              fn)
          )
        }
      }
    }

    # ── Researcher comments and exceptions ────────────────────────
    # These aren't rule hits but are carried as metadata for rendering.
    # comment_status makes the three cases explicit for the renderer:
    #   "present"  - one or more non-empty comments
    #   "blank"    - ACRO recorded a comments array but it was empty/whitespace
    #                (researcher was prompted but left it blank - a gap worth
    #                 flagging, especially for review/fail outputs)
    #   "none"     - no comments field at all
    raw_comments_present <- is.list(out$comments) && length(out$comments) > 0L
    comment_status <- if (length(comments) > 0L) "present"
                      else if (raw_comments_present) "blank"
                      else "none"
    output_data[[uid]] <- list(
      uid            = uid,
      status         = status,
      type           = out_type,
      method         = method,
      command        = command,
      summary        = summary,
      comments       = comments,
      comment_status = comment_status,
      exception      = exception,
      has_exception  = nzchar(exception),
      timestamp      = timestamp,
      hits           = out_hits,
      files          = output_file_map[[uid]] %||% character(0)
    )
  }

  # ── Checklist ──────────────────────────────────────────────────────
  checklist <- NULL
  if (is.list(parsed$checklist) && length(parsed$checklist) > 0L) {
    checklist <- lapply(parsed$checklist, function(item) {
      list(
        id      = as.character(item$id %||% "")[1L],
        label   = as.character(item$label %||% "")[1L],
        checked = isTRUE(item$checked)
      )
    })
  }

  # ── Return ─────────────────────────────────────────────────────────
  list(
    hits = hits,
    acro_data = list(
      version         = acro_version,
      config          = acro_config,
      outputs         = output_data,
      file_map        = file_map,
      session_summary = session_summary,
      checklist       = checklist
    )
  )
}


# ── Evidence builders ────────────────────────────────────────────────────

# Session summary evidence for ACR-007
.acro_session_evidence <- function(session_summary, acro_config) {
  # Build a summary data frame for the evidence table renderer
  cfg_rows <- data.frame(
    Parameter = character(0),
    Value     = character(0),
    stringsAsFactors = FALSE
  )
  if (is.list(acro_config)) {
    for (key in names(acro_config)) {
      val <- acro_config[[key]]
      cfg_rows <- rbind(cfg_rows, data.frame(
        Parameter = key,
        Value     = as.character(val %||% "")[1L],
        stringsAsFactors = FALSE
      ))
    }
  }

  list(
    type    = "table",
    data    = cfg_rows,
    title   = "ACRO Session Configuration",
    caption = sprintf(
      "ACRO v%s - %d outputs: %d pass, %d fail, %d review",
      session_summary$version,
      session_summary$n_total,
      session_summary$n_pass,
      session_summary$n_fail,
      session_summary$n_review
    ),
    flag_cols = character(0)
  )
}

# SDC failure detail extraction - walks the sdc.cells structure
.acro_sdc_summary <- function(out) {
  files_list <- out$files
  tests_failed  <- character(0)
  total_cells   <- 0L
  cell_details  <- list()

  if (!is.list(files_list)) {
    return(list(summary_text = out$summary %||% "SDC check failed",
                tests_failed = character(0),
                total_cells  = 0L,
                cell_details = list()))
  }

  for (f in files_list) {
    sdc <- f$sdc
    if (!is.list(sdc)) next
    smry <- sdc$summary
    cells <- sdc$cells

    # Count from summary
    if (is.list(smry)) {
      for (test_name in ACRO_SDC_TESTS) {
        n <- as.integer(smry[[test_name]] %||% 0L)
        if (!is.na(n) && n > 0L) {
          tests_failed <- c(tests_failed, test_name)
          total_cells  <- total_cells + n
        }
      }
    }

    # Capture cell positions
    if (is.list(cells)) {
      for (test_name in names(cells)) {
        positions <- cells[[test_name]]
        if (!is.list(positions) || length(positions) == 0L) next
        for (pos in positions) {
          if (is.list(pos) || (is.numeric(pos) && length(pos) == 2L)) {
            r <- as.integer(pos[[1]] %||% pos[1])
            c_idx <- as.integer(pos[[2]] %||% pos[2])
            cell_details[[length(cell_details) + 1L]] <- list(
              file = as.character(f$name %||% "")[1L],
              test = test_name,
              row  = r,
              col  = c_idx
            )
          }
        }
      }
    }
  }

  tests_failed <- unique(tests_failed)
  summary_text <- if (length(tests_failed) > 0L) {
    sprintf("%s (%d cell(s) across %s)",
            out$summary %||% "SDC check failed",
            total_cells,
            paste(tests_failed, collapse = ", "))
  } else {
    as.character(out$summary %||% "SDC check failed")
  }

  list(
    summary_text = summary_text,
    tests_failed = tests_failed,
    total_cells  = total_cells,
    cell_details = cell_details
  )
}

# SDC failure evidence for ACR-001 - cell-level detail table
.acro_sdc_evidence <- function(out, sdc_detail) {
  # Build a data frame of failing cells
  if (length(sdc_detail$cell_details) == 0L) {
    # Fallback: use the outcome grid if available
    return(.acro_outcome_evidence(out))
  }

  rows <- lapply(sdc_detail$cell_details, function(cd) {
    data.frame(
      File = cd$file,
      Test = cd$test,
      Row  = cd$row,
      Col  = cd$col,
      stringsAsFactors = FALSE
    )
  })
  cell_df <- do.call(rbind, rows)

  # Add researcher comments and exception as caption
  comments  <- if (is.list(out$comments) && length(out$comments) > 0L) {
    paste(vapply(out$comments, function(c) as.character(c)[1L], character(1)),
          collapse = "; ")
  } else ""
  exception <- as.character(out$exception %||% "")[1L]

  caption_parts <- character(0)
  if (nzchar(comments))  caption_parts <- c(caption_parts, paste0("Comments: ", comments))
  if (nzchar(exception)) caption_parts <- c(caption_parts, paste0("Exception: ", exception))
  caption <- paste(caption_parts, collapse = " | ")

  list(
    type         = "table",
    data         = cell_df,
    title        = sprintf("ACRO SDC failures - %s", out$uid %||% ""),
    caption      = caption,
    flag_cols = "Test"
  )
}

# Outcome grid evidence - renders the per-category × per-value outcome
.acro_outcome_evidence <- function(out) {
  outcome <- out$outcome
  if (!is.list(outcome) || length(outcome) == 0L) return(NULL)

  rows <- list()
  for (cat_name in names(outcome)) {
    cat_vals <- outcome[[cat_name]]
    if (!is.list(cat_vals)) next
    for (val_name in names(cat_vals)) {
      status <- as.character(cat_vals[[val_name]])[1L]
      rows[[length(rows) + 1L]] <- data.frame(
        Category = cat_name,
        Value    = val_name,
        Status   = status,
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(rows) == 0L) return(NULL)
  outcome_df <- do.call(rbind, rows)

  # Flag non-ok cells
  list(
    type         = "table",
    data         = outcome_df,
    title        = sprintf("ACRO outcome grid - %s", out$uid %||% ""),
    caption      = as.character(out$command %||% "")[1L],
    flag_cols = "Status"
  )
}