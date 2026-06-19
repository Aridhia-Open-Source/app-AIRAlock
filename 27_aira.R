# ============================================================
# 27_aira.R
# AIRA integration - "why is this RED?" plain-language summaries.
#
# This module is self-contained. It exposes a small public API
# used by 26_server.R; everything else is module-local state.
#
# Governance posture (see PRS §14.1, airlock_ai_potential.md):
#   - AIRA narrates, it does not classify.
#   - Rule engine output is the input to AIRA; file contents are not.
#   - Every call returns the canonical response shape - never throws,
#     never returns NULL.
#   - Prompts are frozen constants; any change bumps the version.
#
# Depends on (all required for AIRA to operate; missing → disabled):
#   ellmer, future, promises, R.utils
# Depends on (from existing app):
#   %||%  (defined in 05_helpers.R)
#
# Sanctioned HTTP exception: R-FILE-10. This is the only module that
# makes network calls, and only to the configured AIRA base URL.
# ============================================================


# ── Package availability ────────────────────────────────────────────────────

.aira_required_pkgs <- c("ellmer", "future", "promises", "R.utils")

AIRA_PACKAGES_OK <- all(vapply(
  .aira_required_pkgs,
  function(p) requireNamespace(p, quietly = TRUE),
  logical(1)
))

AIRA_MISSING_PACKAGES <- .aira_required_pkgs[!vapply(
  .aira_required_pkgs,
  function(p) requireNamespace(p, quietly = TRUE),
  logical(1)
)]


# ── Constants ───────────────────────────────────────────────────────────────

AIRA_BASE_URL_DEFAULT  <- "https://api.uksouth.saas.aridhia.io/api/aira/v1"
AIRA_TIMEOUT_S_DEFAULT       <- 15L   # per-file default; short because per-file calls are small
AIRA_TIMEOUT_S_BATCH_DEFAULT <- 45L   # batch default; larger because batch prompts are 4-10x larger
AIRA_MAX_HITS_IN_PROMPT <- 15L


# ── Prompt registry (FROZEN; strict versioning) ─────────────────────────────
#
# Every character change to `system`, `user_builder`, or `model_params` is
# a new version. Old versions are never edited or removed - they remain in
# this file forever so historical audit entries can be replayed.
#
# To add a version: copy the whole list element, rename the key, change
# what you need, and point `ACTIVE_PROMPT` at it. Do not modify an existing
# version in place.


# Helper: format a single hit for the user-message hit list. Deterministic.
.fmt_hit_summary_v1 <- function(h) {
  rule    <- h$rule    %||% "?"
  outcome <- h$outcome %||% "?"
  detail  <- h$detail  %||% ""
  if (nzchar(detail)) {
    sprintf("- %s (%s): %s", rule, outcome, detail)
  } else {
    sprintf("- %s (%s)", rule, outcome)
  }
}

# Helper: filter to non-GREEN hits, order by severity, cap at
# AIRA_MAX_HITS_IN_PROMPT. GREEN hits represent "rule checked, found
# nothing" - feeding them to AIRA is noise and can lead to reassurance
# phrasing that undercuts the real findings. We feed AIRA only what
# actually fired against the file. Deterministic.
.select_hits_summary_v1 <- function(hits) {
  if (length(hits) == 0) return(list(selected = list(), n_dropped = 0L))
  outcomes <- vapply(hits, function(h) h$outcome %||% "GREEN", character(1))
  keep_mask <- outcomes != "GREEN"
  hits <- hits[keep_mask]
  outcomes <- outcomes[keep_mask]
  if (length(hits) == 0) return(list(selected = list(), n_dropped = 0L))
  severity <- c(RED = 1L, UNCERTAIN = 2L, AMBER = 3L)
  ranks <- severity[outcomes]
  ranks[is.na(ranks)] <- 4L
  # Stable sort: severity first, then original index
  ord <- order(ranks, seq_along(hits))
  hits <- hits[ord]
  if (length(hits) <= AIRA_MAX_HITS_IN_PROMPT) {
    list(selected = hits, n_dropped = 0L)
  } else {
    list(
      selected  = hits[seq_len(AIRA_MAX_HITS_IN_PROMPT)],
      n_dropped = length(hits) - AIRA_MAX_HITS_IN_PROMPT
    )
  }
}

# Builder for summary_v1. Takes a run_dte() return, returns the user message.
.user_builder_summary_v1 <- function(result) {
  file_name  <- result$file           %||% "(unknown file)"
  type_label <- result$type_label     %||% (result$file_type %||% "unknown")
  classif    <- result$classification %||% "UNCERTAIN"
  hits       <- result$hits           %||% list()

  sel <- .select_hits_summary_v1(hits)
  hit_lines <- vapply(sel$selected, .fmt_hit_summary_v1, character(1))

  parts <- c(
    sprintf("File: %s", file_name),
    sprintf("Type: %s", type_label),
    sprintf("Classification: %s", classif),
    "",
    "Rule findings:"
  )
  if (length(hit_lines) == 0) {
    parts <- c(parts, "- (no rule findings)")
  } else {
    parts <- c(parts, hit_lines)
    if (sel$n_dropped > 0L) {
      parts <- c(parts, sprintf("- ... and %d further finding(s) of lower severity", sel$n_dropped))
    }
  }
  parts <- c(parts, "", "Write the summary now.")
  paste(parts, collapse = "\n")
}

# Validator for summary_v1. Returns TRUE if the response is well-formed,
# or a short character(1) reason string if not.
.validator_summary_v1 <- function(text) {
  if (!is.character(text) || length(text) != 1L || is.na(text))
    return("not a single character string")
  text <- trimws(text)
  if (!nzchar(text))              return("empty")
  n <- nchar(text)
  if (n < 20L)                    return("too short")
  if (n > 500L)                   return("too long")
  # Forbid Markdown structure, bullets, code fences
  if (grepl("(^|\n)\\s*#",        text, perl = TRUE))  return("contains markdown heading")
  if (grepl("(^|\n)\\s*[-*\u2022]\\s", text, perl = TRUE)) return("contains bullet list")
  if (grepl("```",                text, fixed = TRUE)) return("contains code fence")
  # Forbid injection-echo openings
  lead <- tolower(substr(text, 1L, 30L))
  bad_leads <- c("system:", "user:", "assistant:",
                 "i am an ai", "as an ai", "as a language model",
                 "i'm an ai", "i am a language model")
  for (bl in bad_leads) {
    if (startsWith(lead, bl)) return(sprintf("opens with '%s'", bl))
  }
  # Require at least one sentence terminator character to actually appear
  # in the text. Without this check, a run-on string with no punctuation
  # passes because strsplit on a pattern that doesn't match returns the
  # whole input as a single piece.
  if (!grepl("[.!?]", text, perl = TRUE))
    return("no sentence terminator")
  # Split on [.!?] followed by whitespace or end-of-string; count
  # non-empty pieces to bound sentence count.
  pieces <- strsplit(text, "[.!?](\\s|$)", perl = TRUE)[[1]]
  pieces <- pieces[nzchar(trimws(pieces))]
  n_sent <- length(pieces)
  if (n_sent < 1L) return("no sentence terminator")
  if (n_sent > 4L) return(sprintf("%d sentences (expected 1-2, tolerated up to 4)", n_sent))
  TRUE
}


# ── Batch summary helpers (for batch_summary_v1) ────────────────────────────
#
# Batch summary aggregates rule output across all files. We truncate
# aggressively so the prompt stays bounded in token count even for large
# batches. Caps:
#   - per file:  at most AIRA_MAX_HITS_IN_PROMPT non-GREEN hits, ordered by severity
#   - per batch: at most AIRA_MAX_FILES_IN_BATCH_PROMPT non-GREEN files, ordered by severity
# GREEN files are excluded from the detailed file list but counted in the
# clean-file total.

AIRA_MAX_FILES_IN_BATCH_PROMPT <- 20L

# Return the severity rank of a classification. Lower = more severe.
.severity_rank <- function(cl) {
  switch(cl %||% "GREEN",
    "RED"       = 1L,
    "UNCERTAIN" = 2L,
    "AMBER"     = 3L,
    "GREEN"     = 4L,
    5L)
}

# Reduce a single result to a compact structure for the batch prompt.
# Uses the same hit filtering (.select_hits_summary_v1) as the per-file
# prompt so we don't repeat truncation logic.
.compact_result_for_batch <- function(r) {
  sel <- .select_hits_summary_v1(r$hits %||% list())
  hit_lines <- vapply(sel$selected, .fmt_hit_summary_v1, character(1))
  list(
    file           = r$file %||% "(unknown)",
    type_label     = r$type_label %||% (r$file_type %||% "unknown"),
    classification = r$classification %||% "GREEN",
    hit_lines      = hit_lines,
    n_dropped      = sel$n_dropped
  )
}

# Builder for batch_summary_v1. Takes the full res list, returns the user
# message. Deterministic; golden-testable.

# ── Batch summary v2: rules + per-file AI review synthesis ─────────────
#
# Takes a list with two entries: `res` (standard rule-engine results) and
# `aira_reviews` (named list keyed by filepath, values are canonical 5-
# field AIRA responses with status="ok" for completed reviews).
#
# Produces a richer prompt than v1: for each non-GREEN file, includes
# BOTH the rule-engine findings AND the AI's per-file risk_level and
# one-sentence assessment (Level 2 detail). This lets the summariser
# produce a holistic summary that can note where the rule engine and AI
# agree, where they disagree, and how the AI review materially shifts
# the picture for individual files.
#
# If a file has no AIRA review (GREEN files, or AIRA still in flight
# somehow - normally the button that triggers v2 is disabled until all
# non-GREEN files have a review, but we handle absence defensively),
# its block omits the AI section.
#
# Malformed AI responses (JSON didn't parse) are treated by showing a
# raw-text snippet prefix rather than skipping the file - reviewer-
# valuable signal may still be in unstructured AI output.

# Validator for batch_summary_v1. Accepts either a short "all clean"
# response (1 sentence, shorter length) or a full paragraph (3-5 sentences,
# longer). Same forbidden structures as per-file.
.validator_batch_summary_v1 <- function(text) {
  if (!is.character(text) || length(text) != 1L || is.na(text))
    return("not a single character string")
  text <- trimws(text)
  if (!nzchar(text)) return("empty")
  n <- nchar(text)
  if (n < 20L)    return("too short")
  if (n > 1200L)  return("too long")
  # Structure forbidden same as summary_v1
  if (grepl("(^|\n)\\s*#",             text, perl = TRUE))  return("contains markdown heading")
  if (grepl("(^|\n)\\s*[-*\u2022]\\s", text, perl = TRUE))  return("contains bullet list")
  if (grepl("```",                     text, fixed = TRUE)) return("contains code fence")
  # Injection-echo openings
  lead <- tolower(substr(text, 1L, 30L))
  bad_leads <- c("system:", "user:", "assistant:",
                 "i am an ai", "as an ai", "as a language model",
                 "i'm an ai", "i am a language model")
  for (bl in bad_leads) {
    if (startsWith(lead, bl)) return(sprintf("opens with '%s'", bl))
  }
  if (!grepl("[.!?]", text, perl = TRUE))
    return("no sentence terminator")
  # Sentence count bounds: 1 to 7. A one-sentence "all clean" is valid;
  # a 3-5 sentence paragraph is the norm; 6-7 is tolerated for large
  # batches with genuinely many at-risk files.
  pieces <- strsplit(text, "[.!?](\\s|$)", perl = TRUE)[[1]]
  pieces <- pieces[nzchar(trimws(pieces))]
  n_sent <- length(pieces)
  if (n_sent < 1L) return("no sentence terminator")
  if (n_sent > 7L) return(sprintf("%d sentences (expected 1-7)", n_sent))
  TRUE
}


# ── batch_summary_v3 builder (consumes v2 per-file responses) ─────────────
#
# v3 (2026-04-28) extends v2's input format to consume the richer
# structured signal from disclosure_review_v2 per-file responses. Where
# v2 only sent the AI's risk_level and one-sentence assessment, v3
# additionally surfaces:
#
#   - INSUFFICIENT risk level (new in disclosure_review_v2): files
#     where the AI could not meaningfully assess content, distinct from
#     genuine LOW. The summariser must distinguish these in narrative.
#   - Concern flag names (e.g. direct_identifiers_present,
#     small_count_risk, content_unreadable). Flag names enable the
#     summariser to identify cross-file patterns without flooding the
#     prompt with full per-file explanations.
#   - Implicit blind-spot signal: if many files have content_unreadable
#     or metadata_only flags, the summariser can note this is a batch
#     where reviewer judgement is essential.
#
# The builder accepts either v1 or v2 per-file responses. v1 responses
# have no concerns array, so the per-file block degrades gracefully -
# just risk_level and assessment, same as v2 prompt format. This means
# v3 works correctly during the cutover period where some cached
# responses are v1 and new ones are v2.
.user_builder_batch_summary_v3 <- function(input) {
  # Same input contract as v2: list(res, aira_reviews) or bare res.
  if (is.list(input) && !is.null(input$res)) {
    res          <- input$res
    aira_reviews <- input$aira_reviews %||% list()
  } else {
    res          <- input
    aira_reviews <- list()
  }

  if (length(res) == 0L) {
    return(paste(
      "Batch size: 0 files.",
      "",
      "This is an empty batch.",
      "",
      "Write the summary now.",
      sep = "\n"))
  }

  n_total <- length(res)
  classes <- vapply(res, function(r) r$classification %||% "GREEN", character(1))
  n_by <- c(
    RED       = sum(classes == "RED"),
    UNCERTAIN = sum(classes == "UNCERTAIN"),
    AMBER     = sum(classes == "AMBER"),
    GREEN     = sum(classes == "GREEN")
  )

  # All-clean batches: same fast path as v2.
  if (n_by["RED"] == 0L && n_by["UNCERTAIN"] == 0L && n_by["AMBER"] == 0L) {
    return(paste(
      sprintf("Batch size: %d files.", n_total),
      "Classifications: GREEN only.",
      "",
      "This batch is entirely clean. No rule findings, no AI review needed.",
      "",
      "Write the summary now.",
      sep = "\n"))
  }

  # Non-GREEN files ordered by severity then original index.
  non_green_idx <- which(classes != "GREEN")
  ranks <- vapply(res[non_green_idx], function(r)
    .severity_rank(r$classification), integer(1))
  ord <- order(ranks, non_green_idx)
  selected_idx <- non_green_idx[ord]
  n_non_green_total <- length(selected_idx)

  if (n_non_green_total > AIRA_MAX_FILES_IN_BATCH_PROMPT) {
    n_dropped_files <- n_non_green_total - AIRA_MAX_FILES_IN_BATCH_PROMPT
    selected_idx <- selected_idx[seq_len(AIRA_MAX_FILES_IN_BATCH_PROMPT)]
  } else {
    n_dropped_files <- 0L
  }

  # Counters for the prompt-tail summary. Aggregating concern flags
  # across files lets the summariser identify cross-file patterns.
  n_ai_reviewed     <- 0L
  n_ai_insufficient <- 0L
  flag_counts       <- list()  # flag_name -> count of files where it fired

  bump_flag <- function(name) {
    n <- as.character(name)[1L]
    if (!nzchar(n)) return()
    flag_counts[[n]] <<- (flag_counts[[n]] %||% 0L) + 1L
  }

  file_blocks <- vapply(selected_idx, function(idx) {
    r <- res[[idx]]
    compact <- .compact_result_for_batch(r)
    rule_cls <- compact$classification

    header <- sprintf("- %s [%s, rule: %s]:",
                      compact$file, compact$type_label, rule_cls)

    # Rule findings block (unchanged from v2)
    if (length(compact$hit_lines) == 0L) {
      rule_block <- "    (no rule findings detail)"
    } else {
      rule_block <- paste("    ", compact$hit_lines, sep = "", collapse = "\n")
      if (compact$n_dropped > 0L) {
        rule_block <- paste0(rule_block,
          sprintf("\n    ... and %d further finding(s) of lower severity",
                  compact$n_dropped))
      }
    }

    # AI review block - lookup by filepath, fallback to file name.
    # Detects v1 vs v2 by presence of 'concerns' field. v1 path produces
    # the same compact line v2 prompt used; v2 path adds a flag list.
    ai_block <- ""
    r_key <- r$filepath %||% r$file
    review <- if (!is.null(r_key)) aira_reviews[[r_key]] else NULL

    if (!is.null(review) && identical(review$status %||% "", "ok")) {
      n_ai_reviewed <<- n_ai_reviewed + 1L
      parsed <- NULL
      if (requireNamespace("jsonlite", quietly = TRUE)) {
        parsed <- tryCatch(
          jsonlite::fromJSON(review$text %||% "", simplifyVector = FALSE),
          error = function(e) NULL)
      }
      if (!is.null(parsed) && is.list(parsed$engine_alignment)) {
        # v6 detection: engine_alignment field present. v6 has a
        # different response shape (no risk_level, no assessment, no
        # concerns). Synthesise the legacy "AI: LEVEL - text" line
        # from v6's fields so the batch summary prompt format stays
        # the same.
        agrees <- as.character(parsed$engine_alignment$agrees %||% "")
        rationale <- substr(trimws(as.character(
          parsed$engine_alignment$rationale %||% "")), 1L, 200L)

        # Map v6's three-state alignment to a legacy-shaped indicator
        # that still communicates "AI's view of the engine's verdict":
        #   yes           -> AI-AGREES
        #   no            -> AI-DISAGREES
        #   cannot_assess -> AI-CANNOT-ASSESS  (counts as INSUFFICIENT
        #                                       for the aggregate counter)
        legacy_label <- switch(agrees,
          "yes"           = "AI-AGREES",
          "no"            = "AI-DISAGREES",
          "cannot_assess" = "AI-CANNOT-ASSESS",
          "AI-CANNOT-ASSESS")
        if (legacy_label == "AI-CANNOT-ASSESS") {
          n_ai_insufficient <<- n_ai_insufficient + 1L
        }

        # structure_summary stands in for assessment in the legacy
        # block.
        structure_summary <- substr(trimws(as.character(
          parsed$structure_summary %||% "")), 1L, 300L)

        # Anomalies: count and surface to the batch prompt. These
        # replace v2's concerns flags - they're aggregated across
        # files via bump_flag for cross-file pattern detection.
        anomaly_str <- ""
        anomalies <- parsed$anomalies %||% list()
        if (is.list(anomalies) && length(anomalies) > 0L) {
          for (a in anomalies) {
            if (!is.list(a)) next
            col <- as.character(a$column %||% "(file)")
            bump_flag(paste0("anomaly:", col))
          }
          anomaly_str <- sprintf(" [%d anomaly observation%s]",
                                 length(anomalies),
                                 if (length(anomalies) == 1L) "" else "s")
        }

        # Dataset recognition - high-value signal worth surfacing
        # explicitly when present.
        dr <- parsed$dataset_recognition %||% list()
        ds_str <- ""
        if (isTRUE(dr$recognised)) {
          name <- as.character(dr$name %||% "(unnamed)")
          conf <- as.character(dr$confidence %||% "?")
          ds_str <- sprintf(" [recognised: %s (%s confidence)]",
                            name, conf)
        }

        ai_block <- sprintf("    AI: %s - %s%s%s",
                            legacy_label, structure_summary,
                            ds_str, anomaly_str)
      } else if (!is.null(parsed) &&
          !is.null(parsed$risk_level) &&
          !is.null(parsed$assessment)) {
        rl <- toupper(as.character(parsed$risk_level)[1L])
        # Accept v2's INSUFFICIENT in addition to the v1 levels.
        if (!rl %in% c("LOW","MEDIUM","HIGH","UNCERTAIN","INSUFFICIENT")) {
          rl <- "UNCERTAIN"
        }
        if (identical(rl, "INSUFFICIENT")) {
          n_ai_insufficient <<- n_ai_insufficient + 1L
        }
        assessment <- substr(trimws(as.character(parsed$assessment)), 1L, 300L)

        # v2 detection: presence of concerns array. Extract flag names
        # only (no explanations). Flag names go into the prompt and
        # also into the cross-file aggregate counter.
        flag_str <- ""
        if (is.list(parsed$concerns) && length(parsed$concerns) > 0L) {
          flags <- vapply(parsed$concerns,
                          function(f) as.character(f)[1L], character(1))
          flags <- flags[nzchar(flags)]
          for (f in flags) bump_flag(f)
          if (length(flags) > 0L) {
            flag_str <- sprintf(" [concerns: %s]",
                                paste(flags, collapse = ", "))
          }
        }

        ai_block <- sprintf("    AI: %s - %s%s", rl, assessment, flag_str)
      } else {
        # Malformed JSON - fall back to first 200 chars of raw text.
        raw <- substr(trimws(as.character(review$text %||% "")), 1L, 200L)
        if (nzchar(raw)) {
          ai_block <- sprintf("    AI (unstructured): %s", raw)
        }
      }
    }

    if (nzchar(ai_block)) {
      paste(header, rule_block, ai_block, sep = "\n")
    } else {
      paste(header, rule_block, sep = "\n")
    }
  }, character(1))

  classification_line <- sprintf(
    "Classifications: RED=%d, UNCERTAIN=%d, AMBER=%d, GREEN=%d",
    n_by["RED"], n_by["UNCERTAIN"], n_by["AMBER"], n_by["GREEN"])

  # Unique rule IDs that fired
  all_rules <- unlist(lapply(res, function(r)
    vapply(r$hits %||% list(), function(h) h$rule %||% "", character(1))),
    use.names = FALSE)
  all_rules <- sort(unique(all_rules[nzchar(all_rules) & all_rules != "GREEN"]))

  # Cross-file flag summary. Sorted by frequency descending so the most
  # common concerns appear first. The summariser uses this to identify
  # batch-wide patterns ("five files share direct_identifiers_present").
  flag_summary <- if (length(flag_counts) > 0L) {
    f_names <- names(flag_counts)
    f_vals  <- unlist(flag_counts, use.names = FALSE)
    ord_f   <- order(-f_vals, f_names)
    paste(sprintf("%s (%d file%s)",
                  f_names[ord_f], f_vals[ord_f],
                  ifelse(f_vals[ord_f] == 1L, "", "s")),
          collapse = ", ")
  } else {
    "(no v2 concern flags - per-file responses are v1 or pre-v2 format)"
  }

  parts <- c(
    sprintf("Batch size: %d files.", n_total),
    classification_line,
    sprintf("AI disclosure review completed for %d of %d non-GREEN file(s).",
            n_ai_reviewed, n_non_green_total),
    if (n_ai_insufficient > 0L)
      sprintf("AI returned INSUFFICIENT (could not assess content) for %d file(s).",
              n_ai_insufficient) else character(0),
    "",
    "Files with non-GREEN findings (most severe first).",
    "For each: rule engine findings and the AI's independent assessment",
    "(LOW / MEDIUM / HIGH / UNCERTAIN / INSUFFICIENT) with assessment",
    "and any concern flags from the AI's structured review.",
    "",
    paste(file_blocks, collapse = "\n\n"),
    if (n_dropped_files > 0L)
      sprintf("\n... and %d further non-GREEN file(s) of lower severity",
              n_dropped_files) else character(0),
    "",
    sprintf("Unique rule IDs fired across batch: %s",
            if (length(all_rules) > 0L) paste(all_rules, collapse = ", ")
            else "(none)"),
    "",
    sprintf("AI concern flags across batch (frequency-ordered): %s",
            flag_summary),
    "",
    "Produce a holistic 3-5 sentence summary of this batch's disclosure",
    "risk picture. Important guidance:",
    "- The rule engine and AI can disagree. When they do, say so and",
    "  briefly note which is more likely correct given the evidence.",
    "- INSUFFICIENT means the AI could not assess the file at all - it",
    "  is NOT the same as LOW. Treat INSUFFICIENT files as files where",
    "  reviewer judgement is essential. Do not summarise INSUFFICIENT",
    "  files as 'low risk'.",
    "- If specific concern flags appear across multiple files, name",
    "  them as cross-file patterns. Translate the flag name into",
    "  plain English (e.g. 'direct_identifiers_present' -> 'direct",
    "  identifiers'). Do not use the raw flag tokens in your prose.",
    "- Do not simply repeat the rule counts.",
    "- Do not mention the number of files reviewed by AI; focus on the",
    "  substantive risk picture.",
    "",
    "Write the summary now."
  )
  paste(parts, collapse = "\n")
}


# ── Disclosure review helpers (for disclosure_review_v1) ──────────────────
#
# Disclosure review is a second-opinion feature. The rule engine classifies
# a file (often RED due to strict record-level heuristics); AIRA assesses
# whether, given the column names, rule findings, and file metadata, the
# classification looks appropriate or potentially overbroad.
#
# The prompt is metadata-only: file name, size, type, classification,
# column names, and per-hit findings including any flag_cols from evidence.
# No row data is ever sent to AIRA. Columns are the only file-content
# information transmitted, and only their names (not values).
#
# The response is JSON (with graceful degradation to free-form text if
# malformed). The JSON carries a risk level (LOW/MEDIUM/HIGH/UNCERTAIN),
# a one-line assessment, a per-column classification map, and reasoning.

AIRA_MAX_COLS_IN_PROMPT     <- 60L   # truncate very-wide tables
AIRA_MAX_COL_NAME_LENGTH    <- 64L   # truncate pathologically long names

# Sampling constants (disclosure_review_v3+). These bound the
# structural sample sent to AIRA so prompts remain within budget.
# The engine (.read_for_profile / .build_sample_rows in 17_engine.R)
# reads these at runtime; load order matters - 27_aira.R sources
# before 17_engine.R.
AIRA_SAMPLE_HEAD_ROWS       <- 5L    # rows from start of file
AIRA_SAMPLE_TAIL_ROWS       <- 5L    # rows from end of file
AIRA_SAMPLE_CELL_MAX_CHARS  <- 80L   # per-cell character cap before truncation

# Content excerpt cap (disclosure_review_v4+). Defensive second cap on
# top of the engine's AIRA_CONTENT_EXCERPT_MAX_BYTES; in practice the
# two should agree (1 byte = 1 char for our content). The engine
# extracts the excerpt and trims to its byte cap; this trims again at
# the prompt-builder if anything larger somehow slips through.
AIRA_CONTENT_EXCERPT_MAX_CHARS <- 4096L

# Truncate a column name list for the prompt.
.truncate_col_names <- function(col_names) {
  if (is.null(col_names) || !is.character(col_names)) return(list(
    names = character(0), dropped = 0L))
  # Truncate individual names to bound prompt size
  trimmed <- substr(col_names, 1L, AIRA_MAX_COL_NAME_LENGTH)
  if (length(trimmed) <= AIRA_MAX_COLS_IN_PROMPT) {
    return(list(names = trimmed, dropped = 0L))
  }
  list(
    names   = trimmed[seq_len(AIRA_MAX_COLS_IN_PROMPT)],
    dropped = length(trimmed) - AIRA_MAX_COLS_IN_PROMPT
  )
}

# Format size_bytes as a human-readable string.
.fmt_size_disc <- function(size_bytes) {
  if (is.null(size_bytes) || !is.numeric(size_bytes) || is.na(size_bytes))
    return("unknown size")
  if (size_bytes < 1024)            return(sprintf("%d B",  as.integer(size_bytes)))
  if (size_bytes < 1048576)         return(sprintf("%.1f KB", size_bytes / 1024))
  if (size_bytes < 1073741824)      return(sprintf("%.1f MB", size_bytes / 1048576))
  sprintf("%.2f GB", size_bytes / 1073741824)
}

# Format a single hit for disclosure-review, including flag_cols when present.
.fmt_hit_disc_review_v1 <- function(h) {
  rule    <- h$rule    %||% "?"
  outcome <- h$outcome %||% "?"
  detail  <- h$detail  %||% ""
  ev_cols <- h$evidence$flag_cols %||% character(0)
  base <- if (nzchar(detail))
    sprintf("- %s (%s): %s", rule, outcome, detail)
  else
    sprintf("- %s (%s)", rule, outcome)
  if (length(ev_cols) > 0) {
    shown <- head(ev_cols, 20L)
    suffix <- if (length(ev_cols) > 20L)
      sprintf(" (+%d more)", length(ev_cols) - 20L)
    else ""
    paste0(base, " [flagged columns: ",
           paste(shown, collapse=", "), suffix, "]")
  } else {
    base
  }
}

# Builder for disclosure_review_v1.

# Validator for disclosure_review_v1.
#
# Response is expected to be parseable as JSON with keys:
#   risk_level: one of "LOW", "MEDIUM", "HIGH", "UNCERTAIN"
#   assessment: string, one sentence
#   column_classifications: object mapping column_name -> category
#     where category is one of:
#       "direct_id", "quasi_id", "sensitive", "non_identifying", "unknown"
#   reasoning: string, 2-3 sentences
#
# Malformed JSON is NOT rejected here - we want the response to reach the
# UI so the reviewer sees *something* useful even if the model went off-
# format. Server-side parsing with fallback to raw text is the UI's job.
# This validator only enforces minimal liveness: non-empty string, not
# pathologically short, not a prompt-injection echo.


# ── Disclosure review helpers (for disclosure_review_v2) ──────────────────
#
# v2 design (2026-04-28): observations + concern flags rather than
# verdicts. The AI produces a structured response with:
#   - risk_level (5 levels including new INSUFFICIENT for absence-of-
#     evidence cases - file unreadable, metadata-only, etc.)
#   - assessment (1-2 sentence factual description of what the file is)
#   - concerns (array of named flags from a fixed vocabulary)
#   - concern_explanations (object mapping each flag to a one-line reason)
#   - blind_spots (array of things the AI could not evaluate)
#   - reasoning (paragraph explaining the risk_level choice)
#   - reviewer_focus (what specifically the reviewer should check)
#   - column_classifications (same as v1)
#
# The flag vocabulary is fixed in the system prompt. The AI is forbidden
# from inventing flag names. This makes the response greppable for
# downstream tooling (batch summary, PDF report) and gives reviewers a
# consistent structured signal rather than free-form prose.

# Builder for disclosure_review_v2. Same input shape as v1 - the system
# prompt does the work of asking for the new response shape.

# Validator for disclosure_review_v2.
#
# Same minimal liveness checks as v1, but with a higher upper bound on
# response length because v2 has more fields (concerns array, concern
# explanations, blind spots, reviewer focus). A typical v2 response is
# ~700-1500 characters; allowing up to 8000 gives margin without
# accepting pathologically large outputs.


# ── Disclosure review helpers (for disclosure_review_v3) ──────────────────
#
# v3 design (2026-04-30): bounded sample data + per-column profiles.
# v2 operated on column names + rule findings only and produced
# observations like "id has 100% uniqueness, therefore direct
# identifier" that were technically true but uninformative when the
# id was actually a row index. v3 sees head + tail sample rows and
# per-column type/range/distinct profiles, allowing it to
# distinguish row indices from real identifiers, enums from
# free-text, and aggregate values from per-row content.
#
# Response shape is identical to v2 - PDF report, batch summary,
# and UI consume the same JSON schema. The validator
# (.validator_disclosure_review_v2) is reused; it is keyed by shape
# not by prompt version.
#
# Input shape extends v2's with two new optional fields on result:
#   $column_profiles : named list of fixed-shape per-column profiles,
#                      one per entry in $col_names (named match).
#                      list() = no profiles available.
#   $sample_rows     : data.frame with .position column ("head"/"tail")
#                      plus the file's columns; cells already truncated
#                      to AIRA_SAMPLE_CELL_MAX_CHARS. NULL = no sample
#                      available (PDFs, binary files, parse failure).
#
# When either field is empty/NULL, the builder renders a "no
# profile/sample available" line and the system prompt tells AIRA to
# choose INSUFFICIENT.

# Format a single column profile for the prompt. Deterministic.
.fmt_col_profile_v3 <- function(col_name, prof) {
  if (is.null(prof)) {
    return(sprintf("- %s: (no profile available)", col_name))
  }
  type     <- prof$type           %||% "unknown"
  distinct <- prof$distinct_count %||% NA_integer_
  nulls    <- prof$null_count     %||% NA_integer_
  total    <- prof$total_count    %||% NA_integer_

  parts <- c(sprintf("type=%s", type))
  if (!is.na(distinct)) parts <- c(parts, sprintf("distinct=%d", as.integer(distinct)))
  if (!is.na(nulls))    parts <- c(parts, sprintf("null=%d",     as.integer(nulls)))
  if (!is.na(total))    parts <- c(parts, sprintf("total=%d",    as.integer(total)))

  # Numeric range OR string length stats - never both.
  prof_min <- prof$min %||% NA_real_
  prof_max <- prof$max %||% NA_real_
  prof_mnl <- prof$min_length %||% NA_integer_
  prof_mxl <- prof$max_length %||% NA_integer_

  if (type %in% c("integer","numeric") &&
      !is.na(prof_min) && !is.na(prof_max)) {
    parts <- c(parts, sprintf("range=[%s,%s]",
                              format(prof_min, scientific=FALSE, trim=TRUE),
                              format(prof_max, scientific=FALSE, trim=TRUE)))
  } else if (type == "character" &&
             !is.na(prof_mnl) && !is.na(prof_mxl)) {
    parts <- c(parts, sprintf("length=[%d,%d]",
                              as.integer(prof_mnl),
                              as.integer(prof_mxl)))
  }

  sprintf("- %s: %s", col_name, paste(parts, collapse = ", "))
}

# Format the sample data frame as an aligned text block. Cells are
# already truncated by the engine. Returns character(0) if nothing
# usable is present.
.fmt_sample_block_v3 <- function(sample_rows) {
  if (is.null(sample_rows) || !is.data.frame(sample_rows) ||
      nrow(sample_rows) == 0L) {
    return(character(0))
  }

  cols <- setdiff(names(sample_rows), ".position")
  if (length(cols) == 0L) return(character(0))

  # Truncate column count for prompt budget. Reuses v2's constant.
  if (length(cols) > AIRA_MAX_COLS_IN_PROMPT) {
    cols_used    <- cols[seq_len(AIRA_MAX_COLS_IN_PROMPT)]
    cols_dropped <- length(cols) - AIRA_MAX_COLS_IN_PROMPT
  } else {
    cols_used    <- cols
    cols_dropped <- 0L
  }

  # Render head and tail blocks separately so AIRA sees the structure.
  has_position <- ".position" %in% names(sample_rows)
  out <- character(0)
  for (pos in c("head", "tail")) {
    rows_pos <- if (has_position) {
      sample_rows[sample_rows$.position == pos, , drop = FALSE]
    } else if (pos == "head") {
      sample_rows
    } else {
      sample_rows[integer(0), , drop = FALSE]
    }
    if (nrow(rows_pos) == 0L) next

    label <- if (pos == "head") "First rows:" else "Last rows:"
    out <- c(out, label)
    out <- c(out, paste(cols_used, collapse = " | "))
    for (i in seq_len(nrow(rows_pos))) {
      vals <- vapply(cols_used, function(cn) {
        v <- rows_pos[[cn]][i]
        if (is.na(v)) "<NA>" else as.character(v)
      }, character(1))
      out <- c(out, paste(vals, collapse = " | "))
    }
    out <- c(out, "")
  }

  if (cols_dropped > 0L) {
    out <- c(out,
             sprintf("(... and %d further column(s) truncated for prompt budget)",
                     cols_dropped))
  }

  out
}

# Builder for disclosure_review_v3.


# ── Disclosure review helpers (for disclosure_review_v4) ──────────────────
#
# v4 design (2026-04-30): per-file-type framing + content excerpt for
# non-tabular files. v3 only had structured profiles for
# tabular/statistical files; everything else (VCF, PDF, source code,
# JSON, etc.) reached AIRA as bare metadata + rule findings, and AIRA
# correctly chose INSUFFICIENT for "binary or unparseable" - even though
# the inspectors had read the content for their rules.
#
# v4 input shape extends v3's with two new fields on result:
#   $type_framing    : 1-3 sentence orientation describing the file
#                      type and its disclosure-relevant features.
#                      Always populated (defaults to a generic message
#                      for unrecognised types).
#   $content_excerpt : a textual excerpt for non-tabular files - VCF
#                      header + first data lines, source code lines,
#                      extracted PDF text, archive listing, sheet
#                      names, etc. NULL for tabular files (use
#                      sample_rows instead) and for genuinely binary
#                      files (image, dicom, nifti, binary).
#
# Response shape unchanged from v2/v3 - the validator
# .validator_disclosure_review_v2 is reused.

# Truncate a content excerpt for the prompt. The engine has already
# capped at ~4KB; this is a defensive second cap in case a bigger
# excerpt arrives. Returns a single character string.
.truncate_excerpt_v4 <- function(excerpt,
                                 max_chars = AIRA_CONTENT_EXCERPT_MAX_CHARS) {
  if (is.null(excerpt) || !is.character(excerpt)) return(NULL)
  txt <- if (length(excerpt) > 1L) paste(excerpt, collapse = "\n") else excerpt[1L]
  if (is.na(txt) || !nzchar(txt)) return(NULL)
  if (nchar(txt) > max_chars) {
    txt <- paste0(substr(txt, 1L, max_chars - 1L), "\u2026")
  }
  txt
}

# Builder for disclosure_review_v4.


# ── Disclosure review helpers (for disclosure_review_v6) ──────────────────
#
# v6 design (2026-04-30): redesigned response shape.
#
# v3->v5 had AIRA do mechanical work the engine already does well
# (per-column classifications, paraphrasing rule findings, reciting
# blind spots). For tabular files this produced 3-4KB of output mostly
# made of repetitive column entries. The bottleneck on workspace LLMs
# is decode time, which scales linearly with output - so output size
# matters as much as input size.
#
# v6 reframes AIRA's role: do what only AIRA can do.
#   - Recognise published / well-known datasets (UCI, NHANES, MIMIC,
#     UK Biobank, etc.) - high-value contextual signal the engine
#     cannot produce
#   - Spot anomalies the engine missed - column name vs values
#     mismatches, semantic oddities
#   - Take a position on the engine's classification (agree/disagree/
#     cannot_assess) with a rationale
#   - Tell the reviewer what to actually look at
#
# What's gone vs v5:
#   - column_classifications (the engine already classifies columns;
#     AIRA's per-column verdicts mostly duplicated this)
#   - risk_level (replaced by engine_alignment - single source of
#     authority on classification, with AIRA as peer reviewer)
#   - concerns / concern_explanations (redundant with engine findings)
#   - blind_spots (folded into engine_alignment.rationale when relevant)
#   - reasoning (folded into engine_alignment.rationale)
#
# What's new vs v5:
#   - dataset_recognition with conservative confidence levels
#   - anomalies as a list of "things the engine missed", expected to
#     be empty most of the time
#   - engine_alignment as three-state agreement statement
#
# Response shape:
# {
#   "dataset_recognition": {
#     "recognised": <bool>,
#     "name": <string|null>,
#     "confidence": "low"|"medium"|"high"|null,
#     "evidence": <string|null>
#   },
#   "structure_summary": <string, 1-2 sentences, mandatory>,
#   "anomalies": [
#     {"column": <string|null>, "observation": <string>}
#   ],
#   "engine_alignment": {
#     "agrees": "yes"|"no"|"cannot_assess",
#     "rationale": <string, 1-2 sentences>
#   },
#   "reviewer_focus": <string, 1 sentence>
# }
#
# Input pruning:
#   - For tabular files, column_profiles include only "interesting"
#     columns (engine-flagged, high-cardinality, low-cardinality, or
#     generically-named). Computed by the engine via
#     .select_interesting_columns_v6. All column names still listed -
#     pruning affects detail level, not visibility.
#   - Sample data unchanged - still head + tail rows, all columns.
#   - Content excerpt unchanged for non-tabular files.
#
# v5 stays available for rollback. The legacy batch summary builder
# (.user_builder_batch_summary_v3) detects v6 responses by presence
# of engine_alignment field and extracts equivalents.

# Validator for disclosure_review_v6. Different shape from v2/v3/v4/v5
# so a new validator is needed. Checks the structural contract: parses
# as JSON, has the right top-level fields with the right types,
# enum values for confidence and agrees match the vocabulary.
.validator_disclosure_review_v6 <- function(text) {
  if (!is.character(text) || length(text) != 1L || is.na(text))
    return("not a single character string")
  text <- trimws(text)
  if (!nzchar(text))      return("empty")
  if (nchar(text) > 8000L) return("too long")
  if (nchar(text) < 50L)  return("too short")

  # Forbid injection-echo openings.
  lead <- tolower(substr(text, 1L, 30L))
  bad_leads <- c("system:", "user:", "assistant:",
                 "i am an ai", "as an ai", "as a language model",
                 "i'm an ai", "i am a language model")
  for (bl in bad_leads) {
    if (startsWith(lead, bl)) return(sprintf("opens with '%s'", bl))
  }

  # Strip code fences if any escaped past the entry point.
  if (grepl("^```", text)) {
    text <- sub("^```[a-zA-Z]*\\s*", "", text)
    text <- sub("```\\s*$",          "", text)
    text <- trimws(text)
  }

  parsed <- tryCatch(
    jsonlite::fromJSON(text, simplifyVector = FALSE),
    error = function(e) NULL)
  if (is.null(parsed) || !is.list(parsed))
    return("not valid JSON object")

  required <- c("dataset_recognition", "structure_summary",
                "anomalies", "engine_alignment", "reviewer_focus")
  missing <- setdiff(required, names(parsed))
  if (length(missing) > 0L)
    return(sprintf("missing field: %s", paste(missing, collapse = ", ")))

  # dataset_recognition shape
  dr <- parsed$dataset_recognition
  if (!is.list(dr))                       return("dataset_recognition not an object")
  if (is.null(dr$recognised))             return("dataset_recognition.recognised missing")
  if (!is.logical(dr$recognised))         return("dataset_recognition.recognised not boolean")
  if (isTRUE(dr$recognised)) {
    if (is.null(dr$name) || !nzchar(as.character(dr$name)))
      return("dataset_recognition.name required when recognised=true")
    conf <- as.character(dr$confidence %||% "")
    if (!conf %in% c("low","medium","high"))
      return("dataset_recognition.confidence must be low/medium/high when recognised=true")
    if (is.null(dr$evidence) || !nzchar(as.character(dr$evidence)))
      return("dataset_recognition.evidence required when recognised=true")
  }

  # structure_summary
  if (!is.character(parsed$structure_summary) ||
      length(parsed$structure_summary) != 1L ||
      !nzchar(parsed$structure_summary))
    return("structure_summary must be a non-empty string")

  # anomalies must be a list (possibly empty)
  if (!is.list(parsed$anomalies))
    return("anomalies not a list")
  for (i in seq_along(parsed$anomalies)) {
    a <- parsed$anomalies[[i]]
    if (!is.list(a))
      return(sprintf("anomalies[%d] not an object", i))
    if (is.null(a$observation) || !nzchar(as.character(a$observation)))
      return(sprintf("anomalies[%d].observation missing", i))
  }

  # engine_alignment shape
  ea <- parsed$engine_alignment
  if (!is.list(ea))                       return("engine_alignment not an object")
  agrees <- as.character(ea$agrees %||% "")
  if (!agrees %in% c("yes","no","cannot_assess"))
    return("engine_alignment.agrees must be yes/no/cannot_assess")
  if (is.null(ea$rationale) || !nzchar(as.character(ea$rationale)))
    return("engine_alignment.rationale missing")

  # reviewer_focus
  if (!is.character(parsed$reviewer_focus) ||
      length(parsed$reviewer_focus) != 1L ||
      !nzchar(parsed$reviewer_focus))
    return("reviewer_focus must be a non-empty string")

  TRUE
}

# Format a single column profile for v6. Same as v3's formatter -
# shape unchanged.
.fmt_col_profile_v6 <- .fmt_col_profile_v3

# Format the sample data block - unchanged from v3.
.fmt_sample_block_v6 <- .fmt_sample_block_v3

# Builder for disclosure_review_v6. Like v4 but with column-profile
# pruning: only "interesting" columns get detail, the rest are listed
# by name only.
.user_builder_disclosure_review_v6 <- function(result) {
  file_name  <- result$file           %||% "(unknown file)"
  type_label <- result$type_label     %||% (result$file_type %||% "unknown")
  size_bytes <- result$size_bytes
  classif    <- result$classification %||% "UNCERTAIN"
  score      <- result$score          %||% NA_integer_
  col_names  <- result$col_names      %||% character(0)
  hits       <- result$hits           %||% list()
  profiles   <- result$column_profiles %||% list()
  sample_df  <- result$sample_rows
  framing    <- result$type_framing   %||%
                "This is a file of unrecognised type."
  excerpt    <- .truncate_excerpt_v4(result$content_excerpt)

  # Hits, sorted and capped.
  sel <- .select_hits_summary_v1(hits)
  hit_lines <- vapply(sel$selected, .fmt_hit_disc_review_v1, character(1))

  # Column names - all of them (capped). Visible to AIRA so dataset
  # recognition and anomaly detection still work even when profiles
  # are pruned.
  has_col_names <- length(col_names) > 0L
  col_block <- if (!has_col_names) {
    NULL
  } else {
    tc <- .truncate_col_names(col_names)
    if (tc$dropped > 0L) {
      paste0(paste(tc$names, collapse = ", "),
             sprintf(" ... and %d further column(s) truncated", tc$dropped))
    } else {
      paste(tc$names, collapse = ", ")
    }
  }

  # Column profiles - PRUNED to interesting columns only.
  has_profiles <- length(profiles) > 0L
  profile_lines <- NULL
  pruned_count <- 0L
  if (has_profiles) {
    interesting <- if (exists(".select_interesting_columns_v6", mode = "function")) {
      .select_interesting_columns_v6(col_names, profiles, hits)
    } else {
      # Fallback: if the engine helper isn't loaded for any reason,
      # render all profiles (safer to over-include than to break).
      col_names
    }
    pruned_count <- length(col_names) - length(interesting)

    if (length(interesting) == 0L && length(col_names) > 0L) {
      # No columns met the "interesting" bar - typical for a clean
      # file. Skip the profile block entirely; column names alone are
      # enough at this point.
      profile_lines <- NULL
    } else {
      lines <- vapply(interesting,
                      function(cn) .fmt_col_profile_v6(cn, profiles[[cn]]),
                      character(1))
      if (pruned_count > 0L) {
        lines <- c(lines,
                   sprintf("- (%d further column(s) - profiles omitted; values visible in sample data)",
                           pruned_count))
      }
      profile_lines <- lines
    }
  }

  # Sample data block - unchanged.
  sample_lines <- .fmt_sample_block_v6(sample_df)
  has_sample   <- length(sample_lines) > 0L

  parts <- c(
    sprintf("File: %s", file_name),
    sprintf("Type: %s", type_label),
    sprintf("Size: %s", .fmt_size_disc(size_bytes)),
    sprintf("Rule-engine classification: %s (score %s/100)",
            classif,
            if (is.na(score)) "?" else as.character(as.integer(score))),
    "",
    "File-type orientation:",
    framing,
    ""
  )

  if (has_col_names) {
    parts <- c(parts,
               sprintf("Column count: %d", length(col_names)),
               "Column names:",
               col_block,
               "")
  }

  if (!is.null(profile_lines)) {
    parts <- c(parts,
               "Column profiles (interesting columns only):",
               profile_lines,
               "")
  }

  if (has_sample) {
    parts <- c(parts, "Sample data:", sample_lines, "")
  }

  if (!is.null(excerpt) && nzchar(excerpt)) {
    parts <- c(parts,
               sprintf("Content excerpt (truncated to %d bytes):",
                       AIRA_CONTENT_EXCERPT_MAX_CHARS),
               excerpt,
               "")
  }

  if (is.null(profile_lines) && !has_sample &&
      (is.null(excerpt) || !nzchar(excerpt))) {
    parts <- c(parts,
               "Content access:",
               paste("(No column profiles, sample data, or content excerpt",
                     "available - assessment must rely on file metadata,",
                     "type orientation, and rule findings only)"),
               "")
  }

  parts <- c(parts, "Rule findings:")
  if (length(hit_lines) == 0L) {
    parts <- c(parts, "- (no rule findings)")
  } else {
    parts <- c(parts, hit_lines)
    if (sel$n_dropped > 0L) {
      parts <- c(parts,
                 sprintf("- ... and %d further finding(s) of lower severity",
                         sel$n_dropped))
    }
  }
  parts <- c(parts, "",
             "Produce your structured assessment now. Reply with the JSON schema specified in the system instructions only.")
  paste(parts, collapse = "\n")
}


# ── ACRO context for disclosure_review_v7 ─────────────────────────────────
# Formats the ACRO session metadata attached to a member result
# (result$acro_outputs) into a prompt block. This is CONTEXT for richer
# observation, not a verdict for AIRA to ratify. The researcher's comments
# and exceptions are the researcher's own CLAIMS - the block frames them as
# such so AIRA checks them against the file rather than treating them as
# established fact. AIRA's role and output shape are unchanged from v6; it
# still does not produce a competing verdict, and ACRO/the engine remain the
# authorities. See R-AIRA-05 (input shaping) and PRS 14.1.
.fmt_acro_context_v7 <- function(acro_outputs) {
  if (is.null(acro_outputs) || length(acro_outputs) == 0L) return(NULL)

  `%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b
  one <- function(o) {
    lines <- character(0)
    uid    <- o$uid    %||% "(unnamed output)"
    status <- toupper(as.character(o$status %||% "unknown"))
    method <- o$method %||% NULL
    hdr <- sprintf("- ACRO output '%s': SDC status %s", uid, status)
    if (!is.null(method) && nzchar(method)) hdr <- paste0(hdr, sprintf(" (%s)", method))
    lines <- c(lines, hdr)

    summ <- o$summary %||% NULL
    if (!is.null(summ) && nzchar(summ)) {
      lines <- c(lines, sprintf("    ACRO test summary: %s", summ))
    }

    # Researcher's own claims - explicitly framed as claims, not facts.
    cmts <- o$comments %||% character(0)
    cmts <- cmts[nzchar(cmts)]
    if (length(cmts) > 0L) {
      lines <- c(lines, "    Researcher's stated comments (the researcher's own claims - verify against the file, do not treat as established fact):")
      lines <- c(lines, vapply(cmts, function(c) sprintf("      \"%s\"", c), character(1)))
    }

    if (isTRUE(o$has_exception)) {
      exc <- o$exception %||% ""
      lines <- c(lines, sprintf(
        "    Researcher requested a release exception (a claim to scrutinise): \"%s\"",
        exc))
    }
    lines
  }

  block <- unlist(lapply(acro_outputs, one), use.names = FALSE)
  if (length(block) == 0L) return(NULL)
  block
}

# Builder for disclosure_review_v7. Identical to v6 but inserts an ACRO
# context block (when the file is an ACRO package member with
# result$acro_outputs) before the rule findings. Same response shape as v6,
# so the v6 validator is reused (R-AIRA-15: validator keyed by shape).
.user_builder_disclosure_review <- function(result) {
  # Start from the exact v6 body, then splice the ACRO block in before the
  # closing instruction. Reusing the v6 builder keeps the two in lockstep
  # for everything except the added context.
  base <- .user_builder_disclosure_review_v6(result)

  acro_lines <- .fmt_acro_context_v7(result$acro_outputs %||% NULL)
  if (is.null(acro_lines)) {
    # No ACRO context (standalone file): v7 behaves exactly as v6.
    return(base)
  }

  acro_block <- paste(c(
    "",
    "ACRO analysis-time context (for richer observation, NOT a verdict to ratify):",
    "This file was produced under ACRO, which ran formal SDC tests against the",
    "underlying microdata at analysis time - information you cannot see directly.",
    "ACRO and the rule engine remain the authorities on classification. Use this",
    "context to sharpen your observations: note where the file's structure appears",
    "consistent or inconsistent with what ACRO and the researcher state, and flag",
    "anything they may have missed. The researcher's comments are their own claims;",
    "check them against the observable file rather than accepting them as fact.",
    acro_lines,
    ""
  ), collapse = "\n")

  # Splice the ACRO block in just before the final 'Produce your structured
  # assessment now' instruction so it sits with the other input context.
  marker <- "Produce your structured assessment now."
  if (grepl(marker, base, fixed = TRUE)) {
    base <- sub(marker, paste0(sub("\n*$", "", acro_block), "\n\n", marker),
                base, fixed = TRUE)
    base
  } else {
    # Marker not found (shouldn't happen) - append rather than lose context.
    paste(base, acro_block, sep = "\n")
  }
}


# ── disclosure_review_v8: ACRO consistency observation ────────────────────
# v8 adds ONE optional output field to the v6/v7 shape: acro_consistency.
# It is present only for ACRO members and carries AIRA's observation of
# whether the file's observable structure is consistent with what ACRO and
# the researcher claimed. It is PURELY INFORMATIONAL - it never changes a
# classification; the engine decides and ACRO leads (PRS 14.1). It is an
# observation of claim-vs-file match, never a release verdict. Because the
# output shape changed (new field), v8 has its own validator per R-AIRA-15.
# v8's user_builder is v7's (it already supplies the ACRO context the model
# needs). For standalone files the model returns acro_consistency with
# assessed="not_applicable" (or omits it); the renderer shows it only when
# assessed="yes".

.validator_disclosure_review <- function(text) {
  # First run the v6 validator for the shared base shape. It strips fences
  # and validates the five core fields; v8 adds the optional sixth.
  base_ok <- .validator_disclosure_review_v6(text)
  if (!isTRUE(base_ok)) return(base_ok)

  text <- trimws(text)
  if (grepl("^```", text)) {
    text <- sub("^```[a-zA-Z]*\\s*", "", text)
    text <- sub("```\\s*$",          "", text)
    text <- trimws(text)
  }
  parsed <- tryCatch(jsonlite::fromJSON(text, simplifyVector = FALSE),
                     error = function(e) NULL)
  if (is.null(parsed) || !is.list(parsed)) return("not valid JSON object")

  # acro_consistency is OPTIONAL. Absent is fine (older shape / standalone
  # file where the model omitted it). When present, validate its shape.
  ac <- parsed$acro_consistency
  if (is.null(ac)) return(TRUE)
  if (!is.list(ac)) return("acro_consistency not an object")

  assessed <- as.character(ac$assessed %||% "")
  if (!assessed %in% c("yes", "no", "not_applicable"))
    return("acro_consistency.assessed must be yes/no/not_applicable")

  if (identical(assessed, "yes")) {
    consistent <- as.character(ac$consistent %||% "")
    if (!consistent %in% c("yes", "no", "partial", "cannot_determine"))
      return("acro_consistency.consistent must be yes/no/partial/cannot_determine when assessed=yes")
    if (is.null(ac$observation) || !nzchar(as.character(ac$observation)))
      return("acro_consistency.observation required when assessed=yes")
  }
  TRUE
}

# v8 builder is identical to v7 (same input including ACRO context). The
# difference between v7 and v8 is the system prompt and the validator, not
# the user message.

# v8 system addendum: instructs the model on the new acro_consistency field.
# Appended after the v7 ACRO-context addendum so v8's system = v6 base +
# v7 addendum + v8 addendum. Strong, explicit framing that the field is an
# observation of claim-vs-file match, never a release verdict, and never
# affects the classification.
.DISCLOSURE_CONSISTENCY_ADDENDUM <- paste(
  "",
  "ACRO CONSISTENCY OBSERVATION (acro_consistency field):",
  "When the input contains an 'ACRO analysis-time context' block, add one",
  "extra field to your JSON response named acro_consistency. Its job is to",
  "report whether the file's OBSERVABLE structure is consistent with what",
  "ACRO and the researcher claimed - nothing more.",
  "",
  "Shape:",
  "  \"acro_consistency\": {",
  "    \"assessed\": \"yes\"|\"no\"|\"not_applicable\",",
  "    \"consistent\": \"yes\"|\"no\"|\"partial\"|\"cannot_determine\",",
  "    \"observation\": <string, 1-2 sentences>",
  "  }",
  "",
  "- assessed=\"yes\"  when ACRO context is present and you could compare a",
  "  claim against the observable file.",
  "- assessed=\"no\"   when ACRO context is present but you could not compare",
  "  (e.g. the claim concerns suppression you cannot see in the sample).",
  "- assessed=\"not_applicable\" when there is NO ACRO context (a standalone",
  "  file). In this case set consistent=\"cannot_determine\" and observation",
  "  to an empty string. The renderer hides the field in this case.",
  "",
  "consistent: does the file match the researcher's/ACRO's account?",
  "  - \"yes\"             observable structure matches the claim",
  "  - \"no\"              observable structure contradicts the claim",
  "  - \"partial\"         matches in part, diverges in part",
  "  - \"cannot_determine\" the relevant evidence is not observable",
  "",
  "observation: state the comparison concretely, citing what the researcher",
  "claimed and what the file shows. Example: \"The researcher states six",
  "cells were suppressed; the sample shows cells marked with the suppression",
  "token, consistent with that account.\" Or: \"The researcher states",
  "suppression was applied, but the sample contains unsuppressed counts",
  "below the threshold - the file does not appear consistent with the claim.\"",
  "",
  "CRITICAL FRAMING:",
  "- This is an OBSERVATION of claim-vs-file match. It is NOT a release",
  "  verdict and NOT a safety judgement. Never write 'safe to release' or",
  "  'approve' or any decision language here.",
  "- It does NOT change any classification. The engine decides; ACRO leads.",
  "- A mismatch is something for the REVIEWER to weigh, not a conclusion you",
  "  draw. Report what you see; stop there.",
  "- Treat the researcher's comments as claims, never as established fact.",
  sep = "\n"
)


# v7 system prompt is v6's system plus an ACRO-context addendum. Defined as
# a standalone variable so it can be referenced inside the PROMPTS list
# literal (which cannot reference itself during construction). The v6 base
# is captured after PROMPTS is built (see just below the list); here we hold
# only the addendum text.
.DISCLOSURE_ACRO_ADDENDUM <- paste(
  "",
  "ACRO CONTEXT (when present in the input):",
  "Some files are outputs produced under ACRO, which ran formal SDC",
  "tests against the underlying microdata at analysis time. When the",
  "input includes an 'ACRO analysis-time context' block, use it to",
  "inform - not replace - your observations:",
  "- ACRO and the rule engine remain the authorities on the verdict.",
  "  Do NOT treat ACRO's status as a verdict to agree with; your",
  "  engine_alignment is still about the rule-engine classification.",
  "- The researcher's comments and exceptions are the researcher's",
  "  own CLAIMS. Check them against what you can observe in the file.",
  "  If the file's structure is consistent with a claim, you may say",
  "  so; if it is not, note the discrepancy as an anomaly. Never",
  "  accept a claim as fact just because the researcher stated it.",
  "- Do not let ACRO's status or the researcher's reassurance soften",
  "  an observation you would otherwise make. The reviewer needs your",
  "  independent read precisely because it may catch what the",
  "  analysis-time checks and the researcher did not.",
  sep = "\n"
)

PROMPTS <- list(
  why_red_summary = list(
    version      = "why_red_summary",
    created      = "2026-04-17",
    notes        = paste(
      "Initial version. Produces a 1-2 sentence plain-language summary",
      "of rule-engine findings for RED/AMBER/UNCERTAIN files. Input is",
      "the rule engine's output, not file contents. GREEN hits (rules",
      "that checked and found nothing) are filtered out before the",
      "prompt is built. The remaining hits are ordered by severity and",
      "capped at", AIRA_MAX_HITS_IN_PROMPT, "with a truncation marker."
    ),
    system       = paste(
      "You summarise statistical disclosure control (SDC) findings for a",
      "data egress reviewer working in a Trusted Research Environment.",
      "",
      "You are given a structured list of rules that fired on a single",
      "file. Produce a plain-language summary in ONE OR TWO sentences.",
      "",
      "Rules you must follow:",
      "- First sentence: direct identifier or credential risks, if any.",
      "- Second sentence (only if applicable): indirect risks such as",
      "  small counts, linkage risk, sensitive phenotypes, or structural",
      "  concerns. If no such risks are present, omit the second sentence.",
      "- Paraphrase the findings. Do NOT name rule IDs.",
      "- Do NOT quote or repeat any literal values from the findings",
      "  (no participant IDs, no postcodes, no NHS numbers, no names).",
      "  Counts and column names are acceptable.",
      "- Do NOT invent findings that are not in the input.",
      "- Do NOT make recommendations, suggestions, or remediation advice.",
      "- Do NOT use Markdown, bullet points, headings, or code blocks.",
      "- Plain prose only.",
      sep = "\n"
    ),
    user_builder = .user_builder_summary_v1,
    model_params = list(
      model       = "workspace-chat",
      temperature = 0,
      max_tokens  = 200L
    ),
    response_validator = .validator_summary_v1
  ),

  batch_summary = list(
    version      = "batch_summary",
    created      = "2026-04-28",
    notes        = paste(
      "Consumes disclosure_review_v2 per-file responses. Adds awareness",
      "of the new INSUFFICIENT risk level (file unreadable / metadata",
      "only - distinct from LOW) and surfaces cross-file concern-flag",
      "patterns to the summariser. Per-file prompt blocks now include",
      "concern flag names alongside risk_level and assessment, allowing",
      "the summariser to identify batch-wide patterns like 'three files",
      "share direct identifier concerns'. v2 stays available for",
      "rollback. Backward compatible: v1 per-file responses degrade",
      "gracefully, just without flag aggregation."
    ),
    system       = paste(
      "You summarise statistical disclosure control (SDC) findings across",
      "a batch of files for a data egress reviewer working in a Trusted",
      "Research Environment.",
      "",
      "You are given THREE signals for each non-GREEN file:",
      "(a) the rule engine's findings (heuristic, can over-flag),",
      "(b) the AI's independent per-file assessment with risk level",
      "    LOW / MEDIUM / HIGH / UNCERTAIN / INSUFFICIENT, and",
      "(c) optional concern flags from a fixed vocabulary that name",
      "    specific properties the AI observed",
      "    (e.g. direct_identifiers_present, small_count_risk,",
      "    content_unreadable, aggregate_statistics).",
      "",
      "You are also given a frequency-ordered batch summary of which",
      "concern flags appear across multiple files. Use this to identify",
      "cross-file patterns.",
      "",
      "Produce a holistic plain-language summary, one paragraph.",
      "",
      "Rules you must follow:",
      "- If the batch is entirely clean (zero RED, AMBER, or UNCERTAIN",
      "  findings), produce exactly ONE reassuring sentence confirming",
      "  this. Do NOT invent risks. Do NOT pad.",
      "- Otherwise, produce 3 to 5 sentences that integrate the signals:",
      "    * Identify where rule engine and AI agree on high risk -",
      "      these are the strongest concerns.",
      "    * Identify where they disagree. Rule-engine RED files that",
      "      the AI rated LOW are candidates for reviewer reassurance.",
      "      AI-rated HIGH files that the rules missed (rare) are",
      "      candidates for additional scrutiny.",
      "    * If specific concern flags appear across multiple files,",
      "      mention them as a batch-wide pattern. Translate the flag",
      "      name into plain English in your prose; do not use the raw",
      "      flag token. Examples:",
      "        direct_identifiers_present -> 'direct identifiers'",
      "        quasi_identifiers_present  -> 'combinations that could",
      "                                       re-identify'",
      "        small_count_risk           -> 'small unsuppressed counts'",
      "        sensitive_attributes_present -> 'sensitive attributes'",
      "        content_unreadable / metadata_only / binary_content ->",
      "                                       'files the AI could not",
      "                                       fully read'",
      "    * Name specific files with the highest integrated risk and",
      "      describe the concerns in plain English.",
      "- INSUFFICIENT is NOT the same as LOW. INSUFFICIENT means the AI",
      "  could not assess the file's content (unreadable, metadata-",
      "  only, binary). When INSUFFICIENT files appear in the batch,",
      "  flag them as files where reviewer judgement is essential.",
      "  Do NOT describe them as low risk.",
      "- Do NOT simply repeat the rule counts, and do NOT list every",
      "  file.",
      "- Do NOT mention the number of files reviewed by AI - focus on",
      "  substance.",
      "- You MAY name filenames and use column names.",
      "- Paraphrase the findings. Do NOT name rule IDs (e.g. 'TAB-018').",
      "- Do NOT use raw concern flag tokens in your prose. Translate",
      "  them to plain English.",
      "- Do NOT quote or repeat any literal values from the findings",
      "  (no participant IDs, no postcodes, no NHS numbers, no names).",
      "  Counts and column names are acceptable.",
      "- Do NOT invent findings that are not in the input.",
      "- Do NOT make recommendations, suggestions, or remediation",
      "  advice.",
      "- Do NOT use Markdown, bullet points, headings, or code blocks.",
      "- Plain prose, one paragraph only.",
      sep = "\n"
    ),
    user_builder = .user_builder_batch_summary_v3,
    model_params = list(
      model       = "workspace-chat",
      temperature = 0,
      max_tokens  = 500L
    ),
    response_validator = .validator_batch_summary_v1
  ),

  disclosure_review = list(
    version      = "disclosure_review",
    created      = "2026-04-30",
    notes        = paste(
      "Redesigned response shape. v3-v5 had AIRA do mechanical work",
      "the engine already does well (per-column classifications,",
      "paraphrasing rule findings, reciting blind spots) - producing",
      "3-4KB of repetitive output for a 32-column file. v6 reframes",
      "AIRA's role to do what only AIRA can do: dataset recognition,",
      "anomaly spotting, take a position on the engine's verdict.",
      "Smaller output budget, focused content. Different validator",
      "(.validator_disclosure_review_v6) because the response shape",
      "is genuinely different. v5 stays available for rollback. The",
      "batch summary builder (batch_summary_v3) detects v6 responses",
      "by presence of engine_alignment field and extracts equivalents."
    ),
    system       = paste(
      "You are a disclosure-risk advisor for a reviewer working in a",
      "Trusted Research Environment.",
      "",
      "The rule engine has already classified the file (RED/AMBER/",
      "GREEN/UNCERTAIN) using deterministic checks: regex-based",
      "identifier detection, k-anonymity, small-count thresholds,",
      "format conformance, etc. The engine is excellent at these",
      "mechanical checks. Your role is NOT to redo this work.",
      "",
      "YOUR ROLE: do what only AIRA can do.",
      "1. Recognise published or well-known datasets - UCI ML",
      "   benchmarks, NHANES, MIMIC, UK Biobank field codes,",
      "   Framingham, GTEx, ICGC, GEO deposits, 1000 Genomes,",
      "   public reference data. The engine cannot do this; you can.",
      "2. Spot anomalies the engine missed - column name vs values",
      "   mismatches, semantic oddities, structural inconsistencies.",
      "3. Take a position on the engine's classification: agree,",
      "   disagree, or cannot assess. Provide a rationale.",
      "4. Tell the reviewer what to actually look at in this file.",
      "",
      "You DO NOT classify each column. The engine already classifies",
      "columns. You DO NOT paraphrase the rule findings - the reviewer",
      "already sees those. You DO NOT recite blind spots as a list",
      "(those that matter belong in your engine_alignment rationale).",
      "",
      "INPUT YOU RECEIVE:",
      "  - file metadata, file-type orientation, rule-engine",
      "    classification, rule findings",
      "  - column names (all of them, capped at 60)",
      "  - column profiles - ONLY for columns that warrant detail",
      "    (engine-flagged, very high or very low cardinality,",
      "    or generically named). Most measurement columns are",
      "    omitted from profiles - their values are visible in the",
      "    sample data.",
      "  - sample data (head + tail rows) for tabular files",
      "  - content excerpt for non-tabular files",
      "",
      "RESPONSE SHAPE - return ONLY this JSON, no other text:",
      "{",
      "  \"dataset_recognition\": {",
      "    \"recognised\": <bool>,",
      "    \"name\": <string|null>,",
      "    \"confidence\": \"low\"|\"medium\"|\"high\"|null,",
      "    \"evidence\": <string|null>",
      "  },",
      "  \"structure_summary\": <string, 1-2 sentences>,",
      "  \"anomalies\": [",
      "    {\"column\": <string|null>, \"observation\": <string>}",
      "  ],",
      "  \"engine_alignment\": {",
      "    \"agrees\": \"yes\"|\"no\"|\"cannot_assess\",",
      "    \"rationale\": <string, 1-2 sentences>",
      "  },",
      "  \"reviewer_focus\": <string, 1 sentence>",
      "}",
      "",
      "DATASET RECOGNITION - be conservative.",
      "Set recognised=true ONLY when the combination of column names,",
      "value patterns, and structural shape uniquely identifies a",
      "specific known published dataset. Common column names like 'id',",
      "'age', 'sex', 'outcome' are insufficient on their own; you need",
      "either:",
      "  (a) a substantially complete column-name match against a",
      "      specific dataset's published schema (>=70% of columns",
      "      match expected names), OR",
      "  (b) value patterns characteristic of that dataset (e.g.",
      "      specific value ranges, identifier formats, or fingerprint",
      "      values).",
      "When recognised=true, provide:",
      "  - name: the dataset's canonical name (e.g. \"UCI Breast Cancer",
      "    Wisconsin (Diagnostic)\")",
      "  - confidence:",
      "    * high   = essentially certain (multiple distinctive matches)",
      "    * medium = column names match strongly but values not verified",
      "    * low    = resembles a known dataset but uncertain",
      "  - evidence: cite SPECIFIC column names AND/OR sample values",
      "    that support the recognition. Generic statements like",
      "    \"matches a UCI dataset\" without specific evidence are NOT",
      "    acceptable.",
      "When recognised=false, set name=null, confidence=null,",
      "evidence=null.",
      "",
      "WHEN IN DOUBT, set recognised=false. A false positive (claiming",
      "a dataset is the wrong public dataset) is worse than admitting",
      "you don't recognise it.",
      "",
      "ANOMALIES - things the engine missed.",
      "Anomalies should add information beyond the rule findings.",
      "",
      "Useful anomaly examples:",
      "  - A column named 'var3' whose sample values match NHS number",
      "    patterns (the engine doesn't read column-name-vs-values",
      "    semantics; you do).",
      "  - A column called 'age' containing values 1..N suggesting",
      "    it's actually a row index (the engine sees high",
      "    cardinality but not the semantic mismatch).",
      "  - A column called 'postcode' containing hash-like values",
      "    rather than postcodes (the engine may report 'no postcode",
      "    pattern matched' - you can say what's there instead).",
      "  - A pair of columns whose values together would re-identify",
      "    even though neither is identifying alone, that the engine's",
      "    quasi-identifier rules don't already cover.",
      "",
      "NOT useful (do NOT include):",
      "  - A column the engine has already flagged as a direct",
      "    identifier - you would just be restating the engine's",
      "    finding.",
      "  - Generic warnings about possible risks not grounded in the",
      "    sample data.",
      "  - Restating a rule finding in different words.",
      "",
      "Empty anomalies list (anomalies: []) is the EXPECTED case for",
      "most files. Do not pad the list.",
      "",
      "ENGINE ALIGNMENT - peer review of the engine's classification.",
      "Set agrees:",
      "  - \"yes\"            = the engine's classification looks",
      "                          correct given what you observed",
      "  - \"no\"             = you'd reach a different verdict;",
      "                          rationale must explain why",
      "  - \"cannot_assess\"  = you don't have enough content to take",
      "                          a position (genuinely opaque file,",
      "                          tiny sample, etc.)",
      "rationale: 1-2 sentences explaining the agreement or",
      "disagreement, citing specific observations. If something the",
      "engine couldn't see would change the verdict (e.g. \"this",
      "looks like the published UCI dataset, which moves the risk",
      "from MEDIUM to LOW\"), say so here.",
      "",
      "REVIEWER FOCUS - one sentence telling the reviewer what to",
      "actually look at when they open this file. Concrete and",
      "specific. Not \"review carefully\" - say WHAT to look at.",
      "",
      "STRUCTURE SUMMARY - 1-2 sentences describing what the file",
      "looks like in concrete terms. Include columnar shape, value",
      "ranges if striking, format if non-tabular. Mandatory; never",
      "empty.",
      "",
      "RULES YOU MUST FOLLOW:",
      "- Output JSON ONLY, no prose before or after, no Markdown",
      "  code fences.",
      "- Ground every claim in observable input (column names, sample",
      "  values, profile statistics, excerpt content). Do NOT invent",
      "  observations not present in the input.",
      "- The engine remains the authority on classification. You",
      "  give the reviewer concrete content-grounded context for the",
      "  engine's verdict and flag what the engine missed - you do",
      "  not produce a competing verdict.",
      "- Conservative on dataset recognition. False positives mislead",
      "  reviewers in ways the engine cannot catch.",
      "- Empty anomalies list is normal. Do not pad.",
      "- Be terse. The shape is small for a reason.",
      sep = "\n"
    ),
    user_builder = .user_builder_disclosure_review,
    model_params = list(
      model       = "workspace-chat",
      temperature = 0,
      max_tokens  = 2400L
    ),
    response_validator = .validator_disclosure_review
  )
)

# disclosure_review's system is the base reviewer prompt (defined in the entry
# above) plus two conditional sections appended here: the ACRO context addendum
# and the consistency-observation addendum. Composition happens after PROMPTS is
# built because a list literal cannot reference itself during construction.
# Both sections are written to be conditional on the input: for non-ACRO files
# the builder supplies no ACRO block, the model ignores the ACRO instructions
# and returns acro_consistency as not_applicable (which the renderer hides), so
# the single prompt serves ACRO and non-ACRO files alike.
PROMPTS$disclosure_review$system <- paste(
  PROMPTS$disclosure_review$system,
  .DISCLOSURE_ACRO_ADDENDUM,
  .DISCLOSURE_CONSISTENCY_ADDENDUM,
  sep = "\n"
)

# Active prompt per use case. A thin indirection: the audit log records which
# prompt produced each review via this name, and it is the seam through which
# versioning can be reintroduced at deployment if required.
ACTIVE_PROMPT <- list(
  why_red_summary    = "why_red_summary",
  batch_summary      = "batch_summary",
  disclosure_review  = "disclosure_review"
)


# ── Public: prompt building ─────────────────────────────────────────────────

# Build the prompt pair for a given version. Deterministic; golden-testable.
# Returns list(system = chr(1), user = chr(1), version = chr(1)).
build_prompt <- function(result, version = "why_red_summary") {
  pr <- PROMPTS[[version]]
  if (is.null(pr))
    stop(sprintf("Unknown prompt version: '%s'", version))
  list(
    system  = pr$system,
    user    = pr$user_builder(result),
    version = pr$version
  )
}

# Convenience wrapper for the "why is this RED?" use case. Resolves the
# currently-active prompt version from ACTIVE_PROMPT. This is what the
# server calls; tests that want to pin a specific version use build_prompt().
build_prompt_summary_v1 <- function(result) {
  build_prompt(result, version = "why_red_summary")
}

# Convenience wrapper for the batch-summary use case. Input is the full
# `res` list (not a single result).
build_prompt_batch_summary_v1 <- function(res) {
  build_prompt(res, version = "batch_summary")
}


# ── Module-local state ──────────────────────────────────────────────────────
#
# A small environment is used instead of top-level assign() so that state is
# confined to this module and can be reset cleanly (e.g. by tests).

.aira_state <- new.env(parent = emptyenv())
.aira_state$client          <- NULL
.aira_state$client_cfg_hash <- NA_character_
.aira_state$override        <- NULL


# Test-only seam: swap the real ellmer client for a mock. Pass NULL to clear.
# The override is consulted by aira_summarise_file() BEFORE the package-
# availability gate, so tests can run without ellmer installed.
set_aira_client_override <- function(fn_or_null) {
  if (!is.null(fn_or_null) && !is.function(fn_or_null))
    stop("override must be a function(prompt, model_params) or NULL")
  .aira_state$override <- fn_or_null
  invisible(NULL)
}

get_aira_client_override <- function() .aira_state$override


# ── Enabledness and client construction ─────────────────────────────────────

# Hash the parts of cfg that affect client construction. If these change,
# the cached client is rebuilt. With ellmer 0.4.0 the system prompt is
# baked into the client at construction time (see aira_client), so the
# active prompt version is part of the hash - bumping the version
# invalidates the cached client automatically.
.cfg_client_hash <- function(cfg, version) {
  ac <- cfg$aira %||% list()
  paste(
    ac$base_url %||% AIRA_BASE_URL_DEFAULT,
    ac$model    %||% "workspace-chat",
    version,
    # disable_thinking affects client construction (passed as
    # chat_template_kwargs via api_args). Toggling rebuilds the client.
    isTRUE(ac$disable_thinking %||% TRUE),
    sep = "|"
  )
}

# Decide whether AIRA should run for this session.
aira_is_enabled <- function(cfg) {
  if (!AIRA_PACKAGES_OK) return(FALSE)
  ac <- if (is.list(cfg)) cfg$aira else NULL
  if (!is.list(ac)) return(FALSE)
  if (!isTRUE(ac$enabled)) return(FALSE)
  if (!nzchar(Sys.getenv("WORKSPACE_API_KEY"))) return(FALSE)
  TRUE
}

# Build (or return cached) ellmer client. Returns NULL if disabled or if
# construction fails. Never throws.
#
# ellmer 0.4.0 changes (2026-04-17):
#   - chat_openai() now targets the Responses API (/v1/responses), which
#     AIRA's OpenAI-compat shim does not implement. We use
#     chat_openai_compatible() instead, which stays on /v1/chat/completions
#     and the classic {messages:[...]} body shape that AIRA accepts.
#   - system_prompt is baked into the client at construction time rather
#     than passed per call; the client is rebuilt when the active prompt
#     version changes (see .cfg_client_hash).
#   - api_key is deprecated in 0.4.0. Use credentials, a zero-argument
#     function returning a named list of auth headers.
#
# Thinking-mode-off (2026-04-30):
#   - When cfg$aira$disable_thinking is TRUE (the default), we attempt
#     to pass chat_template_kwargs = {"enable_thinking": false} via
#     ellmer's api_args mechanism. This tells vLLM/SGLang-style
#     inference servers to skip the model's <think>...</think> block,
#     which on reasoning models can dominate decode time and bytes.
#   - The exact ellmer parameter name (api_args / extra_args / extra_body)
#     varies between versions and is not stable enough to rely on
#     across deployments. We try the most likely name, and on failure
#     fall back to constructing the client WITHOUT the thinking-off
#     parameter rather than failing the AIRA path entirely. The fallback
#     state is recorded in .aira_state$client_thinking_disabled so the
#     audit log can surface whether it took effect.
#   - Verify in the workspace by checking the audit log: the
#     thinking_disabled field in aira_review_call_completed events
#     reflects whether the parameter was successfully passed at client
#     construction time.
aira_client <- function(cfg, version = NULL) {
  if (!aira_is_enabled(cfg)) return(NULL)
  if (is.null(version)) version <- ACTIVE_PROMPT$why_red_summary %||% "why_red_summary"
  pr <- PROMPTS[[version]]
  if (is.null(pr)) return(NULL)

  want_hash <- .cfg_client_hash(cfg, version)
  if (!is.null(.aira_state$client) &&
      identical(.aira_state$client_cfg_hash, want_hash)) {
    return(.aira_state$client)
  }
  ac <- cfg$aira %||% list()
  base_url <- ac$base_url %||% AIRA_BASE_URL_DEFAULT
  model    <- ac$model    %||% "workspace-chat"
  want_no_thinking <- isTRUE(ac$disable_thinking %||% TRUE)

  # Build credentials function once; used in all branches.
  creds <- function() list(
    Authorization = paste("Bearer", Sys.getenv("WORKSPACE_API_KEY"))
  )

  client                     <- NULL
  thinking_disabled_in_client <- FALSE
  thinking_attempt_reason     <- ""

  if (want_no_thinking) {
    # Attempt construction WITH api_args first. We name the field exactly
    # as vLLM/SGLang expects on the wire; the question is only whether
    # ellmer's chat_openai_compatible accepts an api_args argument and
    # forwards it into the request body.
    extra <- list(chat_template_kwargs = list(enable_thinking = FALSE))
    client <- tryCatch(
      ellmer::chat_openai_compatible(
        base_url      = base_url,
        model         = model,
        system_prompt = pr$system,
        credentials   = creds,
        api_args      = extra
      ),
      error = function(e) {
        thinking_attempt_reason <<- paste0("api_args path: ",
                                           conditionMessage(e))
        NULL
      }
    )
    if (!is.null(client)) {
      thinking_disabled_in_client <- TRUE
    }
  }

  # Fallback path: either disable_thinking is FALSE in config, or the
  # api_args attempt failed. Construct the client without the parameter.
  # The model will think if it's a reasoning model; we just won't have
  # disabled it. AIRA still works.
  if (is.null(client)) {
    client <- tryCatch(
      ellmer::chat_openai_compatible(
        base_url      = base_url,
        model         = model,
        system_prompt = pr$system,
        credentials   = creds
      ),
      error = function(e) NULL
    )
  }

  .aira_state$client                    <- client
  .aira_state$client_cfg_hash           <- want_hash
  .aira_state$client_thinking_disabled  <- thinking_disabled_in_client
  .aira_state$client_thinking_reason    <- thinking_attempt_reason

  # Log the outcome of the thinking-off attempt at client construction
  # time. The audit log will show one of:
  #   - aira_thinking_off_applied     (parameter accepted by ellmer)
  #   - aira_thinking_off_unsupported (ellmer rejected api_args; fallback used)
  #   - (neither, if disable_thinking is FALSE in config)
  # Both events fire once per client construction (not per call), so a
  # session that processes 50 files sees one entry for the active state.
  if (want_no_thinking && !is.null(client) && exists("log_event", mode = "function")) {
    if (thinking_disabled_in_client) {
      tryCatch(log_event("INFO", "aira_thinking_off_applied",
                         prompt_version = version,
                         model          = model),
               error = function(e) NULL)
    } else {
      tryCatch(log_event("WARN", "aira_thinking_off_unsupported",
                         reason         = thinking_attempt_reason,
                         prompt_version = version,
                         model          = model),
               error = function(e) NULL)
    }
  }

  client
}

# Public introspection: is the cached client constructed with thinking-off
# in effect? Returns NA if no client has been constructed yet, FALSE if
# the client was built but thinking-off failed to apply, TRUE if it
# applied successfully.
aira_thinking_disabled <- function() {
  if (is.null(.aira_state$client)) return(NA)
  isTRUE(.aira_state$client_thinking_disabled)
}


# ── Canonical response constructor ──────────────────────────────────────────
#
# Every AIRA entry point returns this shape. Five statuses only.

.aira_response <- function(status,
                           text           = "",
                           prompt_version = NA_character_,
                           duration_ms    = 0L,
                           reason         = "",
                           timing_ms      = NULL,
                           diag           = NULL) {
  list(
    status         = status,
    text           = text,
    prompt_version = prompt_version,
    duration_ms    = as.integer(duration_ms),
    reason         = reason,
    timing_ms      = timing_ms,
    diag           = diag
  )
}


# ── Phase timer for performance instrumentation ──────────────────────────
#
# Used inside the sync AIRA functions (aira_summarise_file, aira_summarise_
# batch, aira_review_disclosure) to measure where the ~7s per-call budget
# actually goes. Phases recorded:
#   client_construction_ms  time in aira_client() (per-worker, not cached
#                           across worker processes)
#   inference_ms            time in client$chat() (network + LLM inference)
#   parse_ms                time in response parsing/validation
#
# future_overhead_ms is measured separately by the async wrapper (outside
# the future body) and attached to the response by the caller.
#
# The timer is returned from the sync function as a named integer list
# inside response$timing_ms. The Shiny layer reads this list and includes
# each field in the aira_*_call_completed log event, where the numbers
# can be inspected post-hoc.
.aira_timer <- function() {
  ts <- list()
  ts$t0 <- proc.time()[["elapsed"]]
  ts$marks <- list()
  structure(ts, class = "aira_timer")
}

# Mark a phase boundary. elapsed_since is "start" (default, measure from
# timer creation) or a previously-stored mark name.
.aira_timer_mark <- function(tmr, name, elapsed_since = "t0") {
  now <- proc.time()[["elapsed"]]
  base <- if (elapsed_since == "t0") tmr$t0 else tmr$marks[[elapsed_since]]$abs
  if (is.null(base)) base <- tmr$t0  # defensive: fall back to timer start
  tmr$marks[[name]] <- list(
    abs      = now,
    phase_ms = as.integer(round((now - base) * 1000))
  )
  tmr
}

# Finalise: return a named integer list of phase_ms values, one per mark.
.aira_timer_finalise <- function(tmr) {
  if (length(tmr$marks) == 0L) return(NULL)
  setNames(
    lapply(tmr$marks, function(m) as.integer(m$phase_ms)),
    names(tmr$marks)
  )
}


# ── Core: synchronous summarise ─────────────────────────────────────────────
#
# Never throws. Never returns NULL. Always returns an aira_response.

aira_summarise_file <- function(result,
                                cfg,
                                client_override = NULL) {
  t0 <- proc.time()[["elapsed"]]
  elapsed_ms <- function() as.integer(round((proc.time()[["elapsed"]] - t0) * 1000))

  # Resolve prompt version up front so we can stamp every response with it.
  version <- ACTIVE_PROMPT$why_red_summary %||% "why_red_summary"
  pr      <- PROMPTS[[version]]
  if (is.null(pr)) {
    return(.aira_response(
      status         = "unavailable",
      prompt_version = NA_character_,
      duration_ms    = elapsed_ms(),
      reason         = sprintf("prompt version '%s' missing from registry", version)
    ))
  }

  # Resolve the client: explicit arg > module override > real client.
  # The override path intentionally bypasses the package-availability gate
  # so tests can run without ellmer installed.
  override <- client_override %||% get_aira_client_override()
  using_override <- !is.null(override)

  if (!using_override && !aira_is_enabled(cfg)) {
    reason <- if (!AIRA_PACKAGES_OK) {
      sprintf("packages missing: %s",
              paste(AIRA_MISSING_PACKAGES, collapse = ", "))
    } else if (!isTRUE((cfg$aira %||% list())$enabled)) {
      "disabled by config"
    } else if (!nzchar(Sys.getenv("WORKSPACE_API_KEY"))) {
      "no WORKSPACE_API_KEY in environment"
    } else {
      "disabled"
    }
    return(.aira_response(
      status         = "disabled",
      prompt_version = version,
      duration_ms    = elapsed_ms(),
      reason         = reason
    ))
  }

  # Build prompt. This is pure R and should not throw, but guard anyway.
  prompt <- tryCatch(
    build_prompt(result, version = version),
    error = function(e) NULL
  )
  if (is.null(prompt)) {
    return(.aira_response(
      status         = "unavailable",
      prompt_version = version,
      duration_ms    = elapsed_ms(),
      reason         = "prompt build failed"
    ))
  }

  # Resolve real client if no override. Pass version so the client is
  # built (or refreshed) with the system prompt matching this call's
  # prompt version.
  client <- if (using_override) override else aira_client(cfg, version = version)
  if (is.null(client)) {
    return(.aira_response(
      status         = "unavailable",
      prompt_version = version,
      duration_ms    = elapsed_ms(),
      reason         = "client construction failed"
    ))
  }

  # Per-file timeout lookup. Prefer cfg$aira$timeout_s_file; fall back to
  # cfg$aira$timeout_s (shared setting) for backward compatibility; final
  # fallback is the per-file default.
  ac <- cfg$aira %||% list()
  timeout_s <- as.integer(ac$timeout_s_file %||% ac$timeout_s %||% AIRA_TIMEOUT_S_DEFAULT)
  if (is.na(timeout_s) || timeout_s <= 0L) timeout_s <- AIRA_TIMEOUT_S_DEFAULT

  # Invoke. Two paths: override (test mock) and real (ellmer).
  raw <- NULL
  timed_out <- FALSE
  call_err  <- NULL

  call_fn <- function() {
    if (using_override) {
      # Mock contract: function(prompt, model_params) -> character(1) or
      # a list with $text. May also return a condition-like list to signal
      # failures, but in practice tests use stop() / NULL to exercise errors.
      override(prompt, pr$model_params)
    } else {
      # Real ellmer path. With ellmer 0.4.0 and chat_openai_compatible,
      # the system prompt is baked into the client at construction, so
      # $chat() takes a bare user string - not a message-history list.
      # The return value is typically a character(1) but ellmer internals
      # may wrap it; the post-call normaliser below handles both shapes.
      client$chat(prompt$user)
    }
  }

  # withTimeout wraps the call with a hard deadline. R.utils is required
  # for AIRA to be enabled, so it's present on the real path. On the
  # override path we skip the timeout wrapping - mocks are expected to
  # return promptly; tests that want to exercise timeouts use status =
  # "timeout" via the mock response helper instead.
  if (using_override) {
    raw <- tryCatch(call_fn(), error = function(e) { call_err <<- e; NULL })
  } else {
    raw <- tryCatch(
      R.utils::withTimeout(call_fn(), timeout = timeout_s, onTimeout = "error"),
      TimeoutException = function(e) { timed_out <<- TRUE; NULL },
      error            = function(e) { call_err  <<- e;    NULL }
    )
  }

  if (timed_out) {
    return(.aira_response(
      status         = "timeout",
      prompt_version = version,
      duration_ms    = elapsed_ms(),
      reason         = sprintf("exceeded %ds timeout", timeout_s)
    ))
  }
  if (!is.null(call_err)) {
    return(.aira_response(
      status         = "unavailable",
      prompt_version = version,
      duration_ms    = elapsed_ms(),
      reason         = conditionMessage(call_err)
    ))
  }

  # Normalise raw to character(1). Mocks may return a list with $text; be
  # generous about what we accept, strict about what we validate.
  text <- if (is.list(raw)) {
    raw$text %||% raw$content %||% ""
  } else if (is.character(raw) && length(raw) >= 1L) {
    # Collapse multi-element character to single string on newlines.
    paste(raw, collapse = "\n")
  } else if (is.null(raw)) {
    ""
  } else {
    as.character(raw)[1L]
  }
  if (is.na(text)) text <- ""
  text <- trimws(text)

  valid <- pr$response_validator(text)
  if (!isTRUE(valid)) {
    return(.aira_response(
      status         = "malformed",
      text           = substr(text, 1L, 500L),
      prompt_version = version,
      duration_ms    = elapsed_ms(),
      reason         = as.character(valid)
    ))
  }

  .aira_response(
    status         = "ok",
    text           = text,
    prompt_version = version,
    duration_ms    = elapsed_ms(),
    reason         = ""
  )
}


# ── Async wrapper ──────────────────────────────────────────────────────────
#
# Returns a promise that resolves to an aira_response. Never rejects -
# failures resolve to a non-"ok" response. If future/promises are not
# available, returns a synchronous fallback wrapped in a trivial promise-
# like via promises::promise_resolve if possible, otherwise returns the
# sync result directly. The server should check AIRA_PACKAGES_OK before
# calling this.

aira_summarise_file_async <- function(result, cfg) {
  if (!AIRA_PACKAGES_OK) {
    # Fully synchronous fallback; still returns a response object.
    return(aira_summarise_file(result, cfg))
  }

  # Capture the override BY VALUE in the parent so it travels with the
  # future. If we dereferenced .aira_state$override inside the worker,
  # the worker's fresh R session wouldn't see it.
  override_snapshot <- get_aira_client_override()

  # Capture API key explicitly; future::multisession workers inherit a
  # clean environment and may not see Sys.getenv() the same way.
  api_key <- Sys.getenv("WORKSPACE_API_KEY")

  fut <- future::future({
    # Inside the worker: re-attach the override, re-seed the API key,
    # then call the sync summariser. The cfg and result are serialised
    # into the worker automatically by future.
    Sys.setenv(WORKSPACE_API_KEY = api_key)
    if (!is.null(override_snapshot)) {
      set_aira_client_override(override_snapshot)
    }
    aira_summarise_file(result, cfg)
  }, seed = TRUE)

  promises::then(
    promises::as.promise(fut),
    onFulfilled = function(value) value,
    onRejected  = function(err) {
      .aira_response(
        status         = "unavailable",
        prompt_version = ACTIVE_PROMPT$why_red_summary %||% "why_red_summary",
        duration_ms    = 0L,
        reason         = sprintf("async failure: %s", conditionMessage(err))
      )
    }
  )
}


# ── Batch summary (sync + async) ───────────────────────────────────────────
#
# Mirrors aira_summarise_file but takes the full res list, uses
# ACTIVE_PROMPT$batch_summary, and its own prompt/builder/validator.
# Never throws. Always returns the canonical 5-field response.

aira_summarise_batch <- function(res,
                                 cfg,
                                 client_override = NULL,
                                 aira_reviews    = NULL) {
  t0 <- proc.time()[["elapsed"]]
  elapsed_ms <- function() as.integer(round((proc.time()[["elapsed"]] - t0) * 1000))
  tmr <- .aira_timer()

  version <- ACTIVE_PROMPT$batch_summary %||% "batch_summary"
  pr      <- PROMPTS[[version]]
  if (is.null(pr)) {
    return(.aira_response(
      status         = "unavailable",
      prompt_version = NA_character_,
      duration_ms    = elapsed_ms(),
      reason         = sprintf("prompt version '%s' missing from registry", version)
    ))
  }

  override <- client_override %||% get_aira_client_override()
  using_override <- !is.null(override)

  if (!using_override && !aira_is_enabled(cfg)) {
    reason <- if (!AIRA_PACKAGES_OK) {
      sprintf("packages missing: %s",
              paste(AIRA_MISSING_PACKAGES, collapse = ", "))
    } else if (!isTRUE((cfg$aira %||% list())$enabled)) {
      "disabled by config"
    } else if (!nzchar(Sys.getenv("WORKSPACE_API_KEY"))) {
      "no WORKSPACE_API_KEY in environment"
    } else {
      "disabled"
    }
    return(.aira_response(
      status         = "disabled",
      prompt_version = version,
      duration_ms    = elapsed_ms(),
      reason         = reason
    ))
  }

  # Build prompt. v2 takes (res, aira_reviews) wrapped in a list; v1 takes
  # just res. Caller passes aira_reviews only when v2 is active, but we
  # also handle defensively if v1 is active and aira_reviews is passed -
  # the v1 builder just ignores it.
  builder_input <- if (identical(version, "batch_summary_v2")) {
    list(res = res, aira_reviews = aira_reviews %||% list())
  } else {
    res
  }
  prompt <- tryCatch(
    build_prompt(builder_input, version = version),
    error = function(e) NULL
  )
  if (is.null(prompt)) {
    return(.aira_response(
      status         = "unavailable",
      prompt_version = version,
      duration_ms    = elapsed_ms(),
      reason         = "prompt build failed"
    ))
  }

  client <- if (using_override) override else aira_client(cfg, version = version)
  if (is.null(client)) {
    return(.aira_response(
      status         = "unavailable",
      prompt_version = version,
      duration_ms    = elapsed_ms(),
      reason         = "client construction failed",
      timing_ms      = .aira_timer_finalise(
        .aira_timer_mark(tmr, "client_construction_ms"))
    ))
  }
  tmr <- .aira_timer_mark(tmr, "client_construction_ms")

  # Batch timeout lookup. Prefer cfg$aira$timeout_s_batch; fall back to
  # cfg$aira$timeout_s (shared per-file/batch setting from earlier config
  # shapes) for backward compatibility; final fallback is the batch default.
  ac <- cfg$aira %||% list()
  timeout_s <- as.integer(ac$timeout_s_batch %||% ac$timeout_s %||% AIRA_TIMEOUT_S_BATCH_DEFAULT)
  if (is.na(timeout_s) || timeout_s <= 0L) timeout_s <- AIRA_TIMEOUT_S_BATCH_DEFAULT

  raw <- NULL
  timed_out <- FALSE
  call_err  <- NULL

  call_fn <- function() {
    if (using_override) {
      override(prompt, pr$model_params)
    } else {
      # Real ellmer path. System prompt is baked in at client construction.
      client$chat(prompt$user)
    }
  }

  if (using_override) {
    raw <- tryCatch(call_fn(), error = function(e) { call_err <<- e; NULL })
  } else {
    raw <- tryCatch(
      R.utils::withTimeout(call_fn(), timeout = timeout_s, onTimeout = "error"),
      TimeoutException = function(e) { timed_out <<- TRUE; NULL },
      error            = function(e) { call_err  <<- e;    NULL }
    )
  }
  tmr <- .aira_timer_mark(tmr, "inference_ms", "client_construction_ms")

  if (timed_out) {
    return(.aira_response(
      status         = "timeout",
      prompt_version = version,
      duration_ms    = elapsed_ms(),
      reason         = sprintf("exceeded %ds timeout", timeout_s),
      timing_ms      = .aira_timer_finalise(tmr)
    ))
  }
  if (!is.null(call_err)) {
    return(.aira_response(
      status         = "unavailable",
      prompt_version = version,
      duration_ms    = elapsed_ms(),
      reason         = conditionMessage(call_err),
      timing_ms      = .aira_timer_finalise(tmr)
    ))
  }

  # Normalise raw to character(1); same handling as per-file.
  text <- if (is.list(raw)) {
    raw$text %||% raw$content %||% ""
  } else if (is.character(raw) && length(raw) >= 1L) {
    paste(raw, collapse = "\n")
  } else if (is.null(raw)) {
    ""
  } else {
    as.character(raw)[1L]
  }
  if (is.na(text)) text <- ""
  text <- trimws(text)

  valid <- pr$response_validator(text)
  tmr <- .aira_timer_mark(tmr, "parse_ms", "inference_ms")
  if (!isTRUE(valid)) {
    return(.aira_response(
      status         = "malformed",
      text           = substr(text, 1L, 500L),
      prompt_version = version,
      duration_ms    = elapsed_ms(),
      reason         = as.character(valid),
      timing_ms      = .aira_timer_finalise(tmr)
    ))
  }

  .aira_response(
    status         = "ok",
    text           = text,
    prompt_version = version,
    duration_ms    = elapsed_ms(),
    reason         = "",
    timing_ms      = .aira_timer_finalise(tmr)
  )
}

aira_summarise_batch_async <- function(res, cfg, aira_reviews = NULL) {
  if (!AIRA_PACKAGES_OK) {
    return(aira_summarise_batch(res, cfg, aira_reviews = aira_reviews))
  }

  override_snapshot <- get_aira_client_override()
  api_key           <- Sys.getenv("WORKSPACE_API_KEY")
  ar_snapshot       <- aira_reviews %||% list()

  # Capture wall-clock at submission time so we can back out future
  # overhead after the result comes back (see onFulfilled below).
  wrapper_t0 <- Sys.time()

  fut <- future::future({
    Sys.setenv(WORKSPACE_API_KEY = api_key)
    if (!is.null(override_snapshot)) {
      set_aira_client_override(override_snapshot)
    }
    aira_summarise_batch(res, cfg, aira_reviews = ar_snapshot)
  }, seed = TRUE)

  promises::then(
    promises::as.promise(fut),
    onFulfilled = function(value) {
      # Compute the overhead attributable to future: everything not already
      # accounted for by the sync function's phase timings.
      wrapper_total_ms <- as.integer(round(
        as.numeric(difftime(Sys.time(), wrapper_t0, units="secs")) * 1000))
      phase_total <- sum(unlist(value$timing_ms %||% list()), na.rm = TRUE)
      if (!is.list(value$timing_ms)) value$timing_ms <- list()
      value$timing_ms$future_overhead_ms <-
        max(0L, wrapper_total_ms - as.integer(phase_total))
      value
    },
    onRejected  = function(err) {
      .aira_response(
        status         = "unavailable",
        prompt_version = ACTIVE_PROMPT$batch_summary %||% "batch_summary",
        duration_ms    = 0L,
        reason         = sprintf("async failure: %s", conditionMessage(err))
      )
    }
  )
}


# ── Disclosure review: sync + async ────────────────────────────────────────
#
# On-demand per-file disclosure-risk triage. Mirrors aira_summarise_batch
# structure but operates on a single `result` and uses the per-file timeout.
# Returns the canonical 5-field response shape with JSON in the `text`
# field (or an error status on timeout/unavailable/malformed). Parsing the
# JSON is the caller's responsibility (with fallback to free-form display).

# ── Context-window / model-testing diagnostics ──────────────────────────────
#
# These populate response$diag, which the Shiny layer spreads into the
# aira_review_* log event (see R-AIRA-09). They answer the questions a
# context-window problem poses: how big was the prompt, how big was the
# response, and did the model run out of room (truncation). All best-effort
# and non-fatal; a failure here never affects the review result.

# Best-effort extraction of finish reason and real token counts from the
# ellmer client's last turn. Field names vary by ellmer version and provider,
# so every access is guarded; on any miss the fields stay NA and the diag
# builder falls back to a structural truncation heuristic. Confirm the slot
# names against the deployed ellmer version on first run (cf. R-AIRA-14, which
# already guards ellmer version differences for api_args).
.aira_finish_info <- function(client, using_override) {
  out <- list(finish_reason = NA_character_,
              input_tokens  = NA_integer_,
              output_tokens = NA_integer_)
  if (isTRUE(using_override) || is.null(client)) return(out)
  lt <- tryCatch(client$last_turn(), error = function(e) NULL)
  if (is.null(lt)) return(out)
  tk <- tryCatch(lt@tokens, error = function(e) NULL)
  if (is.numeric(tk) && length(tk) >= 2L) {
    out$input_tokens  <- as.integer(tk[[1L]])
    out$output_tokens <- as.integer(tk[[2L]])
  }
  fr <- tryCatch(lt@json$choices[[1L]]$finish_reason, error = function(e) NULL)
  if (is.null(fr))
    fr <- tryCatch(lt@json$choices[[1L]]$stop_reason, error = function(e) NULL)
  if (!is.null(fr) && nzchar(as.character(fr)[1L]))
    out$finish_reason <- as.character(fr)[1L]
  out
}

# Build the diag block. parse_ok is the validator result (TRUE on the ok path,
# FALSE on malformed). likely_truncated prefers the model's own finish reason
# ("length" = cut off by max_tokens); failing that, a structural heuristic: a
# non-empty response that did not close its top-level JSON and failed to parse.
.aira_build_diag <- function(prompt, text, parse_ok, model, max_tokens,
                             thinking_disabled, finish) {
  sys_chars  <- nchar(prompt$system %||% "")
  usr_chars  <- nchar(prompt$user   %||% "")
  resp_chars <- nchar(text %||% "")
  total_in   <- sys_chars + usr_chars
  fr         <- finish$finish_reason %||% NA_character_
  trimmed    <- trimws(text %||% "")
  ends_closed <- grepl("\\}\\s*$", trimmed)
  likely_truncated <- identical(fr, "length") ||
    (!isTRUE(parse_ok) && nzchar(trimmed) && !ends_closed)
  list(
    prompt_system_chars    = as.integer(sys_chars),
    prompt_user_chars      = as.integer(usr_chars),
    prompt_total_chars     = as.integer(total_in),
    prompt_approx_tokens   = as.integer(round(total_in / 4)),
    response_chars         = as.integer(resp_chars),
    response_approx_tokens = as.integer(round(resp_chars / 4)),
    input_tokens           = finish$input_tokens  %||% NA_integer_,
    output_tokens          = finish$output_tokens %||% NA_integer_,
    finish_reason          = fr,
    likely_truncated       = isTRUE(likely_truncated),
    max_tokens             = as.integer(max_tokens %||% NA_integer_),
    model                  = as.character(model %||% NA_character_),
    thinking_disabled      = thinking_disabled
  )
}

# Head+tail excerpt for a malformed/truncated response. The head parses fine;
# the tail shows where the model stopped, which is what you need to confirm
# truncation. Replaces the head-only substr() previously stored.
.aira_malformed_excerpt <- function(text, max_chars = 2000L) {
  text <- text %||% ""
  n <- nchar(text)
  if (n <= max_chars) return(text)
  head_n <- as.integer(round(max_chars * 0.6))
  tail_n <- max_chars - head_n - 30L
  if (tail_n < 1L) return(substr(text, 1L, max_chars))
  paste0(substr(text, 1L, head_n),
         sprintf("\n...[%d chars omitted]...\n", n - head_n - tail_n),
         substr(text, n - tail_n + 1L, n))
}

# Opt-in capture of the exact prompt and raw response for sharing with someone
# debugging another model. OFF by default (cfg$aira$capture_prompts). Writes one
# human-readable text file per call to a separate aira_capture/ directory
# alongside the diagnostic log, NOT into the main JSONL. The structured diag is
# already in airlock.jsonl; this file is for a person to read, so the system,
# user and response are written with their real line breaks under clear
# delimiters rather than escaped onto one line. It persists full prompt content
# (which may include sample cell values). Never throws.
.aira_capture_record <- function(cfg, version, prompt, text, status,
                                 diag, label = "review") {
  ac <- cfg$aira %||% list()
  if (!isTRUE(ac$capture_prompts %||% FALSE)) return(invisible(NULL))
  tryCatch({
    dir <- file.path(DIAG_DIR, "aira_capture")
    if (!dir.exists(dir))
      dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    ts   <- format(Sys.time(), "%Y%m%dT%H%M%OS3", tz = "UTC")
    safe <- gsub("[^A-Za-z0-9._-]", "_", as.character(label)[1L])
    path <- file.path(dir, sprintf("%s_%s.txt", ts, substr(safe, 1L, 60L)))

    bar <- strrep("=", 78L)
    fmt <- function(k, v) sprintf("%-18s : %s", k, as.character(v %||% "")[1L])

    # Metadata header: the at-a-glance answers for a context-window problem.
    meta <- c(
      bar,
      "AIRAlock AI disclosure review - captured prompt and response",
      bar,
      fmt("timestamp",        ts),
      fmt("file",             label),
      fmt("prompt_version",   version),
      fmt("status",           status),
      fmt("model",            diag$model),
      fmt("finish_reason",    diag$finish_reason),
      fmt("likely_truncated", diag$likely_truncated),
      fmt("max_tokens",       diag$max_tokens),
      fmt("thinking_disabled", diag$thinking_disabled),
      "",
      fmt("prompt chars",     sprintf("%s total (system %s, user %s)",
                                      diag$prompt_total_chars %||% NA,
                                      diag$prompt_system_chars %||% NA,
                                      diag$prompt_user_chars %||% NA)),
      fmt("prompt tokens~",   diag$prompt_approx_tokens),
      fmt("response chars",   diag$response_chars),
      fmt("response tokens~", diag$response_approx_tokens),
      fmt("input_tokens",     diag$input_tokens),
      fmt("output_tokens",    diag$output_tokens),
      ""
    )

    body <- c(
      bar, "SYSTEM MESSAGE", bar, "",
      strsplit(prompt$system %||% "", "\n", fixed = TRUE)[[1L]], "",
      bar, "USER MESSAGE", bar, "",
      strsplit(prompt$user %||% "", "\n", fixed = TRUE)[[1L]], "",
      bar, "RAW RESPONSE", bar, "",
      strsplit(text %||% "", "\n", fixed = TRUE)[[1L]]
    )

    writeLines(c(meta, body), path)
  }, error = function(e) invisible(NULL))
}


aira_review_disclosure <- function(result,
                                   cfg,
                                   client_override = NULL) {
  t0 <- proc.time()[["elapsed"]]
  elapsed_ms <- function() as.integer(round((proc.time()[["elapsed"]] - t0) * 1000))
  tmr <- .aira_timer()

  version <- ACTIVE_PROMPT$disclosure_review %||% "disclosure_review"
  pr      <- PROMPTS[[version]]
  if (is.null(pr)) {
    return(.aira_response(
      status         = "unavailable",
      prompt_version = NA_character_,
      duration_ms    = elapsed_ms(),
      reason         = sprintf("prompt version '%s' missing from registry", version)
    ))
  }

  override <- client_override %||% get_aira_client_override()
  using_override <- !is.null(override)

  if (!using_override && !aira_is_enabled(cfg)) {
    reason <- if (!AIRA_PACKAGES_OK) {
      sprintf("packages missing: %s",
              paste(AIRA_MISSING_PACKAGES, collapse = ", "))
    } else if (!isTRUE((cfg$aira %||% list())$enabled)) {
      "disabled by config"
    } else if (!nzchar(Sys.getenv("WORKSPACE_API_KEY"))) {
      "no WORKSPACE_API_KEY in environment"
    } else {
      "disabled"
    }
    return(.aira_response(
      status         = "disabled",
      prompt_version = version,
      duration_ms    = elapsed_ms(),
      reason         = reason
    ))
  }

  prompt <- tryCatch(
    build_prompt(result, version = version),
    error = function(e) NULL
  )
  if (is.null(prompt)) {
    return(.aira_response(
      status         = "unavailable",
      prompt_version = version,
      duration_ms    = elapsed_ms(),
      reason         = "prompt build failed"
    ))
  }

  client <- if (using_override) override else aira_client(cfg, version = version)
  if (is.null(client)) {
    return(.aira_response(
      status         = "unavailable",
      prompt_version = version,
      duration_ms    = elapsed_ms(),
      reason         = "client construction failed",
      timing_ms      = .aira_timer_finalise(
        .aira_timer_mark(tmr, "client_construction_ms"))
    ))
  }
  tmr <- .aira_timer_mark(tmr, "client_construction_ms")

  # Disclosure review uses the per-file timeout budget. Prompt is small
  # (metadata only) but response is JSON with column classifications,
  # which can be longer than a 1-2 sentence summary, so the file
  # timeout's typical 15s should still be adequate.
  ac <- cfg$aira %||% list()
  timeout_s <- as.integer(ac$timeout_s_file %||% ac$timeout_s %||% AIRA_TIMEOUT_S_DEFAULT)
  if (is.na(timeout_s) || timeout_s <= 0L) timeout_s <- AIRA_TIMEOUT_S_DEFAULT

  raw <- NULL
  timed_out <- FALSE
  call_err  <- NULL

  call_fn <- function() {
    if (using_override) {
      override(prompt, pr$model_params)
    } else {
      client$chat(prompt$user)
    }
  }

  run_call <- function() {
    if (using_override) {
      tryCatch(call_fn(), error = function(e) { call_err <<- e; NULL })
    } else {
      tryCatch(
        R.utils::withTimeout(call_fn(), timeout = timeout_s, onTimeout = "error"),
        TimeoutException = function(e) { timed_out <<- TRUE; NULL },
        error            = function(e) { call_err  <<- e;    NULL }
      )
    }
  }

  raw <- run_call()

  # One retry on a transport/client error (not a timeout, not the override
  # path). A transient empty or invalid first-call response - commonly an LLM
  # cold start - makes ellmer reject the turn with an S7 validation error
  # (e.g. "@json must be <list>, not <NULL>"). Clear the cached client so no
  # half-built conversation state is reused, rebuild fresh, and try once more.
  # A genuine outage fails again and surfaces as unavailable.
  if (!using_override && !timed_out && !is.null(call_err)) {
    first_err <- call_err
    call_err  <- NULL
    .aira_state$client <- NULL
    fresh <- tryCatch(aira_client(cfg, version = version), error = function(e) NULL)
    if (!is.null(fresh)) {
      client <- fresh
      Sys.sleep(0.5)
      tryCatch(
        log_event("INFO", "aira_review_call_retry",
                  prompt_version = version,
                  reason = conditionMessage(first_err)),
        error = function(e) NULL)
      raw <- run_call()
    } else {
      call_err <- first_err
    }
  }
  tmr <- .aira_timer_mark(tmr, "inference_ms", "client_construction_ms")

  if (timed_out) {
    return(.aira_response(
      status         = "timeout",
      prompt_version = version,
      duration_ms    = elapsed_ms(),
      reason         = sprintf("exceeded %ds timeout", timeout_s),
      timing_ms      = .aira_timer_finalise(tmr)
    ))
  }
  if (!is.null(call_err)) {
    return(.aira_response(
      status         = "unavailable",
      prompt_version = version,
      duration_ms    = elapsed_ms(),
      reason         = conditionMessage(call_err),
      timing_ms      = .aira_timer_finalise(tmr)
    ))
  }

  text <- if (is.list(raw)) {
    raw$text %||% raw$content %||% ""
  } else if (is.character(raw) && length(raw) >= 1L) {
    paste(raw, collapse = "\n")
  } else if (is.null(raw)) {
    ""
  } else {
    as.character(raw)[1L]
  }
  if (is.na(text)) text <- ""
  text <- trimws(text)

  # Some LLMs wrap JSON in a Markdown code fence despite the system
  # instruction to return JSON only. Strip the fence before validating.
  if (grepl("^```", text)) {
    text <- sub("^```(?:json)?\\s*\n?", "", text, perl = TRUE)
    text <- sub("\n?```\\s*$", "",             text, perl = TRUE)
    text <- trimws(text)
  }

  valid <- pr$response_validator(text)
  tmr <- .aira_timer_mark(tmr, "parse_ms", "inference_ms")

  # Diagnostics for model testing / context-window debugging. Best-effort;
  # never affects the review result. Computed once and used on both the
  # malformed and ok return paths.
  finish <- .aira_finish_info(client, using_override)
  dg <- .aira_build_diag(
    prompt            = prompt,
    text              = text,
    parse_ok          = isTRUE(valid),
    model             = (cfg$aira %||% list())$model,
    max_tokens        = pr$model_params$max_tokens,
    thinking_disabled = aira_thinking_disabled(),
    finish            = finish
  )
  .aira_capture_record(cfg, version, prompt, text,
                       status = if (isTRUE(valid)) "ok" else "malformed",
                       diag = dg, label = result$file %||% "review")

  if (!isTRUE(valid)) {
    return(.aira_response(
      status         = "malformed",
      text           = .aira_malformed_excerpt(text),
      prompt_version = version,
      duration_ms    = elapsed_ms(),
      reason         = as.character(valid),
      timing_ms      = .aira_timer_finalise(tmr),
      diag           = dg
    ))
  }

  .aira_response(
    status         = "ok",
    text           = text,
    prompt_version = version,
    duration_ms    = elapsed_ms(),
    reason         = "",
    timing_ms      = .aira_timer_finalise(tmr),
    diag           = dg
  )
}

aira_review_disclosure_async <- function(result, cfg) {
  if (!AIRA_PACKAGES_OK) {
    return(aira_review_disclosure(result, cfg))
  }

  override_snapshot <- get_aira_client_override()
  api_key           <- Sys.getenv("WORKSPACE_API_KEY")

  # Capture wall-clock at submission time so we can back out future
  # overhead after the result comes back (see onFulfilled below).
  wrapper_t0 <- Sys.time()

  fut <- future::future({
    Sys.setenv(WORKSPACE_API_KEY = api_key)
    if (!is.null(override_snapshot)) {
      set_aira_client_override(override_snapshot)
    }
    aira_review_disclosure(result, cfg)
  }, seed = TRUE)

  promises::then(
    promises::as.promise(fut),
    onFulfilled = function(value) {
      wrapper_total_ms <- as.integer(round(
        as.numeric(difftime(Sys.time(), wrapper_t0, units="secs")) * 1000))
      phase_total <- sum(unlist(value$timing_ms %||% list()), na.rm = TRUE)
      if (!is.list(value$timing_ms)) value$timing_ms <- list()
      value$timing_ms$future_overhead_ms <-
        max(0L, wrapper_total_ms - as.integer(phase_total))
      value
    },
    onRejected  = function(err) {
      .aira_response(
        status         = "unavailable",
        prompt_version = ACTIVE_PROMPT$disclosure_review %||% "disclosure_review",
        duration_ms    = 0L,
        reason         = sprintf("async failure: %s", conditionMessage(err))
      )
    }
  )
}


# ── Introspection helpers (used by config UI and diagnostics) ──────────────

aira_status_summary <- function(cfg) {
  ac <- cfg$aira %||% list()
  list(
    packages_ok       = AIRA_PACKAGES_OK,
    missing_packages  = AIRA_MISSING_PACKAGES,
    enabled_in_config = isTRUE(ac$enabled),
    api_key_present   = nzchar(Sys.getenv("WORKSPACE_API_KEY")),
    base_url          = ac$base_url  %||% AIRA_BASE_URL_DEFAULT,
    model             = ac$model     %||% "workspace-chat",
    timeout_s_file    = ac$timeout_s_file  %||% ac$timeout_s %||% AIRA_TIMEOUT_S_DEFAULT,
    timeout_s_batch   = ac$timeout_s_batch %||% ac$timeout_s %||% AIRA_TIMEOUT_S_BATCH_DEFAULT,
    # Thinking-mode-off introspection. Three states:
    #   - requested_in_config: TRUE if user wants thinking off
    #   - disabled_in_client: TRUE if the running client successfully
    #     applied the parameter; FALSE if ellmer rejected api_args; NA
    #     if no client has been constructed yet
    disable_thinking_requested = isTRUE(ac$disable_thinking %||% TRUE),
    disable_thinking_in_client = aira_thinking_disabled(),
    active_prompts    = ACTIVE_PROMPT,
    available_prompts = names(PROMPTS)
  )
}