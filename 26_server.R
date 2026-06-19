# Server function
# Auto-split from app.R - do not edit the monolithic file


# ============================================================
# Crash-trace logger (global scope per R-U-11)
# ============================================================
# Why this exists, and why it's separate from log_event():
#
# The standard log_event() writes to airlock.jsonl, but observed crashes
# leave that log silent past the startup line. The cause is that the R
# process is being killed by the OS (OOM, SIGKILL) or by a native-library
# segfault, faster than buffered writes flush to disk. We need a logger
# that flushes per call, so the *last successful write* is preserved up
# to the moment the process dies.
#
# Format: ISO-second timestamp, phase (START/DONE/INFO/ERROR), message.
# Plain text, one event per line. File: <LOG_DIR>/inspection_trace.log.
# Open-append-close per call with explicit flush() to bypass userspace
# buffering. Wasteful in normal operation; correct for crash debugging.
#
# To find the crash: tail -1 inspection_trace.log. The last START with no
# matching DONE is the file (or operation) that killed R.

TRACE_LOG_PATH <- tryCatch(
  file.path(LOG_DIR, "inspection_trace.log"),
  error = function(e) file.path(tempdir(), "inspection_trace.log"))

trace_log <- function(phase, msg = "", ...) {
  # Never throw - this must be safe to call from any code path including
  # error handlers. Failures are silent (log-the-logger is a recipe for
  # infinite recursion).
  tryCatch({
    extras <- list(...)
    extra_str <- if (length(extras) > 0L)
      paste(sprintf("%s=%s",
                    names(extras),
                    vapply(extras, function(v)
                      substr(as.character(v %||% "")[1L], 1L, 200L),
                      character(1))),
            collapse = " ")
    else ""
    line <- sprintf("%s [%s] %s%s\n",
                    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                    phase,
                    msg,
                    if (nzchar(extra_str)) paste0("  ", extra_str) else "")
    con <- file(TRACE_LOG_PATH, open = "at")
    cat(line, file = con)
    flush(con)
    close(con)
  }, error = function(e) NULL)
  invisible(NULL)
}


# ============================================================
# UTF-8 sanitiser (global scope per R-U-11)
# ============================================================
# Strip invalid UTF-8 bytes from a string. Required because malformed
# robustness fixtures (bad_utf8.csv, binary_as.*, embedded_nul.*) contain
# byte sequences that aren't valid UTF-8. When inspectors capture sample
# values, parse error text, or other content from those files, the bytes
# end up in the result list as character data tagged "UTF-8" but with
# invalid byte sequences inside.
#
# Shiny serialises rendered HTML over a WebSocket text frame, which the
# browser decodes as UTF-8. Any invalid UTF-8 byte in the rendered
# output causes the browser to drop the WebSocket frame with
#   "Could not decode a text frame as UTF-8"
# which presents as the screen greying out and the app appearing to
# have crashed.
#
# iconv(x, from="UTF-8", to="UTF-8", sub="") is R's standard idiom for
# scrubbing invalid byte sequences. The "sub" argument controls what
# replaces invalid bytes; sub="" drops them, sub="?" replaces with
# question mark. We use empty so the rendered HTML is clean rather than
# pockmarked with question marks.
#
# Never throws. Returns "" on NULL or empty. NA in becomes "" out.
safe_utf8 <- function(x) {
  if (is.null(x)) return("")
  if (length(x) == 0L) return(character(0))
  # iconv on character vectors works element-wise. Non-character input
  # is coerced first via as.character() inside tryCatch (so factors,
  # numerics, integer64 etc all flow through cleanly).
  tryCatch({
    s <- if (is.character(x)) x else as.character(x)
    s[is.na(s)] <- ""
    iconv(s, from = "UTF-8", to = "UTF-8", sub = "")
  }, error = function(e) {
    # Fallback: replace each element individually so a single bad value
    # does not poison the whole vector.
    vapply(seq_along(x), function(i) {
      tryCatch({
        v <- as.character(x[[i]])
        if (is.na(v)) return("")
        iconv(v, from = "UTF-8", to = "UTF-8", sub = "")
      }, error = function(e) "")
    }, character(1))
  })
}

# Recursively scrub every string in a list / nested list. Used by
# sanitise_result to catch strings inside $hits[[i]]$detail, evidence
# data frame cells, captions, etc.
safe_utf8_recurse <- function(x) {
  if (is.character(x)) return(safe_utf8(x))
  if (is.data.frame(x)) {
    for (col in names(x)) {
      if (is.character(x[[col]])) x[[col]] <- safe_utf8(x[[col]])
      else if (is.factor(x[[col]])) {
        levels(x[[col]]) <- safe_utf8(levels(x[[col]]))
      }
    }
    # Column names too
    names(x) <- safe_utf8(names(x))
    return(x)
  }
  if (is.list(x)) {
    if (length(x) > 0L) {
      for (i in seq_along(x)) {
        xi <- x[[i]]
        # CRITICAL: never assign NULL back into x[[i]] - in R that DELETES
        # the element and shifts every later index down, so seq_along (fixed
        # up front) then runs past the shrunk list end -> "subscript out of
        # bounds". NULL needs no UTF-8 scrubbing, so skip it entirely. This
        # matters for structures with optional NULL fields (e.g. ACRO
        # acro_data when a session has no config or checklist block).
        if (is.null(xi)) next
        x[[i]] <- safe_utf8_recurse(xi)
      }
    }
    if (!is.null(names(x))) names(x) <- safe_utf8(names(x))
    return(x)
  }
  # Numeric, logical, raw etc - pass through unchanged.
  x
}


# ============================================================
# Result sanitiser (global scope per R-U-11)
# ============================================================
# Belt-and-braces guard for renderers. Fills in any missing fields on
# a result list with safe defaults. Called by output$results_ui and
# output$batch_header_ui at the top of their per-file loops so that
# every downstream access (r$classification, r$size_bytes, r$score, etc)
# can assume a well-formed scalar value.
#
# Why this exists: degenerate inspector outputs (on malformed test
# fixtures) sometimes return result lists with NULL or missing fields.
# Render code that does `r$size_bytes < 1048576` then throws "argument
# is of length zero" because NULL < anything is logical(0). The error
# escapes the renderUI and disconnects the Shiny session.
#
# Also applies UTF-8 scrubbing to every string field, recursively into
# hits and evidence. This prevents invalid UTF-8 bytes from malformed
# test fixtures reaching the WebSocket and crashing the browser frame.
#
# Defaults are conservative: missing classification becomes UNCERTAIN
# (forces visible display rather than silent GREEN), missing score
# becomes 0, missing size becomes 0. Hits is always at minimum a list
# containing a single PARSE/UNCERTAIN sentinel so the file always has
# at least one rule output to render.
#
# Never throws. If the input itself is not a list, returns a synthetic
# error result that displays as UNCERTAIN with an error message.
sanitise_result <- function(r) {
  if (!is.list(r)) {
    return(list(
      file           = "(invalid result)",
      filepath       = "",
      file_type      = "binary",
      type_label     = "unknown",
      size_bytes     = 0L,
      classification = "UNCERTAIN",
      score          = 0L,
      hits           = list(list(rule = "PARSE", outcome = "UNCERTAIN",
                                 detail = "Inspector returned non-list result")),
      col_names      = character(0)
    ))
  }
  list(
    file           = safe_utf8(
      if (is.character(r$file) && length(r$file) == 1L && nzchar(r$file))
        r$file else "(unknown file)"),
    filepath       = safe_utf8(
      if (is.character(r$filepath) && length(r$filepath) == 1L)
        r$filepath else (r$file %||% "")),
    file_type      = safe_utf8(
      if (is.character(r$file_type) && length(r$file_type) == 1L)
        r$file_type else "binary"),
    type_label     = safe_utf8(
      if (is.character(r$type_label) && length(r$type_label) == 1L)
        r$type_label else "unknown"),
    size_bytes     = {
      sb <- r$size_bytes
      if (is.numeric(sb) && length(sb) == 1L && !is.na(sb) && sb >= 0)
        as.numeric(sb)
      else 0
    },
    classification = {
      cl <- r$classification
      if (is.character(cl) && length(cl) == 1L &&
          cl %in% c("RED","AMBER","GREEN","UNCERTAIN"))
        cl
      else "UNCERTAIN"
    },
    score          = {
      sc <- r$score
      if (is.numeric(sc) && length(sc) == 1L && !is.na(sc))
        as.integer(min(100, max(0, sc)))
      else 0L
    },
    hits           = {
      h <- r$hits
      # Three cases for hits:
      # 1. Valid list (possibly empty) - preserve as-is. An empty list
      #    is a legitimate signal from inspectors that explicitly
      #    checked the file and found nothing concerning. Common for
      #    clean SVGs, empty .R scripts, and similar files that should
      #    classify GREEN with no rule output. The renderer handles
      #    empty hits cleanly with a "No rule hits" message.
      # 2. NULL - the result has no hits field at all. This is the
      #    pathological case where the result was built incorrectly.
      #    Inject a synthetic PARSE/UNCERTAIN hit so the file has at
      #    least something to display.
      # 3. Non-list (e.g. a single hit not wrapped in list()) - same
      #    as case 2; treat as malformed.
      h <- if (is.list(h)) h
           else list(list(rule = "PARSE", outcome = "UNCERTAIN",
                          detail = "Inspector returned malformed hits"))
      # Recursive UTF-8 scrub - reaches into $detail, $evidence$data,
      # $evidence$caption, $evidence$lines, $evidence$src etc. This is
      # the critical path: malformed-fixture content captured by
      # inspectors lives in nested hit fields and would otherwise reach
      # the WebSocket as invalid UTF-8.
      lapply(h, safe_utf8_recurse)
    },
    col_names      = {
      cn <- r$col_names
      if (is.character(cn)) safe_utf8(cn) else character(0)
    },
    # ── ACRO fields ──
    # These are set by acro_batch_integrate() and drive package grouping
    # (acro_group_results) and comment rendering (render_acro_comments_block).
    # sanitise_result rebuilds the result from a fixed field set, so these
    # MUST be carried through explicitly or grouping silently sees no tags
    # and renders every ACRO file as a flat standalone card. Scrubbed for
    # UTF-8 safety like other nested content since they hold researcher text.
    package_id   = if (is.character(r$package_id) && length(r$package_id) == 1L)
                     safe_utf8(r$package_id) else NULL,
    package_role = if (is.character(r$package_role) && length(r$package_role) == 1L)
                     safe_utf8(r$package_role) else NULL,
    acro_data    = if (is.list(r$acro_data)) safe_utf8_recurse(r$acro_data) else NULL,
    acro_outputs = if (is.list(r$acro_outputs)) safe_utf8_recurse(r$acro_outputs) else NULL
  )
}


# ============================================================
# Disclosure review render helper (global scope per R-U-11)
# ============================================================
# Renders the per-file AIRA disclosure-review UI slot. Four states:
#   not requested, not in flight      -> muted "Request AI disclosure review" button
#   requested, in flight              -> spinner chip
#   requested, response with status="ok"   -> parsed banner (with JSON fallback)
#   requested, response with other status  -> greyed error chip
#
# resp is the canonical 5-field response (or NULL). in_flight and
# requested are logical scalars. fid is the sanitised file id used to
# build the input/output names and must match the id used in the card.
#
# Never writes to inputs/outputs. All rendering is local to the returned
# tag tree.

# Helper: try to parse the ok-response text as JSON with our expected
# schema. Returns list(parsed=<list>|NULL, err=<string>|NULL). Never
# throws. jsonlite is not available (air-gapped workspace constraints
# per R-FILE-01 no longer apply because AIRA has R-FILE-10 carveout,
# but we use a minimal base-R JSON parser for robustness and because
# the workspace may or may not have jsonlite loaded).
.parse_disclosure_review_json <- function(text) {
  if (!is.character(text) || length(text) != 1L || !nzchar(text)) {
    return(list(parsed = NULL, err = "empty text"))
  }
  # Try jsonlite first if available - it's the correct tool. Fall back
  # to declaring parse failure if not. Downstream code must be NULL-safe.
  if (requireNamespace("jsonlite", quietly = TRUE)) {
    parsed <- tryCatch(
      jsonlite::fromJSON(text, simplifyVector = FALSE),
      error = function(e) NULL
    )
    if (is.null(parsed)) {
      return(list(parsed = NULL, err = "not valid JSON"))
    }
    if (!is.list(parsed)) {
      return(list(parsed = NULL, err = "JSON not an object"))
    }

    # Shape detection. v6 has engine_alignment; v3/v4/v5 have risk_level
    # + assessment + reasoning. Anything else is malformed.
    is_v6 <- is.list(parsed$engine_alignment)
    is_v5_or_earlier <- !is.null(parsed$risk_level) &&
                        !is.null(parsed$assessment) &&
                        !is.null(parsed$reasoning)

    if (!is_v6 && !is_v5_or_earlier) {
      return(list(parsed = NULL, err = "JSON missing required keys"))
    }

    if (is_v6) {
      # v6 normalisation. Validate the discriminator fields and coerce
      # any strings-where-arrays-expected into arrays.
      ea <- parsed$engine_alignment
      agrees <- tolower(as.character(ea$agrees %||% "")[1L])
      if (!agrees %in% c("yes", "no", "cannot_assess")) {
        agrees <- "cannot_assess"
      }
      parsed$engine_alignment$agrees <- agrees

      # anomalies: expected to be a list of {column, observation} objects.
      # Tolerate a string-list ("anomaly1", "anomaly2") by wrapping each
      # string into a {column: null, observation: <string>} object.
      anomalies <- parsed$anomalies
      if (is.null(anomalies)) {
        anomalies <- list()
      } else if (is.list(anomalies)) {
        anomalies <- lapply(anomalies, function(a) {
          if (is.list(a)) {
            list(column = a$column %||% NA_character_,
                 observation = as.character(a$observation %||% "")[1L])
          } else {
            list(column = NA_character_,
                 observation = as.character(a)[1L])
          }
        })
        anomalies <- Filter(function(a) nzchar(a$observation), anomalies)
      } else {
        anomalies <- list()
      }
      parsed$anomalies <- anomalies

      # dataset_recognition: ensure all expected fields exist (default
      # to recognised=false if anything looks off).
      dr <- parsed$dataset_recognition
      if (!is.list(dr) || !isTRUE(dr$recognised)) {
        parsed$dataset_recognition <- list(recognised = FALSE,
                                           name = NULL,
                                           confidence = NULL,
                                           evidence = NULL)
      }

      # acro_consistency (v8, optional): AIRA's observation of whether the
      # file is consistent with what ACRO/the researcher claimed. Present
      # only for ACRO members. Normalise to a clean shape or NULL. Render
      # only when assessed == "yes".
      acn <- parsed$acro_consistency
      if (is.list(acn)) {
        assessed <- tolower(as.character(acn$assessed %||% "")[1L])
        if (!assessed %in% c("yes", "no", "not_applicable")) {
          assessed <- "not_applicable"
        }
        consistent <- tolower(as.character(acn$consistent %||% "")[1L])
        if (!consistent %in% c("yes", "no", "partial", "cannot_determine")) {
          consistent <- "cannot_determine"
        }
        parsed$acro_consistency <- list(
          assessed   = assessed,
          consistent = consistent,
          observation = as.character(acn$observation %||% "")[1L]
        )
      } else {
        parsed$acro_consistency <- NULL
      }

      parsed$shape <- "v6"
    } else {
      # v3/v4/v5 normalisation - existing logic.
      rl <- toupper(as.character(parsed$risk_level)[1L])
      if (!rl %in% c("LOW","MEDIUM","HIGH","UNCERTAIN","INSUFFICIENT")) {
        rl <- "UNCERTAIN"
      }
      parsed$risk_level <- rl

      parsed$is_v2 <- !is.null(parsed$concerns) ||
                      !is.null(parsed$blind_spots) ||
                      !is.null(parsed$reviewer_focus)

      if (isTRUE(parsed$is_v2)) {
        if (!is.null(parsed$concerns) && !is.list(parsed$concerns)) {
          parsed$concerns <- as.list(as.character(parsed$concerns))
        }
        if (!is.null(parsed$blind_spots) && !is.list(parsed$blind_spots)) {
          parsed$blind_spots <- as.list(as.character(parsed$blind_spots))
        }
      }

      parsed$shape <- "v5"
    }

    return(list(parsed = parsed, err = NULL))
  }
  list(parsed = NULL, err = "jsonlite not available")
}

# Helper: returns the CSS modifier class for the AI banner. After the
# 2026-04-28 redesign, all completed-review banners use a single neutral
# style regardless of risk level, so reviewers do not interpret the AI
# panel as a verdict. The modifier returned here is for the wrapper
# class only; the textual badge ("Minor observations" etc.) carries the
# advisory information instead of colour.
.disclosure_risk_mod <- function(rl) {
  # All risk levels collapse to the same neutral modifier. Kept as a
  # function (rather than inlined) so future audits of risk-driven
  # styling have a single point to inspect.
  "aira-review-neutral"
}

# Helper: convert risk_level to descriptive observation label. Replaces
# the previous LOW/MEDIUM/HIGH RISK pills that read like verdicts.
#
# UNCERTAIN gets its own case ("Inconclusive review") rather than the
# default "Unable to assess" - UNCERTAIN is a valid AI conclusion ("I
# considered the file and cannot reach a confident verdict"), distinct
# from INSUFFICIENT ("I could not see the file's content at all").
# Reviewers were misreading "Unable to assess" as an AI failure when
# in fact the AI had assessed and produced an honest hedged answer.
.disclosure_risk_label <- function(rl) {
  switch(toupper(rl %||% "UNCERTAIN"),
    "LOW"          = "Minor observations",
    "MEDIUM"       = "Notable observations",
    "HIGH"         = "Significant observations",
    "UNCERTAIN"    = "Inconclusive review",
    "INSUFFICIENT" = "Insufficient evidence",
                     "Unable to assess")
}

# v6 equivalent. v6 reframes the AI's pill from "AI's verdict on the
# file" to "AI's view of the engine's verdict". Three states map to
# three labels; same neutral colour treatment as v5 to avoid the
# pill-as-verdict misreading.
.disclosure_alignment_label <- function(agrees) {
  switch(tolower(agrees %||% "cannot_assess"),
    "yes"           = "AI agrees with engine",
    "no"            = "AI disagrees with engine",
    "cannot_assess" = "AI cannot assess",
                      "AI cannot assess")
}

# v6 confidence to a short adjective for the recognition callout.
.disclosure_recognition_confidence_label <- function(conf) {
  switch(tolower(conf %||% ""),
    "high"   = "high confidence",
    "medium" = "medium confidence",
    "low"    = "low confidence",
                "unspecified confidence")
}

render_aira_review_banner <- function(resp, in_flight, fid) {
  # Outer tryCatch: the AI response is parsed and rendered here, and a
  # malformed shape (or any unexpected condition while building UI tags)
  # would otherwise propagate and disconnect the session. Render-time
  # failures degrade to a small visible error block.
  tryCatch({

  # State 0: skipped from AI review (e.g. file is parse-only and AI has
  # nothing to evaluate). Surfaced via a special marker in resp.
  if (is.list(resp) && identical(resp$status %||% "", "skipped_parse_only")) {
    return(div(class = "aira-review-banner aira-review-skipped",
      div(class = "aira-review-hd", "AI OBSERVATIONS"),
      div(class = "aira-review-body",
        tags$em(style = "color:var(--text-hint);",
                "AI review not applicable for this file: rule engine could ",
                "not read its content."))
    ))
  }

  # State 1: in flight. Spinner chip.
  if (isTRUE(in_flight)) {
    return(div(class = "aira-review-banner aira-review-loading",
      div(class = "aira-review-hd", "AI OBSERVATIONS"),
      div(class = "aira-review-body",
        tags$em(style = "color:var(--text-muted);",
                "AI reviewing this file..."))
    ))
  }

  # State 2: no response yet and not in flight. This file is queued for
  # automatic background dispatch. Muted chip tells the reviewer AIRA
  # will get to this file without requiring action.
  if (is.null(resp)) {
    return(div(class = "aira-review-banner aira-review-queued",
      div(class = "aira-review-hd", "AI OBSERVATIONS"),
      div(class = "aira-review-body",
        tags$em(style = "color:var(--text-hint);", "Queued for AI review..."))
    ))
  }

  # State 3: error states (timeout, unavailable, disabled, unknown status).
  if (!identical(resp$status, "ok") && !identical(resp$status, "malformed")) {
    st <- resp$status %||% ""
    label <- switch(st,
      "timeout"     = "AI review timed out",
      "unavailable" = "Unable to assess",
      "disabled"    = "AI review disabled",
                      "AI review error")
    # Sub-message is user-facing only. For transport/client failures we do NOT
    # surface raw client internals (e.g. ellmer S7 validation text); the full
    # reason is recorded in the audit log. Timeout's reason is already clean.
    sub <- switch(st,
      "timeout"     = resp$reason %||% "",
      "unavailable" = "the AI service did not return a usable response for this file",
      "disabled"    = "",
                      "")
    return(div(class = "aira-review-banner aira-review-err",
      div(class = "aira-review-hd", "AI OBSERVATIONS"),
      div(class = "aira-review-body",
        tags$em(style = "color:var(--text-hint);", label,
          if (nzchar(sub))
            tags$span(style = "color:var(--text-hint); font-size:var(--fs-body);",
                      paste0("  \u00b7  ", sub))))
    ))
  }

  # State 5: response received. Try to parse JSON; degrade to raw text if not.
  parse_result <- .parse_disclosure_review_json(resp$text %||% "")

  if (!is.null(parse_result$parsed)) {
    # Parsed successfully: structured banner. Single neutral colour
    # scheme regardless of risk level (see .disclosure_risk_mod).
    p <- parse_result$parsed

    if (identical(p$shape, "v6")) {
      # ── v6 layout ────────────────────────────────────────────────
      # Different content priorities than v5/earlier: alignment pill
      # rather than risk pill; structure_summary as factual line;
      # dataset_recognition as a callout when present; anomalies as
      # a list (often empty); rationale as the explanatory paragraph;
      # reviewer_focus stays the same.

      ea       <- p$engine_alignment %||% list()
      agrees   <- as.character(ea$agrees %||% "cannot_assess")
      pill_lbl <- .disclosure_alignment_label(agrees)

      # Dataset recognition callout. Only renders when the AI claims
      # recognition. Confidence label is shown alongside name; evidence
      # is shown as a smaller second line so reviewers can sanity-check
      # the claim.
      recog_block <- NULL
      dr <- p$dataset_recognition %||% list()
      if (isTRUE(dr$recognised)) {
        name <- as.character(dr$name %||% "(unnamed dataset)")[1L]
        conf <- .disclosure_recognition_confidence_label(dr$confidence)
        evid <- as.character(dr$evidence %||% "")[1L]
        recog_block <- div(class = "aira-recognition",
          tags$div(class = "aira-recognition-hd",
            tags$span(class = "aira-recognition-icon", "\u2605"),
            tags$span(class = "aira-recognition-name", name),
            tags$span(class = "aira-recognition-conf", conf)
          ),
          if (nzchar(evid))
            tags$p(class = "aira-recognition-evid", evid)
        )
      }

      # Anomalies block. Empty list is the expected case; render
      # nothing rather than an empty placeholder.
      anomalies_block <- NULL
      anomalies <- p$anomalies %||% list()
      if (is.list(anomalies) && length(anomalies) > 0L) {
        an_items <- lapply(anomalies, function(a) {
          col <- a$column
          col <- if (is.null(col) || is.na(col) ||
                     !nzchar(as.character(col)[1L])) NULL
                 else as.character(col)[1L]
          obs <- as.character(a$observation %||% "")[1L]
          if (!is.null(col)) {
            tags$li(class = "aira-anomaly-item",
              tags$code(class = "aira-anomaly-col", col),
              tags$span(class = "aira-anomaly-obs",
                        paste0(" - ", obs)))
          } else {
            tags$li(class = "aira-anomaly-item",
              tags$span(class = "aira-anomaly-obs", obs))
          }
        })
        anomalies_block <- div(class = "aira-anomalies",
          tags$div(class = "aira-section-label",
                   sprintf("Anomalies (%d)", length(anomalies))),
          tags$ul(class = "aira-anomalies-list", an_items)
        )
      }

      # Reviewer focus - same shape as v5.
      reviewer_focus_block <- NULL
      rf <- as.character(p$reviewer_focus %||% "")[1L]
      if (nzchar(rf)) {
        reviewer_focus_block <- div(class = "aira-reviewer-focus",
          tags$div(class = "aira-section-label",
                   "Suggested reviewer focus"),
          tags$p(class = "aira-reviewer-focus-text", rf)
        )
      }

      structure_summary <- as.character(p$structure_summary %||% "")[1L]
      rationale         <- as.character(ea$rationale        %||% "")[1L]

      # ACRO consistency observation (v8). Renders only when the AI
      # actually assessed the file against the ACRO/researcher claims.
      # Purely an observation of claim-vs-file match - never a verdict.
      # Placed first in the body so it sits adjacent to the ACRO comments
      # block rendered just above this banner.
      acro_consistency_block <- NULL
      acn <- p$acro_consistency
      if (is.list(acn) && identical(as.character(acn$assessed %||% ""), "yes")) {
        consistent <- as.character(acn$consistent %||% "cannot_determine")
        obs        <- as.character(acn$observation %||% "")[1L]
        cons_lbl <- switch(consistent,
          "yes"               = "Consistent with researcher's account",
          "no"                = "Diverges from researcher's account",
          "partial"           = "Partly consistent with researcher's account",
          "cannot_determine"  = "Could not determine consistency",
                                "Consistency observation")
        cons_mod <- switch(consistent,
          "yes"     = "acn-consistent",
          "no"      = "acn-divergent",
          "partial" = "acn-partial",
                      "acn-indeterminate")
        acro_consistency_block <- div(
          class = paste("aira-acro-consistency", cons_mod),
          tags$div(class = "aira-acro-consistency-hd",
            tags$span(class = "aira-acro-consistency-icon", "\u21C4"),
            tags$span(cons_lbl)
          ),
          if (nzchar(obs))
            tags$p(class = "aira-acro-consistency-obs", obs)
        )
      }

      return(div(class = "aira-review-banner aira-review-neutral",
        div(class = "aira-review-hd",
          tags$span("AI OBSERVATIONS"),
          tags$span(class = "aira-review-risk-pill", pill_lbl)
        ),
        div(class = "aira-review-body",
          acro_consistency_block,
          tags$p(class = "aira-review-assessment", structure_summary),
          recog_block,
          anomalies_block,
          if (nzchar(rationale))
            tags$p(class = "aira-review-reasoning", rationale),
          reviewer_focus_block
        ),
        div(class = "aira-review-foot",
          "Advisory only \u2014 the rule-engine classification remains the authoritative check.")
      ))
    }

    # ── v5 (and earlier) layout ──────────────────────────────────
    # Preserved unchanged so rollback to v5 - or in-flight v5
    # responses still in cache - render correctly.
    mod <- .disclosure_risk_mod(p$risk_level)
    risk_label <- .disclosure_risk_label(p$risk_level)

    # Column classifications, if provided. Render as a compact grid.
    col_block <- NULL
    cc <- p$column_classifications
    if (is.list(cc) && length(cc) > 0L) {
      col_rows <- lapply(names(cc), function(cn) {
        cat <- tolower(as.character(cc[[cn]])[1L] %||% "unknown")
        cat_class <- switch(cat,
          "direct_id"       = "col-cat-direct",
          "quasi_id"        = "col-cat-quasi",
          "sensitive"       = "col-cat-sensitive",
          "non_identifying" = "col-cat-nonid",
                              "col-cat-unknown")
        cat_label <- switch(cat,
          "direct_id"       = "direct ID",
          "quasi_id"        = "quasi-ID",
          "sensitive"       = "sensitive",
          "non_identifying" = "non-identifying",
                              "unknown")
        tags$div(class = "col-cat-row",
          tags$span(class = "col-cat-name", cn),
          tags$span(class = paste("col-cat-tag", cat_class), cat_label))
      })
      col_block <- tags$details(class = "aira-review-cols",
        tags$summary("Column classifications (", length(cc), ")"),
        div(class = "col-cat-grid", col_rows)
      )
    }

    # v2-specific blocks: concerns list, blind spots, reviewer focus.
    # When v1 shape detected, all three are NULL and only the v1
    # assessment + reasoning + col_block render (preserving v1 layout
    # for any rollback or in-flight v1 responses still in cache).
    concerns_block <- NULL
    blind_spots_block <- NULL
    reviewer_focus_block <- NULL

    if (isTRUE(p$is_v2)) {
      # Concerns: array of flag names. concern_explanations: optional
      # object mapping flag name to one-line reason. Render as a
      # bulleted list; if an explanation is present for a flag, append
      # it after a separator.
      conc <- p$concerns
      if (is.list(conc) && length(conc) > 0L) {
        ce <- p$concern_explanations
        if (!is.list(ce)) ce <- list()
        conc_items <- lapply(conc, function(flag) {
          flag_name <- as.character(flag)[1L]
          explanation <- as.character(ce[[flag_name]] %||% "")[1L]
          if (nzchar(explanation)) {
            tags$li(class = "aira-concern-item",
              tags$code(class = "aira-concern-flag", flag_name),
              tags$span(class = "aira-concern-expl",
                        paste0(" - ", explanation)))
          } else {
            tags$li(class = "aira-concern-item",
              tags$code(class = "aira-concern-flag", flag_name))
          }
        })
        concerns_block <- div(class = "aira-concerns",
          tags$div(class = "aira-section-label", "Concerns"),
          tags$ul(class = "aira-concerns-list", conc_items)
        )
      }

      # Blind spots: array of strings. The AI is asked to declare what
      # it could not evaluate. Render as a muted bulleted list.
      bs <- p$blind_spots
      if (is.list(bs) && length(bs) > 0L) {
        bs_items <- lapply(bs, function(s) {
          tags$li(class = "aira-blind-spot-item", as.character(s)[1L])
        })
        blind_spots_block <- div(class = "aira-blind-spots",
          tags$div(class = "aira-section-label",
                   "Not evaluated by AI"),
          tags$ul(class = "aira-blind-spots-list", bs_items)
        )
      }

      # Reviewer focus: short prose telling the reviewer what to check.
      rf <- as.character(p$reviewer_focus %||% "")[1L]
      if (nzchar(rf)) {
        reviewer_focus_block <- div(class = "aira-reviewer-focus",
          tags$div(class = "aira-section-label",
                   "Suggested reviewer focus"),
          tags$p(class = "aira-reviewer-focus-text", rf)
        )
      }
    }

    return(div(class = paste("aira-review-banner", mod),
      div(class = "aira-review-hd",
        tags$span("AI OBSERVATIONS"),
        tags$span(class = "aira-review-risk-pill", risk_label)
      ),
      div(class = "aira-review-body",
        tags$p(class = "aira-review-assessment",
               as.character(p$assessment %||% "")),
        concerns_block,
        blind_spots_block,
        reviewer_focus_block,
        tags$p(class = "aira-review-reasoning",
               as.character(p$reasoning  %||% "")),
        col_block
      ),
      div(class = "aira-review-foot",
        "Advisory only \u2014 the rule-engine classification remains the authoritative check.")
    ))
  }

  # JSON parse failed (or malformed status). Show raw text in a muted banner
  # with a short note. The reviewer can still extract meaning from the raw
  # response if the model produced useful prose despite format failure.
  div(class = "aira-review-banner aira-review-neutral",
    div(class = "aira-review-hd",
      tags$span("AI OBSERVATIONS"),
      tags$span(class = "aira-review-risk-pill", "Unstructured")
    ),
    div(class = "aira-review-body",
      tags$p(style = "font-size:var(--fs-body); color:var(--text-hint); margin-bottom:0.3rem;",
             tags$em("AI did not return structured output; showing raw response.")),
      tags$pre(class = "aira-review-raw",
        as.character(resp$text %||% ""))
    ),
    div(class = "aira-review-foot",
      "Advisory only \u2014 the rule-engine classification remains the authoritative check.")
  )

  }, error = function(e) {
    # Render-time failure. Log with traceback, return a small visible
    # error block instead of letting the error escape and disconnect
    # the session.
    log_event("ERROR", "aira_banner_render_failed",
              fid     = fid,
              message = conditionMessage(e),
              trace   = .capture_trace())
    trace_log("ERROR", "aira_banner_render_failed",
              fid = fid, message = conditionMessage(e))
    div(class = "aira-review-banner aira-review-err",
      div(class = "aira-review-hd", "AI OBSERVATIONS"),
      div(class = "aira-review-body",
        tags$em(style = "color:#C62828;",
                paste("AI panel could not render:", conditionMessage(e))))
    )
  })
}


# ============================================================
# Batch header helpers (global scope per R-U-11)
# ============================================================
# Each function takes inputs and returns a tagList (or NULL when the
# zone should not render). They're called from inside output$batch_header_ui
# to assemble the consolidated header.

# Zone 4: cross-file linkage risk. Returns NULL when no risk is detected
# or when the batch has fewer than 2 files.
render_linkage_risk_fn <- function(res) {
  if (length(res) < 2) return(NULL)

  risk <- check_linkage_risk(res,
    min_categories   = 3L,
    min_shared_files = 2L)
  if (is.null(risk)) return(NULL)

  COL_LINK_RED  <- "#B71C1C"
  COL_LINK_AMB  <- "#E65100"
  BG_LINK_RED   <- "#FFEBEE"
  BG_LINK_AMB   <- "#FFF3E0"

  finding_cards <- lapply(risk$findings, function(f) {
    col <- if (f$level == "RED") COL_LINK_RED else COL_LINK_AMB
    bg  <- if (f$level == "RED") BG_LINK_RED  else BG_LINK_AMB

    extra <- if (f$type == "shared_columns" && !is.null(f$shared_map)) {
      top <- head(f$shared_map, 3)
      tagList(lapply(names(top), function(col_name)
        div(style="font-size:var(--fs-body); color:var(--text-muted); margin-top:0.2rem;",
          tags$code(style="font-size:var(--fs-body);", col_name),
          tags$span(style="color:var(--text-hint);",
            paste0(" - appears in: ",
              paste(top[[col_name]], collapse=", "))))
      ))
    } else if (f$type == "qi_combination" && !is.null(f$categories)) {
      div(style="font-size:var(--fs-body); color:var(--text-muted); margin-top:0.3rem;",
        paste0("Categories: ",
          paste(names(f$categories), collapse=" \u00b7 ")))
    } else NULL

    div(style=paste0(
      "border-left:4px solid ", col, "; background:", bg, "; ",
      "border-radius:5px; padding:0.55rem 0.75rem; margin-bottom:0.4rem;"),
      div(style="display:flex; align-items:center; gap:0.5rem;",
        span(style=paste0(
          "font-size:var(--fs-body); font-weight:800; padding:0.1rem 0.45rem; ",
          "border-radius:10px; background:", col, "; color:white; flex-shrink:0;"),
          f$level),
        tags$strong(style=paste0("font-size:var(--fs-emphasis); color:", col, ";"), f$title)
      ),
      div(style="font-size:var(--fs-emphasis); color:#333; margin-top:0.25rem;", f$detail),
      extra
    )
  })

  div(class="bh-zone bh-zone-linkage",
    div(class="bh-zone-hd",
      paste0("\u26d3 CROSS-FILE LINKAGE RISK \u00b7 ",
        risk$n_tab_files, " tabular files assessed")),
    tagList(finding_cards)
  )
}

# Zone 7: rule summary, the detailed list of rules fired. Returns NULL
# when no rules fired. Called from inside the collapsible details wrapper
# in batch_header_ui, so no section header is rendered here.
render_rule_summary_fn <- function(res) {
  if (length(res) == 0) return(NULL)

  rule_map <- list()
  for (r in res) {
    fid <- gsub("[^a-zA-Z0-9]","_", r$file)
    for (h in r$hits) {
      key <- h$rule
      if (is.null(rule_map[[key]])) {
        rd <- RULES[[gsub("-","",h$rule)]]
        rule_map[[key]] <- list(
          rule    = h$rule,
          outcome = h$outcome,
          label   = if (!is.null(rd)) rd$label else h$rule,
          files   = list()
        )
      }
      rule_map[[key]]$files <- c(rule_map[[key]]$files, list(list(
        file   = r$file,
        fid    = fid,
        detail = h$detail %||% ""
      )))
    }
  }

  if (length(rule_map) == 0) return(NULL)

  # Sort: RED > UNCERTAIN > AMBER > GREEN, then alphabetically by rule ID
  oc_ord <- c(RED=1L, UNCERTAIN=2L, AMBER=3L, GREEN=4L)
  ord <- order(
    sapply(rule_map, function(x) oc_ord[x$outcome] %||% 5L),
    names(rule_map)
  )
  rule_map <- rule_map[ord]

  div(class="rs-wrap",
    lapply(rule_map, function(entry) {
      bcls <- switch(entry$outcome,
        RED="hb-red", AMBER="hb-amb", GREEN="hb-grn", "hb-unc")
      n_f   <- length(entry$files)
      fnames <- paste(sapply(entry$files, function(f) f$file), collapse=", ")
      if (nchar(fnames) > 80) fnames <- paste0(substr(fnames,1,78),"...")
      div(class="rs-block",
        div(class="rs-row",
          span(class=paste("hbadge", bcls), entry$outcome),
          tags$code(class="rs-rid", entry$rule),
          span(class="rs-label", entry$label),
          div(style="margin-left:auto; display:flex; align-items:center; gap:0.6rem; flex-shrink:0;",
            span(class="rs-count", n_f, if(n_f==1)" file" else " files"),
            tags$span(style=paste0("font-size:var(--fs-body); color:var(--text-hint); font-style:italic; ",
              "max-width:260px; overflow:hidden; text-overflow:ellipsis; white-space:nowrap;"),
              title=fnames, fnames)
          )
        )
      )
    })
  )
}

# Zone 5: AIRA batch-summary panel. Takes the current reactive state and
# returns the appropriate visual state. Flattened to blend with the header
# container (no left border, transparent background) vs the standalone
# .aira-batch-slot which has its own framing.
render_batch_aira_fn <- function(aira_batch_in_flight_val,
                                 aira_batch_data_val,
                                 cfg, n_files,
                                 readiness = NULL) {
  if (!aira_is_enabled(cfg)) return(NULL)

  uc_batch <- tryCatch(
    cfg$aira$use_cases$batch_summary$enabled,
    error = function(e) TRUE
  )
  if (identical(uc_batch, FALSE)) return(NULL)

  in_flight <- isTRUE(aira_batch_in_flight_val)
  resp      <- aira_batch_data_val
  pv        <- ACTIVE_PROMPT$batch_summary %||% "batch_summary"

  hd <- function() div(class = "aira-batch-slot-hd",
      span("AI SUMMARY"),
      span(class = "aira-batch-slot-tag",
           paste0("AIRA \u00b7 ", resp$prompt_version %||% pv)))

  regen_btn <- function() actionLink("aira_generate_batch",
      class = "aira-batch-regen",
      tagList(tags$span(style="margin-right:0.25rem;", "\u21bb"),
              "Regenerate summary"))

  # In flight
  if (in_flight) {
    return(div(class = "bh-zone bh-zone-aira in-header aira-batch-slot-loading",
      hd(),
      div(class = "aira-batch-slot-body",
          tags$em(style="color:var(--text-muted);",
                  sprintf("AIRA is reading %d file%s...",
                          n_files, if (n_files != 1L) "s" else "")))
    ))
  }

  # Ready-to-generate state (no response yet). Two sub-states depending
  # on whether per-file AI reviews are all complete: ready -> enabled
  # button, not-ready -> disabled button with explanatory note.
  if (is.null(resp)) {
    rd <- readiness %||% list(ready = TRUE, n_total = n_files,
                              n_complete = n_files, n_pending = 0L,
                              n_failed = 0L)
    all_ready  <- isTRUE(rd$ready)
    n_complete <- as.integer(rd$n_complete %||% 0L)
    n_total    <- as.integer(rd$n_total    %||% 0L)
    n_pending  <- as.integer(rd$n_pending  %||% 0L)
    n_failed   <- as.integer(rd$n_failed   %||% 0L)

    if (all_ready) {
      return(div(class = "bh-zone bh-zone-aira in-header aira-batch-slot-ready",
        hd(),
        div(class = "aira-batch-slot-body",
          div(style = "display:flex; align-items:center; gap:0.8rem; flex-wrap:wrap;",
            actionButton("aira_generate_batch",
              sprintf("Generate AI Summary (%d file%s)",
                      n_files, if (n_files != 1L) "s" else ""),
              class = "btn btn-sm aira-batch-generate"),
            tags$span(style="color:var(--text-hint); font-size:var(--fs-emphasis);",
                      "May take up to 45 seconds for large batches.")
          ))
      ))
    }

    # Not-ready state. Disable the button and explain why.
    note <- if (n_failed > 0L && n_pending == 0L) {
      sprintf("AI review complete for %d of %d file(s); %d failed. Batch summary needs a successful AI review for every non-GREEN file.",
              n_complete, n_total, n_failed)
    } else if (n_pending > 0L && n_failed == 0L) {
      sprintf("Waiting for AI review to complete (%d of %d done, %d still in progress).",
              n_complete, n_total, n_pending)
    } else if (n_pending > 0L && n_failed > 0L) {
      sprintf("AI review: %d of %d done, %d in progress, %d failed. Batch summary needs a successful AI review for every non-GREEN file.",
              n_complete, n_total, n_pending, n_failed)
    } else {
      "Batch summary requires AI disclosure review to complete for every non-GREEN file."
    }

    return(div(class = "bh-zone bh-zone-aira in-header aira-batch-slot-ready",
      hd(),
      div(class = "aira-batch-slot-body",
        div(style = "display:flex; align-items:center; gap:0.8rem; flex-wrap:wrap;",
          tags$button(
            id    = "aira_generate_batch",
            type  = "button",
            class = "btn btn-sm aira-batch-generate",
            disabled = NA,
            sprintf("Generate AI Summary (%d file%s)",
                    n_files, if (n_files != 1L) "s" else "")
          ),
          tags$span(class = "aira-batch-not-ready-note", note)
        ))
    ))
  }

  # OK
  if (identical(resp$status, "ok")) {
    return(div(class = "bh-zone bh-zone-aira in-header aira-batch-slot-ok",
      hd(),
      div(class = "aira-batch-slot-body", resp$text),
      div(class = "aira-batch-slot-footer", regen_btn())
    ))
  }

  # Error
  label <- switch(resp$status,
    "timeout"     = "AIRA batch summary timed out",
    "unavailable" = "AIRA batch summary unavailable",
    "malformed"   = "AIRA batch summary returned unexpected output",
    "disabled"    = return(NULL),
    "AIRA unknown status")
  # Show a clean sub-message; never surface raw client internals in the UI
  # (the full reason is in the audit log). Timeout's reason is already clean.
  sub <- if (identical(resp$status, "timeout")) (resp$reason %||% "")
         else if (identical(resp$status, "unavailable"))
                "the AI service did not return a usable response"
         else ""
  div(class = "bh-zone bh-zone-aira in-header aira-batch-slot-err",
    hd(),
    div(class = "aira-batch-slot-body",
        tags$em(style="color:var(--text-hint);", label,
                if (nzchar(sub))
                  tags$span(style="color:var(--text-hint); font-size:var(--fs-body);",
                            paste0("  \u00b7  ", sub)))),
    div(class = "aira-batch-slot-footer", regen_btn())
  )
}


# ============================================================
# ACRO researcher comments block (global scope per R-U-11)
# ============================================================
# Renders a prominent, highlighted block surfacing the researcher's
# review comments and exception justifications captured by ACRO at
# analysis time, plus the SDC status of each correlated output.
#
# Requirement: the reviewer must SEE that comments were left, and the
# app must make clear it has surfaced them. Three cases per output,
# driven by acro_outputs[[i]]$comment_status:
#   "present" - show the comment text, highlighted
#   "blank"   - researcher was prompted but left it empty: flag as a gap
#   "none"    - no comments array at all: quiet (nothing to show)
# Exceptions (the researcher's justification for releasing a disclosive
# output) are shown the same way; an absent exception on a fail/review
# output is itself worth flagging.
#
# acro_outputs is the list attached to a file result by
# acro_batch_integrate(). Each element: uid, status, type, method,
# command, summary, comments (chr vector), comment_status, exception,
# has_exception. Returns NULL when there is nothing to surface, so the
# card simply omits the block.
#
# Never throws; wrapped so a malformed metadata shape cannot disconnect
# the session.
render_acro_comments_block <- function(acro_outputs) {
  tryCatch({
    if (!is.list(acro_outputs) || length(acro_outputs) == 0L) return(NULL)

    status_pill <- function(st) {
      st <- tolower(st %||% "")
      cfg <- switch(st,
        "fail"   = list(bg = "#FFEBEE", fg = "#B71C1C", lbl = "ACRO: FAIL"),
        "review" = list(bg = "#FFF3E0", fg = "#E65100", lbl = "ACRO: REVIEW"),
        "pass"   = list(bg = "#E8F5E9", fg = "#2E7D32", lbl = "ACRO: PASS"),
                   list(bg = "#ECEFF1", fg = "#455A64", lbl = paste0("ACRO: ", toupper(st))))
      tags$span(class = "acro-status-pill",
        style = sprintf("background:%s; color:%s;", cfg$bg, cfg$fg),
        cfg$lbl)
    }

    # Build one panel per ACRO output correlated to this file.
    panels <- lapply(acro_outputs, function(o) {
      if (!is.list(o)) return(NULL)
      uid    <- as.character(o$uid %||% "")[1L]
      status <- tolower(as.character(o$status %||% "")[1L])
      method <- as.character(o$method %||% "")[1L]
      otype  <- as.character(o$type %||% "")[1L]
      cstat  <- as.character(o$comment_status %||% "none")[1L]
      comments <- o$comments %||% character(0)
      exception <- as.character(o$exception %||% "")[1L]
      has_exc  <- isTRUE(o$has_exception) || nzchar(exception)

      # Comments sub-block
      comments_ui <- if (cstat == "present" && length(comments) > 0L) {
        div(class = "acro-comment-list",
          tags$div(class = "acro-comment-label",
            tags$span(class = "acro-comment-icon", "\U0001F4AC"),
            sprintf("Researcher comment%s", if (length(comments) != 1L) "s" else "")),
          tagList(lapply(comments, function(cm)
            tags$blockquote(class = "acro-comment-text", as.character(cm)[1L])))
        )
      } else if (cstat == "blank") {
        div(class = "acro-comment-gap",
          tags$span(class = "acro-comment-icon", "\u26A0"),
          "Researcher was prompted for a comment but left it blank.")
      } else NULL

      # Exception sub-block (the justification for releasing a flagged output)
      exception_ui <- if (has_exc) {
        div(class = "acro-exception",
          tags$div(class = "acro-comment-label",
            tags$span(class = "acro-comment-icon", "\U0001F4DD"),
            "Researcher exception (justification for release)"),
          tags$blockquote(class = "acro-exception-text", exception))
      } else if (status %in% c("fail", "review")) {
        div(class = "acro-comment-gap",
          tags$span(class = "acro-comment-icon", "\u26A0"),
          "No exception justification recorded for this flagged output.")
      } else NULL

      # Only render a panel if there is something to show for it.
      if (is.null(comments_ui) && is.null(exception_ui)) return(NULL)

      div(class = "acro-output-panel",
        div(class = "acro-output-hd",
          status_pill(status),
          tags$code(class = "acro-output-uid", uid),
          if (nzchar(method))
            tags$span(class = "acro-output-method", method)
          else if (nzchar(otype))
            tags$span(class = "acro-output-method", otype)
        ),
        comments_ui,
        exception_ui
      )
    })

    panels <- Filter(Negate(is.null), panels)
    if (length(panels) == 0L) return(NULL)

    div(class = "acro-comments-block",
      div(class = "acro-comments-hd",
        tags$span(class = "acro-comments-hd-icon", "\U0001F50E"),
        tags$span("ACRO REVIEW COMMENTS"),
        tags$span(class = "acro-comments-hd-note",
          "Captured during the researcher's ACRO session")),
      tagList(panels),
      div(class = "acro-comments-foot",
        "These notes were written by the researcher at analysis time and are surfaced here for the egress reviewer.")
    )
  }, error = function(e) {
    log_event("ERROR", "acro_comments_render_failed",
              message = conditionMessage(e))
    NULL
  })
}


# ============================================================
# ACRO package container (global scope per R-U-11)
# ============================================================
# Renders one ACRO package as a bounded container: a session header
# (title, ACRO version, SDC config, the researcher's checklist, and a
# member status roll-up) followed by the member output cards nested
# inside. The ACRO review leads; general file rules still render within
# each member card (render_card_fn is the per-file card renderer passed
# in from the results_ui closure).
#
# pkg is one element of acro_group_results()$packages:
#   $package_id, $metadata (results.json result), $members (list of
#   member results), $acro_data (config, checklist, summary,
#   members_present / members_missing).
#
# Never throws; degrades to rendering the member cards plain if the
# header build fails, so a malformed session can't hide the outputs.
render_acro_package <- function(pkg, render_card_fn) {
  tryCatch({
    ad <- pkg$acro_data %||% list()
    ss <- ad$session_summary %||% list()
    cfg_block <- ad$config %||% list()

    title   <- as.character(ss$title %||% ad$title %||% "")[1L]
    version <- as.character(ss$version %||% ad$version %||% "")[1L]
    n_total <- as.integer(ss$n_total  %||% 0L)
    n_pass  <- as.integer(ss$n_pass   %||% 0L)
    n_fail  <- as.integer(ss$n_fail   %||% 0L)
    n_rev   <- as.integer(ss$n_review %||% 0L)

    members_missing <- ad$members_missing %||% character(0)

    # ── Status roll-up pills ──
    rollup <- div(class = "acro-pkg-rollup",
      if (n_fail > 0L)
        tags$span(class = "acro-pkg-pill acro-pkg-pill-fail",
                  sprintf("%d fail", n_fail)),
      if (n_rev > 0L)
        tags$span(class = "acro-pkg-pill acro-pkg-pill-review",
                  sprintf("%d review", n_rev)),
      if (n_pass > 0L)
        tags$span(class = "acro-pkg-pill acro-pkg-pill-pass",
                  sprintf("%d pass", n_pass)),
      tags$span(class = "acro-pkg-pill acro-pkg-pill-total",
                sprintf("%d output%s", n_total, if (n_total != 1L) "s" else ""))
    )

    # ── SDC config summary (the TRE risk appetite at analysis time) ──
    cfg_items <- list()
    .cfg_line <- function(key, label) {
      v <- cfg_block[[key]]
      if (is.null(v)) return(NULL)
      tags$span(class = "acro-pkg-cfg-item",
        tags$span(class = "acro-pkg-cfg-key", label),
        tags$span(class = "acro-pkg-cfg-val", as.character(v)[1L]))
    }
    cfg_ui <- div(class = "acro-pkg-cfg",
      .cfg_line("safe_threshold",  "threshold"),
      .cfg_line("safe_pratio_p",   "p-ratio"),
      .cfg_line("safe_nk_n",       "nk-n"),
      .cfg_line("safe_nk_k",       "nk-k"),
      .cfg_line("safe_dof_threshold", "dof"),
      .cfg_line("zeros_are_disclosive", "zeros disclosive")
    )

    # ── Researcher checklist (prominent; can be in tension with findings) ──
    checklist <- ad$checklist %||% list()
    checklist_ui <- if (length(checklist) > 0L) {
      items <- lapply(checklist, function(it) {
        checked <- isTRUE(it$checked)
        lbl <- as.character(it$label %||% "")[1L]
        lbl <- trimws(gsub("\\s+", " ", lbl))
        tags$li(class = if (checked) "acro-chk-item acro-chk-yes"
                        else          "acro-chk-item acro-chk-no",
          tags$span(class = "acro-chk-mark", if (checked) "\u2713" else "\u2717"),
          tags$span(class = "acro-chk-label", lbl))
      })
      div(class = "acro-pkg-checklist",
        div(class = "acro-pkg-checklist-hd",
          tags$span(class = "acro-pkg-checklist-icon", "\U0001F4CB"),
          "Researcher attestation checklist"),
        tags$ul(class = "acro-chk-list", items)
      )
    } else NULL

    # ── Missing-member warning ──
    missing_ui <- if (length(members_missing) > 0L) {
      div(class = "acro-pkg-missing",
        tags$span(class = "acro-pkg-missing-icon", "\u26A0"),
        sprintf("ACRO declares %d output file%s not present in this batch: %s",
                length(members_missing),
                if (length(members_missing) != 1L) "s" else "",
                paste(members_missing, collapse = ", ")))
    } else NULL

    # ── Package header ──
    header <- div(class = "acro-pkg-hd",
      div(class = "acro-pkg-hd-top",
        tags$span(class = "acro-pkg-badge", "ACRO PACKAGE"),
        tags$span(class = "acro-pkg-title",
                  if (nzchar(title)) title else "(untitled session)"),
        if (nzchar(version))
          tags$span(class = "acro-pkg-version", paste0("v", version)),
        rollup
      ),
      cfg_ui,
      checklist_ui,
      missing_ui,
      div(class = "acro-pkg-precedence-note",
        "ACRO statistical disclosure review leads. General egress rules ",
        "are also applied to each output and shown within its card.")
    )

    # ── Member cards ──
    member_cards <- if (length(pkg$members) > 0L) {
      lapply(pkg$members, render_card_fn)
    } else {
      list(div(class = "acro-pkg-no-members",
        "No output files from this ACRO session are present in the batch. ",
        "Add the referenced output files to review them."))
    }

    div(class = "acro-package",
      header,
      div(class = "acro-pkg-members", member_cards)
    )
  }, error = function(e) {
    log_event("ERROR", "acro_package_render_failed",
              message = conditionMessage(e))
    # Degrade: render member cards plain so outputs are never hidden by a
    # header-build failure.
    tagList(lapply(pkg$members %||% list(), render_card_fn))
  })
}


# ============================================================
# SERVER
# ============================================================
server <- function(input, output, session) {

  # ── AIRA async plan ────────────────────────────────────────
  # future::plan() is process-wide, not per-session. Set once; idempotent.
  # multisession gives us real parallelism without requiring fork support.
  # If future is unavailable the AIRA module degrades to synchronous and
  # this plan() call is harmless (AIRA_PACKAGES_OK gate prevents use).
  if (AIRA_PACKAGES_OK) {
    tryCatch(
      future::plan(future::multisession, workers = 2L),
      error = function(e) log_event("WARN", "aira_plan_failed",
                                    message = conditionMessage(e)))
  }

  log_event("INFO", "session_start",
            mode        = APP_MODE,
            app_dir     = APP_DIR,
            r_version   = R.version.string,
            session_id  = session$token %||% "unknown")

  # ── Rule configuration reactive ────────────────────────────
  parse_lines <- function(x) {
    v <- trimws(strsplit(x %||% "", "\n")[[1]])
    v[nchar(v) > 0]
  }

  active_cfg    <- reactiveVal({
    c0 <- tryCatch(load_cfg(DEFAULT_CFG), error = function(e) NULL)
    if (is.list(c0)) c0 else DEFAULT_CFG   # corrupt/missing config -> safe defaults
  })
  # Accumulates per-key diffs across all Apply clicks in this session.
  # Each entry: list(label, default_val, applied_val, rules, applied_at)
  cfg_changes_rv <- reactiveVal(list())

  # ── Baseline governance ───────────────────────────────────────────────────
  # Reactive holding the current live config hash





  observeEvent(input$cfg_apply, {
    tryCatch({
      # NA-safe coerce: raw HTML inputs return "" which as.integer converts to NA
      ni <- function(x, def) { v <- suppressWarnings(as.integer(x %||% def)); if (!length(v)||is.na(v)) as.integer(def) else v }
      nd <- function(x, def) { v <- suppressWarnings(as.numeric( x %||% def)); if (!length(v)||is.na(v)) as.numeric(def)  else v }
      new_cfg <- list(
        id_patterns          = parse_lines(input$cfg_id_patterns),
        sensitive_phenotypes = parse_lines(input$cfg_sensitive_phenotypes),
        restricted_fields    = parse_lines(input$cfg_restricted_fields),
        gwas_cols            = parse_lines(input$cfg_gwas_cols),
        count_threshold      = max(2L, ni(input$cfg_count_threshold,    5)),
        tab003_min_rows      = max(2L, ni(input$cfg_tab003_min_rows,   10)),
        tab003_cardinality   = min(0.99, max(0.5, nd(input$cfg_tab003_cardinality, 85) / 100)),
        size_threshold_gb    = max(0.1, nd(input$cfg_size_gb,           5)),
        gen003_min_cols      = max(2L, ni(input$cfg_gen003_min_cols,    4)),
        img002_eid_digits    = max(4L, ni(input$cfg_eid_digits,         7)),
        img002_patient_word  = isTRUE(input$cfg_patient_word),
        img002_extra_words   = parse_lines(input$cfg_extra_words),
        img004_keywords      = trimws(input$cfg_img004_keywords %||%
                                 "circos|igv|oncoplot|tmb|mutational"),
        scr003_min_term_length = max(2L, ni(input$cfg_scr003_min_len,   3)),
        htm_table_rows       = max(5L, ni(input$cfg_htm_table_rows,    20)),
        scr006_flag_outputs  = isTRUE(input$cfg_scr006_flag_outputs),
        scr006_pii_scan      = isTRUE(input$cfg_scr006_pii_scan),
        robj_large_mb        = max(10L, ni(input$cfg_robj_large_mb,   100)),
        robj_model_names     = parse_lines(input$cfg_robj_model_names),
        columnar_outcome     = if (isTRUE(input$cfg_columnar_red)) "RED" else "AMBER",
        free_text_patterns   = parse_lines(input$cfg_free_text_patterns),
        derived_id_patterns  = parse_lines(input$cfg_derived_id_patterns),
        kanon_enabled        = isTRUE(input$cfg_kanon_enabled),
        kanon_max_rows       = max(100L, ni(input$cfg_kanon_max_rows, 10000)),
        kanon_max_qi_cols    = max(2L, min(10L, ni(input$cfg_kanon_max_qi, 6))),
        # TAB-023: suppression back-calculation
        suppression_markers  = parse_lines(input$cfg_supp_markers),
        total_row_patterns   = parse_lines(input$cfg_total_patterns),
        # TAB-024: free-text NER
        ner_enabled          = isTRUE(input$cfg_ner_enabled),
        ner_person_titles    = parse_lines(input$cfg_ner_titles),
        ner_geo_places       = parse_lines(input$cfg_ner_places),
        ner_inst_patterns    = parse_lines(input$cfg_ner_insts),
        ner_occ_patterns     = parse_lines(input$cfg_ner_occ),
        ner_name_exclusions  = parse_lines(input$cfg_ner_exclusions)
      )
      # Session config carries the AIRA block forward so Apply does not drop it
      # and the capture toggle takes effect this session. It is deliberately NOT
      # persisted: the config serialiser handles the flat rule keys only, and a
      # nested aira/use_cases list does not round-trip through it. AIRA defaults
      # come from DEFAULT_CFG on each start; capture_prompts is a session toggle.
      # Base on the live aira block only if it is a proper list (a corrupt or
      # legacy config can leave it atomic); otherwise fall back to DEFAULT_CFG.
      prev_aira <- if (is.list(active_cfg()) && is.list(active_cfg()$aira)) {
        active_cfg()$aira
      } else {
        DEFAULT_CFG$aira
      }
      if (!is.list(prev_aira)) prev_aira <- list()
      session_cfg <- new_cfg
      session_cfg$aira <- modifyList(
        prev_aira,
        list(capture_prompts = isTRUE(input$cfg_aira_capture_prompts))
      )
      active_cfg(session_cfg)
      saved <- save_cfg(new_cfg)   # persist flat keys only - no nested aira

      # Compute what changed vs DEFAULT_CFG and merge into session change log.
      # Merging means a key that was changed twice only appears once (latest value).
      cfg_val_eq <- function(av, dv) {
        if (is.null(av) || is.null(dv) || length(av)==0 || length(dv)==0) return(TRUE)
        if (is.logical(av) && is.logical(dv)) return(identical(av, dv))
        if ((is.numeric(av)||is.integer(av)) && (is.numeric(dv)||is.integer(dv)))
          return(all(abs(as.numeric(av)-as.numeric(dv)) <= 1e-9, na.rm=TRUE))
        identical(as.character(av), as.character(dv))
      }
      # Labels for each key (mirrors cfg_labels in the PDF function)
      .cl <- list(
        count_threshold        = list(label="Small cell count threshold",              rules="TAB-004/005/009"),
        tab003_min_rows        = list(label="Per-participant detection: minimum rows",  rules="TAB-003"),
        tab003_cardinality     = list(label="Per-participant detection: uniqueness ratio", rules="TAB-003"),
        size_threshold_gb      = list(label="File size limit (GB)",                   rules="TAB-008"),
        gen003_min_cols        = list(label="GWAS summary: min column matches",        rules="GEN-003"),
        img002_eid_digits      = list(label="SVG identifier digit length",             rules="IMG-002"),
        img002_patient_word    = list(label="SVG patient word scan",                   rules="IMG-002"),
        img004_keywords        = list(label="Oncology plot keywords",                  rules="IMG-004"),
        scr003_min_term_length = list(label="Script phenotype min term length",        rules="SCR-003"),
        htm_table_rows         = list(label="HTML/Markdown large table threshold",     rules="HTM-006/MD-003"),
        scr006_flag_outputs    = list(label="Notebook output cell flagging",           rules="SCR-006"),
        scr006_pii_scan        = list(label="Notebook output PII scan",                rules="SCR-006"),
        robj_large_mb          = list(label="Serialised R object size limit (MB)",     rules="SER-001"),
        columnar_outcome       = list(label="Columnar format default outcome",         rules="COL-001"),
        kanon_enabled          = list(label="K-anonymity estimation enabled",          rules="TAB-015/016"),
        kanon_max_rows         = list(label="K-anonymity: maximum rows",               rules="TAB-015/016"),
        kanon_max_qi_cols      = list(label="K-anonymity: maximum QI columns",        rules="TAB-015/016"),
        ner_enabled            = list(label="Free-text NER scan enabled",             rules="TAB-024")
      )
      existing <- cfg_changes_rv()
      ts <- format(Sys.time(), "%H:%M:%S")
      for (key in names(.cl)) {
        av <- new_cfg[[key]];  dv <- DEFAULT_CFG[[key]]
        if (is.null(av) || is.null(dv)) next
        if (!cfg_val_eq(av, dv)) {
          # Changed from default - add/update entry
          fmt_v <- function(v) {
            if (is.logical(v)) if(v) "Yes" else "No"
            else as.character(v)
          }
          existing[[key]] <- list(
            label       = .cl[[key]]$label,
            rules       = .cl[[key]]$rules,
            default_val = fmt_v(dv),
            applied_val = fmt_v(av),
            applied_at  = ts
          )
        } else {
          # Back to default - remove from change log if previously recorded
          existing[[key]] <- NULL
        }
      }
      cfg_changes_rv(existing)

      # Build summary of current changes for notification
      n_chg <- length(existing)
      chg_summary <- if (n_chg == 0)
        "All parameters at defaults."
      else {
        chg_labels <- sapply(existing, function(ch)
          paste0(ch$label, " (", ch$default_val, " \u2192 ", ch$applied_val, ")"))
        paste0(n_chg, " parameter(s) changed from default:\n",
               paste(chg_labels, collapse="\n"))
      }
      showNotification(
        if (saved)
          tagList(
            tags$strong(paste0("\u2713 Configuration applied",
              if (n_chg > 0) paste0(" (", n_chg, " change(s) from default)") else "")),
            tags$br(),
            if (n_chg > 0)
              tagList(
                tags$div(style="font-size:var(--fs-emphasis); margin-top:0.3rem;",
                  lapply(existing, function(ch)
                    tags$div(paste0("\u2022 ", ch$label, ": ",
                      ch$default_val, " \u2192 ", ch$applied_val))
                  )
                ),
                tags$div(style="font-size:var(--fs-body); color:var(--text-hint); margin-top:0.25rem;",
                  "Take effect on next Run Assessment.")
              )
            else
              tags$span(style="font-size:var(--fs-emphasis);", "All parameters at defaults."))
        else
          tagList(
            tags$strong(paste0("\u2713 Configuration applied",
              if (n_chg > 0) paste0(" (", n_chg, " change(s) from default)") else "")),
            tags$br(),
            tags$span(style="font-size:var(--fs-emphasis);",
              "Active for this session (save to disk failed \u2014 check permissions).")),
        type=if (saved) "message" else "warning",
        duration=6
      )
    }, error=function(e) {
      showNotification(
        tagList(tags$strong("\u26a0 Configuration error"),
                tags$br(),
                tags$span(style="font-size:var(--fs-emphasis);", conditionMessage(e))),
        type="error", duration=10
      )
    })
  }, ignoreInit=TRUE)

  observeEvent(input$cfg_reset, {
    active_cfg(DEFAULT_CFG)
    cfg_changes_rv(list())   # clear accumulated diffs on full reset
    delete_cfg()
    updateTextAreaInput(session, "cfg_id_patterns",
      value=paste(participant_id_patterns, collapse="\n"))
    updateTextAreaInput(session, "cfg_sensitive_phenotypes",
      value=paste(sensitive_phenotypes, collapse="\n"))
    updateTextAreaInput(session, "cfg_restricted_fields",
      value=paste(restricted_fields, collapse="\n"))
    updateTextAreaInput(session, "cfg_gwas_cols",
      value=paste(gwas_cols, collapse="\n"))
    updateTextAreaInput(session, "cfg_extra_words",     value="")
    updateTextAreaInput(session, "cfg_img004_keywords",
      value="circos|igv|oncoplot|tmb|mutational")
    session$sendCustomMessage("resetCfgNumerics", list())
    session$sendCustomMessage("resetScr003", list())
    updateCheckboxInput(session, "cfg_scr006_flag_outputs", value=TRUE)
    updateCheckboxInput(session, "cfg_scr006_pii_scan",     value=TRUE)
    updateCheckboxInput(session, "cfg_columnar_red",        value=FALSE)
    updateTextAreaInput(session, "cfg_robj_model_names",
      value=paste(c("model","fit","lm","glm","cox","surv",
                    "rf","xgb","result","summary","output"), collapse="\n"))
    updateTextAreaInput(session, "cfg_free_text_patterns",
      value=paste(free_text_patterns, collapse="\n"))
    updateTextAreaInput(session, "cfg_derived_id_patterns",
      value=paste(derived_id_patterns, collapse="\n"))
    updateCheckboxInput(session, "cfg_kanon_enabled", value=TRUE)
    showNotification(
      tagList(tags$strong("\u21ba Reset to defaults"),
              tags$br(),
              tags$span(style="font-size:var(--fs-emphasis);",
                "All parameters restored to installation defaults.")),
      type="message", duration=4
    )
  }, ignoreInit=TRUE)

  # ── Core session state (declared first - referenced by folder and reviewer observers) ──
  res_data          <- reactiveVal(list())
  session_decisions <- reactiveVal(list())
  registered_rev    <- new.env(hash=TRUE, parent=emptyenv())
  registered_ev     <- new.env(hash=TRUE, parent=emptyenv())  # dedup evidence observers
  log_trigger       <- reactiveVal(0L)
  batch_files_rv    <- reactiveVal(list())  # persistent cross-folder file batch
  # Airlock folder creation - stores the user's chosen location and name
  airlock_location_rv    <- reactiveVal(NULL)
  airlock_folder_name_rv <- reactiveVal(NULL)

  # ── AIRA batch-summary state (on-demand via button) ──────────────────────
  # aira_batch_data:      NULL or canonical 5-field response
  # aira_batch_in_flight: TRUE while the single batch call is pending
  # aira_batch_requested: TRUE once the user has clicked Generate. Gates
  #                       dispatch so summaries are on-demand, not automatic.
  # All three cleared on new assessment alongside res_data().
  aira_batch_data      <- reactiveVal(NULL)
  aira_batch_in_flight <- reactiveVal(FALSE)
  aira_batch_requested <- reactiveVal(FALSE)

  # ── AIRA disclosure review state (per-file, automatic background dispatch) ─
  # aira_review_data:       filepath -> canonical 5-field response
  # aira_review_in_flight:  filepath -> TRUE while that file's call is pending
  # aira_review_batch_id:   integer, incremented on each new assessment. Any
  #                         in-flight dispatch captures the id at scheduling
  #                         time and aborts on mismatch, so a new assessment
  #                         cleanly pre-empts the previous batch's queue.
  # aira_review_batch_total: total count of non-GREEN files in the current
  #                         dispatch queue. Drives the "N of M" progress line.
  # All cleared on new assessment and after report-generation cleanup.
  aira_review_data        <- reactiveVal(list())
  aira_review_in_flight   <- reactiveVal(list())
  aira_review_batch_id    <- reactiveVal(0L)
  aira_review_batch_total <- reactiveVal(0L)

  # excluded_paths: filepaths the reviewer has explicitly removed from
  # this batch via the per-file or bulk exclude buttons. Used to:
  #   - filter stale AIRA responses arriving for excluded files (an
  #     in-flight AIRA call cannot be cancelled cleanly, so we let it
  #     complete and discard the result on arrival)
  #   - drive the visual marker that prevents the file from rendering
  #     while still keeping batch_id stable
  # Cleared whenever a fresh assessment runs (alongside res_data()).
  excluded_paths <- reactiveVal(character(0))

  # remove_files_from_batch(paths, reason): apply exclusions to the
  # canonical batch state. Removes matching entries from res_data,
  # session_decisions, aira_review_data, aira_review_in_flight, and
  # records the paths in excluded_paths so any in-flight AIRA writes
  # for them get discarded on arrival.
  #
  # Multiple reactiveVal writes happen here in a deliberate order:
  #   1. excluded_paths first - so any in-flight handler that fires
  #      between writes sees the path as excluded and skips its write.
  #   2. res_data - the canonical batch list. Drives all renders.
  #   3. ancillary state - aira data, decisions etc. Removed by key.
  # No reactive observer should be triggered between step 1 and 2;
  # Shiny coalesces writes within a single observer body so this is
  # safe to call from inside an observeEvent.
  #
  # paths: character vector of filepaths to exclude.
  # reason: short string for logging (e.g. "manual", "bulk_uncertain",
  #         "bulk_insufficient").
  remove_files_from_batch <- function(paths, reason) {
    paths <- as.character(paths)
    paths <- paths[!is.na(paths) & nzchar(paths)]
    if (length(paths) == 0L) return(invisible(NULL))

    # Step 1: mark as excluded BEFORE removing from res_data, so any
    # in-flight AIRA response handlers see the new state on arrival.
    cur_excl <- excluded_paths()
    excluded_paths(unique(c(cur_excl, paths)))

    # Step 2: filter res_data. Match by filepath (canonical) with file
    # name as fallback because the rendered card uses r$file in some
    # places (file picker may produce results with no filepath).
    cur_res <- res_data()
    cur_res <- Filter(function(r) {
      r_path <- r$filepath %||% r$file
      !(r_path %in% paths)
    }, cur_res)
    res_data(cur_res)

    # Step 3: ancillary state. Remove by key from each list.
    cur_data <- aira_review_data()
    for (p in paths) cur_data[[p]] <- NULL
    aira_review_data(cur_data)

    cur_infl <- aira_review_in_flight()
    for (p in paths) cur_infl[[p]] <- NULL
    aira_review_in_flight(cur_infl)

    # Decisions are keyed by file name (basename), not filepath.
    cur_decs <- session_decisions()
    base_paths <- basename(paths)
    for (b in base_paths) cur_decs[[b]] <- NULL
    session_decisions(cur_decs)

    # Decrement aira_review_batch_total by the number of non-GREEN
    # excluded files so the progress counter "X of N" shrinks. Files
    # whose AIRA response had already been recorded count toward the
    # already-completed numerator naturally (because we removed their
    # entry from aira_review_data above, n_done shrinks too).
    cur_total <- aira_review_batch_total()
    aira_review_batch_total(max(0L, cur_total - length(paths)))

    log_event("INFO", "files_excluded",
              n_files = length(paths),
              reason  = reason,
              files   = paste(basename(paths), collapse = ", "))
    trace_log("INFO", "files_excluded",
              n = length(paths),
              reason = reason)

    invisible(NULL)
  }

  # Readiness summary for the batch-summary button (v2 prompt). The batch
  # summary integrates rule findings AND per-file AI reviews, so it can
  # only run after every non-GREEN file has a completed AI review (status
  # "ok"). This reactive returns a list(ready, n_total, n_complete,
  # n_pending, n_failed) driven off res_data() and aira_review_data().
  # Consumed by render_batch_aira_fn (disabled-state rendering) and by
  # the dispatch observer (defensive gate; user button click still checks).
  aira_reviews_readiness_rv <- reactive({
    res <- res_data()
    non_green <- Filter(function(r) !identical(r$classification %||% "", "GREEN"), res)
    n_total <- length(non_green)
    if (n_total == 0L) {
      return(list(ready = FALSE, n_total = 0L, n_complete = 0L,
                  n_pending = 0L, n_failed = 0L))
    }
    reviews <- aira_review_data()
    in_flight <- aira_review_in_flight()
    n_complete <- 0L; n_failed <- 0L; n_pending <- 0L
    for (r in non_green) {
      k <- r$filepath %||% r$file
      resp <- if (!is.null(k)) reviews[[k]] else NULL
      if (is.null(resp)) {
        n_pending <- n_pending + 1L
        next
      }
      st <- resp$status %||% ""
      if (identical(st, "ok")) {
        n_complete <- n_complete + 1L
      } else {
        n_failed <- n_failed + 1L
      }
    }
    list(
      ready      = (n_complete == n_total),
      n_total    = n_total,
      n_complete = n_complete,
      n_pending  = n_pending,
      n_failed   = n_failed
    )
  })

  # dispatch_one: recursively processes the AIRA review queue one file at
  # a time. Each call creates a promise in the current reactive context
  # (inherits the session's reactive domain), and schedules the next file
  # from its own onFulfilled/onRejected callback. The main thread gets
  # to handle other work between each call - progress ticks update, the
  # reviewer can click around, make decisions, etc.
  #
  # Sequential rather than parallel: each AIRA call spends ~7.7s of
  # synchronous setup on the main thread before handing off. Firing 21
  # in parallel serialises that setup anyway and makes the UI feel
  # frozen. One-at-a-time is honest about the cost and keeps the UI
  # responsive throughout.
  dispatch_one <- function(queue, batch_id, cfg, batch_t0) {
    # Stop if a new assessment pre-empted us.
    if (!identical(batch_id, aira_review_batch_id())) {
      trace_log("INFO", "aira_dispatch_preempted",
                batch_id = batch_id)
      log_event("INFO", "aira_review_batch_preempted",
                batch_id = batch_id)
      return(invisible(NULL))
    }
    # Queue empty - we're done.
    if (length(queue) == 0L) {
      total_ms <- as.integer(
        as.numeric(difftime(Sys.time(), batch_t0, units="secs")) * 1000)
      trace_log("INFO", "aira_dispatch_complete",
                batch_id = batch_id,
                duration_ms = total_ms)
      log_event("INFO", "aira_review_batch_complete",
                batch_id   = batch_id,
                duration_ms = total_ms)
      return(invisible(NULL))
    }

    first <- queue[[1]]
    rest  <- queue[-1]
    r_local  <- first$r
    fp_local <- first$fp
    pv <- ACTIVE_PROMPT$disclosure_review %||% "disclosure_review"

    # Mark this file in-flight before dispatch so the UI flips to spinner.
    infl <- aira_review_in_flight()
    infl[[fp_local]] <- TRUE
    aira_review_in_flight(infl)

    # AIRA START trace BEFORE promise creation. If the AIRA dispatch path
    # is what's killing R, we'll see this line as the last entry.
    trace_log("AIRA-START",
              sprintf("file=%s pos=%d/%d",
                      basename(fp_local %||% "(unknown)"),
                      aira_review_batch_total() - length(queue) + 1L,
                      aira_review_batch_total()),
              path = fp_local,
              batch_id = batch_id)

    log_event("INFO", "aira_review_call_started",
              file           = fp_local,
              prompt_version = pv,
              batch_id       = batch_id,
              queue_position = aira_review_batch_total() - length(queue) + 1L,
              queue_total    = aira_review_batch_total())

    prom <- tryCatch(
      aira_review_disclosure_async(r_local, cfg),
      error = function(e) {
        trace_log("AIRA-ERROR",
                  sprintf("dispatch threw: %s", conditionMessage(e)),
                  path = fp_local)
        log_event("WARN", "aira_review_call_dispatch_failed",
                  file           = fp_local,
                  prompt_version = pv,
                  batch_id       = batch_id,
                  message        = conditionMessage(e))
        promises::promise_resolve(list(
          status         = "unavailable",
          text           = "",
          prompt_version = pv,
          duration_ms    = 0L,
          reason         = paste0("dispatch: ", conditionMessage(e))))
      }
    )

    handle_response <- function(resp) {
      # AIRA DONE trace as soon as the response lands. Pairs with AIRA-START.
      trace_log("AIRA-DONE",
                sprintf("file=%s status=%s",
                        basename(fp_local %||% "(unknown)"),
                        resp$status %||% "?"),
                duration_ms = resp$duration_ms %||% 0L)

      # Abort write if batch changed during the call.
      if (!identical(batch_id, aira_review_batch_id())) {
        log_event("INFO", "aira_review_call_stale_response",
                  file     = fp_local,
                  batch_id = batch_id)
        return(invisible(NULL))
      }

      # Abort write if the file has been excluded from the batch by the
      # reviewer while the AIRA call was in flight. The call cannot be
      # cancelled cleanly so it completes; we just discard the result.
      # Continues to the next queue item below.
      if (fp_local %in% excluded_paths()) {
        log_event("INFO", "aira_review_call_excluded_response",
                  file     = fp_local,
                  batch_id = batch_id)
        # Still need to chain to next file - fall through to the
        # dispatch_one(rest, ...) call at the end.
        cur_in <- aira_review_in_flight()
        cur_in[[fp_local]] <- NULL
        aira_review_in_flight(cur_in)
        dispatch_one(rest, batch_id, cfg, batch_t0)
        return(invisible(NULL))
      }

      cur <- aira_review_data()
      cur[[fp_local]] <- resp
      aira_review_data(cur)

      cur_in <- aira_review_in_flight()
      cur_in[[fp_local]] <- NULL
      aira_review_in_flight(cur_in)

      ev <- switch(resp$status %||% "",
        "ok"          = list(level = "INFO", name = "aira_review_call_completed"),
        "timeout"     = list(level = "WARN",  name = "aira_review_timeout"),
        "unavailable" = list(level = "WARN",  name = "aira_review_unavailable"),
        "malformed"   = list(level = "WARN",  name = "aira_review_malformed_output"),
        "disabled"    = list(level = "INFO",  name = "aira_review_disabled_by_config"),
        list(level = "WARN", name = "aira_review_unknown_status"))
      # Phase timings from 27_aira.R (client construction, inference,
      # parse, future overhead). Missing fields land as NA_integer_ so
      # the log line always has the same schema regardless of which
      # path the response took. See .aira_timer in 27_aira.R.
      tm <- resp$timing_ms %||% list()
      dg <- resp$diag %||% list()
      log_event(ev$level, ev$name,
                file                   = fp_local,
                prompt_version         = resp$prompt_version %||% pv,
                duration_ms            = as.integer(resp$duration_ms %||% 0L),
                status                 = resp$status %||% "",
                reason                 = resp$reason %||% "",
                n_chars                = nchar(resp$text %||% ""),
                batch_id               = batch_id,
                client_construction_ms = as.integer(tm$client_construction_ms %||% NA_integer_),
                inference_ms           = as.integer(tm$inference_ms %||% NA_integer_),
                parse_ms               = as.integer(tm$parse_ms %||% NA_integer_),
                future_overhead_ms     = as.integer(tm$future_overhead_ms %||% NA_integer_),
                prompt_system_chars    = as.integer(dg$prompt_system_chars %||% NA_integer_),
                prompt_user_chars      = as.integer(dg$prompt_user_chars %||% NA_integer_),
                prompt_total_chars     = as.integer(dg$prompt_total_chars %||% NA_integer_),
                prompt_approx_tokens   = as.integer(dg$prompt_approx_tokens %||% NA_integer_),
                response_approx_tokens = as.integer(dg$response_approx_tokens %||% NA_integer_),
                input_tokens           = as.integer(dg$input_tokens %||% NA_integer_),
                output_tokens          = as.integer(dg$output_tokens %||% NA_integer_),
                finish_reason          = dg$finish_reason %||% NA_character_,
                likely_truncated       = isTRUE(dg$likely_truncated),
                max_tokens             = as.integer(dg$max_tokens %||% NA_integer_),
                model                  = dg$model %||% NA_character_,
                thinking_disabled      = dg$thinking_disabled %||% NA)

      # Chain: fire next file from inside this callback. Continues to
      # inherit reactive domain from the original observer.
      dispatch_one(rest, batch_id, cfg, batch_t0)
    }

    promises::then(
      prom,
      onFulfilled = handle_response,
      onRejected  = function(err) {
        # Promise-layer failure (different from a response with status
        # "unavailable"). Build a synthetic response so the UI still
        # shows something useful, then continue the queue.
        resp <- list(
          status         = "unavailable",
          text           = "",
          prompt_version = pv,
          duration_ms    = 0L,
          reason         = paste0("promise rejected: ", conditionMessage(err)))
        log_event("WARN", "aira_review_promise_rejected",
                  file     = fp_local,
                  batch_id = batch_id,
                  message  = conditionMessage(err))
        handle_response(resp)
      }
    )
    invisible(NULL)
  }

  # ── Folder selection ─────────────────────────────────────────────────────
  file_dir_rv <- reactiveVal(FILE_DIR_DEFAULT)

  # ── Modal folder browser ──────────────────────────────────────────────────
  # modal_dir_rv tracks where the user is *browsing* inside the modal.
  # It only syncs to file_dir_rv() when they click "Select this folder".
  modal_dir_rv <- reactiveVal(FILE_DIR_DEFAULT)

  # Helper: enforce workspace ceiling and normalise path
  safe_dir <- function(path) {
    real <- tryCatch(normalizePath(path, mustWork=FALSE), error=function(e) path)
    root <- tryCatch(normalizePath(WORKSPACE_FILES, mustWork=FALSE), error=function(e) WORKSPACE_FILES)
    if (!startsWith(real, root)) return(NULL)
    if (!dir.exists(real)) return(NULL)
    real
  }

  # Open modal - seed modal_dir_rv with current working directory
  observeEvent(input$open_folder_browser, {
    modal_dir_rv(file_dir_rv())
    session$sendCustomMessage("showModal", list(id="folderBrowserModal"))
  })

  # Breadcrumb inside modal - clickable segments
  output$fb_breadcrumb_ui <- renderUI({
    p    <- modal_dir_rv()
    root <- tryCatch(normalizePath(WORKSPACE_FILES, mustWork=FALSE), error=function(e) WORKSPACE_FILES)
    rel  <- if (startsWith(p, root)) substring(p, nchar(root)+1) else p
    parts <- Filter(nzchar, strsplit(rel, "/", fixed=TRUE)[[1]])
    # Build cumulative paths for each segment
    paths <- Reduce(function(acc, seg) c(acc, file.path(tail(acc,1), seg)),
                    parts, init=root, accumulate=TRUE)
    segs <- c("~", parts)
    tagList(lapply(seq_along(segs), function(i) {
      is_last <- i == length(segs)
      tags$span(
        tags$span(
          segs[i],
          style=paste0(
            "cursor:", if(is_last) "default" else "pointer", "; ",
            "color:", if(is_last) "#003366" else "#0066A1", "; ",
            if(!is_last) "text-decoration:underline;" else "font-weight:700;"),
          onclick=if(!is_last) sprintf(
            "Shiny.setInputValue('fb_crumb_click', '%s', {priority:'event'})",
            paths[i]) else ""
        ),
        if (!is_last) tags$span(" / ", style="color:#AAA; margin:0 0.1rem;")
      )
    }))
  })

  # Up button - greyed out at workspace root
  output$fb_up_ui <- renderUI({
    p    <- modal_dir_rv()
    root <- tryCatch(normalizePath(WORKSPACE_FILES, mustWork=FALSE), error=function(e) WORKSPACE_FILES)
    real <- tryCatch(normalizePath(p, mustWork=FALSE), error=function(e) p)
    at_root <- (real == root)
    actionButton("fb_up", "\u2191  Up",
      class="btn btn-sm btn-outline-secondary",
      style=paste0("font-size:var(--fs-body); padding:0.2rem 0.6rem;",
                   if(at_root) " opacity:0.4; pointer-events:none;" else ""))
  })

  # Folder list inside modal
  output$fb_folder_list_ui <- renderUI({
    p    <- modal_dir_rv()
    subs <- tryCatch(
      list.dirs(p, recursive=FALSE, full.names=TRUE),
      error=function(e) character(0)
    )
    subs <- subs[!grepl("^\\.", basename(subs))]
    subs <- sort(subs)
    if (length(subs) == 0)
      return(div(class="fb-empty", "No subfolders here"))
    tagList(lapply(subs, function(fp) {
      nm <- basename(fp)
      div(class="fb-folder-row",
        onclick=sprintf(
          "Shiny.setInputValue('fb_folder_click', '%s', {priority:'event'})",
          gsub("'", "\\'", fp, fixed=TRUE)),
        tags$span(class="fb-folder-icon", "\U0001F4C1"),
        tags$span(class="fb-folder-name", nm),
        tags$span(style="font-size:var(--fs-body); color:#AAA; flex-shrink:0;", "\u203a")
      )
    }))
  })

  # Current modal selection shown in footer
  output$fb_selected_path_ui <- renderUI({
    p    <- modal_dir_rv()
    root <- tryCatch(normalizePath(WORKSPACE_FILES, mustWork=FALSE), error=function(e) WORKSPACE_FILES)
    rel  <- if (startsWith(p, root)) sub(root, "~", p, fixed=TRUE) else p
    tags$span(rel)
  })

  # Navigate into a folder by clicking its row
  observeEvent(input$fb_folder_click, {
    p <- safe_dir(input$fb_folder_click)
    if (!is.null(p)) modal_dir_rv(p)
  }, ignoreNULL=TRUE, ignoreInit=TRUE)

  # Navigate via breadcrumb click
  observeEvent(input$fb_crumb_click, {
    p <- safe_dir(input$fb_crumb_click)
    if (!is.null(p)) modal_dir_rv(p)
  }, ignoreNULL=TRUE, ignoreInit=TRUE)

  # Up button inside modal
  observeEvent(input$fb_up, {
    current <- modal_dir_rv()
    p <- safe_dir(dirname(current))
    if (!is.null(p)) modal_dir_rv(p)
  }, ignoreNULL=TRUE, ignoreInit=TRUE)

  # Confirm - apply the browsed path to the working directory
  # ── Breadcrumb display (sidebar, reflects confirmed directory) ─────────────
  output$breadcrumb_display <- renderText({
    p    <- file_dir_rv()
    root <- WORKSPACE_FILES
    rel  <- if (startsWith(p, root)) sub(root, "~", p, fixed=TRUE) else p
    rel
  })

  # ── navigate_to - applies a confirmed path to the working directory ─────────
  navigate_to <- function(path) {
    if (!dir.exists(path)) return()
    real_path <- tryCatch(normalizePath(path, mustWork=FALSE), error=function(e) path)
    real_root <- tryCatch(normalizePath(WORKSPACE_FILES, mustWork=FALSE), error=function(e) WORKSPACE_FILES)
    if (!startsWith(real_path, real_root)) {
      showNotification(paste0("\u26a0 Navigation is restricted to ", WORKSPACE_FILES),
        type="warning", duration=5)
      return()
    }
    file_dir_rv(path)
    modal_dir_rv(path)
    # Do NOT clear results or batch - browsing to add more files should not
    # lose an existing assessment or the accumulated batch
    load_files()
  }

  # File list
  fdata <- reactiveVal(list())
  load_files <- function() {
    fps <- tryCatch(list.files(file_dir_rv(), full.names=TRUE), error=function(e) character(0))
    # Filter out directories and unreadable entries; size-label safely handles NA
    size_label <- function(sz) {
      if (is.na(sz) || !is.numeric(sz))             return("-")
      if (sz < 1024)                                return(paste0(sz, "B"))
      if (sz < 1048576)                             return(paste0(round(sz/1024, 1), "KB"))
      paste0(round(sz/1048576, 2), "MB")
    }
    entries <- lapply(fps, function(fp) {
      tryCatch({
        info <- file.info(fp)
        if (isTRUE(info$isdir)) return(NULL)          # skip directories
        sz   <- info$size
        ft   <- detect_file_type(fp)
        list(path = fp, name = basename(fp), ftype = ft,
             icon = type_icon(ft), size_label = size_label(sz))
      }, error = function(e) NULL)
    })
    fdata(Filter(Negate(is.null), entries))
  }
  observe({ load_files() })
  observeEvent(input$refresh, load_files())

  observeEvent(input$sel_all_btn, {
    session$sendCustomMessage("setAllCheckboxes", list(checked=TRUE))
    all_paths <- sapply(fdata(), function(f) f$path)
    session$sendInputMessage("selected_files", list(value=as.list(all_paths)))
  })
  observeEvent(input$sel_none_btn, {
    session$sendCustomMessage("setAllCheckboxes", list(checked=FALSE))
    session$sendInputMessage("selected_files", list(value=list()))
  })

  # ── Add selected files to batch ────────────────────────────────────────────
  observeEvent(input$add_to_batch_btn, {
    sel <- get_selected()
    if (length(sel) == 0) {
      showNotification("Select at least one file first.", type="warning", duration=3)
      return()
    }
    current  <- batch_files_rv()
    in_batch <- sapply(current, function(f) f$path)
    fd       <- fdata()
    fd_map   <- setNames(fd, sapply(fd, function(f) f$path))
    added    <- 0
    for (p in sel) {
      if (p %in% in_batch) next
      fi <- fd_map[[p]]
      if (!is.null(fi)) { current <- c(current, list(fi)); added <- added + 1 }
    }
    batch_files_rv(current)
    if (added > 0) {
      showNotification(
        paste0("\u2713  ", added, " file(s) added \u00b7 ",
               length(current), " in batch"),
        type="message", duration=2)
      session$sendCustomMessage("setAllCheckboxes", list(checked=FALSE))
      session$sendInputMessage("selected_files", list(value=list()))
    } else {
      showNotification("All selected files are already in the batch.", type="warning", duration=3)
    }
  }, ignoreInit=TRUE)

  # ── Remove one file from batch ─────────────────────────────────────────────
  observeEvent(input$remove_batch_path, {
    p <- input$remove_batch_path
    if (is.null(p) || !nzchar(p)) return()
    batch_files_rv(Filter(function(f) f$path != p, batch_files_rv()))
  }, ignoreNULL=TRUE, ignoreInit=TRUE)

  # ── Clear entire batch ─────────────────────────────────────────────────────
  observeEvent(input$clear_batch_btn, {
    batch_files_rv(list())
  }, ignoreInit=TRUE)

  output$file_list <- renderUI({
    fd <- fdata()
    if (length(fd)==0) return(div(style="color:#AAA; font-size:var(--fs-emphasis); padding:0.5rem;",
      paste("No files found in", file_dir_rv())))
    # Which paths are already in the batch?
    batch_paths <- sapply(batch_files_rv(), function(f) f$path)
    tagList(
      # Hidden checkbox group for selection state
      tags$div(style="display:none;",
        checkboxGroupInput("selected_files", label=NULL,
          choiceValues=lapply(fd, function(f) f$path),
          choiceNames=lapply(fd, function(f) f$name),
          selected=NULL)
      ),
      # Custom file rows with inline checkbox + preview button
      tagList(lapply(seq_along(fd), function(i) {
        f        <- fd[[i]]
        row_id   <- paste0("frow_", i)
        cb_id    <- paste0("fcb_", i)
        pv_id    <- paste0("fprev_", i)
        can_pv   <- can_preview(f$ftype, f$path)
        in_batch <- f$path %in% batch_paths
        div(
          id=row_id,
          class=if (in_batch) "frow-in-batch" else NULL,
          style=paste0(
            "display:flex; align-items:center; gap:0.4rem; ",
            "padding:0.28rem 0.4rem; margin-bottom:2px; ",
            "background:white; border:1px solid #E2EAF0; border-radius:5px; ",
            "transition:border-color 0.1s;"),
          # Checkbox (JS-driven, syncs with hidden checkboxGroupInput)
          tags$input(type="checkbox", id=cb_id,
            value=f$path, class="file-cb",
            style="flex-shrink:0; width:14px; height:14px; cursor:pointer; margin:0;",
            onclick=sprintf(
              "var cb=document.getElementById('%s');
               var vals=[];
               document.querySelectorAll('.file-cb:checked').forEach(function(el){vals.push(el.value);});
               Shiny.setInputValue('selected_files', vals, {priority:'event'});",
              cb_id)),
          # Type badge
          tags$span(class=paste0("f-badge t-", f$ftype), f$icon,
            style="flex-shrink:0;"),
          # Filename
          tags$span(class="f-name", f$name),
          # In-batch indicator
          if (in_batch) tags$span(class="in-batch-badge", "IN BATCH"),
          # Size
          tags$span(class="f-size", f$size_label),
          # Preview button (only for supported types)
          if (can_pv)
            tags$button(
              style=paste0(
                "flex-shrink:0; font-size:var(--fs-caption); padding:0.08rem 0.35rem; ",
                "border-radius:3px; border:1px solid #C8D8E8; ",
                "background:#F0F7FC; color:#0066A1; cursor:pointer; ",
                "font-weight:600; white-space:nowrap;"),
              onclick=sprintf(
                "Shiny.setInputValue('preview_file', '%s', {priority:'event'});
                 var modal=new bootstrap.Modal(document.getElementById('previewModal'));
                 modal.show();",
                gsub("'","\\'", f$path, fixed=TRUE)),
              "\u25A4 Preview"
            )
        )
      }))
    )
  })

  get_selected <- reactive({
    input$selected_files %||% character(0)
  })

  # Assessment results

  observeEvent(input$assess, {
    # Auto-collapse the file panel into review mode: once an assessment
    # runs, the reviewer is working through results, so the file browser
    # slides away to give the results full width. The reviewer can reopen
    # it via the rail to remove files and re-run.
    session$sendCustomMessage("collapseFilePanel", list())
    # Outer withCallingHandlers catches warnings/messages/conditions that
    # don't register with tryCatch - notably shiny.silent.error and some
    # C-level condition signals. Logs everything that would otherwise
    # silently abort the observer.
    withCallingHandlers({
    tryCatch({
      batch <- batch_files_rv()
      if (length(batch) == 0) {
        showNotification("Add files to the batch first using the \u2018+ Add to batch\u2019 button.", type="warning")
        return()
      }
      log_event("INFO", "batch_start",
                n_files = length(batch),
                mode    = APP_MODE)
      t0 <- Sys.time()
      # Clear previous results before running a new assessment
      res_data(list())
      session_decisions(list())
      rm(list=ls(envir=registered_rev), envir=registered_rev)
      # registered_ev is never cleared - Shiny observers cannot be unregistered;
      # clearing and re-registering causes observer stacking (double-toggle bug)
      ev_open(list())  # close all evidence panels
      # AIRA batch state must clear alongside res_data so stale summaries don't
      # render against a new batch.
      aira_batch_data(NULL)
      aira_batch_in_flight(FALSE)
      aira_batch_requested(FALSE)
      # AIRA per-file disclosure-review state clears alongside res_data.
      # Bumping the batch id here pre-empts any dispatches still running
      # from the previous batch - their onFulfilled handlers will see the
      # mismatch and abort before writing stale data.
      aira_review_data(list())
      aira_review_in_flight(list())
      aira_review_batch_id(aira_review_batch_id() + 1L)
      aira_review_batch_total(0L)
      # excluded_paths clears alongside res_data; the previous batch's
      # exclusions don't carry over to a new one.
      excluded_paths(character(0))
      log_event("DEBUG", "state_cleared")

      log_event("DEBUG", "progress_about_to_start")
      trace_log("INFO", "assessment_starting",
                n_files = length(batch))
      withProgress(message="Running assessment\u2026", value=0, {
        log_event("DEBUG", "progress_entered")
        trace_log("INFO", "withProgress_entered")
        r <- lapply(seq_along(batch), function(i) {
          incProgress(1/length(batch), detail=batch[[i]]$name)
          f_t0 <- Sys.time()
          # START trace BEFORE run_dte. If run_dte segfaults, this is the
          # last line we'll see for this file. The (i)/(N) prefix and the
          # full path tell us exactly which file killed the process.
          fp <- batch[[i]]$path
          fsize <- tryCatch(
            as.character(file.info(fp)$size),
            error = function(e) "?")
          trace_log("START",
                    sprintf("[%03d/%03d] %s",
                            i, length(batch),
                            basename(fp %||% "(unknown)")),
                    path = fp,
                    size = fsize)
          result <- tryCatch(
            run_dte(batch[[i]]$path, cfg=active_cfg()),
            error = function(e) {
              trace_log("ERROR",
                        sprintf("[%03d/%03d] run_dte threw",
                                i, length(batch)),
                        path = fp,
                        message = conditionMessage(e))
              log_event("ERROR", "run_dte_threw",
                        file      = batch[[i]]$path,
                        message   = conditionMessage(e),
                        trace     = .capture_trace())
              list(classification = "UNCERTAIN",
                   hits = list(list(rule="PARSE", outcome="UNCERTAIN",
                                    detail=paste0("Engine failure: ", conditionMessage(e)))),
                   file = batch[[i]]$path,
                   score = NA_real_)
            }
          )
          # DONE trace AFTER tryCatch returns. If we see this we know
          # run_dte completed (success or caught error); whatever crashes
          # later must be in subsequent code.
          trace_log("DONE",
                    sprintf("[%03d/%03d] %s",
                            i, length(batch),
                            basename(fp %||% "(unknown)")),
                    classification = result$classification %||% "?",
                    n_hits = length(result$hits %||% list()),
                    duration_ms = as.integer(
                      as.numeric(difftime(Sys.time(), f_t0, units="secs")) * 1000))
          log_event("DEBUG", "file_assessed",
                    file           = batch[[i]]$path,
                    classification = result$classification,
                    n_hits         = length(result$hits %||% list()),
                    duration_ms    = as.integer(
                      as.numeric(difftime(Sys.time(), f_t0, units="secs")) * 1000))
          result
        })
        trace_log("INFO", "lapply_returned",
                  n_results = length(r))
        log_event("DEBUG", "lapply_returned_inside_progress",
                  n_results = length(r))
      })
      trace_log("INFO", "withProgress_exited",
                n_results = length(r))
      log_event("DEBUG", "progress_closure_exited", n_results = length(r))

      # ACRO batch integration: correlate ACRO results.json with output files
      # in the batch. Injects per-output ACRO hits into matched file results
      # and escalates classifications where ACRO found SDC failures. No-op
      # when no ACRO results files are present in the batch.
      r <- acro_batch_integrate(r, active_cfg())
      trace_log("INFO", "acro_batch_integrate_complete",
                n_results = length(r))

      res_data(r)
      trace_log("INFO", "res_data_set",
                n_results = length(r))
      log_event("DEBUG", "res_data_set", n_results = length(r))

      log_event("INFO", "batch_end",
                n_files     = length(batch),
                duration_ms = as.integer(
                  as.numeric(difftime(Sys.time(), t0, units="secs")) * 1000))
    }, error = function(e) {
      log_event("ERROR", "assessment_failed",
                message = conditionMessage(e),
                trace   = .capture_trace())
      showNotification(
        tagList(
          tags$strong("Assessment failed"), tags$br(),
          tags$span(style="font-size:var(--fs-emphasis);", conditionMessage(e)), tags$br(),
          tags$span(style="font-size:var(--fs-body); color:var(--text-muted);",
                    paste0("See log: ", DIAG_LOG))
        ),
        type = "error", duration = NULL)
    })
    },
    warning = function(w) {
      log_event("WARN", "assess_warning",
                message = conditionMessage(w),
                class   = paste(class(w), collapse = "|"))
      invokeRestart("muffleWarning")
    },
    message = function(m) {
      log_event("DEBUG", "assess_message",
                message = trimws(conditionMessage(m)))
      invokeRestart("muffleMessage")
    },
    condition = function(c) {
      # Catches non-error, non-warning conditions including shiny.silent.error
      if (!inherits(c, c("error", "warning", "message"))) {
        log_event("WARN", "assess_condition",
                  message = conditionMessage(c),
                  class   = paste(class(c), collapse = "|"))
      }
    })
  })

  # ── Evidence toggle state ────────────────────────────────────
  ev_open <- reactiveVal(list())   # named logical: hid -> TRUE/FALSE

  # Observe all possible evidence button clicks dynamically
  observeEvent(res_data(), {
    .perf_t0 <- Sys.time()
    res <- res_data()
    log_event("INFO", "perf_evidence_observer_start",
              n_files = length(res))
    .perf_env <- new.env(parent=emptyenv()); .perf_env$n <- 0L
    for (r in res) {
      local({
        file_id <- gsub("[^a-zA-Z0-9]","_", r$file)
        for (hi in seq_along(r$hits)) {
          h <- r$hits[[hi]]
          if (is.null(h$evidence)) next
          hid <- paste0("ev_", file_id, "_", hi)
          local({
            lid <- hid
            if (exists(lid, envir=registered_ev)) return()
            assign(lid, TRUE, envir=registered_ev)
            .perf_env$n <- .perf_env$n + 1L
            observeEvent(input[[lid]], {
              cur <- ev_open()
              cur[[lid]] <- !isTRUE(cur[[lid]])
              ev_open(cur)
              # Update button label
              lbl <- if(isTRUE(cur[[lid]])) "Hide evidence ▲" else "Show evidence ▼"
              updateActionButton(session, lid, label=lbl)
            }, ignoreInit=TRUE)

            output[[paste0("ev_panel_", lid)]] <- renderUI({
              if (!isTRUE(ev_open()[[lid]])) return(NULL)
              res2 <- res_data()
              # Find the matching hit
              for (r2 in res2) {
                fid2 <- gsub("[^a-zA-Z0-9]","_", r2$file)
                for (hi2 in seq_along(r2$hits)) {
                  check_id <- paste0("ev_", fid2, "_", hi2)
                  if (check_id != lid) next
                  h2  <- r2$hits[[hi2]]
                  ev  <- h2$evidence
                  bar <- switch(h2$outcome %||% "",
                    "RED"=RED_C, "AMBER"=AMBER_C, "GREEN"=GREEN_C, "#7B1FA2")
                  if (is.null(ev)) return(NULL)
                  # identical() is NULL-safe and length-safe; ev$type == "X"
                  # throws "argument is of length zero" if ev$type is NULL,
                  # which can happen on degenerate inspector output.
                  panel_content <- if (identical(ev$type, "table")) {
                    render_evidence_html(ev, bar)
                  } else if (identical(ev$type, "lines")) {
                    render_script_evidence(ev, bar)
                  } else if (identical(ev$type, "svg")) {
                    tagList(
                      div(style="font-size:11px; color:var(--text-muted); margin-bottom:6px; font-style:italic;",
                        ev$caption %||% "SVG preview"),
                      div(style=paste0(
                            "border:2px solid ", bar, "; border-radius:6px; ",
                            "overflow:auto; max-height:380px; background:white; padding:4px;",
                            "text-align:center;"),
                        HTML(ev$src %||% ""))
                    )
                  } else NULL
                  if (is.null(panel_content)) return(NULL)
                  return(div(class="ev-panel", panel_content))
                }
              }
              NULL
            })
          })
        }
      })
    }
    log_event("INFO", "perf_evidence_observer_complete",
              n_files    = length(res),
              n_registered = .perf_env$n,
              elapsed_ms = as.integer(
                as.numeric(difftime(Sys.time(), .perf_t0, units="secs")) * 1000))
  })


  # ── AIRA batch-summary dispatch (on-demand via button) ──────
  # Triggered by the Generate AI Summary button in the batch-summary slot.
  # Keeps AIRA load proportional to user intent; lets the 45s batch timeout
  # be accepted explicitly rather than surprising the user mid-assessment.
  observeEvent(input$aira_generate_batch, {
    res <- res_data()
    if (length(res) == 0) return()

    cfg <- active_cfg()
    if (!aira_is_enabled(cfg)) return()

    uc_batch <- tryCatch(
      cfg$aira$use_cases$batch_summary$enabled,
      error = function(e) TRUE
    )
    if (identical(uc_batch, FALSE)) {
      log_event("INFO", "aira_batch_use_case_disabled")
      return()
    }

    if (isTRUE(aira_batch_in_flight())) return()

    # Defensive gate: the button should be disabled when reviews aren't
    # all complete, but in case the user hits the button during a race
    # (e.g. completed between render and click), check readiness again.
    rd <- aira_reviews_readiness_rv()
    if (!isTRUE(rd$ready)) {
      log_event("INFO", "aira_batch_blocked_reviews_incomplete",
                n_complete = rd$n_complete,
                n_total    = rd$n_total,
                n_pending  = rd$n_pending,
                n_failed   = rd$n_failed)
      showNotification(
        sprintf("AI batch summary needs a successful AI review for every non-GREEN file (%d of %d complete).",
                rd$n_complete, rd$n_total),
        type = "warning", duration = 8)
      return()
    }

    # Regeneration: clear previous result and cycle through loading state.
    aira_batch_data(NULL)
    aira_batch_requested(TRUE)
    aira_batch_in_flight(TRUE)
    pv <- ACTIVE_PROMPT$batch_summary %||% "batch_summary"

    # Snapshot the review data now so the prompt reflects exactly the
    # state the user confirmed by clicking the button. The async call
    # body uses this snapshot regardless of later reactive updates.
    aira_reviews_snapshot <- aira_review_data()

    log_event("INFO", "aira_batch_call_started",
              n_files        = length(res),
              n_ai_reviews   = length(aira_reviews_snapshot),
              prompt_version = pv,
              trigger        = "user_button")

    prom <- tryCatch(
      aira_summarise_batch_async(res, cfg,
                                 aira_reviews = aira_reviews_snapshot),
      error = function(e) {
        log_event("WARN", "aira_batch_call_dispatch_failed",
                  prompt_version = pv,
                  message        = conditionMessage(e))
        promises::promise_resolve(list(
          status         = "unavailable",
          text           = "",
          prompt_version = pv,
          duration_ms    = 0L,
          reason         = paste0("dispatch: ", conditionMessage(e))))
      }
    )

    promises::then(
      prom,
      onFulfilled = function(resp) {
        aira_batch_data(resp)
        aira_batch_in_flight(FALSE)

        ev <- switch(resp$status,
                     "ok"          = list(level = "INFO", name = "aira_batch_call_completed"),
                     "timeout"     = list(level = "WARN",  name = "aira_batch_timeout"),
                     "unavailable" = list(level = "WARN",  name = "aira_batch_unavailable"),
                     "malformed"   = list(level = "WARN",  name = "aira_batch_malformed_output"),
                     "disabled"    = list(level = "INFO",  name = "aira_batch_disabled_by_config"),
                     list(level = "WARN", name = "aira_batch_unknown_status"))
        # Phase timings from 27_aira.R - see .aira_timer for the schema.
        tm <- resp$timing_ms %||% list()
        log_event(ev$level, ev$name,
                  prompt_version         = resp$prompt_version %||% pv,
                  duration_ms            = as.integer(resp$duration_ms %||% 0L),
                  status                 = resp$status,
                  reason                 = resp$reason %||% "",
                  n_chars                = nchar(resp$text %||% ""),
                  client_construction_ms = as.integer(tm$client_construction_ms %||% NA_integer_),
                  inference_ms           = as.integer(tm$inference_ms %||% NA_integer_),
                  parse_ms               = as.integer(tm$parse_ms %||% NA_integer_),
                  future_overhead_ms     = as.integer(tm$future_overhead_ms %||% NA_integer_))
      },
      onRejected  = function(err) {
        aira_batch_in_flight(FALSE)
        aira_batch_data(list(
          status         = "unavailable",
          text           = "",
          prompt_version = pv,
          duration_ms    = 0L,
          reason         = paste0("promise rejected: ", conditionMessage(err))))
        log_event("WARN", "aira_batch_promise_rejected",
                  prompt_version = pv,
                  message        = conditionMessage(err))
      }
    )
  }, ignoreInit = TRUE)

  # ── AIRA disclosure-review observers (per-file, automatic) ───────────
  #
  # Fires when res_data() updates. Does two things:
  #
  #   (1) Registers output[[paste0("aira_review_", fid)]] renderers for
  #       every non-GREEN file. These read aira_review_data /
  #       aira_review_in_flight and call render_aira_review_banner() to
  #       produce "Queued" chip / spinner / banner as appropriate.
  #
  #   (2) Kicks off sequential background dispatch via chained promises.
  #       One file's AIRA call fires at a time. Each call's onFulfilled
  #       schedules the next file's dispatch, which lets the main thread
  #       breathe between calls (progress indicator updates, UI stays
  #       responsive, reviewer can interact with other cards).
  #
  # Promises are created inside this observeEvent (reactive context), so
  # reactive writes in onFulfilled/onRejected inherit the session's
  # reactive domain automatically - same pattern as the batch-summary
  # dispatch at line 1147.
  #
  # Batch-id guard: each dispatch captures the batch id that was current
  # when it was scheduled. If a new assessment starts mid-queue, the id
  # increments, and pending callbacks abort on mismatch rather than
  # writing into the new batch's state.
  observeEvent(res_data(), {
    res <- res_data()
    trace_log("INFO", "aira_observer_fired",
              n_results = length(res))

    # Register the renderers for every non-GREEN file. Runs even when
    # AIRA is disabled - the banner's rendering logic handles that case
    # (shows a greyed "AI review disabled" banner).
    for (r_item in res) {
      if (identical(r_item$classification, "GREEN")) next
      local({
        r    <- r_item
        fid  <- gsub("[^a-zA-Z0-9]","_", r$file)
        rid  <- paste0("aira_review_", fid)
        r_fp <- r$filepath %||% r$file

        output[[rid]] <- renderUI({
          resp      <- aira_review_data()[[r_fp]]
          in_flight <- isTRUE(aira_review_in_flight()[[r_fp]])
          render_aira_review_banner(resp, in_flight, fid)
        })
      })
    }
    trace_log("INFO", "aira_renderers_registered")

    # Gate dispatch on AIRA being enabled and the use case being enabled.
    cfg <- active_cfg()
    if (!aira_is_enabled(cfg)) {
      trace_log("INFO", "aira_dispatch_skipped_aira_disabled")
      log_event("INFO", "aira_review_batch_skipped_aira_disabled")
      return()
    }
    uc_enabled <- tryCatch(
      isTRUE(cfg$aira$use_cases$disclosure_review$enabled),
      error = function(e) FALSE
    )
    if (!uc_enabled) {
      trace_log("INFO", "aira_dispatch_skipped_use_case_disabled")
      log_event("INFO", "aira_review_batch_skipped_use_case_disabled")
      return()
    }

    # Build the queue of non-GREEN files to process.
    #
    # Files whose only hits are PARSE-class rules (file unreadable) are
    # skipped from AIRA dispatch - the AI has nothing to evaluate
    # beyond the fact that the file is unreadable, which the rule
    # engine has already said. Skipped files are seeded with a special
    # marker in aira_review_data so the renderer shows a "not
    # applicable" banner instead of "queued for AI review".
    cur_data <- aira_review_data()
    seeded_skipped <- 0L
    for (r_item in res) {
      if (identical(r_item$classification, "GREEN")) next
      r_fp <- r_item$filepath %||% r_item$file
      if (is.null(r_fp) || !nzchar(r_fp)) next
      if (!is.null(cur_data[[r_fp]])) next
      if (isTRUE(is_parse_only_result(r_item))) {
        cur_data[[r_fp]] <- list(
          status         = "skipped_parse_only",
          text           = "",
          prompt_version = ACTIVE_PROMPT$disclosure_review %||% "disclosure_review",
          duration_ms    = 0L,
          reason         = "rule engine could not parse file content")
        seeded_skipped <- seeded_skipped + 1L
      }
    }
    if (seeded_skipped > 0L) {
      aira_review_data(cur_data)
      log_event("INFO", "aira_review_skipped_parse_only",
                n_files = seeded_skipped)
      trace_log("INFO", "aira_skipped_parse_only",
                n_files = seeded_skipped)
    }

    queue <- Filter(Negate(is.null), lapply(res, function(r_item) {
      if (identical(r_item$classification, "GREEN")) return(NULL)
      r_fp <- r_item$filepath %||% r_item$file
      if (is.null(r_fp) || !nzchar(r_fp)) return(NULL)
      # Skip files that already have a cached response (covers the new
      # parse-only-skipped files seeded above, plus any genuinely
      # cached prior response - historic behaviour preserved).
      if (!is.null(aira_review_data()[[r_fp]])) return(NULL)
      list(r = r_item, fp = r_fp)
    }))
    n_total <- length(queue)
    if (n_total == 0L) {
      trace_log("INFO", "aira_dispatch_skipped_empty_queue")
      log_event("INFO", "aira_review_batch_skipped_empty_queue")
      return()
    }

    # Bump batch id so any in-flight dispatches from a previous assessment
    # detect the change and abort without writing.
    this_batch_id <- aira_review_batch_id() + 1L
    aira_review_batch_id(this_batch_id)

    # Set total so the progress indicator can render "X of N".
    aira_review_batch_total(n_total)

    trace_log("INFO", "aira_dispatch_starting",
              n_files = n_total,
              batch_id = this_batch_id)
    log_event("INFO", "aira_review_batch_start",
              n_files  = n_total,
              batch_id = this_batch_id)

    dispatch_one(queue, this_batch_id, cfg, batch_t0 = Sys.time())
  })

  # ── Results UI ───────────────────────────────────────────────
  output$results_ui <- renderUI({
    tryCatch({
      res <- res_data()
      if (length(res)==0)
        return(div(class="ph", div(class="ph-icon", "-"),
          tags$h5("No assessment yet"),
          tags$p(style="font-size:var(--fs-emphasis);", "Select files and click Run Assessment")))

      # Sanitise every result so downstream NULL-comparisons cannot throw.
      # See sanitise_result() at top of file.
      res <- lapply(res, sanitise_result)

      .perf_t0 <- Sys.time()
      log_event("INFO", "perf_results_ui_render_start", n_files = length(res))

      # Per-file card renderer. Named so it can render both standalone
      # files and ACRO package members with identical card structure.
      # Returns the full .file-card div for one result `r`.
      render_one_card <- function(r) {
      cl <- r$classification
      # Classification modifier for the outer file-card
      fc_mod <- switch(cl, "GREEN"="file-card-green","AMBER"="file-card-amber",
                           "RED"="file-card-red","file-card-unc")
      bar <- switch(cl, "GREEN"=GREEN_C,"AMBER"=AMBER_C,"RED"=RED_C,"#7B1FA2")
      lbl <- switch(cl, "GREEN"="GREEN - Egress Approved","AMBER"="AMBER - Review Required",
                    "RED"="RED - Egress Rejected","UNCERTAIN - Manual Review")
      fid_hint <- gsub("[^a-zA-Z0-9]","_", r$file)
      size_str <- if(r$size_bytes<1048576) paste0(round(r$size_bytes/1024,1)," KB")
                  else                     paste0(round(r$size_bytes/1048576,2)," MB")
      # ── File card: single framed container for the whole per-file unit ──
      # Outer .file-card has a left border matching classification so the
      # reviewer can see at a glance where one file ends and the next begins.
      # Inside: verdict banner, meta line, AIRA slot, rule results,
      # reviewer strip, post-decision state. No hr separator between files -
      # margin on .file-card does the work.
      div(class = paste("file-card", fc_mod), id = paste0("fcard-", fid_hint),
        # Verdict banner: classification label + filename + optional preview.
        # Score and rule count removed from here - they live in the meta line
        # below to avoid three-way duplication (banner / meta / batch header).
        div(class="fc-banner",
          div(class="fc-label",
            style=paste0("background:",bar,";"),
            lbl),
          div(class="fc-filename", r$file),
          if (can_preview(r$file_type, r$filepath))
            tags$button(class="fc-preview-btn",
              onclick=sprintf(
                "Shiny.setInputValue('preview_file','%s',{priority:'event'});
                 var m=new bootstrap.Modal(document.getElementById('previewModal'));
                 m.show();",
                gsub("'", "\\'", r$filepath, fixed=TRUE)),
              "\u25a4 Preview"),
          # Per-file exclude (\u00d7) button. Removes this single file from
          # the batch on click, no confirmation modal (per design - the
          # reviewer's intent is unambiguous when they click next to a
          # specific file). The filepath is sent to input$exclude_one
          # which routes through the bulk_exclude_observer below. Shiny
          # priority:'event' ensures repeated clicks register even on
          # the same path.
          tags$button(class="fc-exclude-btn",
            title="Remove this file from the batch",
            onclick=sprintf(
              "Shiny.setInputValue('exclude_one','%s',{priority:'event'});",
              gsub("'", "\\'", r$filepath %||% r$file, fixed=TRUE)),
            "\u00d7 Remove")
        ),
        # Meta line: single row of muted inline text. Replaces the former
        # 4-box meta-strip - same information, ~80% less vertical space.
        div(class="fc-meta-line",
          tags$span(class="fc-meta-item", r$type_label),
          tags$span(class="fc-meta-sep", "\u00b7"),
          tags$span(class="fc-meta-item", size_str),
          tags$span(class="fc-meta-sep", "\u00b7"),
          tags$span(class="fc-meta-item",
            sprintf("Score %d/100", r$score)),
          tags$span(class="fc-meta-sep", "\u00b7"),
          tags$span(class="fc-meta-item",
            sprintf("%d rule%s fired",
                    length(r$hits),
                    if(length(r$hits)!=1) "s" else ""))
        ),
        div(class="sect-hd","RULE RESULTS"),
        if (length(r$hits)==0)
          div(style="color:var(--text-hint); font-size:var(--fs-emphasis);","No rule hits.")
        else {
          hit_ids <- paste0("ev_", gsub("[^a-zA-Z0-9]","_", r$file),
                            "_", seq_along(r$hits))
          tagList(lapply(seq_along(r$hits), function(hi) {
            h   <- r$hits[[hi]]
            hid <- hit_ids[hi]
            hcl <- switch(h$outcome,"RED"="hr-red","AMBER"="hr-amb","GREEN"="hr-grn","hr-unc")
            bcl <- switch(h$outcome,"RED"="hb-red","AMBER"="hb-amb","GREEN"="hb-grn","hb-unc")
            ebc <- switch(h$outcome,"RED"="ev-btn-red","AMBER"="ev-btn-amb",
                          "GREEN"="ev-btn-grn","ev-btn-amb")
            rd  <- RULES[[gsub("-","",h$rule)]]
            has_ev  <- !is.null(h$evidence)
            is_svg_ev <- has_ev && isTRUE(h$evidence$type == "svg")
            # Fetch remediation for this hit (NULL for GREEN/clean-pass rules)
            rem_text <- get_remediation(h$rule, h$detail %||% "")
            rid_btn  <- paste0("rem_", hid)

            div(class=paste("hit-card",hcl),
              div(style="flex-shrink:0;",
                span(class=paste("hbadge",bcl), h$outcome),
                div(class="h-rid", h$rule)
              ),
              div(style="flex:1; min-width:0;",
                div(class="h-det", h$detail),
                if (!is.null(rd))

                # Remediation guidance - shown for RED/AMBER hits
                if (!is.null(rem_text) && h$outcome %in% c("RED","AMBER","UNCERTAIN"))
                  div(class="rem-block",
                    div(class="rem-hd",
                      tags$span(class="rem-icon", "🔧"),
                      "How to fix this"
                    ),
                    div(class="rem-body", rem_text)
                  ),

                if (is_svg_ev)
                  # SVG: open full preview modal directly
                  tags$button(
                    style=paste0(
                      "font-size:var(--fs-caption); padding:0.12rem 0.5rem; border-radius:4px; ",
                      "border:1px solid currentColor; cursor:pointer; margin-top:0.35rem; ",
                      "display:inline-flex; align-items:center; gap:0.2rem; ",
                      "background:transparent; font-weight:600; transition:opacity 0.12s; ",
                      "color:", bar, ";"),
                    onclick=sprintf(
                      "Shiny.setInputValue('preview_file','%s',{priority:'event'});
                       var m=new bootstrap.Modal(document.getElementById('previewModal'));
                       m.show();",
                      gsub("'", "\\'", r$filepath, fixed=TRUE)),
                    "\u25a4 Preview image"
                  ),
                if (has_ev && !is_svg_ev)
                  actionButton(hid, "Show evidence \u25bc",
                    class=paste("ev-btn", ebc),
                    style="margin-top:0.35rem;"),
                if (has_ev && !is_svg_ev)
                  uiOutput(paste0("ev_panel_", hid))
              )
            )
          }))
        },
        # ── ACRO researcher comments ──
        # Two sources, depending on which file this card is:
        #   - A correlated OUTPUT file (e.g. crosstab.pkl): comments arrive
        #     as r$acro_outputs, injected by acro_batch_integrate.
        #   - The results.json SESSION file itself: comments live in
        #     r$acro_data$outputs; surface the whole session here so the
        #     reviewer sees every comment even if they only added the JSON
        #     (and not the output files) to the batch.
        # See render_acro_comments_block.
        if (!is.null(r$acro_outputs))
          render_acro_comments_block(r$acro_outputs)
        else if (!is.null(r$acro_data) && is.list(r$acro_data$outputs))
          render_acro_comments_block(unname(r$acro_data$outputs)),
        # ── AIRA disclosure review banner (RED/AMBER/UNCERTAIN only) ──
        # Slot shows three states:
        #   - not requested: "Request AI disclosure review" button
        #   - in flight:     loading chip
        #   - response:      structured banner or raw-text fallback
        # Renders via output[[paste0("aira_review_", fid_hint)]] registered
        # in the per-file AIRA review observer. GREEN files get no slot at all.
        if (!identical(cl, "GREEN"))
          uiOutput(paste0("aira_review_", fid_hint)),
        # ── Review strip ─────────────────────────────────
        local({
          fid  <- gsub("[^a-zA-Z0-9]","_", r$file)
          rid  <- paste0("rev_submit_", fid)
          oid  <- paste0("rev_outcome_", fid)
          nid  <- paste0("rev_note_", fid)
          logged_id <- paste0("rev_logged_", fid)
          cl <- r$classification

          div(class="review-strip",
            tags$label(if (APP_MODE == "reviewer") "Reviewer decision:" else "Your response:"),
            div(style="display:flex; gap:0.5rem; align-items:flex-start; flex-wrap:wrap;",
              selectInput(oid, label=NULL,
                choices=if (APP_MODE == "reviewer") switch(cl,
                  RED   = c(
                    "Confirm rejection - egress not permitted"              = "RED",
                    "Approve exception - I accept responsibility for this decision" = "GREEN",
                    "Refer for specialist review"                           = "AMBER"
                  ),
                  AMBER = c(
                    "Refer for further review"                              = "AMBER",
                    "Approve - no disclosure risk identified"               = "GREEN",
                    "Confirm rejection - egress not permitted"              = "RED"
                  ),
                  GREEN = c(
                    "Approve egress"                                        = "GREEN",
                    "Escalate - requires further scrutiny"                  = "AMBER",
                    "Confirm rejection - egress not permitted"              = "RED"
                  ),
                  c(
                    "Refer for specialist review"                           = "AMBER",
                    "Confirm rejection - egress not permitted"              = "RED"
                  )
                ) else switch(cl,
                  RED   = c(
                    "Accept - I will remediate and resubmit"               = "RED",
                    "Request exception - file is safe, see justification"  = "GREEN",
                    "Request manual airlock review"                        = "AMBER"
                  ),
                  AMBER = c(
                    "Accept for airlock review - flagged content noted"    = "AMBER",
                    "Confirm safe - no disclosure risk found"              = "GREEN",
                    "Withdraw from this submission"                        = "RED"
                  ),
                  GREEN = c(
                    "Confirm - include in this submission"                 = "GREEN",
                    "Request review - I have a concern"                    = "AMBER",
                    "Withdraw from this submission"                        = "RED"
                  ),
                  c(
                    "Request specialist manual review"                     = "AMBER",
                    "Withdraw from this submission"                        = "RED"
                  )
                ), width="auto") |>
                tagAppendAttributes(
                  onchange = sprintf(
                    "dteCheckOverrideNote(this,'%s','%s','%s','%s')", cl, nid, rid, oid)
                ),
              div(style="flex:1; min-width:220px;",
                textInput(nid, label=NULL,
                  placeholder=if (APP_MODE == "reviewer") switch(cl,
                    RED   = "Governance justification - document why this exception is warranted and who has approved it",
                    AMBER = "Record your assessment rationale for the audit log",
                    GREEN = "Approval note for audit record (optional)",
                    "Assessment note for audit record"
                  ) else switch(cl,
                    RED   = paste0("Justification for airlock exception - explain why",
                                   " this file is safe despite the finding (required if requesting exception)"),
                    AMBER = "Add context to assist the airlock reviewer (optional)",
                    GREEN = "Note (optional)",
                    "Note (optional)"
                  )) |>
                  tagAppendAttributes(
                    oninput = sprintf(
                      "dteNoteInput(this,'%s','%s','%s')", oid, rid, cl)
                  )
              ),
              actionButton(rid,
                if (APP_MODE == "reviewer") "Record decision" else "Save response",
                class="btn btn-sm btn-primary rev-submit")
            ),
            uiOutput(logged_id)
          )
        })
      )
      }  # end render_one_card

      # Group the flat result list into ACRO packages + standalone files.
      # ACRO packages render as a bounded container: session header (title,
      # version, config, checklist, member roll-up) leading, then the member
      # cards nested inside. Standalone files render as before. This makes
      # the ACRO review lead the presentation while general file rules still
      # show within each member card.
      grouped <- tryCatch(acro_group_results(res),
                          error = function(e) list(packages = list(), standalone = res))

      package_blocks <- lapply(grouped$packages, function(pkg) {
        render_acro_package(pkg, render_one_card)
      })

      standalone_blocks <- lapply(grouped$standalone, render_one_card)

      ui <- tagList(package_blocks, standalone_blocks)
      log_event("INFO", "perf_results_ui_render_complete",
                n_files    = length(res),
                elapsed_ms = as.integer(
                as.numeric(difftime(Sys.time(), .perf_t0, units="secs")) * 1000))
    ui
    }, error = function(e) {
      log_event("ERROR", "results_ui_render_failed",
                message = conditionMessage(e),
                trace   = .capture_trace())
      div(class="ph",
          style="color:#C62828; border:1px solid #C62828; padding:1rem;",
          tags$h5("Error rendering results"),
          tags$p(style="font-size:var(--fs-emphasis);", conditionMessage(e)),
          tags$p(style="font-size:var(--fs-body); color:var(--text-muted);",
                 paste0("See log: ", DIAG_LOG)))
    })
  })

  # ── Rule summary ─────────────────────────────────────────────────────────────
  # ── Consolidated batch header ────────────────────────────────
  # Single renderer for all batch-level context: RAG tiles, risk score,
  # cross-file linkage, AIRA batch summary, batch actions, rule summary.
  # Replaces the former rule_summary_ui, linkage_risk_ui, review_complete_ui,
  # and aira_batch_summary_ui outputs. See helper functions at top of file:
  # render_linkage_risk_fn, render_rule_summary_fn, render_batch_aira_fn.
  output$batch_header_ui <- renderUI({
    tryCatch({
    res <- res_data()
    if (length(res) == 0) return(NULL)
    # Sanitise every result so downstream NULL-comparisons cannot throw.
    # Cheap: scalar field reads with safe defaults. See sanitise_result()
    # at top of file for details.
    res <- lapply(res, sanitise_result)
    .perf_t0 <- Sys.time()
    log_event("INFO", "perf_batch_header_render_start", n_files = length(res))

    decs <- session_decisions()
    cfg  <- active_cfg()

    # ── Counts ─────────────────────────────────────────────
    n_total <- length(res)
    n_red   <- sum(sapply(res, function(r) identical(r$classification, "RED")))
    n_amber <- sum(sapply(res, function(r) identical(r$classification, "AMBER")))
    n_green <- sum(sapply(res, function(r) identical(r$classification, "GREEN")))
    n_unc   <- sum(sapply(res, function(r) identical(r$classification, "UNCERTAIN")))

    # ── Risk score (same calc as results_tab_badge) ────────
    sc <- tryCatch(
      calculate_batch_score(res,
        check_linkage_risk(res, min_categories=3L, min_shared_files=2L)),
      error = function(e) NULL)

    # ── Zone 1: scale + re-run action ──────────────────────
    # Count ACRO packages so the reviewer sees the batch structure
    # (e.g. "1 ACRO package + 2 files") rather than a flat file count.
    n_pkg <- tryCatch(
      length(acro_group_results(res)$packages),
      error = function(e) 0L)
    scale_label <- if (n_pkg > 0L) {
      n_standalone <- tryCatch(length(acro_group_results(res)$standalone),
                               error = function(e) n_total)
      paste0("BATCH \u00b7 ", n_pkg, " ACRO package",
             if (n_pkg != 1L) "s" else "",
             if (n_standalone > 0L)
               paste0(" + ", n_standalone, " file", if (n_standalone != 1L) "s" else "")
             else "")
    } else {
      paste0("BATCH \u00b7 ", n_total, " file", if(n_total!=1)"s" else "")
    }
    zone1 <- div(class="bh-zone bh-zone-scale",
      div(class="bh-scale-label", scale_label),
      actionButton("assess", class="btn-bh-rerun",
        tagList(tags$span(style="margin-right:0.3rem;", "\u21bb"),
                "Re-run assessment"))
    )

    # ── Zone 2: RAG tiles ──────────────────────────────────
    tile <- function(cls, label, n) div(class=paste("rag-tile", cls),
      div(class="rag-tile-n", n),
      div(class="rag-tile-label", label))
    zone2 <- div(class="bh-zone bh-rag-row",
      tile("rag-red",   "RED",   n_red),
      tile("rag-amber", "AMBER", n_amber),
      tile("rag-green", "GREEN", n_green),
      if (n_unc > 0) tile("rag-unc",   "UNCERTAIN", n_unc) else NULL
    )

    # ── Zone 3: risk score bar ─────────────────────────────
    zone3 <- if (!is.null(sc)) {
      total <- as.integer(sc$total %||% 0L)
      band  <- sc$band %||% ""
      col   <- sc$tl_colour %||% "#999"
      pct   <- max(0L, min(100L, total))
      div(class="bh-zone bh-score",
        div(class="bh-score-row",
          span(class="bh-score-label", "Risk score"),
          div(class="bh-score-bar-track",
            div(class="bh-score-bar-fill",
              style=sprintf("width:%d%%; background:%s;", pct, col))),
          span(class="bh-score-value",
            sprintf("%d / 100", total)),
          span(class="bh-score-band",
            style=paste0("background:", col, ";"),
            band),
          tags$span(class="bh-score-info",
            title="Click for score breakdown",
            onclick="Shiny.setInputValue('show_score_breakdown', Math.random(), {priority:'event'});",
            "\u2139")
        )
      )
    } else NULL

    # ── Zone 4: cross-file linkage (conditional) ───────────
    zone4 <- render_linkage_risk_fn(res)

    # ── Zone 5: AIRA batch summary ─────────────────────────
    zone5 <- render_batch_aira_fn(
      aira_batch_in_flight(),
      aira_batch_data(),
      cfg,
      n_total,
      readiness = aira_reviews_readiness_rv())

    # ── Zone 5b: AIRA disclosure-review progress ───────────
    # Shows a progress row while the automatic per-file review queue is
    # processing. Reactive on aira_review_batch_total / aira_review_data /
    # aira_review_in_flight so it updates live as each file completes.
    # Hidden when nothing is queued (total == 0) or when all complete.
    review_total <- aira_review_batch_total()
    zone5b <- if (review_total > 0L) {
      rdata <- aira_review_data()
      rinfl <- aira_review_in_flight()
      n_done      <- length(rdata)
      n_inflight  <- sum(vapply(rinfl, isTRUE, logical(1)))
      n_queued    <- review_total - n_done - n_inflight
      n_failed    <- sum(vapply(rdata, function(r) {
        !identical(r$status %||% "", "ok") &&
        !identical(r$status %||% "", "malformed")
      }, logical(1)))
      all_done <- n_done >= review_total
      pct <- as.integer(round(100 * n_done / max(1L, review_total)))

      if (all_done) {
        # Terminal state: all files assessed. Brief confirmation row,
        # styled muted; disappears on next assessment when total resets.
        div(class = "bh-zone bh-review-progress bh-review-progress-done",
          div(class = "bh-zone-hd", "AI DISCLOSURE REVIEWS"),
          div(class = "bh-review-progress-body",
            tags$span(class = "bh-review-progress-status",
              sprintf("\u2713 Complete \u00b7 %d file%s assessed",
                      review_total,
                      if (review_total != 1L) "s" else "")),
            if (n_failed > 0L)
              tags$span(class = "bh-review-progress-failed",
                sprintf(" \u00b7 %d failed", n_failed))
          )
        )
      } else {
        # In-progress state: bar + counts.
        div(class = "bh-zone bh-review-progress",
          div(class = "bh-zone-hd", "AI DISCLOSURE REVIEWS"),
          div(class = "bh-review-progress-body",
            tags$span(class = "bh-review-progress-status",
              sprintf("%d of %d complete", n_done, review_total)),
            if (n_failed > 0L)
              tags$span(class = "bh-review-progress-failed",
                sprintf(" \u00b7 %d failed", n_failed)),
            if (n_queued > 0L || n_inflight > 0L)
              tags$span(class = "bh-review-progress-remaining",
                sprintf(" \u00b7 %d remaining",
                        n_queued + n_inflight))
          ),
          div(class = "bh-review-progress-bar-outer",
            div(class = "bh-review-progress-bar-inner",
                style = sprintf("width: %d%%;", pct))
          )
        )
      }
    } else NULL

    # ── Zone exclude: bulk-remove buttons ──────────────────
    # Visible only when the batch contains at least one UNCERTAIN file
    # (rule-engine classification) or at least one INSUFFICIENT-rated
    # file (AI review). Two buttons rendered side by side; either or
    # both may be present. Click triggers a confirmation modal.
    n_insuff <- 0L
    insuff_paths <- character(0)
    aira_data_now <- aira_review_data()
    for (r in res) {
      r_path <- r$filepath %||% r$file
      review <- aira_data_now[[r_path]]
      if (is.null(review)) next
      if (!identical(review$status %||% "", "ok")) next
      txt <- review$text %||% ""
      if (!nzchar(txt)) next
      if (!requireNamespace("jsonlite", quietly = TRUE)) next
      parsed <- tryCatch(
        jsonlite::fromJSON(txt, simplifyVector = FALSE),
        error = function(e) NULL)
      if (is.null(parsed) || is.null(parsed$risk_level)) next
      rl <- toupper(as.character(parsed$risk_level)[1L])
      if (identical(rl, "INSUFFICIENT")) {
        n_insuff <- n_insuff + 1L
        insuff_paths <- c(insuff_paths, r_path)
      }
    }

    zone_exclude <- if (n_unc > 0L || n_insuff > 0L) {
      div(class="bh-zone bh-exclude",
        div(class="bh-zone-hd", "REMOVE FROM BATCH"),
        div(class="bh-exclude-row",
          if (n_unc > 0L)
            actionButton("bulk_exclude_uncertain",
              sprintf("\u00d7  Remove UNCERTAIN  \u00b7  %d file%s",
                      n_unc, if (n_unc != 1L) "s" else ""),
              class="btn-bulk-exclude")
          else NULL,
          if (n_insuff > 0L)
            actionButton("bulk_exclude_insufficient",
              sprintf("\u00d7  Remove INSUFFICIENT  \u00b7  %d file%s",
                      n_insuff, if (n_insuff != 1L) "s" else ""),
              class="btn-bulk-exclude")
          else NULL,
          span(class="bh-exclude-note",
            "Removes files entirely - they will not appear in the report.")
        )
      )
    } else NULL

    # ── Zone 6: batch actions ──────────────────────────────
    n_decided <- sum(sapply(res, function(r) !is.null(decs[[r$file]])))
    all_done  <- n_decided == n_total
    n_left    <- n_total - n_decided

    n_acc <- sum(sapply(res, function(r)
      r$classification %in% c("GREEN","AMBER") && is.null(decs[[r$file]])))
    n_rej <- sum(sapply(res, function(r)
      r$classification == "RED" && is.null(decs[[r$file]])))

    # Determine overall verdict for the Review Complete message (only used when all_done)
    overall_final <- if (all_done && length(decs) > 0) {
      # Find the first decided outcome as a starting point, then escalate
      # to RED/AMBER if present. Doesn't assume res[[1]] is decided.
      ov <- NA_character_
      for (r in res) {
        d <- decs[[r$file]]
        if (!is.null(d) && !is.null(d$outcome)) { ov <- d$outcome; break }
      }
      for (d in decs) {
        if (is.null(d) || is.null(d$outcome)) next
        if (d$outcome == "RED")  { ov <- "RED"; break }
        if (d$outcome == "AMBER")  ov <- "AMBER"
      }
      ov
    } else NA_character_

    zone6 <- div(class="bh-zone bh-actions",
      div(class="bh-zone-hd", "ACTIONS"),
      # Accept all GREEN & AMBER
      if (n_acc > 0)
        div(class="bh-action-row",
          actionButton("accept_all",
            sprintf("\u2714  Accept all GREEN & AMBER  \u00b7  %d file%s",
                    n_acc, if(n_acc!=1)"s" else ""),
            class="btn-accept-all"),
          span(class="bh-action-note",
            "RED files require individual review or use Reject all RED.")
        )
      else NULL,
      # Reject all RED
      if (n_rej > 0)
        div(class="bh-action-row",
          actionButton("reject_all_red",
            sprintf("\u2718  Reject all RED  \u00b7  %d file%s",
                    n_rej, if(n_rej!=1)"s" else ""),
            class="btn-reject-all"),
          span(class="bh-action-note",
            "Confirms rejection and removes file(s) from the batch. Rationale note required.")
        )
      else NULL,
      # Review Complete readiness strip + button. When not all_done, we
      # render a tagAppendAttributes wrapper to add disabled="disabled"
      # instead of passing disabled= to actionButton (which behaves
      # inconsistently across bslib versions).
      div(class=paste0("bh-action-row bh-review-complete-row ",
                       if (all_done) "bh-rc-ready" else "bh-rc-pending"),
        if (all_done)
          actionButton("review_complete",
            if (APP_MODE == "reviewer")
              "\u2705  Complete Review - Generate Governance Report"
            else
              "\u2705  Submit for Airlock Review - Generate Report",
            class="btn-review-complete")
        else
          actionButton("review_complete",
            if (APP_MODE == "reviewer")
              "\u2705  Complete Review - Generate Governance Report"
            else
              "\u2705  Submit for Airlock Review - Generate Report",
            class="btn-review-complete") |>
          tagAppendAttributes(disabled = "disabled"),
        span(class="bh-rc-status",
          if (all_done)
            sprintf("%s - %d of %d decisions recorded",
                    if (!is.na(overall_final) && nzchar(overall_final))
                      paste0(overall_final, ": ready to generate report")
                    else "Ready to generate report",
                    n_decided, n_total)
          else
            sprintf("%d of %d decisions recorded - %d file%s still need%s a decision",
                    n_decided, n_total, n_left,
                    if (n_left!=1)"s" else "",
                    if (n_left==1)"s" else "")
        )
      )
    )

    # ── Zone 7: rule summary, collapsible ──────────────────
    n_rules <- length(unique(unlist(lapply(res,
      function(r) sapply(r$hits, `[[`, "rule")))))
    zone7 <- if (n_rules > 0) {
      tags$details(class="bh-zone bh-rule-summary",
        tags$summary(class="bh-rule-summary-hd",
          sprintf("Rule summary \u00b7 %d rule%s fired across %d file%s",
                  n_rules, if(n_rules!=1)"s" else "",
                  n_total,  if(n_total!=1)"s" else "")),
        render_rule_summary_fn(res)
      )
    } else NULL

    # ── Assemble ───────────────────────────────────────────
    out <- div(class="batch-header",
      zone1,
      zone2,
      zone3,
      zone4,
      zone5,
      zone5b,
      zone_exclude,
      zone6,
      zone7
    )
    log_event("INFO", "perf_batch_header_render_complete",
              n_files    = length(res),
              elapsed_ms = as.integer(
                as.numeric(difftime(Sys.time(), .perf_t0, units="secs")) * 1000))
    out
    }, error = function(e) {
      # Hard guard: if anything in the batch header render path throws,
      # log it and return a small visible error block rather than letting
      # the error escape and disconnect the session.
      log_event("ERROR", "batch_header_render_failed",
                message = conditionMessage(e),
                trace   = .capture_trace())
      trace_log("ERROR", "batch_header_render_failed",
                message = conditionMessage(e))
      div(class = "ph",
          style = "color:#C62828; border:1px solid #C62828; padding:0.6rem; margin:0.4rem 0; border-radius:4px;",
          tags$strong("Batch header could not render"),
          tags$br(),
          tags$span(style = "font-size:var(--fs-emphasis);", conditionMessage(e)))
    })
  })

  # Batch summary
  # ── Batch section header ──────────────────────────────────────────────────
  output$assess_btn_ui <- renderUI({
    n <- length(batch_files_rv())
    lbl <- paste0("Run Assessment  \u00b7  ", n, " file", if (n != 1) "s" else "")
    actionButton("assess", lbl, class="btn-run")
  })

  # Rail label: vertical batch count shown on the collapsed file rail so
  # the reviewer always knows the batch size and can reopen to edit it.
  output$batch_rail_count_ui <- renderUI({
    n <- length(batch_files_rv())
    paste0("Files \u00b7 ", n, " in batch")
  })

  output$batch_sect_hd_ui <- renderUI({
    n <- length(batch_files_rv())
    div(class="sect-hd",
      style=if (n > 0) "color:var(--brand-navy);" else "color:#AAA;",
      paste0("BATCH", if (n > 0) paste0(" (", n, " file", if(n!=1)"s" else "", ")") else " - empty")
    )
  })

  # ── Batch panel - cross-folder file list ──────────────────────────────────
  output$batch_panel_ui <- renderUI({
    batch <- batch_files_rv()
    if (length(batch) == 0)
      return(div(class="batch-empty",
        "No files added. Browse to a folder, select files, then click + Add to batch."))

    tagList(lapply(batch, function(f) {
      folder <- tryCatch(basename(dirname(f$path)), error=function(e) "")
      div(class="batch-file-row",
        tags$span(class=paste0("f-badge t-", f$ftype), f$icon,
          style="flex-shrink:0;"),
        div(class="batch-file-info",
          div(class="batch-file-name", f$name),
          div(class="batch-file-path",
            tags$span(style="color:var(--brand-blue); margin-right:0.2rem;",
              paste0(folder, "/")))
        ),
        tags$span(class="f-size", f$size_label),
        tags$button(class="batch-remove-btn",
          `data-path`=f$path,
          title=paste0("Remove ", f$name, " from batch"),
          "\u00d7")
      )
    }))
  })

  # ── Cross-file linkage risk ───────────────────────────────────────────────

  # ── Baseline status panel ─────────────────────────────────────────────────


  # (linkage_risk_ui renderer removed - logic moved to render_linkage_risk_fn
  # and called from output$batch_header_ui)

  # ── Review Complete ───────────────────────────────────────────
  # (review_complete_ui renderer removed - readiness strip and Complete
  # Review button are now rendered as zone 6 of output$batch_header_ui)

  # Stores the intended PDF path when full generation fails, so the fallback
  # basic-report observer can write to the same location.
  # ── Exclude observers ─────────────────────────────────────────────────────
  # Three actions:
  #   - input$exclude_one (per-file × button): instant remove, no modal
  #   - input$bulk_exclude_uncertain: opens confirm modal
  #   - input$bulk_exclude_insufficient: opens confirm modal
  #   - input$bulk_exclude_confirm: actually remove (carries reason from
  #     input$bulk_exclude_pending_reason set by the trigger observer)
  bulk_exclude_pending_reason <- reactiveVal(NULL)
  bulk_exclude_pending_paths  <- reactiveVal(character(0))

  observeEvent(input$exclude_one, {
    fp <- as.character(input$exclude_one)
    if (!nzchar(fp)) return()
    remove_files_from_batch(fp, reason = "manual")
  }, ignoreInit = TRUE)

  observeEvent(input$bulk_exclude_uncertain, {
    res <- res_data()
    targets <- Filter(function(r)
      identical(r$classification, "UNCERTAIN"), res)
    if (length(targets) == 0L) return()
    target_paths <- vapply(targets,
      function(r) r$filepath %||% r$file, character(1))
    bulk_exclude_pending_reason("bulk_uncertain")
    bulk_exclude_pending_paths(target_paths)

    showModal(modalDialog(
      title = span(style="font-weight:700; color:var(--brand-navy);",
        "\u00d7 Remove UNCERTAIN files from batch"),
      tags$p(style="font-size:var(--fs-emphasis); margin-bottom:0.4rem;",
        sprintf("Remove %d UNCERTAIN file%s from this batch?",
                length(targets),
                if (length(targets) != 1L) "s" else "")),
      tags$ul(style="font-size:var(--fs-emphasis); max-height:160px; overflow-y:auto; margin-bottom:0.8rem;",
        lapply(targets, function(r)
          tags$li(
            span(class="hbadge hb-unc",
                 style="font-size:var(--fs-caption); margin-right:0.4rem;", "UNCERTAIN"),
            r$file)
        )
      ),
      tags$p(style="font-size:var(--fs-emphasis); color:var(--text-muted); margin-bottom:0.5rem;",
        "Removed files will not be assessed further or appear in the report. ",
        "This cannot be undone within this batch - to restore, re-add the ",
        "files and re-run the assessment."),
      footer = tagList(
        actionButton("bulk_exclude_confirm", "\u00d7 Remove",
          class="btn btn-danger btn-sm",
          style="font-weight:700;"),
        modalButton("Cancel")
      ),
      easyClose=TRUE, size="m"
    ))
  }, ignoreInit = TRUE)

  observeEvent(input$bulk_exclude_insufficient, {
    res <- res_data()
    aira_data_now <- aira_review_data()

    # Identify INSUFFICIENT files by parsing AIRA review responses
    target_paths <- character(0)
    target_files <- character(0)
    for (r in res) {
      r_path <- r$filepath %||% r$file
      review <- aira_data_now[[r_path]]
      if (is.null(review)) next
      if (!identical(review$status %||% "", "ok")) next
      txt <- review$text %||% ""
      if (!nzchar(txt) ||
          !requireNamespace("jsonlite", quietly = TRUE)) next
      parsed <- tryCatch(
        jsonlite::fromJSON(txt, simplifyVector = FALSE),
        error = function(e) NULL)
      if (is.null(parsed) || is.null(parsed$risk_level)) next
      rl <- toupper(as.character(parsed$risk_level)[1L])
      if (identical(rl, "INSUFFICIENT")) {
        target_paths <- c(target_paths, r_path)
        target_files <- c(target_files, r$file)
      }
    }
    if (length(target_paths) == 0L) return()

    bulk_exclude_pending_reason("bulk_insufficient")
    bulk_exclude_pending_paths(target_paths)

    showModal(modalDialog(
      title = span(style="font-weight:700; color:var(--brand-navy);",
        "\u00d7 Remove INSUFFICIENT files from batch"),
      tags$p(style="font-size:var(--fs-emphasis); margin-bottom:0.4rem;",
        sprintf("Remove %d INSUFFICIENT file%s from this batch?",
                length(target_paths),
                if (length(target_paths) != 1L) "s" else "")),
      tags$p(style="font-size:var(--fs-emphasis); color:var(--text-muted); margin-bottom:0.4rem;",
        "These are files where the AI could not meaningfully assess ",
        "the content (file unreadable, metadata only, binary content)."),
      tags$ul(style="font-size:var(--fs-emphasis); max-height:160px; overflow-y:auto; margin-bottom:0.8rem;",
        lapply(target_files, function(fn)
          tags$li(
            span(style="font-size:var(--fs-caption); margin-right:0.4rem; padding:0.05rem 0.3rem; background:var(--border); color:var(--brand-navy); border-radius:2px; font-weight:700;",
                 "INSUFFICIENT"),
            fn)
        )
      ),
      tags$p(style="font-size:var(--fs-emphasis); color:var(--text-muted); margin-bottom:0.5rem;",
        "Removed files will not be assessed further or appear in the report. ",
        "This cannot be undone within this batch - to restore, re-add the ",
        "files and re-run the assessment."),
      footer = tagList(
        actionButton("bulk_exclude_confirm", "\u00d7 Remove",
          class="btn btn-danger btn-sm",
          style="font-weight:700;"),
        modalButton("Cancel")
      ),
      easyClose=TRUE, size="m"
    ))
  }, ignoreInit = TRUE)

  observeEvent(input$bulk_exclude_confirm, {
    paths  <- bulk_exclude_pending_paths()
    reason <- bulk_exclude_pending_reason()
    if (length(paths) == 0L) {
      removeModal()
      return()
    }
    remove_files_from_batch(paths, reason = reason %||% "bulk_unknown")
    bulk_exclude_pending_paths(character(0))
    bulk_exclude_pending_reason(NULL)
    removeModal()
  }, ignoreInit = TRUE)


  # ── Review complete button - opens the airlock folder modal ─────────────────
  # ── Accept All GREEN & AMBER ──────────────────────────────────────────────
  observeEvent(input$accept_all, {
    res  <- res_data()
    decs <- session_decisions()
    targets <- Filter(function(r)
      r$classification %in% c("GREEN","AMBER") && is.null(decs[[r$file]]), res)
    if (length(targets) == 0) return()

    showModal(modalDialog(
      title = span(style="font-weight:700; color:#2E7D32;",
        "\u2714 Accept all GREEN & AMBER files"),
      tags$p(style="font-size:var(--fs-emphasis); margin-bottom:0.4rem;",
        paste0("Records an "),
        tags$strong("Approved"), " decision for ",
        tags$strong(sprintf("%d file(s):", length(targets)))),
      tags$ul(style="font-size:var(--fs-emphasis); max-height:160px; overflow-y:auto; margin-bottom:0.8rem;",
        lapply(targets, function(r)
          tags$li(
            span(class=paste0("hbadge hb-", tolower(r$classification)),
                 style="font-size:var(--fs-caption); margin-right:0.4rem;", r$classification),
            r$file)
        )
      ),
      tags$p(style="font-size:var(--fs-emphasis); color:var(--text-muted); margin-bottom:0.3rem;",
        tags$strong("Rationale note"), " (required - applied to all files):"),
      textAreaInput("accept_all_note", label=NULL, rows=3,
        placeholder="e.g. NII-002 trigger reviewed - confirmed MNI template, not individual scan. No disclosure risk.",
        width="100%"),
      div(id="accept_all_note_err",
        style="color:#C62828; font-size:var(--fs-emphasis); margin-top:0.25rem; display:none;",
        "\u26a0 A rationale note is required before accepting."),
      tags$p(style="font-size:var(--fs-body); color:var(--text-hint); margin-top:0.5rem;",
        "RED files are excluded and must be reviewed individually. ",
        "Each decision is logged to the audit trail."),
      footer = tagList(
        actionButton("accept_all_confirm", "\u2714 Accept all",
          class="btn btn-success btn-sm",
          style="font-weight:700;",
          onclick=paste0(
            "var v=document.getElementById('accept_all_note').value.trim();",
            "if(!v){",
            "  document.getElementById('accept_all_note_err').style.display='block';",
            "  return false;",
            "}"
          )),
        modalButton("Cancel")
      ),
      easyClose=TRUE, size="m"
    ))
  }, ignoreInit=TRUE)

  observeEvent(input$accept_all_confirm, {
    note <- trimws(input$accept_all_note %||% "")
    if (!nzchar(note)) return()
    res  <- res_data()
    decs <- session_decisions()
    targets <- Filter(function(r)
      r$classification %in% c("GREEN","AMBER") && is.null(decs[[r$file]]), res)
    if (length(targets) == 0) { removeModal(); return() }

    batch_sc <- tryCatch({
      sc <- calculate_batch_score(res,
        check_linkage_risk(res, min_categories=3L, min_shared_files=2L))
      if (!is.null(sc)) sc$total else NA_integer_
    }, error=function(e) NA_integer_)

    n_ok <- 0L
    for (r in targets) {
      ok <- log_decision(r, r$classification, note, batch_risk_score=batch_sc)
      if (isTRUE(ok)) {
        decs[[r$file]] <- list(
          outcome    = r$classification,
          note       = note,
          overridden = FALSE
        )
        # Update the per-file decision badge - same slot that manual decisions fill
        local({
          fid_local <- gsub("[^a-zA-Z0-9]","_", r$file)
          lid       <- paste0("rev_logged_", fid_local)
          lbl_local <- paste0("Confirmed ", r$classification)
          output[[lid]] <- renderUI({
            tagList(
              span(class="rev-logged",
                paste0("\u2713 ", lbl_local, " saved to log"))
            )
          })
        })
        n_ok <- n_ok + 1L
      }
    }
    session_decisions(decs)
    log_trigger(log_trigger() + 1)
    removeModal()
    showNotification(
      tagList(
        tags$strong(sprintf("\u2714 %d file(s) accepted", n_ok)),
        tags$br(),
        tags$span(style="font-size:var(--fs-emphasis);",
          "Decisions logged to audit trail.")
      ),
      type="message", duration=4)
  }, ignoreInit=TRUE)

  # ── Reject All RED ────────────────────────────────────────────────────────
  observeEvent(input$reject_all_red, {
    res  <- res_data()
    decs <- session_decisions()
    targets <- Filter(function(r)
      r$classification == "RED" && is.null(decs[[r$file]]), res)
    if (length(targets) == 0) return()

    showModal(modalDialog(
      title = span(style="font-weight:700; color:#C62828;",
        "\u2718 Reject all RED files"),
      tags$p(style="font-size:var(--fs-emphasis); margin-bottom:0.4rem;",
        paste0("Records a "),
        tags$strong("Rejected"), " decision for ",
        tags$strong(sprintf("%d file(s)", length(targets))),
        " and removes them from the batch:"),
      tags$ul(style="font-size:var(--fs-emphasis); max-height:160px; overflow-y:auto; margin-bottom:0.8rem;",
        lapply(targets, function(r)
          tags$li(
            span(class="hbadge hb-red",
                 style="font-size:var(--fs-caption); margin-right:0.4rem;", "RED"),
            r$file)
        )
      ),
      tags$p(style="font-size:var(--fs-emphasis); color:var(--text-muted); margin-bottom:0.3rem;",
        tags$strong("Rationale note"), " (required - applied to all files):"),
      textAreaInput("reject_all_note", label=NULL, rows=3,
        placeholder="e.g. Files contain participant-level data; researcher to aggregate before resubmission.",
        width="100%"),
      div(id="reject_all_note_err",
        style="color:#C62828; font-size:var(--fs-emphasis); margin-top:0.25rem; display:none;",
        "\u26a0 A rationale note is required before rejecting."),
      tags$p(style="font-size:var(--fs-body); color:var(--text-hint); margin-top:0.5rem;",
        "Each rejection is logged to the audit trail and the file is removed from the batch."),
      footer = tagList(
        actionButton("reject_all_red_confirm", "\u2718 Reject all RED",
          class="btn btn-danger btn-sm",
          style="font-weight:700;",
          onclick=paste0(
            "var v=document.getElementById('reject_all_note').value.trim();",
            "if(!v){",
            "  document.getElementById('reject_all_note_err').style.display='block';",
            "  return false;",
            "}"
          )),
        modalButton("Cancel")
      ),
      easyClose=TRUE, size="m"
    ))
  }, ignoreInit=TRUE)

  observeEvent(input$reject_all_red_confirm, {
    note <- trimws(input$reject_all_note %||% "")
    if (!nzchar(note)) return()
    res  <- res_data()
    decs <- session_decisions()
    targets <- Filter(function(r)
      r$classification == "RED" && is.null(decs[[r$file]]), res)
    if (length(targets) == 0) { removeModal(); return() }

    batch_sc <- tryCatch({
      sc <- calculate_batch_score(res,
        check_linkage_risk(res, min_categories=3L, min_shared_files=2L))
      if (!is.null(sc)) sc$total else NA_integer_
    }, error=function(e) NA_integer_)

    n_ok <- 0L
    rejected_paths <- character(0)
    for (r in targets) {
      ok <- log_decision(r, "RED", note, batch_risk_score=batch_sc)
      if (isTRUE(ok)) {
        decs[[r$file]] <- list(
          outcome    = "RED",
          note       = note,
          overridden = FALSE
        )
        # Track path for batch removal
        if (!is.null(r$filepath) && nzchar(r$filepath))
          rejected_paths <- c(rejected_paths, r$filepath)
        # Update the per-file decision badge
        local({
          fid_local <- gsub("[^a-zA-Z0-9]","_", r$file)
          lid       <- paste0("rev_logged_", fid_local)
          output[[lid]] <- renderUI({
            tagList(
              span(class="rev-logged",
                "\u2713 Rejection recorded \u2014 removed from batch")
            )
          })
        })
        n_ok <- n_ok + 1L
      }
    }
    session_decisions(decs)
    # Remove rejected files from the batch
    if (length(rejected_paths) > 0) {
      batch_files_rv(Filter(
        function(f) !(f$path %in% rejected_paths),
        batch_files_rv()
      ))
    }
    log_trigger(log_trigger() + 1)
    removeModal()
    showNotification(
      tagList(
        tags$strong(sprintf("\u2718 %d RED file(s) rejected", n_ok)),
        tags$br(),
        tags$span(style="font-size:var(--fs-emphasis);",
          "Decisions logged to audit trail and files removed from batch.")
      ),
      type="warning", duration=5)
  }, ignoreInit=TRUE)

  observeEvent(input$review_complete, {
    res  <- res_data()
    decs <- session_decisions()
    if (length(res) == 0 || length(decs) == 0) return()
    # Set reactive defaults that the modal body UI will read
    ts_str <- format(Sys.time(), "%Y%m%d_%H%M%S")
    airlock_folder_name_rv(paste0("airlock_", ts_str))
    default_loc <- tryCatch(dirname(res[[1]]$filepath), error=function(e) file_dir_rv())
    airlock_location_rv(default_loc)
    session$sendCustomMessage("showModal", list(id="airlockFolderModal"))
  })

  # ── Airlock modal body - renders dynamically so timestamp is fresh ───────────
  output$airlock_modal_body_ui <- renderUI({
    res <- res_data()
    if (length(res) == 0) return(NULL)
    default_name <- airlock_folder_name_rv() %||% paste0("airlock_", format(Sys.time(), "%Y%m%d_%H%M%S"))
    n_files      <- length(res)

    hd <- function(txt) div(style=paste0(
      "font-size:var(--fs-caption); font-weight:700; color:var(--brand-navy); letter-spacing:0.06em; ",
      "text-transform:uppercase; margin-bottom:0.3rem; padding-bottom:0.2rem; ",
      "border-bottom:2px solid #0066A1;"), txt)

    tagList(

      # Files in batch
      div(style="margin-bottom:0.75rem;",
        hd(paste0("Files in batch  \u00b7  ", n_files)),
        div(class="airlock-file-list",
          lapply(res, function(r) {
            cl  <- r$classification
            col <- switch(cl, RED=RED_C, AMBER=AMBER_C, GREEN=GREEN_C, "#888")
            div(style="display:flex; align-items:center; gap:0.5rem; padding:0.15rem 0;",
              tags$span(style=paste0(
                "font-weight:800; color:white; background:", col,
                "; font-size:var(--fs-caption); padding:0.05rem 0.35rem; ",
                "border-radius:3px; flex-shrink:0; min-width:46px; text-align:center;"), cl),
              tags$span(style="font-size:var(--fs-emphasis); overflow:hidden; text-overflow:ellipsis; white-space:nowrap;",
                r$file))
          })
        )
      ),

      # Move / Copy toggle
      div(style="margin-bottom:0.75rem;",
        hd("File action"),
        div(style="display:flex; gap:0.5rem;",
          tags$label(id="lbl_move",
            style=paste0(
              "flex:1; display:flex; align-items:center; gap:0.5rem; cursor:pointer; ",
              "padding:0.45rem 0.65rem; border-radius:5px; border:1px solid #0066A1; ",
              "background:#EEF4FB; font-size:var(--fs-emphasis); font-weight:600; color:var(--brand-navy);"),
            tags$input(type="radio", name="airlock_file_action", id="action_move",
              value="move", checked=NA, style="margin:0; cursor:pointer;"),
            div(
              div("Move files", style="font-weight:700;"),
              div(style="font-size:var(--fs-body); color:var(--text-muted); font-weight:400;",
                "Originals removed from source folder")
            )
          ),
          tags$label(id="lbl_copy",
            style=paste0(
              "flex:1; display:flex; align-items:center; gap:0.5rem; cursor:pointer; ",
              "padding:0.45rem 0.65rem; border-radius:5px; border:1px solid #DDD; ",
              "background:#F9F9F9; font-size:var(--fs-emphasis); font-weight:600; color:var(--text-muted);"),
            tags$input(type="radio", name="airlock_file_action", id="action_copy",
              value="copy", style="margin:0; cursor:pointer;"),
            div(
              div("Copy files", style="font-weight:700;"),
              div(style="font-size:var(--fs-body); color:var(--text-hint); font-weight:400;",
                "Originals remain in place")
            )
          )
        ),
        tags$script(HTML(
          "setTimeout(function() {
            document.querySelectorAll(\'input[name=\"airlock_file_action\"]\'
            ).forEach(function(r) {
              r.addEventListener(\'change\', function() {
                Shiny.setInputValue(\'airlock_file_action\', this.value, {priority:\'event\'});
                var isMove = this.value === \'move\';
                var lm = document.getElementById(\'lbl_move\');
                var lc = document.getElementById(\'lbl_copy\');
                if (lm) { lm.style.borderColor=isMove?\'#0066A1\':\'#DDD\';
                           lm.style.background=isMove?\'#EEF4FB\':\'#F9F9F9\';
                           lm.style.color=isMove?\'#003366\':\'#555\'; }
                if (lc) { lc.style.borderColor=isMove?\'#DDD\':\'#0066A1\';
                           lc.style.background=isMove?\'#F9F9F9\':\'#EEF4FB\';
                           lc.style.color=isMove?\'#555\':\'#003366\'; }
              });
            });
            Shiny.setInputValue(\'airlock_file_action\', \'move\', {priority:\'event\'});
          }, 50);"))
      ),

      # Folder name
      div(style="margin-bottom:0.65rem;",
        hd("Folder name"),
        textInput("airlock_folder_name", label=NULL, value=default_name,
          width="100%", placeholder="e.g. airlock_20260401_143022"),
        tags$small(style="color:var(--text-hint); font-size:var(--fs-caption); margin-top:0.1rem; display:block;",
          paste0("Folder will be created in /home/workspace/files/"))
      ),

      # Full path preview
      div(
        hd("Full path"),
        div(class="airlock-path-preview",
          paste0("/home/workspace/files/",
                 if (nzchar(trimws(default_name))) trimws(default_name) else "\u2014"))
      )
    )
  })

  # ── Folder browser confirm (source folder navigation only) ──────────────────
  observeEvent(input$fb_confirm, {
    p <- safe_dir(modal_dir_rv())
    if (is.null(p)) {
      showNotification("\u26a0 Invalid folder selection.", type="warning", duration=4)
      return()
    }
    session$sendCustomMessage("hideModal", list(id="folderBrowserModal"))
    navigate_to(p)
  }, ignoreNULL=TRUE, ignoreInit=TRUE)


  # ── Run report generation (shared logic) ─────────────────────────────────────
  run_report_generation <- function(create_folder) {
    res  <- res_data()
    decs <- session_decisions()
    if (length(res) == 0 || length(decs) == 0) return()

    session$sendCustomMessage("hideModal", list(id="airlockFolderModal"))

    ts_str   <- format(Sys.time(), "%Y%m%d_%H%M%S")
    pdf_name <- paste0("airlock_review_", ts_str, ".pdf")

    if (create_folder) {
      fname    <- trimws(input$airlock_folder_name %||% paste0("airlock_", ts_str))
      fname    <- gsub("[^a-zA-Z0-9_\\-\\.]", "_", fname)  # sanitise
      new_dir  <- file.path("/home/workspace/files", fname)
      pdf_path <- file.path(new_dir, pdf_name)
    } else {
      new_dir  <- NULL
      pdf_path <- file.path(file_dir_rv(), pdf_name)
    }

    withProgress(message="Generating report\u2026", value=0.1, {

      # Check for filename collisions across folders (files with the same name)
      file_names <- sapply(res, function(r) r$file)
      dup_names  <- unique(file_names[duplicated(file_names)])
      if (length(dup_names) > 0)
        showNotification(
          paste0("\u26a0 Filename collision: ",
                 paste(head(dup_names, 3), collapse=", "),
                 " \u2014 files with duplicate names from different folders will be overwritten."),
          type="warning", duration=10)

      # Create folder and move files
      if (!is.null(new_dir)) {
        incProgress(0.1, detail="Creating folder\u2026")
        dir_ok <- tryCatch({
          dir.create(new_dir, recursive=TRUE, showWarnings=FALSE)
          TRUE
        }, error=function(e) FALSE)
        if (!dir_ok || !dir.exists(new_dir)) {
          showNotification(
            paste0("\u26a0 Could not create folder: ", new_dir,
                   " \u2014 check the path is valid and you have write permission."),
            type="error", duration=12)
          return()
        }
        incProgress(0.15, detail="Moving files\u2026")
        do_move     <- !isTRUE(input$airlock_file_action == "copy")
        move_errors <- character(0)
        for (r in res) {
          if (!is.null(r$filepath) && file.exists(r$filepath)) {
            dest <- file.path(new_dir, r$file)
            if (do_move) {
              # Try rename first (fast, same-filesystem); fall back to copy+delete
              moved <- tryCatch(file.rename(r$filepath, dest), error=function(e) FALSE)
              if (!isTRUE(moved)) {
                copied <- tryCatch(file.copy(r$filepath, dest, overwrite=TRUE), error=function(e) FALSE)
                if (isTRUE(copied)) tryCatch(file.remove(r$filepath), error=function(e) NULL)
                else move_errors <- c(move_errors, r$file)
              }
            } else {
              ok <- tryCatch(file.copy(r$filepath, dest, overwrite=TRUE), error=function(e) FALSE)
              if (!isTRUE(ok)) move_errors <- c(move_errors, r$file)
            }
          }
        }
        if (length(move_errors) > 0)
          showNotification(
            paste0("\u26a0 Could not ", if (do_move) "move" else "copy", ": ",
                   paste(head(move_errors, 3), collapse=", ")),
            type="warning", duration=8)
      }

      # Generate the report PDF
      incProgress(0.3, detail="Generating report\u2026")
      sc <- tryCatch(
        calculate_batch_score(res,
          check_linkage_risk(res, min_categories=3L, min_shared_files=2L)),
        error=function(e) NULL)
      incProgress(0.2, detail="Writing PDF\u2026")
      tryCatch(
        generate_review_pdf_basic(res, decs, pdf_path,
          file_dir     = if (!is.null(new_dir)) new_dir else file_dir_rv(),
          cfg          = active_cfg(),
          default_cfg  = DEFAULT_CFG,
          cfg_changes  = cfg_changes_rv(),
          batch_score  = sc,
          aira_reviews = aira_review_data(),
          aira_batch   = aira_batch_data()),
        error=function(e) {
          showNotification(paste0("\u26a0 Report generation failed: ", e$message),
            type="error", duration=15)
          return()
        })
      incProgress(0.15)
    })

    if (!file.exists(pdf_path)) {
      showNotification(
        "\u26a0 Report file was not created. Check output directory permissions.",
        type="error", duration=10)
      return()
    }

    # Success notification
    if (!is.null(new_dir)) {
      showNotification(
        tagList(
          tags$strong("\u2705 Airlock package created"),
          tags$br(),
          tags$span(style="font-size:var(--fs-emphasis);",
            paste0(length(res), " file(s) ",
                   if (do_move) "moved" else "copied", " + report \u2192")),
          tags$br(),
          tags$span(style="font-size:var(--fs-body); font-family:monospace;", new_dir)
        ),
        type="message", duration=18)
    } else {
      showNotification(
        tagList(
          tags$strong("\u2705 Report saved"),
          tags$br(),
          tags$span(style="font-size:var(--fs-emphasis);", pdf_path)
        ),
        type="message", duration=15)
    }

    res_data(list())
    session_decisions(list())
    batch_files_rv(list())
    aira_batch_data(NULL)
    aira_batch_in_flight(FALSE)
    aira_batch_requested(FALSE)
    aira_review_data(list())
    aira_review_in_flight(list())
    aira_review_batch_id(aira_review_batch_id() + 1L)
    excluded_paths(character(0))
    aira_review_batch_total(0L)
    rm(list=ls(envir=registered_rev), envir=registered_rev)
    ev_open(list())  # close panels; registered_ev accumulates, never cleared
    log_trigger(log_trigger() + 1)
  }

  observeEvent(input$airlock_confirm,    run_report_generation(TRUE),  ignoreInit=TRUE)
  observeEvent(input$airlock_report_only, run_report_generation(FALSE), ignoreInit=TRUE)


  output$results_tab_badge <- renderUI({
    res <- res_data()
    if (length(res) == 0) return(NULL)
    n_red   <- sum(sapply(res, function(r) r$classification == "RED"))
    n_amber <- sum(sapply(res, function(r) r$classification == "AMBER"))
    col <- if (n_red > 0) RED_C else if (n_amber > 0) AMBER_C else GREEN_C
    bg  <- if (n_red > 0) RED_BG else if (n_amber > 0) AMBER_BG else GREEN_BG
    sc  <- tryCatch(
      calculate_batch_score(res,
        check_linkage_risk(res, min_categories=3L, min_shared_files=2L)),
      error=function(e) NULL)
    div(style="display:flex; gap:0.3rem; align-items:center; padding:0.1rem 0;",
      # File count pill
      span(class="nav-badge",
        style=paste0("background:", bg, "; color:", col,
                     "; border:1px solid ", col, ";"),
        paste0(length(res), " file", if(length(res)!=1)"s" else "")),
      # Risk score pill - clickable, triggers breakdown modal
      if (!is.null(sc))
        tags$span(
          class="nav-badge",
          style=paste0(
            "background:", sc$tl_colour,
            "; color:white; cursor:pointer; border-bottom:2px solid rgba(255,255,255,0.4); transition:opacity 0.15s;"),
          title="Click to see score breakdown",
          onclick="Shiny.setInputValue('show_score_breakdown', Math.random(), {priority:'event'});",
          paste0("Risk ", sc$total, " \u2139")
        )
    )
  })

  # ── Score breakdown modal ─────────────────────────────────────────────────
  observeEvent(input$show_score_breakdown, {
    res <- res_data()
    if (length(res) == 0) return()
    sc <- tryCatch(
      calculate_batch_score(res,
        check_linkage_risk(res, min_categories=3L, min_shared_files=2L)),
      error=function(e) NULL)
    if (is.null(sc)) return()

    # ── Build top-hits rows ──────────────────────────────────────────────
    hit_rows <- if (length(sc$top_hits) > 0)
      lapply(sc$top_hits, function(h) {
        oc <- h$outcome %||% ""
        col <- switch(oc, RED=RED_C, AMBER=AMBER_C, GREEN=GREEN_C, "#7B1FA2")
        tags$tr(
          tags$td(style=paste0("color:", col, "; font-weight:700; font-size:var(--fs-emphasis); padding:4px 8px; white-space:nowrap;"),
            h$rule),
          tags$td(style="padding:4px 8px; font-size:var(--fs-emphasis); color:var(--text-muted);",
            basename(h$file %||% "")),
          tags$td(style="padding:4px 8px; font-size:var(--fs-emphasis); text-align:right; font-weight:700;",
            paste0("+", h$weight))
        )
      })
    else list(tags$tr(tags$td(colspan="3",
      style="font-size:var(--fs-emphasis); color:var(--text-hint); padding:4px 8px;",
      "No weighted rule hits.")))

    # ── Structure reasons ────────────────────────────────────────────────
    struct_rows <- if (length(sc$struct_reasons) > 0)
      lapply(sc$struct_reasons, function(s)
        tags$li(style="font-size:var(--fs-emphasis); color:var(--text-muted); margin:2px 0;", s))
    else list(tags$li(style="font-size:var(--fs-emphasis); color:var(--text-hint);", "No structural adjustments."))

    # ── Score bar helper ─────────────────────────────────────────────────
    score_bar <- function(val, max_val, col) {
      pct <- min(100, round(val / max_val * 100))
      div(style="background:var(--border); border-radius:4px; height:10px; margin-top:4px; overflow:hidden;",
        div(style=paste0("background:", col, "; width:", pct, "%; height:10px; border-radius:4px;")))
    }

    showModal(modalDialog(
      title=div(style="display:flex; align-items:center; gap:0.6rem;",
        div(style=paste0("background:", sc$tl_colour,
          "; color:white; border-radius:6px; padding:0.25rem 0.7rem; font-size:var(--fs-display); font-weight:900;"),
          sc$total),
        div(
          div(style=paste0("font-size:var(--fs-display); font-weight:700; color:", sc$tl_colour, ";"),
            sc$tl_label),
          div(style="font-size:var(--fs-body); color:var(--text-hint);",
            paste0("Estimated review time: ", sc$rev_time))
        )
      ),
      size="m",
      easyClose=TRUE,
      footer=modalButton("Close"),
      tagList(

        # ── Score composition ──────────────────────────────────────────
        div(style="display:grid; grid-template-columns:1fr 1fr; gap:0.8rem; margin-bottom:1rem;",

          div(style="background:#F5F7FA; border-radius:6px; padding:0.7rem 0.9rem; border:1px solid var(--border);",
            div(style="font-size:var(--fs-body); font-weight:700; color:var(--text-muted); text-transform:uppercase; letter-spacing:0.05em;",
              "Rule Severity"),
            div(style="font-size:var(--fs-display-lg); font-weight:900; color:var(--brand-navy); line-height:1.1;",
              sc$severity_score,
              tags$span(style="font-size:var(--fs-body); font-weight:400; color:var(--text-hint);",
                paste0(" / 80  (raw: ", sc$severity_raw, ")"))),
            score_bar(sc$severity_score, 80, "#0066A1")
          ),

          div(style="background:#F5F7FA; border-radius:6px; padding:0.7rem 0.9rem; border:1px solid var(--border);",
            div(style="font-size:var(--fs-body); font-weight:700; color:var(--text-muted); text-transform:uppercase; letter-spacing:0.05em;",
              "Structural Adjustment"),
            div(style=paste0("font-size:var(--fs-display-lg); font-weight:900; line-height:1.1; color:",
              if(sc$struct_score>0) RED_C else if(sc$struct_score<0) GREEN_C else "#666", ";"),
              if(sc$struct_score>0) paste0("+",sc$struct_score) else as.character(sc$struct_score),
              tags$span(style="font-size:var(--fs-body); font-weight:400; color:var(--text-hint);", " points")),
            score_bar(abs(sc$struct_score), 20, if(sc$struct_score>0) AMBER_C else GREEN_C)
          )
        ),

        # ── File breakdown ─────────────────────────────────────────────
        div(style="display:grid; grid-template-columns:repeat(4,1fr); gap:0.5rem; margin-bottom:1rem;",
          lapply(list(
            list(sc$n_files,  "Files",       "#003366", "#EEF2F7"),
            list(sc$n_red,   "RED",          RED_C,     RED_BG),
            list(sc$n_amber, "AMBER",        AMBER_C,   AMBER_BG),
            list(sc$n_green, "GREEN",        GREEN_C,   GREEN_BG)
          ), function(item)
            div(style=paste0("background:", item[[4]],
              "; border-radius:6px; padding:0.5rem; text-align:center; border:1px solid ",
              item[[3]], "33;"),
              div(style=paste0("font-size:var(--fs-display-lg); font-weight:900; color:", item[[3]], ";"), item[[1]]),
              div(style=paste0("font-size:var(--fs-caption); font-weight:700; color:", item[[3]], "; text-transform:uppercase;"), item[[2]])
            )
          )
        ),

        # ── Top contributing rules ─────────────────────────────────────
        div(style="margin-bottom:0.8rem;",
          div(style="font-size:var(--fs-body); font-weight:700; color:var(--text-muted); text-transform:uppercase; letter-spacing:0.05em; margin-bottom:0.35rem;",
            "Top Contributing Rules (by weight)"),
          tags$table(
            style="width:100%; border-collapse:collapse; background:#F5F7FA; border-radius:6px;",
            tags$thead(tags$tr(
              tags$th(style="font-size:var(--fs-body); color:var(--text-hint); text-align:left; padding:4px 8px; border-bottom:1px solid var(--border);", "Rule"),
              tags$th(style="font-size:var(--fs-body); color:var(--text-hint); text-align:left; padding:4px 8px; border-bottom:1px solid var(--border);", "File"),
              tags$th(style="font-size:var(--fs-body); color:var(--text-hint); text-align:right; padding:4px 8px; border-bottom:1px solid var(--border);", "Weight")
            )),
            tags$tbody(hit_rows)
          )
        ),

        # ── Structural adjustments ────────────────────────────────────
        div(
          div(style="font-size:var(--fs-body); font-weight:700; color:var(--text-muted); text-transform:uppercase; letter-spacing:0.05em; margin-bottom:0.35rem;",
            "Structural Adjustments"),
          tags$ul(style="margin:0; padding-left:1.2rem;", struct_rows)
        ),

        # ── Coverage ──────────────────────────────────────────────────
        div(style="margin-top:0.8rem; padding:0.5rem 0.9rem; background:#F5F7FA; border-radius:6px; border:1px solid var(--border); display:flex; justify-content:space-between; align-items:center;",
          div(style="font-size:var(--fs-body); font-weight:700; color:var(--text-muted); text-transform:uppercase;", "Coverage"),
          div(
            span(style="font-size:var(--fs-display); font-weight:900; color:var(--brand-navy);",
              paste0(sc$coverage_pct, "%")),
            span(style="font-size:var(--fs-body); color:var(--text-hint); margin-left:0.3rem;",
              if (sc$n_limited > 0)
                paste0("(", sc$n_limited, " file(s) with limited inspection coverage)")
              else
                "(all files fully inspected)")
          )
        )
      )
    ))
  }, ignoreNULL=TRUE, ignoreInit=TRUE)

  # ── Rule reference badge - shows fired rule count ────────────────────────
  output$rule_ref_badge <- renderUI({
    res <- res_data()
    if (length(res) == 0) return(NULL)
    fired <- unique(unlist(lapply(res, function(r) sapply(r$hits, `[[`, "rule"))))
    if (length(fired) == 0) return(NULL)
    span(style=paste0(
      "font-size:var(--fs-caption); font-weight:700; padding:0.1rem 0.45rem; ",
      "border-radius:10px; margin-left:0.4rem; ",
      "background:#E3F2FD; color:#1565C0; border:1px solid #90CAF9;"),
      paste0(length(fired), " fired"))
  })

  # ── Help tab ───────────────────────────────────────────────────────────────
  output$help_ui <- renderUI({
    cfg <- active_cfg()
    n_rules  <- length(RULES)
    n_red    <- sum(sapply(RULES, function(r) r$outcome=="RED"))
    n_amber  <- sum(sapply(RULES, function(r) r$outcome=="AMBER"))
    n_green  <- sum(sapply(RULES, function(r) r$outcome=="GREEN"))
    count_thr <- cfg$count_threshold %||% 5L
    n_phen   <- length(cfg$sensitive_phenotypes %||% sensitive_phenotypes)

    # Helper to build a collapsible section
    hs <- function(icon, title, ...) {
      tags$details(
        tags$summary(
          tags$span(icon, style="font-size:var(--fs-display);"),
          title
        ),
        div(class="help-body", ...)
      )
    }

    tl_badge <- function(label, col, bg)
      tags$span(class="help-tl",
        style=paste0("background:",col,"; color:white;"), label)

    rule_eg <- function(...) div(class="help-rule-eg", ...)

    div(class="help-section",

      # ── Quick start ────────────────────────────────────────────────────────
      hs("\U0001F680",
        if (APP_MODE == "reviewer") "Quick Start - Governance Review" else "Quick Start - Three Steps",
        tags$h4("Step 1 - Open a folder"),
        p("Use the file browser on the left to navigate to the folder containing",
          "the files you want to assess. Click", tags$strong("Set"), "to confirm the folder.",
          "Files are listed automatically."),
        tags$h4("Step 2 - Run assessment"),
        p("Select the files to check (use the checkbox at the top to select all),",
          "then click", tags$strong("Run Assessment."),
          "The tool inspects each file against the full rule set and assigns",
          "a RED, AMBER, or GREEN classification."),
        tags$h4(if (APP_MODE == "reviewer") "Step 3 - Record governance decisions" else "Step 3 - Review and log decisions"),
        if (APP_MODE == "reviewer")
          p("Work through the results in the", tags$strong("Egress Assessment"), "tab.",
            "For each file, use the decision dropdown to record your governance determination.",
            "Decisions that differ from the DTE classification require a mandatory justification note,",
            "which will appear in the audit log and governance report.",
            "Once all files are decided, click",
            tags$strong("Complete Review \u2014 Generate Governance Report"), ".")
        else
          p("Work through the results in the", tags$strong("Submission Check"), "tab.",
            "For each file, the dropdown defaults to the most appropriate response for your classification.",
            "If you disagree with the automated classification, select an override",
            "and provide a mandatory justification note, then click",
            tags$strong("Save response."),
            "Once all files have been decided, click",
            tags$strong("Submit for Airlock Review"), "to generate the report.")
      ),

      # ── Classifications ────────────────────────────────────────────────────
      hs("\U0001F7E2", "Understanding Classifications",
        p("Every file receives one of four classifications after assessment:"),
        tags$dl(class="help-kv",
          tags$dt(tl_badge("RED","#C62828","#FFEBEE")),
          tags$dd(paste0("Egress rejected. One or more rules identified a serious disclosure",
            " risk - participant identifiers, PII, unmasked small counts, or equivalent.",
            " The file must not leave the workspace without remediation.")),
          tags$dt(tl_badge("AMBER","#E65100","#FFF3CD")),
          tags$dd(paste0("Manual review required. The tool detected a potential risk",
            " that cannot be confirmed automatically - e.g. a PDF that could not be",
            " text-extracted, or a script containing a sensitive phenotype term in a",
            " non-data context. The reviewer must inspect and decide.")),
          tags$dt(tl_badge("GREEN","#2E7D32","#E8F5E9")),
          tags$dd(paste0("Passed all disclosure checks. No rule fired at RED or AMBER.",
            " The file may proceed to egress, subject to reviewer sign-off.")),
          tags$dt(tl_badge("UNCERTAIN","#0066A1","#E3F2FD")),
          tags$dd(paste0("The file type could not be inspected (e.g. unknown binary format,",
            " encrypted archive). Manual specialist review is required."))
        ),
        tags$h4("Overall batch classification"),
        p("The batch takes the worst classification across all files.",
          "A single RED file makes the whole submission RED,",
          "regardless of how many GREEN files it contains.")
      ),

      # ── Risk score ─────────────────────────────────────────────────────────
      hs("\U0001F4CA", "The Risk Score",
        p("The risk score (0-100) is the DTE's overall measure of disclosure risk",
          "for the whole submission batch. It is", tags$em("not"), "a binary pass/fail -",
          "it is designed to guide reviewer effort allocation."),
        tags$h4("Score bands"),
        tags$dl(class="help-kv",
          tags$dt("0 - 15"), tags$dd("Low Risk - brief review, no RED/AMBER findings or clean batch"),
          tags$dt("16 - 40"), tags$dd("Moderate Risk - review all AMBER evidence panels"),
          tags$dt("41 - 70"), tags$dd(paste0("Elevated Risk - review all RED and AMBER panels;",
            " check linkage risk findings if present")),
          tags$dt("71 - 100"), tags$dd(paste0("High Risk - consider returning to researcher;",
            " extensive reviewer time required"))
        ),
        tags$h4("Three sub-scores"),
        tags$dl(class="help-kv",
          tags$dt("Rule Severity"),
          tags$dd(paste0("Sum of the weights of all rule hits across the batch.",
            " Weights range from 10 (direct identifier exposure) down to 2",
            " (tooling limitation). Capped at 80 to leave room for batch penalties.")),
          tags$dt("Batch Structure"),
          tags$dd(paste0("Penalties applied for submission-level risks that no single",
            " file rule can detect: +15 for shared column names (join-key risk),",
            " +10 for quasi-identifier combination (mosaic effect), +5 for batches",
            " over 5 files. −10 if the entire batch is GREEN.")),
          tags$dt("Coverage"),
          tags$dd(paste0("Percentage of files that were fully machine-inspected.",
            " Files flagged by PDF-006, DOC-006, BIN-001, or ARC-002 could not be",
            " fully assessed and require manual review."))
        ),
        p(tags$strong("Important:"), " the score is computed by the DTE automatically.",
          "Reviewer overrides", tags$em("do not"), "change the score.",
          "The score in the report always reflects what the tool found, not what the",
          "reviewer decided. Override decisions are recorded separately in the audit log.")
      ),

      # ── Workflow ───────────────────────────────────────────────────────────
      hs("\U0001F9D1\u200D\U0001F4BB",
        if (APP_MODE == "reviewer") "Governance Review Workflow" else "Reviewer Workflow",
        if (APP_MODE == "reviewer") tagList(
          tags$h4("Recording a decision"),
          p("Each file has a decision strip beneath its assessment results.",
            "The dropdown defaults to", tags$em("Approve egress"), "(GREEN) or",
            tags$em("Confirm rejection"), "(RED) - the governance-aligned default for that classification.",
            "Click", tags$strong("Record decision"), "to log your determination."),
          p("Any decision that diverges from the DTE classification requires a mandatory",
            "justification note. This note is written to the audit log and appears",
            "verbatim in the governance report. It should be sufficient to demonstrate",
            "that the decision was made on defensible grounds."),
          tags$h4("Grounds for approving an exception"),
          p("An exception (approving egress despite a RED or AMBER finding) is valid when:"),
          tags$ul(
            tags$li("The flagged data does not relate to real individuals (e.g. synthetic data, animal study)."),
            tags$li("The rule fired on metadata, comments, or variable names rather than actual data values."),
            tags$li("Disclosure risk has been mitigated prior to submission (counts suppressed, identifiers pseudonymised)."),
            tags$li("The file has been manually inspected and no disclosure risk was identified.")
          ),
          p(tags$strong("Important:"), " the reviewer accepts responsibility for exception decisions.",
            "Ensure your justification is specific, documented, and consistent with your",
            "organisation's data governance policy."),
          tags$h4("Complete Review"),
          p(tags$strong("All files must have a recorded decision"), "before Complete Review becomes available.",
            "When clicked, it generates a signed PDF governance report saved to the source folder.",
            "The report includes the risk score, per-file rule cards, all decisions and justifications,",
            "configuration snapshot, and a verification hash.",
            "The report is the formal governance record for this egress request."),
          tags$h4("Audit log"),
          p("Every decision is written to the audit log",
            tags$code(".dte_audit_log.csv"), "in the output folder",
            "regardless of whether Complete Review is clicked.",
            "The log is append-only. It records the DTE classification,",
            "reviewer decision, override flag, timestamp, and batch risk score.")
        ) else tagList(
          tags$h4("Logging a decision"),
          p("Each file has a reviewer strip beneath its assessment results.",
            "The dropdown defaults to", tags$em("Confirm \u2014 include in this submission"),
            "\u2014 click", tags$strong("Save response"), "to record your agreement with no note required."),
          p("If you select an override option, the note field becomes mandatory.",
            "You must explain your reasoning before the decision can be logged.",
            "Overrides are highlighted in the generated report for administrator review."),
          tags$h4("Valid override justifications"),
          p("An override is valid when you can document one of the following:"),
          tags$ul(
            tags$li("The flagged data does not relate to real individuals (e.g. synthetic data, animal study)."),
            tags$li("The rule fired on a variable name or comment, not on actual data values."),
            tags$li(paste0("The implied risk has been mitigated prior to submission",
              " (e.g. counts suppressed, identifiers pseudonymised).")),
            tags$li("The file was manually reviewed and no disclosure risk was found.")
          ),
          tags$h4("Submit for Airlock Review"),
          p(tags$strong("All files must be decided"), "before Review Complete becomes available.",
            "When clicked, it generates a signed PDF report and saves it to the source folder.",
            "The report includes the batch risk score, per-file rule cards with evidence,",
            "all override justifications, and a configuration snapshot.",
            "The report hash in the PDF verification page can be used to confirm",
            "the document has not been altered after generation."),
          tags$h4("What happens to the log"),
          p("Every logged decision is written to the audit log",
            tags$code(".dte_audit_log.csv"), "in the output folder",
            "regardless of whether Review Complete is clicked.",
            "The log is append-only and records the DTE classification,",
            "reviewer decision, override flag, timestamp, and batch risk score.")
        )
      ),

      # ── Configuration ──────────────────────────────────────────────────────
      hs("\u2699\uFE0F", "Configuration and Thresholds",
        p("The Assessment Rule Settings panel (below the results area) lets the workspace",
          "administrator adjust the thresholds used by the automated rules.",
          "Changes take effect on the next assessment run.",
          "Configuration is saved automatically and restored when the app restarts."),
        tags$h4("Key thresholds (current values)"),
        tags$dl(class="help-kv",
          tags$dt("Small count threshold"),
          tags$dd(paste0("Currently ", count_thr, ". Values below this in count-type columns",
            " trigger TAB-005 (unmasked counts), TAB-009 (percentage back-calculation),",
            " and TAB-013 (secondary suppression).",
            " The NHS standard is typically 5; some DAAs require 10.")),
          tags$dt("Sensitive phenotype list"),
          tags$dd(paste0("Currently ", n_phen, " term(s). Used by SCR-003, TAB-004, and several",
            " other rules. Terms are matched as whole words in column names,",
            " string literals, and script content.")),
          tags$dt("Cardinality threshold"),
          tags$dd(paste0("If a column has a uniqueness ratio above ",
            cfg$tab003_cardinality%||%0.85,
            " and the file has at least ", cfg$tab003_min_rows%||%10L, " rows,",
            " TAB-003 treats it as individual-level data.")),
          tags$dt("Free text column patterns"),
          tags$dd("Column name patterns used by TAB-012 to detect narrative fields.",
            " Configurable in Assessment Rule Settings \u2014 extend the list to match",
            " site-specific column naming conventions."),
          tags$dt("Derived identifier patterns"),
          tags$dd("Column name patterns used by TAB-014 to detect derived temporal and",
            " spatial variables. Extend to cover project-specific derived field names."),
          tags$dt("K-anonymity estimation"),
          tags$dd(paste0("TAB-015/016. Enabled by default for files up to ",
            format(cfg$kanon_max_rows %||% 10000L, big.mark=","), " rows, using up to ",
            cfg$kanon_max_qi_cols %||% 6L, " quasi-identifier columns. ",
            "The k threshold uses the same value as the small count threshold above. ",
            "Disable for non-individual-level files (GWAS summaries are excluded automatically)."))
        )
      ),

      # ── Rule catalogue ─────────────────────────────────────────────────────
      hs("\U0001F4CB", "Rule Catalogue Overview",
        p(paste0("The DTE runs ", n_rules, " rules across ",
          length(unique(unlist(lapply(RULES, `[[`, "file_types")))),
          " file types. Rules are grouped by file category.")),
        tags$dl(class="help-kv",
          tags$dt(paste0("TAB (", sum(startsWith(names(RULES),"TAB")), ")")),
          tags$dd(paste0("Tabular files: CSV, TSV, XLSX. Checks for identifiers, PII, small counts, ",
            "individual-level data, rare categories, over-precise variables, free text columns, ",
            "secondary suppression risk, derived temporal/spatial identifiers, and k-anonymity.")),
          tags$dt(paste0("GEN (", sum(startsWith(names(RULES),"GEN")), ")")),
          tags$dd("Genomic files: VCF, BIM/FAM/BED, PGEN. Detects per-sample genotype columns and confirms GWAS summary format (GEN-003 is a positive-signal rule)."),
          tags$dt(paste0("SCR (", sum(startsWith(names(RULES),"SCR")), ")")),
          tags$dd("Scripts: R, Python, SQL, shell, notebooks. Checks for hardcoded identifiers, inline data, sensitive phenotype terms, and rendered outputs."),
          tags$dt(paste0("PDF (", sum(startsWith(names(RULES),"PDF")), ")")),
          tags$dd("PDF documents. Extracts text via pdftools R package; checks for identifiers and sensitive content. PDF-007 is a positive-signal rule (no identifiable content found)."),
          tags$dt(paste0("DOC (", sum(startsWith(names(RULES),"DOC")), ")")),
          tags$dd("Office documents: DOCX, ODT, ODP. Inspects embedded XML content."),
          tags$dt(paste0("HTM (", sum(startsWith(names(RULES),"HTM")), ")")),
          tags$dd("HTML / web pages. Checks for embedded JavaScript, participant identifiers, and large embedded tables."),
          tags$dt(paste0("IMG (", sum(startsWith(names(RULES),"IMG")), ")")),
          tags$dd("Images: PNG, JPEG, SVG. Detects SVG with JS, annotated plots, and identifiable faces."),
          tags$dt(paste0("JSON/XML/MD (", sum(startsWith(names(RULES),"JSON"))+sum(startsWith(names(RULES),"XML"))+sum(startsWith(names(RULES),"MD")), ")")),
          tags$dd("Structured text formats. Checks for credentials, identifiers, sensitive phenotypes, and large record arrays."),
          tags$dt(paste0("SER/DAT/COL/BIN/ARC (", sum(startsWith(names(RULES),"SER"))+sum(startsWith(names(RULES),"DAT"))+sum(startsWith(names(RULES),"COL"))+sum(startsWith(names(RULES),"BIN"))+sum(startsWith(names(RULES),"ARC")), ")")),
          tags$dd("Serialised R objects, SQLite databases, columnar formats, binaries, and archives."),
          tags$dt(paste0("DCM (", sum(startsWith(names(RULES),"DCM")), ")")),
          tags$dd(paste0("DICOM medical images (.dcm). Inspects metadata tags for direct patient identifiers, ",
            "burned-in annotation, head imaging modality, day-precision dates, and institution identifiers. ",
            "Requires the oro.dicom R package (installed via dependencies.R). ",
            "DCM-007 is a positive-signal rule confirming no identifier tags were found."))
        ),
        tags$h4("How to read a rule card"),
        rule_eg(
          tags$strong(style="color:#C62828;", "[RED]"),
          tags$code("TAB-001"), "  Participant / Sample Identifiers",
          tags$br(),
          tags$em(style="color:var(--text-muted);",
            "Tests: Detects column headers matching known participant identifier names\u2026"),
          tags$br(),
          tags$strong("Finding: "), "Column 'participant_id' matches identifier pattern",
          tags$br(),
          tags$strong("Evidence: "), "First 8 rows of the file with the flagged column",
          tags$br(),
          "\U0001F527 Remediation: Remove or hash the identifier column before resubmitting"
        ),
        p(tags$strong("Tests"), "\u2014 what the rule is looking for, in plain language."),
        p(tags$strong("Finding"), "\u2014 the specific value or pattern that triggered the rule in", tags$em("this"), "file."),
        p(tags$strong("Evidence"), "\u2014 the actual rows or lines from the file that caused the hit."),
        p(tags$strong("Remediation"), "\u2014 a concrete action the researcher can take to resolve the finding.")
      ),

      # ── New rules ──────────────────────────────────────────────────────────
      hs("\U0001F195", "New Rules - TAB-012, TAB-013, TAB-014",
        p("Three new tabular disclosure rules have been added to address risks commonly",
          "missed in standard disclosure checking."),
        tags$h4("TAB-012 \u2014 Free Text / Narrative Columns (AMBER)"),
        p("Flags columns whose names suggest unstructured narrative text:",
          tags$code("notes"), ",", tags$code("comments"), ",",
          tags$code("description"), ",", tags$code("clinical_history"), ",",
          tags$code("reason"), ", and similar patterns."),
        p("Free text is the most common source of residual PII in research datasets.",
          "A file with clean structured columns may still contain names, dates, and addresses",
          "in a narrative field. The rule fires AMBER because the content cannot be",
          "automatically inspected - it requires human review."),
        p(tags$strong("What to do:"), " remove the column if it is not essential to the output,",
          "or request manual airlock review of the free text content specifically.",
          "The column name patterns can be extended in Assessment Rule Settings."),
        tags$h4("TAB-013 \u2014 Secondary Suppression Risk (RED)"),
        p("Detects rows in summary tables where a count column contains a suppression marker",
          "(\u201c<5\u201d, \u201csuppressed\u201d, \u201c*\u201d) alongside a column containing",
          "a numeric total. In this situation, the suppressed count is recoverable by subtraction."),
        p("Example: a row showing", tags$code("cases=<5"), ",",
          tags$code("controls=47"), ",", tags$code("total=50"),
          "reveals that cases = 50 \u2212 47 = 3, defeating the suppression."),
        p(tags$strong("What to do:"), " apply secondary suppression by suppressing additional",
          "cells, rounding totals, or replacing exact marginals with banded values.",
          "After applying secondary suppression, verify no arithmetic path reveals",
          "any suppressed value."),
        tags$h4("TAB-014 \u2014 Derived Temporal / Spatial Identifiers (AMBER)"),
        p("Flags columns whose names suggest values computed from raw identifiers:",
          tags$code("time_since_diagnosis"), ",", tags$code("age_at_recruitment"), ",",
          tags$code("days_in_hospital"), ",", tags$code("distance_from_centre"), ",",
          tags$code("follow_up_time"), ", and similar patterns."),
        p("Derived variables are not de-identified. A patient who was",
          "\u201c847 days since diagnosis\u201d at a specific date is effectively dated.",
          "When combined with site, diagnosis, or age, derived temporal variables",
          "can uniquely identify individuals in small cohorts."),
        p(tags$strong("What to do:"), " round to coarser units (age to nearest year,",
          "time to nearest quarter, distance to nearest 10\u00a0km band)",
          "and cap extreme values that are themselves identifying.")
      ),

      # ── K-anonymity ────────────────────────────────────────────────────────
      hs("\U0001F512", "K-Anonymity (TAB-015 / TAB-016)",
        p("K-anonymity is a formal privacy measure. For a given set of quasi-identifier",
          "columns, k is the size of the smallest group of individuals who share",
          "identical values across all those columns simultaneously."),
        p("If k\u00a0=\u00a01, at least one individual is unique and can be distinguished",
          "from everyone else in the dataset by their combination of quasi-identifier values",
          "alone \u2014 regardless of whether an explicit identifier column is present.",
          "NHS Statistical Disclosure Control guidance recommends k\u00a0\u2265\u00a05 before egress."),
        tags$h4("How the tool computes k"),
        p("After running an assessment, the tool automatically identifies quasi-identifier",
          "columns using the QI vocabulary (age, sex, ethnicity, geography, diagnosis dates,",
          "occupation, derived temporal variables). It then counts how many rows share each",
          "unique combination of values across those columns. The minimum count is k."),
        p("The evidence table shows the riskiest combinations \u2014 the smallest groups,",
          "sorted ascending. Combinations with k\u00a0=\u00a01 are individuals who are uniquely",
          "identifiable from their quasi-identifier values alone."),
        tags$h4("Understanding the outcome"),
        tags$dl(class="help-kv",
          tags$dt(tags$span(class="hbadge hb-red", "RED")),
          tags$dd(paste0("k is below the configured suppression threshold.",
            " At least one group of individuals is smaller than the minimum permitted count.",
            " Suppression or aggregation is required before egress.")),
          tags$dt(tags$span(class="hbadge hb-amb", "AMBER")),
          tags$dd(paste0("k meets the minimum threshold but is marginal (below 10).",
            " Consider whether further aggregation would improve robustness,",
            " particularly if this dataset will be linked with other outputs.")),
          tags$dt(tags$span(class="hbadge hb-grn", "GREEN")),
          tags$dd(paste0("k \u2265 10 across all detected quasi-identifier combinations.",
            " The file is comfortably above the suppression threshold on the QI columns found."))
        ),
        tags$h4("TAB-016 \u2014 cannot estimate"),
        p("TAB-016 fires when k-anonymity cannot be computed automatically. The two causes are:"),
        tags$ul(
          tags$li(tags$strong("No QI columns recognised \u2014"), " the column names do not match",
            " the built-in QI vocabulary. This does not mean the file is safe. Manually",
            " identify QI columns and compute k in R: ",
            tags$code("df %>% group_by(age_col, sex_col, ...) %>% summarise(n=n()) %>% arrange(n)")),
          tags$li(tags$strong("File exceeds the row limit \u2014"), " computation is disabled for",
            " very large files by default. The limit is configurable in Assessment Rule Settings.",
            " For large files, assess a representative stratified sample.")
        ),
        tags$h4("What k-anonymity does not cover"),
        p("K-anonymity addresses identity disclosure \u2014 whether an individual can be singled out.",
          "It does not directly address:"),
        tags$ul(
          tags$li(tags$strong("Attribute disclosure \u2014"), " whether a sensitive attribute",
            " (diagnosis, outcome) can be inferred even for known group members."),
          tags$li(tags$strong("Linkage risk \u2014"), " whether this file can be joined to",
            " another file to reconstruct individual records (see Cross-File Linkage Risk)."),
          tags$li(tags$strong("Rare values in non-QI columns \u2014"), " TAB-010 handles",
            " rare categories in columns not detected as quasi-identifiers.")
        ),
        p("K-anonymity is a necessary but not sufficient condition for safe egress.",
          "It complements the existing rule set rather than replacing it.")
      ),

      # ── DICOM ──────────────────────────────────────────────────────────────
      hs("\U0001F3E5", "DICOM Medical Images (DCM-001 \u2013 DCM-007)",
        p("DICOM (.dcm) files are medical image containers. They hold two distinct ",
          "categories of disclosure risk: embedded metadata tags and image pixel content."),
        tags$h4("What the tool checks automatically"),
        p("The tool uses the", tags$code("oro.dicom"), "R package to read the DICOM tag ",
          "header without decoding the pixel data. Seven rules cover the main metadata risks:"),
        tags$dl(class="help-kv",
          tags$dt(tags$span(class="hbadge hb-red", "DCM-001")),
          tags$dd(paste0("Direct patient identifier tags: PatientName (0010,0010), PatientID (0010,0020), ",
            "PatientBirthDate (0010,0030), ReferringPhysicianName (0008,0090), AccessionNumber (0008,0050). ",
            "Any non-empty value in these tags is a disclosure risk. ",
            "All must be empty or replaced with pseudonymous values before egress.")),
          tags$dt(tags$span(class="hbadge hb-red", "DCM-002")),
          tags$dd(paste0("BurnedInAnnotation tag (0028,0301) = YES. Patient demographics have been ",
            "rendered directly into the image pixels. Tag stripping alone is not sufficient \u2014 ",
            "the pixels must be de-annotated or re-exported without annotations.")),
          tags$dt(tags$span(class="hbadge hb-amb", "DCM-003")),
          tags$dd(paste0("Head imaging modality (MR or CT) with body part, series description, or study ",
            "description indicating head, brain, skull, face, or cranial anatomy. ",
            "3D head volumes can be surface-rendered to reconstruct a recognisable face. ",
            "This is a biometric re-identification risk even after tag anonymisation.")),
          tags$dt(tags$span(class="hbadge hb-amb", "DCM-004")),
          tags$dd(paste0("StudyDate (0008,0020), ContentDate (0008,0023), or AcquisitionDate (0008,0022) ",
            "contain a full date at day-level precision. Combined with modality and institution, ",
            "a precise date significantly increases re-identification risk.")),
          tags$dt(tags$span(class="hbadge hb-amb", "DCM-005")),
          tags$dd(paste0("InstitutionName (0008,0080), InstitutionAddress (0008,0081), or StationName ",
            "(0008,1010) contain non-empty values. These identify the scanning site and may narrow ",
            "re-identification to patients treated at a specific centre.")),
          tags$dt(tags$span(class="hbadge hb-amb", "DCM-006")),
          tags$dd("The file could not be parsed - corrupted, proprietary transfer syntax, or DICOMDIR index. Manual review required."),
          tags$dt(tags$span(class="hbadge hb-grn", "DCM-007")),
          tags$dd("Positive signal: tags parsed and no direct identifier tags found, BurnedInAnnotation is absent or NO.")
        ),
        tags$h4("What the tool cannot check automatically"),
        p("The following require manual specialist review and are outside what automated ",
          "metadata inspection can determine:"),
        tags$ul(
          tags$li(tags$strong("Facial reconstruction from pixels \u2014"),
            " DCM-003 flags head modality files but cannot determine whether the volume ",
            "contains sufficient facial surface data for reconstruction. A de-facing tool ",
            "(e.g. pydeface) should be applied to head MRI/CT volumes before egress."),
          tags$li(tags$strong("Burned-in text content \u2014"),
            " if DCM-002 does not fire but you suspect burned-in text from visual inspection, ",
            "this requires manual review. The absence of the BurnedInAnnotation=YES tag does ",
            "not guarantee the pixels are clean."),
          tags$li(tags$strong("Pixel-level PHI in non-head scans \u2014"),
            " chest X-rays, ultrasounds, and other modalities can also contain burned-in patient ",
            "information. DCM-002 catches cases where the tag is set; it does not catch cases ",
            "where the tag is absent but annotations are present.")
        ),
        tags$h4("Dependencies"),
        p("DICOM inspection requires the", tags$code("oro.dicom"), "R package, which is ",
          "pure R with no system library dependencies. It is included in",
          tags$code("dependencies.R"), "and installs from CRAN alongside the other packages.",
          if (ORODICOM_OK)
            tags$span(style="color:#2E7D32; font-weight:700;",
              " \u2713 oro.dicom is currently installed.")
          else
            tags$span(style="color:#C62828; font-weight:700;",
              " \u26a0 oro.dicom is NOT installed \u2014 DICOM files cannot be inspected. Run dependencies.R."))
      ),

      # ── Linkage risk ───────────────────────────────────────────────────────
      hs("\U0001F517", "Cross-File Linkage Risk",
        p("When multiple tabular files are submitted together, the Airlock Checker checks for",
          "risks that no single-file rule can detect."),
        tags$h4("Shared column names (join-key risk)"),
        p("If the same column name appears in two or more files, a third party",
          "could join those files together to reconstruct individual-level records,",
          "even if each file individually appears safe.",
          "This fires as RED in the linkage section."),
        rule_eg(
          "Example: ", tags$code("age"), " appears in both demographics.csv and outcomes.csv.",
          " Combined, they may uniquely identify individuals even if neither alone would."
        ),
        tags$h4("Quasi-identifier combination (mosaic effect)"),
        p("If the batch collectively contains three or more quasi-identifier categories",
          "(age, sex, ethnicity, diagnosis date, geography\u2026),",
          "the combination may be re-identifying even if no single file contains all of them.",
          "This fires as AMBER."),
        p("The linkage findings appear between the rule summary and the file results.",
          "They are also included prominently in the generated report.",
          "Reviewers should consider whether the researcher needs all submitted files",
          "or whether a reduced submission would eliminate the linkage risk.")
      ),

      # ── Output files ───────────────────────────────────────────────────────
      hs("\U0001F4C2", "Output Files",
        p("The Airlock Checker writes the following files to its base folder",
          tags$code(OUT_DIR), ":"),
        tags$dl(class="help-kv",
          tags$dt(tags$code(".dte_audit_log.csv")),
          tags$dd("Append-only log of every reviewer decision. Columns: timestamp, file, file_type, size_bytes, DTE classification, batch risk score, rules fired, reviewer decision, override flag, note."),
          tags$dt(tags$code(".dte_config.json")),
          tags$dd("Saved configuration - restored automatically on next app start.")
        ),
        p("The", tags$strong("review report PDF"), "is saved to the",
          tags$em("source folder"), "(the folder currently open in the file browser),",
          "not the output folder. This keeps the report alongside the files it covers."),
        tags$h4("Initial setup - dependencies.R"),
        p("Before running the Airlock Checker for the first time in a new workspace,",
          "run", tags$code("dependencies.R"), "(located alongside app.R) from the Terminal:"),
        tags$pre(style="font-size:var(--fs-emphasis); background:#F0F4F8; padding:0.5rem 0.8rem; border-radius:4px;",
          "Rscript dependencies.R"),
        p("This installs all required R packages including",
          tags$code("pdftools"), "(used for PDF text extraction).",
          if (PDFTOOLS_OK)
            tags$span(style="color:#2E7D32; font-weight:700;", " \u2713 pdftools is currently installed.")
          else
            tags$span(style="color:#C62828; font-weight:700;",
              " \u26a0 pdftools is NOT installed \u2014 PDF files cannot be inspected. Run dependencies.R.")
        )
      )
    )  # end help-section div
  })

  # ── (Removed) Auto-switch to Results tab after assessment ──────────────────
  # The results tabset was removed; results now render directly, so there is
  # no tab to switch to. Observer deleted to avoid calling nav_select on a
  # non-existent tabset.

  # Rule reference panel
  output$rule_ref_ui <- renderUI({
    res <- res_data()
    fired <- if(length(res)>0)
      unique(unlist(lapply(res,function(r) sapply(r$hits,`[[`,"rule"))))
    else character(0)

    tagList(
      div(class="sect-hd","RULE DEFINITIONS"),
      div(style="font-size:var(--fs-body); color:var(--text-hint); margin-bottom:0.6rem;",
        if(length(fired)>0)
          paste0(length(fired)," rule(s) triggered in last assessment - highlighted below")
        else
          "All rules shown. Triggered rules will be highlighted after an assessment."
      ),
      tagList(lapply(RULES, function(r) {
        active <- r$id %in% fired
        border_c <- switch(r$outcome,RED=RED_C,AMBER=AMBER_C,GREEN=GREEN_C,"#7B1FA2")
        obg <- switch(r$outcome,RED=RED_BG,AMBER=AMBER_BG,GREEN=GREEN_BG,"#F3E5F5")
        div(
          class=paste("rule-ref-item", if(active)"rr-active" else "rr-dimmed"),
          style=paste0("border-left-color:",border_c,"; background:",
            if(active)obg else "#F8F9FA",";"),
          div(style="display:flex; align-items:center; gap:0.3rem;",
            span(class="rr-id", style=paste0("color:",border_c,";"), r$id),
            if(active) span(class="rr-trigger","FIRED")
          ),
          div(class="rr-label", r$label),
          div(class="rr-check", r$check),
          
        )
      }))
    )
  })

  # Full rule table
  output$rule_table <- DT::renderDataTable({
    df <- do.call(rbind, lapply(RULES, function(r) data.frame(
      `Rule ID`=r$id, `Rule Name`=r$label, `Outcome`=r$outcome,
      `Check Logic`=r$check,
      `Applicable File Types`=paste(r$file_types,collapse=", "),
      check.names=FALSE, stringsAsFactors=FALSE)))

    DT::datatable(df, rownames=FALSE, class="compact",
      options=list(pageLength=25, scrollX=TRUE, dom="ftp",
        columnDefs=list(
          list(width="75px",  targets=0),   # Rule ID
          list(width="180px", targets=1),   # Rule Name
          list(width="70px",  targets=2),   # Outcome
          list(width="500px", targets=3),   # Check Logic
          list(width="140px", targets=4)))) |>  # File Types
    DT::formatStyle("Outcome",
      backgroundColor=DT::styleEqual(c("RED","AMBER","GREEN"),c(RED_BG,AMBER_BG,GREEN_BG)),
      color=DT::styleEqual(c("RED","AMBER","GREEN"),c(RED_C,AMBER_C,GREEN_C)),
      fontWeight="bold") |>
    DT::formatStyle("Rule ID", fontFamily="monospace", fontWeight="bold", color=ARIDHIA_DARK)
  })

  # ── Review strip observers ───────────────────────────────────
  # registered_rev prevents duplicate observer stacking when res_data() updates

  observeEvent(res_data(), {
    .perf_t0 <- Sys.time()
    res <- res_data()
    log_event("INFO", "perf_reviewer_strip_observer_start",
              n_files = length(res))
    .perf_env <- new.env(parent=emptyenv()); .perf_env$n <- 0L
    for (r_item in res) {
      local({
        r         <- r_item
        fid       <- gsub("[^a-zA-Z0-9]","_", r$file)
        rid       <- paste0("rev_submit_", fid)
        oid       <- paste0("rev_outcome_", fid)
        nid       <- paste0("rev_note_", fid)
        logged_id <- paste0("rev_logged_", fid)
        rid_rm    <- paste0("rm_from_batch_", fid)

        if (exists(rid, envir=registered_rev)) return()
        assign(rid, TRUE, envir=registered_rev)
        .perf_env$n <- .perf_env$n + 1L

        observeEvent(input[[rid]], {
          final_outcome <- input[[oid]] %||% r$classification
          reviewer_note <- input[[nid]] %||% ""

          # Mandatory note for overrides - JS disables button but validate server-side too
          if (final_outcome != r$classification && !nzchar(trimws(reviewer_note))) {
            showNotification(
              "\u26a0 A reviewer note is required when overriding the DTE classification.",
              type="warning", duration=6)
            return()
          }

          # Compute current batch risk score for the audit record
          batch_sc <- tryCatch({
            sc <- calculate_batch_score(res_data(),
              check_linkage_risk(res_data(), min_categories=3L, min_shared_files=2L))
            if (!is.null(sc)) sc$total else NA_integer_
          }, error=function(e) NA_integer_)

          ok <- log_decision(r, final_outcome, reviewer_note,
                             batch_risk_score=batch_sc)

          # Record in session decisions for Review Complete tracking
          decs <- session_decisions()
          decs[[r$file]] <- list(
            outcome    = final_outcome,
            note       = reviewer_note,
            overridden = final_outcome != r$classification
          )
          session_decisions(decs)

          log_trigger(log_trigger() + 1)

          # Any RED decision (confirmed or override) - show badge + remove option
          is_confirmed_red <- final_outcome == "RED"

          output[[logged_id]] <- renderUI({
            if (isTRUE(ok)) {
              lbl <- if (final_outcome == r$classification)
                paste0("Confirmed ", final_outcome)
              else
                paste0("Overridden \u2192 ", final_outcome)
              tagList(
                span(class="rev-logged", paste0("\u2713 ", lbl, " saved to log")),
                if (is_confirmed_red)
                  actionButton(rid_rm,
                    "\u00d7  Remove from batch",
                    class="btn btn-sm btn-outline-danger",
                    style=paste0(
                      "font-size:var(--fs-body); padding:0.15rem 0.6rem; ",
                      "margin-left:0.5rem; font-weight:700;"))
              )
            } else {
              span(class="rev-logged",
                style="background:#FFEBEE; border-color:#C62828; color:#C62828;",
                "\u26a0 Write failed \u2014 check file permissions")
            }
          })
        }, ignoreInit=TRUE)

        # ── Remove from batch observer ──────────────────────────────────────
        # Only honoured when the recorded decision is confirmed RED (no override).
        # Removes the file from res_data() so it disappears from the results and
        # is excluded from the governance report. The audit log entry is preserved.
        observeEvent(input[[rid_rm]], {
          decs <- session_decisions()
          dec  <- decs[[r$file]]
          if (is.null(dec) || dec$outcome != "RED") return()
          # Remove from results - filter by filepath to be unambiguous
          res_data(Filter(function(x) x$filepath != r$filepath, res_data()))
          # Remove from batch so it doesn't reappear on next assessment
          batch_files_rv(Filter(function(f) f$path != r$filepath, batch_files_rv()))
          # Remove from session decisions (no longer in the batch)
          decs[[r$file]] <- NULL
          session_decisions(decs)
          showNotification(
            tagList(
              tags$strong("\u2716  Removed from batch:"),
              tags$br(),
              tags$span(style="font-size:var(--fs-emphasis);", r$file),
              tags$br(),
              tags$span(style="font-size:var(--fs-body); opacity:0.85;",
                "Decision recorded in audit log.")
            ),
            type="message", duration=6)
        }, ignoreInit=TRUE)
      })
    }
    log_event("INFO", "perf_reviewer_strip_observer_complete",
              n_files      = length(res),
              n_registered = .perf_env$n,
              elapsed_ms   = as.integer(
                as.numeric(difftime(Sys.time(), .perf_t0, units="secs")) * 1000))
  })

  # ── Log reactive ─────────────────────────────────────────────
  observeEvent(input$refresh_log, log_trigger(log_trigger() + 1))

  log_data <- reactive({
    log_trigger()
    load_log()
  })

  # ── Learning dashboard: Overview ─────────────────────────────
  output$ld_overview_ui <- renderUI({
    log <- log_data()

    if (is.null(log) || nrow(log)==0) {
      log_exists <- file.exists(LOG_PATH)
      return(div(class="ph",
        div(class="ph-icon", "\U0001F4CA"),
        tags$h6(if (log_exists) "Audit log could not be read" else "No decisions logged yet"),
        tags$p(style="font-size:var(--fs-emphasis); color:#AAA;",
          if (log_exists)
            paste0("The log file exists at ", LOG_PATH,
                   " but could not be parsed. ",
                   "It may be empty, corrupted, or have an unexpected column structure. ",
                   "Click Refresh to retry.")
          else
            paste0("Use the reviewer strip on each assessment result to log decisions. ",
                   "Data will appear here after the first submission."))))
    }

    n_total    <- nrow(log)
    n_override <- sum(log$overridden)
    n_green    <- sum(log$final_outcome=="GREEN")
    n_red      <- sum(log$final_outcome=="RED")
    n_amber    <- sum(log$final_outcome=="AMBER")
    override_rate <- round(n_override / n_total * 100)

    # Per-rule fp summary
    smry <- log_summary(log)
    worst_rule <- if (!is.null(smry) && nrow(smry)>0) smry$rule[1] else "-"
    worst_fp   <- if (!is.null(smry) && nrow(smry)>0) paste0(smry$fp_rate[1],"%") else "-"

    div(
      div(style="display:flex; gap:0.75rem; flex-wrap:wrap; margin-bottom:1rem;",
        div(class="ld-stat",
          div(class="ld-stat-val", n_total),
          div(class="ld-stat-lbl", "Total decisions")),
        div(class="ld-stat",
          div(class="ld-stat-val", style=paste0("color:",GREEN_C), n_green),
          div(class="ld-stat-lbl", "Approved GREEN")),
        div(class="ld-stat",
          div(class="ld-stat-val", style=paste0("color:",AMBER_C), n_amber),
          div(class="ld-stat-lbl", "Escalated AMBER")),
        div(class="ld-stat",
          div(class="ld-stat-val", style=paste0("color:",RED_C), n_red),
          div(class="ld-stat-lbl", "Rejected RED")),
        div(class="ld-stat",
          div(class="ld-stat-val",
            style=if(override_rate>15) paste0("color:",RED_C) else paste0("color:",ARIDHIA_DARK),
            paste0(override_rate,"%")),
          div(class="ld-stat-lbl", "Override rate"))
      ),

      if (n_override > 0) {
        div(style=paste0(
          "background:",AMBER_BG,"; border:1px solid ",AMBER_C,
          "; border-radius:6px; padding:0.55rem 0.75rem; font-size:var(--fs-emphasis); margin-bottom:0.8rem;"),
          tags$strong(style=paste0("color:",AMBER_C), "Threshold review suggested: "),
          paste0(n_override, " of ", n_total, " DTE decisions were overridden by reviewers. "),
          if (!is.null(smry) && nrow(smry)>0)
            paste0("Highest false-positive rule: ", worst_rule, " (", worst_fp, " override rate). "),
          "See Rule Performance tab for detail."
        )
      },

      div(class="sect-hd", "RECENT ACTIVITY"),
      div(style="font-size:var(--fs-emphasis);",
        if (n_total > 0) {
          recent <- tail(log[order(log$timestamp),], 5)
          tagList(lapply(seq_len(nrow(recent)), function(i) {
            row <- recent[i,]
            bg  <- switch(row$final_outcome,
              GREEN=GREEN_BG, RED=RED_BG, AMBER=AMBER_BG, "#F3E5F5")
            col <- switch(row$final_outcome,
              GREEN=GREEN_C, RED=RED_C, AMBER=AMBER_C, "#7B1FA2")
            div(style=paste0(
              "display:flex; gap:0.5rem; align-items:center; padding:0.3rem 0.5rem; ",
              "border-radius:4px; margin-bottom:3px; background:", bg, ";"),
              span(style=paste0("font-weight:800; color:",col,"; min-width:55px; font-size:var(--fs-body);"),
                row$final_outcome),
              span(style="flex:1; overflow:hidden; text-overflow:ellipsis; white-space:nowrap;",
                row$file),
              if (row$overridden)
                span(style=paste0("font-size:var(--fs-caption); color:",AMBER_C,"; white-space:nowrap;"),
                  paste0("overrode ", row$dte_class)),
              span(style="font-size:var(--fs-caption); color:#999; white-space:nowrap;",
                format(as.POSIXct(row$timestamp), "%H:%M %d %b"))
            )
          }))
        }
      )
    )
  })

  # ── Learning dashboard: Rule performance table ───────────────
  output$ld_rules_dt <- DT::renderDataTable({
    log <- log_data()
    if (is.null(log) || nrow(log)==0) {
      return(DT::datatable(
        data.frame(Message="No decisions logged yet - use the reviewer strip to record outcomes."),
        rownames=FALSE, options=list(dom="t")))
    }

    smry <- log_summary(log)
    if (is.null(smry)) {
      return(DT::datatable(
        data.frame(Message="No rule fire data yet."),
        rownames=FALSE, options=list(dom="t")))
    }

    smry$flag <- ifelse(smry$fp_rate > 20, "Review threshold",
                   ifelse(smry$fp_rate > 10, "Monitor", "OK"))
    names(smry) <- c("Rule", "Times Fired", "Times Overridden",
                     "Override Rate %", "Confirm Rate %", "Status")

    DT::datatable(smry, rownames=FALSE, class="compact",
      options=list(pageLength=20, dom="ftp",
        columnDefs=list(list(width="80px", targets=0)))) |>
    DT::formatStyle("Override Rate %",
      background=DT::styleInterval(c(10,20), c(GREEN_BG, AMBER_BG, RED_BG)),
      color=DT::styleInterval(c(10,20), c(GREEN_C, AMBER_C, RED_C)),
      fontWeight="bold") |>
    DT::formatStyle("Status",
      color=DT::styleEqual(c("Review threshold","Monitor","OK"),
                           c(RED_C, AMBER_C, GREEN_C)),
      fontWeight="bold") |>
    DT::formatStyle("Rule", fontFamily="monospace", fontWeight="bold")
  })

  # ── Learning dashboard: Full decision log table ──────────────
  output$ld_log_dt <- DT::renderDataTable({
    log <- log_data()
    if (is.null(log) || nrow(log)==0) {
      return(DT::datatable(
        data.frame(Message="No decisions logged yet."),
        rownames=FALSE, options=list(dom="t")))
    }

    display <- log[, c("timestamp","file","file_type","dte_class",
                       "final_outcome","overridden","rules_fired","reviewer_note")]
    display$timestamp <- format(as.POSIXct(display$timestamp), "%d %b %Y %H:%M")
    names(display) <- c("Time","File","Type","DTE Class","Final","Overridden",
                        "Rules Fired","Reviewer Note")
    display <- display[order(as.POSIXct(log$timestamp), decreasing=TRUE),]

    DT::datatable(display, rownames=FALSE, class="compact",
      options=list(pageLength=15, scrollX=TRUE, dom="ftp",
        columnDefs=list(list(width="120px", targets=0),
                        list(width="80px", targets=c(2,3,4)),
                        list(width="60px", targets=5)))) |>
    DT::formatStyle("Final",
      backgroundColor=DT::styleEqual(c("RED","AMBER","GREEN"),c(RED_BG,AMBER_BG,GREEN_BG)),
      color=DT::styleEqual(c("RED","AMBER","GREEN"),c(RED_C,AMBER_C,GREEN_C)),
      fontWeight="bold") |>
    DT::formatStyle("DTE Class",
      backgroundColor=DT::styleEqual(c("RED","AMBER","GREEN"),c(RED_BG,AMBER_BG,GREEN_BG)),
      color=DT::styleEqual(c("RED","AMBER","GREEN"),c(RED_C,AMBER_C,GREEN_C))) |>
    DT::formatStyle("Overridden",
      color=DT::styleEqual(c(TRUE,FALSE), c(RED_C, GREEN_C)),
      fontWeight="bold")
  })

  # (output$ld_fp_ui renderer removed - Fingerprint Cache tab removed entirely)


  # ── AIRA batch summary renderer ─────────────────────────────
  # Renders at the top of the results pane. Five visual states:
  #   (a) AIRA disabled / use case disabled / no results    -> NULL
  #   (b) Ready (not requested yet)                         -> button
  #   (c) In flight                                         -> loading chip
  #   (d) Response.status == "ok"                           -> summary + regen link
  #   (e) timeout / unavailable / malformed                 -> error chip + regen link
  # (aira_batch_summary_ui renderer removed - logic moved to
  # render_batch_aira_fn and called from output$batch_header_ui)


  # ── File preview ─────────────────────────────────────────────
  preview_data <- reactiveVal(NULL)

  observeEvent(input$preview_file, {
    fp <- input$preview_file
    if (is.null(fp) || fp == "" || !file.exists(fp)) return()
    withProgress(message="Loading preview...", value=0.5, {
      prev <- tryCatch(build_preview(fp), error=function(e)
        list(type="error", msg=conditionMessage(e), meta="Error"))
      preview_data(list(fp=fp, prev=prev))
    })
  })

  output$preview_content <- renderUI({
    pd <- preview_data()
    if (is.null(pd)) {
      return(div(class="ph", style="padding:3rem;",
        div(class="ph-icon", "\u25A4"),
        tags$h6("Select a file to preview"),
        tags$p(style="font-size:var(--fs-emphasis); color:#AAA;",
          "Click the Preview button next to any supported file in the File Browser.")))
    }
    tryCatch(
      render_preview_ui(pd$prev, pd$fp),
      error=function(e)
        div(style="padding:1rem; color:#C62828;",
          tags$strong("Preview error: "), conditionMessage(e))
    )
  })

}