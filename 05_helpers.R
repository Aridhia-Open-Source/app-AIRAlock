# Shared utility functions
# Auto-split from app.R - do not edit the monolithic file

# Helpers: shared utility functions used across inspectors and UI

# Null-coalescing operator
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

# PDF text extraction helper
# Returns a character vector of lines, or NULL on failure.
extract_pdf_text <- function(filepath) {
  if (!PDFTOOLS_OK) return(NULL)
  tryCatch({
    pages <- pdftools::pdf_text(filepath)
    lines <- unlist(strsplit(paste(pages, collapse="\f"), "\n"))
    lines
  }, error=function(e) NULL)
}

# Sanitise data frames for DT display - convert integer64 to character
sanitise_for_dt <- function(df) {
  if (!is.data.frame(df) || ncol(df) == 0) return(df)
  for (col in names(df)) {
    if (inherits(df[[col]], "integer64"))
      df[[col]] <- as.character(df[[col]])
  }
  df
}

# CROSS-FILE LINKAGE RISK DETECTOR
# ============================================================
# ── Text evidence helper - used by all text-based inspectors ─────────────────
# Splits source text into lines, finds matching lines, returns context.
# Compatible with render_script_evidence (type="lines").
mk_text_ev <- function(src_lines, pattern, caption,
                       n_matches=5L, context=1L, perl=TRUE) {
  if (length(src_lines) == 0) return(NULL)
  # Truncate very long lines (e.g. base64 in JSON/XML) before matching
  safe <- ifelse(nchar(src_lines) > 300,
                 paste0(substr(src_lines, 1, 298), "\u2026"), src_lines)
  idx  <- which(grepl(pattern, safe, ignore.case=TRUE, perl=perl))
  if (length(idx) == 0) return(NULL)
  idx  <- head(idx, n_matches)
  ctx  <- sort(unique(unlist(lapply(idx, function(i)
    intersect(seq_len(length(safe)), seq(max(1L, i-context), min(length(safe), i+context)))))))
  list(type="lines", caption=caption,
       lines=lapply(ctx, function(i)
         list(lineno=i, text=safe[[i]], flag=(i %in% idx))))
}

# ── safe_read_text: robust text-file reading ───────────────────────────
# Returns a list(src, lns, status) where:
#   src    = full file content as a safe UTF-8 string, or ""
#   lns    = lines vector, or character(0)
#   status = "ok" | "empty" | "binary" | "unreadable"
#
# Use this instead of readLines() in inspectors that accept text formats
# (JSON, XML, Markdown, HTML, scripts). Handles binary content, invalid
# UTF-8, and embedded NULs that would otherwise crash downstream grepl().
safe_read_text <- function(filepath, binary_threshold = 0.30) {
  sz <- tryCatch(file.info(filepath)$size, error = function(e) 0)
  if (is.na(sz) || sz == 0) {
    return(list(src = "", lns = character(0), status = "empty"))
  }
  raw_bytes <- tryCatch(readBin(filepath, what = "raw", n = sz),
                        error = function(e) raw(0))
  if (length(raw_bytes) == 0) {
    return(list(src = "", lns = character(0), status = "unreadable"))
  }
  # Binary detection: embedded NULs or too many non-printable bytes
  n_nul     <- sum(raw_bytes == as.raw(0))
  printable <- (raw_bytes >= as.raw(0x20) & raw_bytes <= as.raw(0x7E)) |
               raw_bytes == as.raw(0x09) | raw_bytes == as.raw(0x0A) |
               raw_bytes == as.raw(0x0D)
  pct_binary <- 1 - sum(printable) / length(raw_bytes)
  if (n_nul > 0 || pct_binary > binary_threshold) {
    return(list(src = "", lns = character(0), status = "binary",
                n_nul = n_nul, pct_binary = pct_binary))
  }
  # Convert to safe UTF-8
  src_raw <- tryCatch(rawToChar(raw_bytes), error = function(e) NA_character_)
  if (is.na(src_raw)) {
    return(list(src = "", lns = character(0), status = "unreadable"))
  }
  src <- iconv(src_raw, from = "UTF-8", to = "UTF-8", sub = "?")
  lns <- strsplit(src, "\n", fixed = TRUE)[[1]]
  list(src = src, lns = lns, status = "ok")
}

# ── Diagnostic logging ────────────────────────────────────────────────
# Structured JSONL log to DIAG_LOG. One line per event. Safe to call
# from anywhere; never throws (silently no-ops on failure).
#
# Usage:
#   log_event("INFO",  "batch_start",  n_files = 42)
#   log_event("ERROR", "inspector_threw", inspector = "inspect_tabular",
#             file = fp, message = e$message, trace = .capture_trace())
#
# Use log_error(tryCatch(...)) to wrap any code whose failure must be
# logged AND surfaced - see log_caught() below.

# Log-level filter - DEBUG < INFO < WARN < ERROR
.DIAG_LEVELS <- list(DEBUG = 1L, INFO = 2L, WARN = 3L, ERROR = 4L)

# Escape a single value for JSON output. Handles strings, numbers,
# logicals, NULL, NA. Not a full JSON encoder - just enough for
# flat key/value events.
.jv <- function(x) {
  if (is.null(x))           return("null")
  if (length(x) == 0)       return("null")
  if (length(x) > 1)        return(.jv(paste(as.character(x), collapse = " | ")))
  if (is.na(x))             return("null")
  if (is.logical(x))        return(if (x) "true" else "false")
  if (is.numeric(x))        return(as.character(x))
  # String: escape backslash, quote, control chars
  s <- as.character(x)
  s <- gsub("\\\\", "\\\\\\\\", s)
  s <- gsub('"',    '\\\\"',    s)
  s <- gsub("\n",   "\\\\n",    s)
  s <- gsub("\r",   "\\\\r",    s)
  s <- gsub("\t",   "\\\\t",    s)
  s <- gsub("[[:cntrl:]]", "?", s)
  paste0('"', s, '"')
}

log_event <- function(level, event, ...) {
  tryCatch({
    # Filter by level
    level     <- toupper(level)
    cur_level <- .DIAG_LEVELS[[toupper(DIAG_LEVEL %||% "INFO")]] %||% 2L
    evt_level <- .DIAG_LEVELS[[level]] %||% 2L
    if (evt_level < cur_level) return(invisible(NULL))

    # Build JSON line
    fields <- list(...)
    ts     <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
    head   <- sprintf('{"ts":%s,"level":%s,"event":%s',
                      .jv(ts), .jv(level), .jv(event))
    if (length(fields) == 0) {
      line <- paste0(head, "}")
    } else {
      kv <- vapply(names(fields), function(k)
        paste0(.jv(k), ":", .jv(fields[[k]])), character(1))
      line <- paste0(head, ",", paste(kv, collapse = ","), "}")
    }

    # Write. DIAG_LOG must exist; constants set it up at startup.
    cat(line, "\n", file = DIAG_LOG, append = TRUE, sep = "")
  }, error = function(e) invisible(NULL))
}

# Capture the current call stack as a newline-joined string. For use
# inside error handlers - call as `trace = .capture_trace()`.
.capture_trace <- function() {
  calls <- sys.calls()
  if (length(calls) == 0) return("")
  # Drop the last few frames (the error handler itself)
  n <- length(calls)
  keep <- seq_len(max(0L, n - 2L))
  if (length(keep) == 0) return("")
  txt <- vapply(calls[keep], function(cl)
    paste(deparse(cl, nlines = 2L), collapse = " "), character(1))
  paste(txt, collapse = " <- ")
}

# log_caught(expr, event, ...): evaluates expr, logs any error with
# full trace, then re-raises. Use to wrap observer/render bodies
# where the error must be logged AND surfaced to Shiny.
log_caught <- function(expr, event, ...) {
  withCallingHandlers(
    expr,
    error = function(e) {
      log_event("ERROR", event,
                message = conditionMessage(e),
                trace   = .capture_trace(),
                ...)
      # Let the error propagate so Shiny's own handler also sees it
    }
  )
}

# ── ID column linkability classifier ─────────────────────────────────
# Classify the values of an identifier-shaped column by their apparent
# linkability to entities outside the file. Returns a list with:
#   - class:      one of c("synthetic_sequential", "synthetic_uuid",
#                          "synthetic_hash", "unrecognised")
#   - confidence: "high" | "medium" - only "high" classifications are
#                 used to downgrade rule severity
#   - evidence:   short prose string describing why the column was
#                 classified as it was; suitable for the rule's
#                 detail field
#   - n_values:   how many non-NA values were considered
#
# Conservative by design: only returns one of the synthetic_* classes
# when ALL non-NA values match the pattern. A column with a single
# value that doesn't fit the pattern stays "unrecognised", which means
# the calling rule's existing severity (typically RED) is preserved.
# This makes false-negative the only expected error mode (a real ID
# might occasionally classify as unrecognised; a real ID will NEVER
# classify as synthetic).
#
# The classifier does NOT detect "looks like an NHS number" or other
# real-identifier patterns. Existing rules already handle those cases
# correctly. The classifier's job is purely to identify columns that
# are SAFE to demote, not to upgrade severity.
#
# Used by tabular rules (TAB-001, TAB-003) to demote RED -> AMBER for
# columns that are clearly synthetic sequences, UUIDs, or hex hashes
# rather than externally-linkable identifiers.
classify_id_linkability <- function(values) {
  # Defensive: handle NULL, empty, all-NA inputs.
  if (is.null(values) || length(values) == 0L) {
    return(list(class = "unrecognised",
                confidence = "high",
                evidence = "no values to classify",
                n_values = 0L))
  }

  # Remove NA. We classify on the non-NA subset; an entirely-NA column
  # stays unrecognised.
  v <- values[!is.na(values)]
  n <- length(v)
  if (n == 0L) {
    return(list(class = "unrecognised",
                confidence = "high",
                evidence = "all values NA",
                n_values = 0L))
  }

  # Need a minimum number of values to make a confident judgement.
  # A 2-row file with id = c(1, 2) is technically sequential but the
  # signal is too weak to act on. The TAB-003 rule itself requires
  # n_rows >= min_rows (default 10), and TAB-001 fires regardless of
  # row count. Set our own conservative floor.
  if (n < 5L) {
    return(list(class = "unrecognised",
                confidence = "high",
                evidence = sprintf("only %d non-NA value(s); too few to classify",
                                   n),
                n_values = n))
  }

  # ── Try synthetic_sequential (numeric integers near-1..N) ──
  # Coerce to numeric, see whether it looks integer-like.
  num <- suppressWarnings(as.numeric(as.character(v)))
  is_intlike <- all(!is.na(num)) && all(num == floor(num))
  if (is_intlike) {
    sorted <- sort(unique(num))
    diffs  <- diff(sorted)
    if (length(diffs) > 0L) {
      mean_gap <- mean(diffs)
      max_gap  <- max(diffs)
      # "Near-sequential": the unique values, when sorted, advance
      # mostly by 1 with at most a small gap. Allows for small
      # row-deletion effects (max_gap <= 5) and some duplicates
      # (which would shrink the unique set; we don't penalise that).
      #
      # CRITICAL GUARD: refuse to classify as synthetic if the values
      # are large enough to plausibly BE real identifiers. NHS numbers
      # are 10 digits; sequential NHS numbers from a sorted cohort
      # extract (e.g. 9876543210, 9876543211, ...) would otherwise
      # match the sequential pattern with mean_gap = 1 and downgrade
      # what may be real linkable identifiers. The maximum value's
      # digit count is a robust proxy for "this is too big to be a
      # row index". 9,999,999 (7 digits) covers any realistic row-
      # index file; anything 8+ digits is suspicious.
      max_val <- max(sorted)
      max_digits <- if (max_val > 0) floor(log10(max_val)) + 1L else 1L

      if (mean_gap >= 1 && mean_gap <= 1.2 && max_gap <= 5 &&
          max_digits <= 7L) {
        rng <- range(sorted)
        return(list(class = "synthetic_sequential",
                    confidence = "high",
                    evidence = sprintf(
                      "values are sequential integers in range [%s, %s] with mean gap %.2f (likely row index, not externally-linkable)",
                      format(rng[1], scientific = FALSE),
                      format(rng[2], scientific = FALSE),
                      mean_gap),
                    n_values = n))
      }
      # If the sequence guard caught a "looks sequential but values
      # are too large" case, fall through to unrecognised. Note this
      # in evidence so the audit log records WHY the rule didn't
      # downgrade.
      if (mean_gap >= 1 && mean_gap <= 1.2 && max_gap <= 5 &&
          max_digits > 7L) {
        return(list(class = "unrecognised",
                    confidence = "high",
                    evidence = sprintf(
                      "values are sequential integers but maximum value has %d digits (suspiciously large for a row index; could be a real identifier such as NHS number)",
                      max_digits),
                    n_values = n))
      }
    }
  }

  # All remaining checks operate on string form.
  s <- as.character(v)

  # ── Try synthetic_uuid ──
  # Generic UUID pattern (any version): 8-4-4-4-12 hex blocks.
  uuid_re <- "^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$"
  if (all(grepl(uuid_re, s, perl = TRUE))) {
    return(list(class = "synthetic_uuid",
                confidence = "high",
                evidence = sprintf(
                  "all %d values are UUIDs (workspace-internal random IDs, not externally-linkable)",
                  n),
                n_values = n))
  }

  # ── Try synthetic_hash ──
  # Hex strings of one consistent length matching common hash sizes.
  # MD5=32, SHA-1=40, SHA-256=64, plus 16 (truncated) and short 8.
  hex_re <- "^[0-9a-fA-F]+$"
  if (all(grepl(hex_re, s, perl = TRUE))) {
    lens <- unique(nchar(s))
    if (length(lens) == 1L && lens %in% c(16L, 32L, 40L, 64L)) {
      return(list(class = "synthetic_hash",
                  confidence = "high",
                  evidence = sprintf(
                    "all %d values are %d-character hex strings (likely de-identified hash; pre-existing pseudonymisation)",
                    n, lens),
                  n_values = n))
    }
  }

  # ── Default: unrecognised ──
  # Anything else stays unrecognised. The rule's existing severity is
  # preserved. Examples that land here:
  #   - NHS numbers (10 digits, but length is 10 not 16/32/40/64)
  #   - MRNs (mixed alphanumeric, varies)
  #   - Names, postcodes, emails (would have been caught by other rules)
  #   - Mixed columns (some real, some synthetic)
  #   - Values that look like sequential but aren't (large gaps, etc.)
  list(class = "unrecognised",
       confidence = "high",
       evidence = sprintf(
         "%d values do not match any synthetic-ID pattern (sequential, UUID, hex hash); treating as potentially-linkable identifier",
         n),
       n_values = n)
}

# Convenience: given a linkability classification, decide whether the
# calling rule should downgrade RED to AMBER. Returns TRUE only for
# the three positively-classified synthetic types. Anything else
# (including "unrecognised") returns FALSE so the rule's existing
# severity is preserved.
linkability_should_downgrade <- function(classif) {
  if (!is.list(classif) || is.null(classif$class)) return(FALSE)
  isTRUE(classif$class %in% c("synthetic_sequential",
                              "synthetic_uuid",
                              "synthetic_hash"))
}