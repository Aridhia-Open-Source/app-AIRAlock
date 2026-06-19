# Tabular file inspector (CSV, TSV, Excel)
# Auto-split from app.R - do not edit the monolithic file

# ============================================================
# RULE ENGINE - INSPECTORS
# ============================================================

# Detect a pandas multi-index / pivot-table CSV from its raw line structure.
# ACRO's crosstab() and pivot_table() (especially with margins=True) write
# CSVs with a MultiIndex header: the index-name cell is blank so the header
# row starts with a comma, and a SECOND header row (the inner index level)
# also starts with a comma. These files are pre-aggregated summary tables,
# not flat per-row microdata, so the flat-table rules (identifier columns,
# k-anonymity, cardinality, PII scans) do not apply and can throw on the
# unusual shape. Detection is structural and cheap - reads only the first
# few lines, never parses the whole file. Returns TRUE/FALSE, never throws.
#
# Signature (all must hold):
#   - line 1 (header) starts with a comma  -> blank index-name cell
#   - line 2 starts with a comma           -> a second MultiIndex header level
#   - line 2's non-empty cells are non-numeric (they're index labels, not data)
.is_pivot_multiindex_csv <- function(filepath) {
  tryCatch({
    con <- file(filepath, "r", encoding = "UTF-8")
    on.exit(close(con), add = TRUE)
    lines <- readLines(con, n = 3L, warn = FALSE)
    if (length(lines) < 2L) return(FALSE)
    # Strip a leading UTF-8 BOM if present - pandas/Excel sometimes prepend
    # one, and it would otherwise make line 1 not start with the comma we
    # test for, defeating detection. The BOM may appear as the literal
    # character \ufeff or, depending on how the connection decoded it, as
    # the raw bytes EF BB BF at the very start of the first line.
    strip_bom <- function(s) {
      s <- sub("^\ufeff", "", s)                       # decoded BOM char
      s <- sub("^\xef\xbb\xbf", "", s, useBytes = TRUE) # raw BOM bytes
      s
    }
    lines[1] <- strip_bom(lines[1])
    if (!startsWith(lines[1], ",")) return(FALSE)
    if (!startsWith(lines[2], ",")) return(FALSE)
    # Second-row cells (after the leading comma) should be non-numeric labels,
    # not data values - this distinguishes a MultiIndex header from a data row
    # that merely has an empty first field.
    row2 <- strsplit(lines[2], ",", fixed = TRUE)[[1]]
    row2_vals <- trimws(row2[nzchar(trimws(row2))])
    if (length(row2_vals) == 0L) return(FALSE)
    row2_numeric <- suppressWarnings(!any(is.na(as.numeric(row2_vals))))
    !row2_numeric
  }, error = function(e) FALSE)
}

inspect_tabular <- function(filepath, cfg=list()) {
  # Resolve config with fallback to module-level defaults
  id_pats     <- cfg$id_patterns       %||% participant_id_patterns
  sens_ph     <- cfg$sensitive_phenotypes %||% sensitive_phenotypes
  rest_flds   <- cfg$restricted_fields %||% restricted_fields
  g_cols      <- cfg$gwas_cols         %||% gwas_cols
  count_thr   <- cfg$count_threshold   %||% 5L
  min_rows    <- cfg$tab003_min_rows   %||% 10L
  cardinality <- cfg$tab003_cardinality %||% 0.85
  # Upper bound on rows for the "per-participant scale" check in TAB-003.
  # Files exceeding this size are almost certainly transactional or
  # measurement data, not one-row-per-person, so a high-uniqueness column
  # alone shouldn't fire RED. Tunable via cfg$tab003_max_rows_for_red.
  tab003_maxr <- as.integer(cfg$tab003_max_rows_for_red %||% 1000000L)
  size_thr_mb <- (cfg$size_threshold_gb %||% 5) * 1024
  gwas_min    <- cfg$gen003_min_cols   %||% 4L
  ftx_pats    <- cfg$free_text_patterns  %||% free_text_patterns
  drv_pats    <- cfg$derived_id_patterns %||% derived_id_patterns
  kanon_on    <- isTRUE(cfg$kanon_enabled %||% TRUE)
  kanon_maxr  <- as.integer(cfg$kanon_max_rows    %||% 10000L)
  kanon_maxqi <- as.integer(cfg$kanon_max_qi_cols %||% 6L)

  hits <- list()
  tryCatch({
    ext <- tolower(tools::file_ext(filepath))

    # ── Pivot / multi-index guard ──────────────────────────────────────
    # ACRO crosstab/pivot_table outputs are pre-aggregated summary tables
    # with a MultiIndex header, not flat per-row microdata. The flat-table
    # rules below (identifier columns, k-anonymity, cardinality, PII scans)
    # do not apply to them and can throw on the unusual shape. When such a
    # file is recognised, emit a single neutral informational hit and skip
    # the flat-table rules. For ACRO package members this lets the ACRO SDC
    # verdict lead - AIRAlock contributes recognition, not a competing
    # row-level classification. (CSV/TSV only; xlsx pivots are rarer and
    # read differently.)
    if (!(ext %in% c("xlsx","xls")) && .is_pivot_multiindex_csv(filepath)) {
      n_data_lines <- tryCatch({
        ll <- readLines(filepath, warn = FALSE)
        max(0L, length(ll[nzchar(trimws(ll))]) - 2L)  # minus the 2 header rows
      }, error = function(e) NA_integer_)
      hits <- append(hits, list(list(
        rule    = "TAB-025",
        outcome = "GREEN",
        detail  = paste0(
          "Pre-aggregated pivot / cross-tabulation table (multi-index header). ",
          "Row-level disclosure rules do not apply to summary tables of this ",
          "shape. ",
          if (!is.na(n_data_lines))
            paste0("Approximately ", n_data_lines, " summary row(s). ")
          else "",
          "Where this file is part of an ACRO submission, the ACRO statistical ",
          "disclosure control result is the authoritative check.")
      )))
      return(hits)   # skip flat-table rules entirely
    }

    df  <- if (ext %in% c("xlsx","xls")) {
      readxl::read_excel(filepath, n_max=500)
    } else {
      readr::read_csv(filepath, show_col_types=FALSE, n_max=500,
                      col_types=readr::cols(.default=readr::col_character()))
    }

    # readr can return list-type columns for CSVs with embedded quotes, mixed
    # types, or unusual encodings. Flatten any list columns to character so all
    # downstream code can safely use df[[col]] as an atomic vector.
    df <- as.data.frame(
      lapply(df, function(col) {
        if (is.list(col))
          vapply(col, function(x) paste(unlist(x), collapse="|"), character(1))
        else
          col
      }),
      stringsAsFactors=FALSE, check.names=FALSE
    )

    cols_orig  <- names(df)
    cols_lower <- tolower(cols_orig)
    n_rows     <- nrow(df)

    mk_ev <- function(rows_df, flag_cols, caption="") {
      list(type="table", data=rows_df, flag_cols=as.character(flag_cols), caption=caption)
    }

    # TAB-001: Identifier column headers
    # PATCHED 2026-04-30: consult classify_id_linkability() on the
    # flagged column values. If ALL flagged columns classify as
    # synthetic_sequential / synthetic_uuid / synthetic_hash, demote
    # the hit from RED to AMBER. Mixed cases (some synthetic, some
    # unrecognised) keep RED so a real identifier hiding among
    # synthetic ones doesn't slip through.
    id_cols <- cols_orig[sapply(seq_along(cols_lower), function(i)
      any(sapply(id_pats, function(p) grepl(p, cols_lower[i], perl=TRUE))))]
    if (length(id_cols) > 0) {
      link_results <- lapply(id_cols, function(cn)
        classify_id_linkability(df[[cn]]))
      names(link_results) <- id_cols
      all_synthetic <- length(link_results) > 0L &&
        all(vapply(link_results, linkability_should_downgrade, logical(1)))

      if (all_synthetic) {
        per_col <- vapply(id_cols, function(cn)
          sprintf("'%s' (%s)", cn, link_results[[cn]]$class),
          character(1))
        hits <- append(hits, list(list(rule="TAB-001", outcome="AMBER",
          detail=paste0(
            "Identifier-named column(s) appear synthetic: ",
            paste(per_col, collapse=", "),
            " - values do not appear externally-linkable (downgraded from RED)"),
          linkability=link_results,
          evidence=mk_ev(head(df, 8), id_cols,
            paste0("First 8 rows - flagged column(s): ",
                   paste(id_cols, collapse=", "),
                   " (downgraded to AMBER on linkability check)"))
        )))
      } else {
        hits <- append(hits, list(list(rule="TAB-001", outcome="RED",
          detail=paste0("Forbidden identifier column(s): ",
                        paste(id_cols, collapse=", ")),
          linkability=link_results,
          evidence=mk_ev(head(df, 8), id_cols,
            paste0("First 8 rows - flagged column(s): ",
                   paste(id_cols, collapse=", ")))
        )))
      }
    }

    # TAB-006: Raw UKB field codes
    raw_cols <- cols_orig[grepl("^f\\.\\d+\\.\\d+\\.\\d+", cols_lower, perl=TRUE)]
    if (length(raw_cols) > 0)
      hits <- append(hits, list(list(rule="TAB-006", outcome="RED",
        detail=paste0("Raw source field code column(s): ", paste(head(raw_cols,3), collapse=", ")),
        evidence=mk_ev(head(df[, raw_cols, drop=FALSE], 6), raw_cols,
          "Raw source field columns - first 6 rows")
      )))

    # TAB-004: Sensitive phenotype columns
    sens_cols <- cols_orig[sapply(cols_lower, function(cl)
      any(sapply(sens_ph, function(s) grepl(s, cl))))]
    if (length(sens_cols) > 0)
      hits <- append(hits, list(list(rule="TAB-004", outcome="RED",
        detail=paste0("Sensitive phenotype column(s): ",
                      paste(head(sens_cols,3), collapse=", ")),
        evidence=mk_ev(head(df[, sens_cols, drop=FALSE], 8), sens_cols,
          "Sensitive phenotype columns - first 8 rows")
      )))

    # TAB-005: Unmasked small counts
    count_idxs <- which(grepl(
      "^n$|^n_[a-z]|_count$|^count_|^count$|^total$|^total_|_total$|^cases$|^n_cases$|^controls$|^n_controls$|^freq$|^frequency$|^num_|^number_of_",
      cols_lower, perl=TRUE))
    for (ci in count_idxs) {
      col_name <- cols_orig[ci]
      vals     <- suppressWarnings(as.numeric(df[[ci]]))
      bad_rows <- which(!is.na(vals) & vals > 0 & vals < count_thr)
      if (length(bad_rows) > 0) {
        show_df <- df[head(bad_rows, 10), , drop=FALSE]
        hits <- append(hits, list(list(rule="TAB-005", outcome="RED",
          detail=paste0("Unmasked count(s) < ", count_thr, " in '", col_name, "': ",
                        paste(head(vals[bad_rows],4), collapse=", ")),
          evidence=mk_ev(show_df, col_name,
            paste0(length(bad_rows), " row(s) with count < ", count_thr, " in '", col_name, "'"))
        )))
        break
      }
    }

    # TAB-003: Per-participant (high cardinality) detection
    # PATCHED 2026-04-30 (linkability): consult classify_id_linkability()
    # on the triggering column. If values look synthetic, note as AMBER
    # with the downgrade evidence.
    # PATCHED 2026-06-16: TAB-003 no longer carries a RED verdict. A
    # high-uniqueness column is a *signal* that a column may be a
    # per-participant identifier, not proof of a disclosure risk. Whether
    # an unrecognised unique ID is actually externally linkable can only
    # be confirmed by a reviewer who knows the data provenance - a
    # published de-identified dataset (e.g. Wisconsin breast cancer) has a
    # 100%-unique sample-number 'id' that is NOT a re-identifying key.
    # The former QI-context heuristic (firing RED when QI columns like
    # 'diagnosis' appeared elsewhere) produced false positives on exactly
    # these datasets and was a weak proxy for "is this risky". It has been
    # removed. TAB-003 now flags the identifier column as AMBER for
    # reviewer confirmation; the hard RED cases - a real NHS/CHI number or
    # a forbidden identifier column name - are caught with higher
    # confidence by TAB-001 (identifier column names) and TAB-018 (NHS/CHI
    # value scan with checksum validation).
    if (n_rows >= min_rows) {
      for (ci in seq_along(cols_orig)) {
        ratio <- length(unique(df[[ci]])) / n_rows
        if (ratio > cardinality) {
          cn <- cols_orig[ci]
          link <- classify_id_linkability(df[[ci]])
          base_detail <- paste0(
            n_rows, " rows; column '", cn,
            "' uniqueness=", round(ratio*100), "% (threshold: ",
            round(cardinality*100), "%)")

          if (linkability_should_downgrade(link)) {
            # Synthetic ID (sequential / UUID / hash): not externally
            # linkable. AMBER with linkability evidence.
            hits <- append(hits, list(list(rule="TAB-003", outcome="AMBER",
              detail=paste0(base_detail, " - ", link$evidence,
                            " (synthetic identifier, not externally-linkable)"),
              linkability=link,
              evidence=mk_ev(head(df, 10), cn,
                paste0("First 10 rows - '", cn, "' (",
                       link$class, ": not externally-linkable)"))
            )))
          } else {
            # Unrecognised high-uniqueness column. Could be a
            # per-participant identifier, a measurement column, a
            # free-text column, or a dataset-internal sample number.
            # TAB-003 cannot tell which from shape alone, so it flags
            # for reviewer confirmation rather than asserting RED. If the
            # values are a real-world identifier (NHS, CHI, etc) they are
            # caught with higher confidence by TAB-018's value scan.
            hits <- append(hits, list(list(rule="TAB-003", outcome="AMBER",
              detail=paste0(base_detail,
                " - high-uniqueness column may be a per-participant",
                " identifier. Confirm it is not externally linkable",
                " before egress. (Real-world identifiers such as NHS/CHI",
                " numbers are checked separately by the value scan.)"),
              linkability=link,
              evidence=mk_ev(head(df, 10), cn,
                paste0("First 10 rows - '", cn, "' has ",
                  length(unique(df[[ci]])), " unique values across ",
                  n_rows, " rows (", round(ratio*100),
                  "% unique). Reviewer to confirm linkability."))
            )))
          }
          break
        }
      }
    }

    # TAB-002: PII in cell values - superseded by TAB-018/019/020/021 value scan below

    # ── TAB-018/019/020/021: Value-level PII scanning ─────────────────────
    # Sample first 500 rows to keep performance acceptable on large files.
    # Each pattern type fires at most once (first column where it appears).
    scan_df   <- head(df, 500L)
    str_cols_all <- cols_orig[sapply(df, function(v) is.character(v) || is.factor(v))]

    # Helper: validate NHS number via modulo-11 check digit
    valid_nhs <- function(raw) {
      d <- as.integer(strsplit(gsub("[^0-9]", "", raw), "")[[1]])
      if (length(d) != 10L) return(FALSE)
      if (d[1] == 0L) return(FALSE)                        # cannot start with 0
      if (length(unique(d)) == 1L) return(FALSE)           # all same digit = not a real number
      total <- sum(d[1:9] * (10:2))
      rem   <- total %% 11L
      cd    <- 11L - rem
      if (cd == 11L) cd <- 0L
      if (cd == 10L) return(FALSE)       # invalid by spec
      cd == d[10L]
    }

    # Patterns (applied to lower-cased values where appropriate)
    pats <- list(
      # TAB-018 - strong identifiers (RED)
      nhs = list(
        rule="TAB-018", outcome="RED",
        pat  = "\\b([0-9]{3}[- ]?[0-9]{3}[- ]?[0-9]{4})\\b",
        label= "NHS number",
        validate = function(m) {
          # m = character vector of raw matches; keep only those passing mod-11
          d <- gsub("[^0-9]","",m)
          vapply(d, valid_nhs, logical(1))
        }
      ),
      ni = list(
        rule="TAB-018", outcome="RED",
        pat  = "\\b([A-Za-z]{2}[0-9]{6}[A-Da-d])\\b",
        label= "National Insurance number",
        validate = function(m) {
          # Exclude invalid prefixes per HMRC spec
          pfx <- toupper(substr(m, 1, 2))
          bad <- c("BG","GB","NK","KN","NT","TN","ZZ","D","F","I","Q","U","V")
          ok  <- !pfx %in% bad &
                 !substr(pfx,1,1) %in% c("D","F","I","Q","U","V") &
                 !substr(pfx,2,2) %in% c("D","F","I","O","Q","U","V")
          ok
        }
      ),
      chi = list(
        rule="TAB-018", outcome="RED",
        pat  = "\\b([0-3][0-9][0-1][0-9][0-9]{6})\\b",
        label= "CHI number (Scotland \u2014 DOB-encoded 10-digit)",
        validate = function(m) {
          # First 6 digits must be a plausible DDMMYY
          dd <- as.integer(substr(m,1,2))
          mm <- as.integer(substr(m,3,4))
          !is.na(dd) & !is.na(mm) & dd >= 1 & dd <= 31 & mm >= 1 & mm <= 12
        }
      ),
      email = list(
        rule="TAB-018", outcome="RED",
        pat  = "\\b[a-z0-9._%+\\-]+@[a-z0-9.\\-]+\\.[a-z]{2,}\\b",
        label= "Email address",
        validate = function(m) rep(TRUE, length(m))
      ),
      # TAB-019 - postcodes (RED = full, AMBER = outward only)
      postcode_full = list(
        rule="TAB-019", outcome="RED",
        pat  = "\\b([A-Za-z]{1,2}[0-9][0-9A-Za-z]?\\s?[0-9][A-Za-z]{2})\\b",
        label= "UK postcode (full)",
        validate = function(m) {
          # Must have the inward code part (digit + 2 letters)
          grepl("[0-9][A-Za-z]{2}$", trimws(m))
        }
      ),
      # TAB-020 - phone numbers (AMBER)
      phone = list(
        rule="TAB-020", outcome="AMBER",
        pat  = "(?:^|(?<=[^0-9]))((?:\\+44|0044|0)\\s?[1-9][0-9]{8,9})(?=$|[^0-9])",
        label= "UK phone number",
        validate = function(m) rep(TRUE, length(m))
      ),
      # TAB-021 - DOB patterns in non-date-named columns (RED)
      dob = list(
        rule="TAB-021", outcome="RED",
        pat  = paste0(
          "\\b([0-3]?[0-9][/\\-][0-1]?[0-9][/\\-][0-9]{2,4})\\b|",       # DD/MM/YY(YY)
          "\\b([0-9]{4}[/\\-][0-1][0-9][/\\-][0-3][0-9])\\b|",            # YYYY-MM-DD
          "\\b([0-3]?[0-9]\\s+(?:jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec)[a-z]*\\s+[0-9]{4})\\b"
        ),
        label= "Date-of-birth pattern in free-text",
        validate = function(m) rep(TRUE, length(m)),
        # Only fire on columns NOT already named as a date column
        skip_if_date_col = TRUE
      )
    )

    # Date column name heuristic (reuse TAB-005 logic for skip_if_date_col)
    date_col_pat <- "date|dob|birth|born|death|dod|admitted|discharged|visit|event|created|updated|timestamp"

    pii_found <- character(0)   # track which rule IDs have fired to avoid dupes

    for (pname in names(pats)) {
      p <- pats[[pname]]
      if (p$rule %in% pii_found) next

      for (col in str_cols_all) {
        # skip_if_date_col: don't flag DOB pattern in columns obviously named as dates
        if (isTRUE(p$skip_if_date_col) &&
            grepl(date_col_pat, tolower(col), ignore.case=TRUE)) next

        vals <- tolower(as.character(scan_df[[col]]))
        m    <- regmatches(vals, gregexpr(p$pat, vals, perl=TRUE, ignore.case=TRUE))
        matches_flat <- unlist(m)
        if (length(matches_flat) == 0) next

        # Apply validation filter
        valid_mask <- tryCatch(p$validate(matches_flat), error=function(e) rep(TRUE, length(matches_flat)))
        valid_matches <- matches_flat[valid_mask]
        if (length(valid_matches) == 0) next

        # Find which rows had valid matches
        row_has_match <- vapply(seq_along(vals), function(i) {
          rm <- m[[i]]
          if (length(rm) == 0) return(FALSE)
          any(tryCatch(p$validate(rm), error=function(e) rep(TRUE, length(rm))))
        }, logical(1))
        bad_rows <- which(row_has_match)
        if (length(bad_rows) == 0) next

        n_found  <- length(bad_rows)
        ex_vals  <- head(valid_matches, 3)
        # Partially redact examples: keep first 3 and last 2 chars
        redacted <- sapply(ex_vals, function(v) {
          if (nchar(v) <= 5) paste(rep("*", nchar(v)), collapse="")
          else paste0(substr(v,1,3), paste(rep("*", nchar(v)-5), collapse=""), substr(v,nchar(v)-1,nchar(v)))
        })

        ev_df    <- scan_df[head(bad_rows, 8), col, drop=FALSE]
        ev_col   <- col
        ev_cap   <- paste0(n_found, " row(s) with ", p$label, " in '", col, "'")

        hits <- append(hits, list(list(
          rule    = p$rule,
          outcome = p$outcome,
          detail  = paste0(p$label, " detected in column '", col, "' (",
                           n_found, " row(s) in sample). ",
                           "Examples (redacted): ", paste(redacted, collapse=", ")),
          evidence= mk_ev(ev_df, ev_col, ev_cap)
        )))
        pii_found <- c(pii_found, p$rule)
        break   # one hit per pattern type per file
      }
    }

    # ── TAB-022: Column uniqueness profile (informational, always fires) ───
    # Compute per-column stats on full dataset (capped at 5000 rows for speed)
    prof_df  <- head(df, 5000L)
    n_prof   <- nrow(prof_df)
    if (n_prof > 0 && length(cols_orig) > 0) {
      prof_rows <- lapply(cols_orig, function(col) {
        v       <- prof_df[[col]]
        n_miss  <- sum(is.na(v) | (is.character(v) & !nzchar(trimws(as.character(v)))))
        n_uniq  <- length(unique(v[!is.na(v)]))
        pct_uniq <- round(100 * n_uniq / max(n_prof - n_miss, 1), 1)
        pct_miss <- round(100 * n_miss / n_prof, 1)
        dtype   <- if (inherits(v, c("Date","POSIXct","POSIXlt"))) "Date"
                   else if (is.numeric(v)) "Numeric"
                   else if (is.logical(v)) "Logical"
                   else "Character"
        range_s <- if (dtype == "Numeric") {
          mn <- suppressWarnings(min(v, na.rm=TRUE))
          mx <- suppressWarnings(max(v, na.rm=TRUE))
          if (is.finite(mn)) paste0(round(mn,2), " \u2013 ", round(mx,2)) else "\u2014"
        } else if (dtype == "Date") {
          mn <- suppressWarnings(min(v, na.rm=TRUE))
          mx <- suppressWarnings(max(v, na.rm=TRUE))
          if (!is.na(mn)) paste0(as.character(mn), " \u2013 ", as.character(mx)) else "\u2014"
        } else "\u2014"
        risk_flag <- pct_uniq >= 90 && n_uniq > 10
        list(col=col, dtype=dtype, n_uniq=n_uniq, pct_uniq=pct_uniq,
             pct_miss=pct_miss, range_s=range_s, risk_flag=risk_flag)
      })

      prof_data <- as.data.frame(do.call(rbind, lapply(prof_rows, function(r)
        list(
          Column     = r$col,
          Type       = r$dtype,
          `Unique values` = r$n_uniq,
          `Uniqueness %`  = paste0(r$pct_uniq, "%"),
          `Missing %`     = paste0(r$pct_miss, "%"),
          `Range / domain`= r$range_s
        )
      )), stringsAsFactors=FALSE, check.names=FALSE)

      n_high_uniq <- sum(sapply(prof_rows, function(r) r$risk_flag))

      detail_22 <- paste0(
        "Column profile: ", length(cols_orig), " column(s), ",
        n_prof, " row(s) sampled",
        if (n_high_uniq > 0)
          paste0(". \u26a0 ", n_high_uniq, " column(s) with uniqueness \u226590%",
                 " \u2014 review for potential identifier risk")
        else
          " \u2014 no high-uniqueness columns detected"
      )

      # Build evidence as a table - mark high-uniqueness rows visually
      # render_evidence_html uses flag_cols (column names), not row predicates.
      # Prefix high-uniqueness values with a warning marker so the column
      # highlight draws the reviewer's eye to the right rows.
      prof_data[["Uniqueness %"]] <- sapply(seq_along(prof_rows), function(i) {
        r <- prof_rows[[i]]
        if (r$risk_flag) paste0("\u26a0 ", r$pct_uniq, "%") else paste0(r$pct_uniq, "%")
      })
      ev_22 <- list(
        type      = "table",
        data      = prof_data,
        caption   = paste0("Column profile (", n_prof, " rows sampled). ",
                           "\u26a0 marks columns with uniqueness \u226590% ",
                           "\u2014 review for potential identifier risk."),
        flag_cols = if (n_high_uniq > 0) "Uniqueness %" else character(0)
      )

      hits <- append(hits, list(list(
        rule    = "TAB-022",
        outcome = if (n_high_uniq > 0) "AMBER" else "GREEN",
        detail  = detail_22,
        evidence= ev_22
      )))
    }

    # TAB-007: Restricted fields
    rest_cols <- cols_orig[sapply(cols_lower, function(cl)
      any(sapply(rest_flds, function(r) grepl(r, cl))))]
    if (length(rest_cols) > 0)
      hits <- append(hits, list(list(rule="TAB-007", outcome="AMBER",
        detail=paste0("Restricted field(s): ", paste(head(rest_cols,3), collapse=", ")),
        evidence=mk_ev(head(df[, rest_cols, drop=FALSE], 6), rest_cols,
          "Restricted field columns - first 6 rows")
      )))

    # GEN-003: GWAS summary
    if (sum(cols_lower %in% g_cols) >= gwas_min && n_rows > 0)
      hits <- append(hits, list(list(rule="GEN-003", outcome="GREEN",
        detail=paste0("GWAS summary statistics format (",
          paste(intersect(cols_lower, g_cols), collapse=", "), ")"),
        evidence=mk_ev(head(df, 5), character(0),
          "First 5 rows - GWAS summary format confirmed")
      )))

    # ── TAB-009: Percentage back-calculation risk ──────────────────────────
    # Percentage col × known N implies a count. If that count < threshold → RED.
    pct_pat  <- "pct|percent|prop|proportion|prevalence|rate|freq_pct|_pct$"
    denom_pat <- "^n$|^n_[a-z]|_n$|^total$|^total_|_total$|^size$|^denom|^population$|^pop$"
    pct_cols  <- cols_orig[grepl(pct_pat, cols_lower, ignore.case=TRUE, perl=TRUE)]
    denom_cols_tab009 <- cols_orig[grepl(denom_pat, cols_lower, perl=TRUE)]
    # Only run if both types present and file looks like a summary (not per-row)
    if (length(pct_cols) > 0 && length(denom_cols_tab009) > 0 &&
        n_rows <= 500 && n_rows > 0) {
      for (pc in pct_cols) {
        pct_vals <- suppressWarnings(as.numeric(df[[pc]]))
        for (nc in denom_cols_tab009) {
          n_vals <- suppressWarnings(as.numeric(df[[nc]]))
          if (all(is.na(pct_vals)) || all(is.na(n_vals))) next
          # Detect 0-1 vs 0-100 scale
          mx <- max(pct_vals, na.rm=TRUE)
          implied <- if (!is.na(mx) && mx <= 1.01)
            pct_vals * n_vals
          else
            (pct_vals / 100) * n_vals
          bad <- which(!is.na(implied) & implied > 0 & implied < count_thr)
          if (length(bad) > 0) {
            ex_pct <- round(pct_vals[bad[1]], 4)
            ex_n   <- n_vals[bad[1]]
            ex_imp <- round(implied[bad[1]], 1)
            show_cols <- unique(c(pc, nc))
            show_df   <- df[head(bad, 8), show_cols, drop=FALSE]
            hits <- append(hits, list(list(rule="TAB-009", outcome="RED",
              detail=paste0(
                "Column '", pc, "' \u00d7 '", nc, "' implies ",
                length(bad), " count(s) below ", count_thr,
                " (e.g. ", ex_pct, " \u00d7 ", ex_n, " = ", ex_imp, ")"),
              evidence=mk_ev(show_df, show_cols,
                paste0("Rows where percentage \u00d7 N implies count < ",
                       count_thr)))))
            break
          }
        }
        if (any(sapply(hits, function(h) h$rule == "TAB-009"))) break
      }
    }

    # ── TAB-010: Rare category exposure ────────────────────────────────────
    # Categorical columns where any value appears < count_thr times.
    # Skip: already-flagged ID columns, free-text columns (too many uniques),
    # numeric columns, and trivially small datasets.
    if (n_rows >= count_thr && n_rows <= 5000) {
      for (col in cols_orig) {
        x <- df[[col]]
        # Only check string/character columns that look categorical
        if (!is.character(x) && !is.factor(x)) next
        if (col %in% id_cols) next            # already flagged
        n_unique <- length(unique(x[!is.na(x)]))
        if (n_unique < 2 || n_unique > min(50, n_rows * 0.4)) next  # free text
        freq      <- sort(table(x, useNA="no"))
        rare_cats <- names(freq[freq > 0 & freq < count_thr])
        if (length(rare_cats) == 0) next
        # Show the rows containing the rarest category
        bad_rows <- which(x %in% rare_cats)
        show_df  <- df[head(bad_rows, 8), , drop=FALSE]
        hits <- append(hits, list(list(rule="TAB-010", outcome="RED",
          detail=paste0(
            "Column '", col, "' has ", length(rare_cats),
            " rare category/ies with < ", count_thr, " occurrence(s): ",
            paste(head(rare_cats, 4), collapse=", ")),
          evidence=mk_ev(show_df, col,
            paste0("Rows with rare category in '", col, "'")))))
        break   # one hit is sufficient to alert
      }
    }

    # ── TAB-011: Over-precise continuous variables ─────────────────────────
    tab011_hits <- list()

    # Dates: YYYY-MM-DD day-level precision
    date_name_pat <- "date|dob|birth|death|admission|discharge|event_dt|visit"
    date_cols_t11 <- cols_orig[grepl(date_name_pat, cols_lower, ignore.case=TRUE)]
    for (col in date_cols_t11) {
      vals <- as.character(df[[col]])
      day_precise <- grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", vals, perl=TRUE)
      if (sum(day_precise, na.rm=TRUE) >= min(3L, n_rows)) {
        tab011_hits <- c(tab011_hits, paste0(
          "Date column '", col, "' contains day-level precision (YYYY-MM-DD). ",
          "Truncate to year (YYYY) before egress."))
        if (length(tab011_hits) >= 2) break
      }
    }

    # Postcodes: full sector (inward code present)
    pc_name_pat <- "postcode|postal|post_code|zipcode|zip_code"
    pc_cols_t11 <- cols_orig[grepl(pc_name_pat, cols_lower, ignore.case=TRUE)]
    for (col in pc_cols_t11) {
      vals <- toupper(trimws(as.character(df[[col]])))
      # Full UK postcode: outward + space + inward (e.g. SW1A 1AA or SW1A1AA)
      full_pc <- grepl("^[A-Z]{1,2}[0-9][0-9A-Z]?[[:space:]]?[0-9][A-Z]{2}$",
                       vals, perl=TRUE)
      if (sum(full_pc, na.rm=TRUE) >= min(3L, n_rows)) {
        tab011_hits <- c(tab011_hits, paste0(
          "Postcode column '", col, "' contains full sector-level postcodes. ",
          "Truncate to outward code only (e.g. SW1A)."))
        if (length(tab011_hits) >= 2) break
      }
    }

    # Ages: decimal sub-year precision
    age_name_pat <- "^age$|^age_at|_age$|age_yr|age_year|age_months"
    age_cols_t11 <- cols_orig[grepl(age_name_pat, cols_lower, perl=TRUE)]
    for (col in age_cols_t11) {
      vals <- suppressWarnings(as.numeric(df[[col]]))
      if (any(!is.na(vals) & vals > 0 & vals != floor(vals))) {
        ex <- head(vals[!is.na(vals) & vals != floor(vals)], 2)
        tab011_hits <- c(tab011_hits, paste0(
          "Age column '", col, "' contains decimal values (e.g. ",
          paste(round(ex, 2), collapse=", "), "). ",
          "Truncate to whole years before egress."))
        if (length(tab011_hits) >= 2) break
      }
    }

    if (length(tab011_hits) > 0) {
      # Build evidence table of flagged columns.
      # Use vapply with explicit logical(1) so empty-input returns logical(0)
      # rather than list() (which triggers "invalid subscript type 'list'").
      t11_cols <- unique(c(
        date_cols_t11[vapply(date_cols_t11, function(c)
          any(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$",
                    as.character(df[[c]]), perl=TRUE)), logical(1))],
        pc_cols_t11[vapply(pc_cols_t11, function(c) {
          v <- toupper(trimws(as.character(df[[c]])))
          any(grepl("^[A-Z]{1,2}[0-9][0-9A-Z]?[[:space:]]?[0-9][A-Z]{2}$",
                    v, perl=TRUE))
        }, logical(1))],
        age_cols_t11[vapply(age_cols_t11, function(c) {
          v <- suppressWarnings(as.numeric(df[[c]]))
          any(!is.na(v) & v > 0 & v != floor(v))
        }, logical(1))]
      ))
      t11_cols <- t11_cols[t11_cols %in% names(df)]
      show_df  <- if (length(t11_cols) > 0)
        head(df[, t11_cols, drop=FALSE], 8) else head(df, 8)
      hits <- append(hits, list(list(rule="TAB-011", outcome="AMBER",
        detail=paste(tab011_hits, collapse=" | "),
        evidence=mk_ev(show_df, t11_cols,
          "Over-precise columns \u2014 sample values"))))
    }

    # ── TAB-012: Free text / narrative columns ─────────────────────────────
    ftx_cols <- cols_orig[sapply(cols_lower, function(cl)
      any(sapply(ftx_pats, function(p) grepl(p, cl, perl=TRUE))))]
    # Exclude columns already flagged as identifiers or sensitive phenotypes
    ftx_cols <- ftx_cols[!ftx_cols %in% c(id_cols, sens_cols)]
    if (length(ftx_cols) > 0) {
      # Strengthen signal: check if values are long strings (actual narrative)
      has_long_vals <- any(sapply(ftx_cols, function(col) {
        vals <- as.character(df[[col]])
        vals <- vals[!is.na(vals) & nzchar(vals)]
        length(vals) > 0 && mean(nchar(vals), na.rm=TRUE) > 30
      }))
      hits <- append(hits, list(list(rule="TAB-012", outcome="AMBER",
        detail=paste0(
          "Free text column(s) detected: ", paste(head(ftx_cols, 3), collapse=", "),
          if (has_long_vals) " \u2014 long string values suggest narrative content"
          else " \u2014 column name indicates unstructured text"),
        evidence=mk_ev(head(df[, ftx_cols, drop=FALSE], 6), ftx_cols,
          paste0("Free text column(s) \u2014 first 6 rows"))
      )))
    }

    # ── TAB-013: Secondary suppression risk ────────────────────────────────
    # Detect rows where a count column shows a suppression marker AND a total
    # column has a numeric value - suppressed count may be back-calculable.
    supp_markers <- c("<5","< 5","<10","< 10","suppressed","redacted","*","[c]","[s]","c","s")
    total_pat013 <- "^total$|^grand_total$|^total_|_total$|^sum$|^all$|^overall$|^n_total$"
    count_pat013 <- paste0(
      "^n$|^n_[a-z]|_count$|^count_|^count$|^cases$|^n_cases$|",
      "^controls$|^n_controls$|^freq$|^frequency$")
    total_cols_013 <- cols_orig[grepl(total_pat013, cols_lower, perl=TRUE)]
    count_cols_013 <- cols_orig[grepl(count_pat013, cols_lower, perl=TRUE)]

    if (length(total_cols_013) > 0 && length(count_cols_013) > 0 &&
        n_rows > 0 && n_rows <= 300) {
      risk_rows <- integer(0)
      for (ri in seq_len(n_rows)) {
        row_suppressed <- any(sapply(count_cols_013, function(col) {
          val <- trimws(tolower(as.character(df[[col]][ri])))
          val %in% tolower(supp_markers) || grepl("^<[[:space:]]*[0-9]", val)
        }))
        if (!row_suppressed) next
        row_has_total <- any(sapply(total_cols_013, function(col) {
          val <- suppressWarnings(as.numeric(df[[col]][ri]))
          !is.na(val) && val > 0
        }))
        if (row_has_total) risk_rows <- c(risk_rows, ri)
      }
      if (length(risk_rows) > 0) {
        show_cols <- unique(c(head(count_cols_013, 2), head(total_cols_013, 1)))
        show_cols <- show_cols[show_cols %in% names(df)]
        hits <- append(hits, list(list(rule="TAB-013", outcome="RED",
          detail=paste0(
            length(risk_rows), " row(s) contain suppressed count(s) alongside a total column. ",
            "Suppressed value(s) may be recoverable by subtraction from the total."),
          evidence=mk_ev(df[head(risk_rows, 8), show_cols, drop=FALSE], show_cols,
            paste0("Rows where suppression markers appear with totals (",
                   length(risk_rows), " row(s) at risk)"))
        )))
      }
    }

    # ── TAB-014: Derived temporal / spatial identifiers ────────────────────
    drv_cols <- cols_orig[sapply(cols_lower, function(cl)
      any(sapply(drv_pats, function(p) grepl(p, cl, perl=TRUE))))]
    if (length(drv_cols) > 0) {
      # Check if values are numeric (not already caught as date column by TAB-011)
      drv_numeric <- drv_cols[sapply(drv_cols, function(col) {
        vals <- suppressWarnings(as.numeric(df[[col]]))
        sum(!is.na(vals)) >= min(3L, n_rows)
      })]
      if (length(drv_numeric) > 0)
        hits <- append(hits, list(list(rule="TAB-014", outcome="AMBER",
          detail=paste0(
            "Derived temporal/spatial column(s): ",
            paste(head(drv_numeric, 4), collapse=", "),
            " \u2014 these remain quasi-identifying despite being computed values"),
          evidence=mk_ev(head(df[, drv_numeric, drop=FALSE], 8), drv_numeric,
            "Derived identifier columns \u2014 first 8 rows")
        )))
    }

    # ── TAB-015/016: K-anonymity estimation ────────────────────────────────
    # ── GEN-013/014/015: GWAS summary depth validation ────────────────
    is_gwas <- any(sapply(hits, function(h) h$rule == "GEN-003"))
    if (is_gwas) {
      # GEN-014: Unexpectedly low variant count
      if (n_rows < 1000L)
        hits <- append(hits, list(list(rule="GEN-014", outcome="AMBER",
          detail=paste0("GWAS summary statistics with only ", n_rows, " row(s). ",
            "A genuine genome-wide analysis typically contains >100,000 variants. ",
            "Confirm this is an intentional filtered subset and document the rationale."))))

      # GEN-013: Small sample size
      n_col_pat <- "^n$|^n_eff|^neff$|^ncas|^ncon|^n_case|^n_control|^sample_size|^samplesize"
      n_cols_gw <- cols_orig[grepl(n_col_pat, cols_lower, perl=TRUE, ignore.case=TRUE)]
      n_max_all <- NA_real_
      for (nc in n_cols_gw) {
        n_vals <- suppressWarnings(as.numeric(as.character(df[[nc]])))
        n_max  <- suppressWarnings(max(n_vals, na.rm=TRUE))
        if (!is.na(n_max) && is.finite(n_max)) {
          if (n_max < 100)
            hits <- append(hits, list(list(rule="GEN-013", outcome="AMBER",
              detail=paste0("Sample size column '", nc, "' has maximum value ",
                round(n_max), " (< 100). GWAS summary statistics from very small ",
                "cohorts may allow individual reconstruction from association signals."))))
          n_max_all <- max(n_max_all, n_max, na.rm=TRUE)
          break
        }
      }

      # GEN-015: Extreme p-value in small cohort
      p_col_pat <- "^p$|^pval$|^p_value$|^pvalue$|^p\\.value$"
      p_cols_gw <- cols_orig[grepl(p_col_pat, cols_lower, perl=TRUE, ignore.case=TRUE)]
      if (!is.na(n_max_all) && n_max_all < 500) {
        for (pc in p_cols_gw) {
          p_vals <- suppressWarnings(as.numeric(as.character(df[[pc]])))
          p_min  <- suppressWarnings(min(p_vals[p_vals > 0], na.rm=TRUE))
          if (!is.na(p_min) && is.finite(p_min) && p_min < 5e-30) {
            p_vals_num  <- suppressWarnings(as.numeric(as.character(df[[pc]])))
            extreme_idx <- which(!is.na(p_vals_num) & p_vals_num > 0 & p_vals_num < 5e-30)
            ev_015_df   <- df[head(extreme_idx, 6L), , drop=FALSE]
            hits <- append(hits, list(list(rule="GEN-015", outcome="AMBER",
              detail=paste0("Extreme p-value (", formatC(p_min, format="e", digits=2),
                ") in column '", pc, "' with implied N \u2248 ", round(n_max_all),
                ". Extreme signals in small cohorts may identify individual contributors."),
              evidence=mk_ev(ev_015_df, pc,
                paste0(length(extreme_idx), " row(s) with p < 5e-30 in '", pc, "'")))))
            break
          }
        }
      }
    }

    # Skip k-anonymity for GWAS summary files
    is_gwas <- any(sapply(hits, function(h) h$rule == "GEN-003"))
    if (kanon_on && !is_gwas && n_rows >= min_rows) {
      # ── Detect quasi-identifier columns via QI_CATEGORIES vocabulary ───────
      qi_matched <- list()   # col_lower -> category name
      for (cat_name in names(QI_CATEGORIES)) {
        terms <- QI_CATEGORIES[[cat_name]]
        for (ci in seq_along(cols_lower)) {
          col <- cols_lower[ci]
          if (!is.null(qi_matched[[col]])) next  # already matched
          if (any(sapply(terms, function(t)
              grepl(paste0("\\b", t, "\\b"), col, ignore.case=TRUE, perl=TRUE))))
            qi_matched[[col]] <- cat_name
        }
      }
      # Exclude already-flagged identifier columns (trivially k=1)
      id_cols_lower <- tolower(id_cols)
      qi_cols_lower <- setdiff(names(qi_matched), id_cols_lower)
      # Map back to original column names
      qi_cols_orig <- cols_orig[match(qi_cols_lower, cols_lower)]
      qi_cols_orig <- qi_cols_orig[!is.na(qi_cols_orig)]
      # Cap at max QI columns, diversifying across categories
      if (length(qi_cols_orig) > kanon_maxqi) {
        cats_used <- sapply(tolower(qi_cols_orig),
                            function(c) qi_matched[[c]] %||% "Other")
        by_cat <- split(qi_cols_orig, cats_used)
        per_cat <- max(1L, ceiling(kanon_maxqi / length(by_cat)))
        qi_cols_orig <- head(unlist(lapply(by_cat, head, per_cat)), kanon_maxqi)
      }

      if (length(qi_cols_orig) == 0) {
        hits <- append(hits, list(list(rule="TAB-016", outcome="AMBER",
          detail=paste0(
            "No quasi-identifier columns were recognised in this file. ",
            "K-anonymity could not be estimated automatically. ",
            "If this file contains individual-level data, manually identify ",
            "QI columns (age, sex, ethnicity, geography, diagnosis dates etc.) ",
            "and compute k-anonymity before submission."))))
      } else if (n_rows > kanon_maxr) {
        hits <- append(hits, list(list(rule="TAB-016", outcome="AMBER",
          detail=paste0(
            n_rows, " rows \u2014 exceeds the k-anonymity computation limit (",
            format(kanon_maxr, big.mark=","), " rows). ",
            "Detected QI columns: ", paste(head(qi_cols_orig, 4), collapse=", "),
            if (length(qi_cols_orig) > 4)
              paste0(" (+", length(qi_cols_orig)-4, " more)") else "",
            ". Manual k-anonymity assessment required."))))
      } else {
        # ── Compute k across all detected QI columns ──────────────────────
        tryCatch({
          df_qi <- df[, qi_cols_orig, drop=FALSE]
          df_qi <- as.data.frame(
            lapply(df_qi, as.character),
            stringsAsFactors=FALSE, check.names=FALSE)

          group_counts <- df_qi %>%
            dplyr::group_by(dplyr::across(dplyr::everything())) %>%
            dplyr::summarise(.k_n = dplyr::n(), .groups="drop") %>%
            dplyr::arrange(.k_n)

          k_min   <- min(group_counts$.k_n)
          n_grps  <- nrow(group_counts)
          n_below <- sum(group_counts$.k_n < count_thr)
          n_uniq  <- sum(group_counts$.k_n == 1L)
          pct_risk <- round(sum(group_counts$.k_n[group_counts$.k_n < count_thr]) /
                              n_rows * 100, 1)

          # Outcome: RED if k below threshold, AMBER if marginal, GREEN if safe
          outcome_k <- if (k_min < count_thr) "RED"
                       else if (k_min < 10L) "AMBER"
                       else "GREEN"

          detail_k <- paste0(
            "k = ", k_min, " (minimum group size) across ",
            format(n_grps, big.mark=","), " combinations of: ",
            paste(qi_cols_orig, collapse=", "), ".",
            if (outcome_k == "RED")
              paste0(" ", n_below, " combination(s) below threshold of ", count_thr,
                     if (n_uniq > 0)
                       paste0(" including ", n_uniq,
                              " unique individual(s) identifiable from QI values alone.")
                     else ".",
                     if (pct_risk > 0)
                       paste0(" ", pct_risk, "% of rows are in under-suppressed groups."))
            else if (outcome_k == "AMBER")
              paste0(" k meets the minimum threshold of ", count_thr,
                     " but is marginal \u2014 consider further aggregation.")
            else
              paste0(" k \u2265 10 \u2014 comfortably above the suppression threshold.")
          )

          # Build evidence: riskiest combinations (smallest groups first)
          risk_df <- head(as.data.frame(group_counts), 8)
          count_col_nm <- paste0("Group size (threshold: \u2265", count_thr, ")")
          names(risk_df)[names(risk_df) == ".k_n"] <- count_col_nm

          ev_k <- if (outcome_k != "GREEN")
            mk_ev(risk_df, count_col_nm,
              paste0("Smallest equivalence classes \u2014 ",
                     n_below, " combination(s) below threshold of ", count_thr))
          else NULL

          if (outcome_k != "GREEN")
            hits <- append(hits, list(list(
              rule="TAB-015", outcome=outcome_k,
              detail=detail_k, evidence=ev_k)))
          # For GREEN: silently note k in a minimal informational hit
          else
            hits <- append(hits, list(list(
              rule="TAB-015", outcome="GREEN",
              detail=detail_k, evidence=NULL)))

        }, error=function(e) {
          hits <<- append(hits, list(list(rule="TAB-016", outcome="AMBER",
            detail=paste0("K-anonymity computation error: ", conditionMessage(e)))))
        })
      }
    }


    # TAB-008
    sz <- file.info(filepath)$size / 1024 / 1024
    if (sz > size_thr_mb)
      hits <- append(hits, list(list(rule="TAB-008", outcome="AMBER",
        detail=paste0("File size ", round(sz,0), " MB exceeds ",
          round(size_thr_mb/1024, 1), " GB threshold"))))

  }, error=function(e) {
    hits <<- append(hits, list(list(rule="PARSE", outcome="UNCERTAIN",
      detail=paste0("Parse error: ", conditionMessage(e)))))
  })

  # TAB-023 and TAB-024 run OUTSIDE the main tryCatch so they fire
  # independently of any earlier failure (e.g. dplyr unavailable)
  tryCatch({
    # ── TAB-023: Suppression consistency - back-calculation risk ───────────
    supp_markers  <- cfg$suppression_markers %||% DEFAULT_CFG$suppression_markers
    total_pats    <- cfg$total_row_patterns   %||% DEFAULT_CFG$total_row_patterns
    # Build a regex that matches suppression markers as complete cell values
    supp_rx <- paste0("^\\s*(",
      paste(sapply(supp_markers, function(m)
        paste0("\\Q", m, "\\E")), collapse="|"),
      ")\\s*$")
    # Simplified approach: use exact matching for speed/correctness
    is_supp_val <- function(v) trimws(as.character(v)) %in% supp_markers

    # Only run on files that look like aggregated tables (≤ 500 rows, has numeric cols)
    # Include character columns where non-suppressed values are mostly numeric
    # (suppression markers like "<5" cause is.numeric() to return FALSE)
    is_numeric_like <- function(v) {
      if (is.numeric(v)) return(TRUE)
      if (!is.character(v)) return(FALSE)
      non_supp <- v[!is_supp_val(v) & !is.na(v) & nzchar(trimws(v))]
      if (length(non_supp) < 2L) return(FALSE)
      mean(!is.na(suppressWarnings(as.numeric(non_supp)))) >= 0.7
    }
    num_cols      <- cols_orig[sapply(df, is_numeric_like)]
    str_cols_supp <- cols_orig[sapply(df, function(v)
      is.character(v) && any(is_supp_val(v)))]

    if (n_rows <= 500 && n_rows > 1 && length(num_cols) > 0 &&
        length(str_cols_supp) > 0) {
      # Identify "total" rows by first non-numeric column label
      label_cols <- cols_orig[sapply(df, function(v)
        is.character(v) && !any(is_supp_val(v)))]
      total_rx <- paste0("(?i)\\b(",
        paste(total_pats, collapse="|"), ")\\b")
      total_row_idx <- integer(0)
      if (length(label_cols) > 0) {
        for (lc in head(label_cols, 2L)) {
          ti <- which(grepl(total_rx, as.character(df[[lc]]), perl=TRUE))
          if (length(ti) > 0) { total_row_idx <- ti; break }
        }
      }

      back_calc_found <- FALSE
      bc_evidence     <- list()

      # For each numeric column, check row-wise: can suppressed cells be derived?
      for (nc in head(num_cols, 10L)) {
        if (back_calc_found) break
        # Find which rows have suppression in any str_cols_supp column
        # We coerce the numeric col to character and check for suppression markers too
        col_vals  <- df[[nc]]
        # Identify suppressed rows for this column:
        # a row is "suppressed" if any associated str col has a marker
        # Simpler: look for NA in numeric col (suppression sometimes read as NA)
        na_rows <- which(is.na(col_vals))

        # Also scan the raw file for suppression markers in this column position
        # by re-reading as character
        col_chr <- tryCatch(
          suppressWarnings(read.csv(filepath, stringsAsFactors=FALSE,
            colClasses="character", check.names=FALSE)[[nc]]),
          error=function(e) NULL)
        if (is.null(col_chr)) next
        supp_rows <- which(is_supp_val(col_chr))
        if (length(supp_rows) == 0) next

        # Get the total row value for this column
        if (length(total_row_idx) == 0) next
        total_val_chr <- col_chr[total_row_idx[1]]
        total_val     <- suppressWarnings(as.numeric(total_val_chr))
        if (is.na(total_val)) next

        # Non-suppressed, non-total rows
        other_idx  <- setdiff(seq_len(nrow(df)), c(supp_rows, total_row_idx))
        other_vals <- suppressWarnings(as.numeric(col_chr[other_idx]))
        other_sum  <- sum(other_vals, na.rm=TRUE)

        # If exactly one suppressed cell: implied = total - sum(others)
        if (length(supp_rows) == 1L) {
          implied <- total_val - other_sum
          if (is.finite(implied)) {
            back_calc_found <- TRUE
            # Build evidence: show the column with markers
            show_idx <- sort(unique(c(total_row_idx, supp_rows, head(other_idx, 4L))))
            ev_df <- df[show_idx, unique(c(head(label_cols,1), nc)), drop=FALSE]
            ev_df[[nc]] <- col_chr[show_idx]  # show raw values incl markers
            bc_evidence <- list(
              evidence=mk_ev(ev_df, nc,
                paste0("Column '", nc, "': suppressed cell (", supp_markers[1],
                  ") can be back-calculated as total \u2212 sum(others) = ",
                  round(total_val), " \u2212 ", round(other_sum), " = ",
                  round(implied),
                  ". Secondary suppression required.")),
              nc=nc, implied=implied, total_val=total_val, other_sum=other_sum)
          }
        } else if (length(supp_rows) > 1L) {
          # Multiple suppressed: check if total constrains their sum
          implied_sum <- total_val - other_sum
          # Any finite non-negative implied sum is a risk: knowing the combined
          # value of suppressed cells narrows individual values.
          if (is.finite(implied_sum) && implied_sum >= 0) {
            back_calc_found <- TRUE
            show_idx <- sort(unique(c(total_row_idx, supp_rows, head(other_idx,3L))))
            ev_df <- df[show_idx, unique(c(head(label_cols,1), nc)), drop=FALSE]
            ev_df[[nc]] <- col_chr[show_idx]
            bc_evidence <- list(
              evidence=mk_ev(ev_df, nc,
                paste0("Column '", nc, "': ", length(supp_rows),
                  " suppressed cells \u2014 combined value uniquely determined as ",
                  round(implied_sum), ". Secondary suppression required.")),
              nc=nc, implied_sum=implied_sum, supp_n=length(supp_rows))
          }
        }
      }

      if (back_calc_found) {
        # Single suppressed cell = RED (exact value known)
        # Multiple suppressed cells = AMBER (combined sum known)
        is_single <- !is.null(bc_evidence$implied)
        hits <- append(hits, list(list(
          rule="TAB-023",
          outcome=if (is_single) "RED" else "AMBER",
          detail=paste0(
            "Suppression back-calculation possible in column '",
            bc_evidence$nc, "'. ",
            if (is_single)
              paste0("Single suppressed cell is uniquely determined: total (",
                round(bc_evidence$total_val), ") \u2212 sum of visible cells (",
                round(bc_evidence$other_sum), ") = ",
                round(bc_evidence$implied), ". ")
            else
              paste0(bc_evidence$supp_n, " suppressed cells \u2014 combined value ",
                "uniquely determined as ", round(bc_evidence$implied_sum),
                ". Individual values may be inferrable. "),
            "Secondary suppression is required."),
          evidence=bc_evidence$evidence
        )))
      }
    }

    # ── TAB-024: Free-text NER - named entity and quasi-identifier detection ──
    ner_on         <- isTRUE(cfg$ner_enabled        %||% DEFAULT_CFG$ner_enabled)
    ner_titles     <- cfg$ner_person_titles  %||% DEFAULT_CFG$ner_person_titles
    ner_places     <- cfg$ner_geo_places      %||% DEFAULT_CFG$ner_geo_places
    ner_insts      <- cfg$ner_inst_patterns   %||% DEFAULT_CFG$ner_inst_patterns
    ner_occs       <- cfg$ner_occ_patterns    %||% DEFAULT_CFG$ner_occ_patterns
    ner_exclusions <- cfg$ner_name_exclusions %||% DEFAULT_CFG$ner_name_exclusions

    if (ner_on) {
      # Column detection - scan any string column that could plausibly contain
      # clinical narrative or identifying free-text. Thresholds are intentionally
      # permissive: false positives are cheap; false negatives miss real PII.
      free_txt_pats <- c(
        cfg$free_text_patterns %||% free_text_patterns,
        # Clinical / research column name patterns beyond the defaults
        "detail","where","location","site","place","hospital","clinic","history",
        "reason","finding","observation","summary","info","remark","comment",
        "name","patient","subject","individual","person","referr"
      )
      free_cols <- cols_orig[sapply(seq_along(cols_orig), function(i) {
        col <- df[[cols_orig[i]]]
        if (!is.character(col)) return(FALSE)
        n_nonmiss  <- sum(!is.na(col) & nzchar(trimws(col)))
        if (n_nonmiss == 0) return(FALSE)
        # Match by column name
        by_name <- any(sapply(free_txt_pats,
          function(p) grepl(p, cols_lower[i], ignore.case=TRUE, perl=FALSE)))
        # Match by content: avg chars > 8 OR any value > 15 chars
        vals_nna   <- col[!is.na(col) & nzchar(trimws(col))]
        avg_chars  <- mean(nchar(vals_nna), na.rm=TRUE)
        has_long   <- any(nchar(vals_nna) > 15)
        by_content <- avg_chars > 8 || has_long
        by_name || by_content
      })]

      if (length(free_cols) > 0) {
        # Scan a sample of free-text values
        scan_n <- min(200L, n_rows)

        # Pattern 1a - Person name with title prefix (Mr John Smith)
        title_rx <- paste0(
          "(?i)\\b(", paste(ner_titles, collapse="|"), ")\\.?\\s+",
          "[A-Z][a-z]+(?:\\s+[A-Z][a-z]+)?")

        # Pattern 1b - Bare person name: two capitalised words with no
        # common English word as first token (reduces false positives)
        # Used specifically in columns whose name suggests they hold names
        name_col_rx <- "(?i)\\b(name|patient|subject|individual|person|attendee|referr)"
        bare_name_rx <- "\\b[A-Z][a-z]{1,20}\\s+[A-Z][a-z]{1,20}\\b"
        # Common English words that look like capitalised names but aren't
        # Exclusion list from config - use to prevent common non-name phrases
        # being flagged as person names. Configure per language/domain.
        not_name_rx <- if (length(ner_exclusions) > 0)
          paste0("(?i)^(", paste(ner_exclusions, collapse="|"), ")\\b")
        else "(?!x)x"  # never-matching regex when exclusions are empty

        # Pattern 2 - UK place name as a whole word
        place_rx <- paste0(
          "(?i)\\b(", paste(ner_places, collapse="|"), ")\\b")

        # Pattern 3 - Healthcare/institution keywords (configurable)
        inst_rx <- paste0(
          "(?i)\\b(", paste(ner_insts, collapse="|"), ")\\b")

        # Pattern 4a - Full quasi-identifier composite (age + sex + occupation)
        qi_age_rx  <- "\\b[0-9]{1,3}[- ]?(?:year|yr|y/o|years)"
        qi_sex_rx  <- "(?i)\\b(male|female|man|woman|boy|girl|he|she)\\b"
        # Occupation list - configurable for different populations/languages
        qi_occ_rx <- if (length(ner_occs) > 0)
          paste0("(?i)\\b(", paste(ner_occs, collapse="|"), ")\\b")
        else "(?!x)x"  # disabled if empty

        # Pattern 4b - Partial quasi-identifier (age + sex, no occupation)
        # This is AMBER rather than RED - common in clinical datasets
        # where Detail/notes columns contain age and gender information

        ner_found <- list()  # pattern_name -> list of (col, row_idx, text)

        for (fc in free_cols) {
          vals <- as.character(df[[fc]][seq_len(scan_n)])
          vals_nna <- vals[!is.na(vals) & nzchar(trimws(vals))]
          if (length(vals_nna) == 0) next

          check_pattern <- function(rx, pname) {
            if (pname %in% names(ner_found)) return()
            mi <- which(grepl(rx, vals, perl=TRUE))
            if (length(mi) == 0) return()
            ner_found[[pname]] <<- list(
              col=fc, rows=head(mi, 6L),
              texts=head(vals[mi], 6L))
          }

          check_pattern(title_rx,  "person_name")
          check_pattern(place_rx,  "uk_place")
          check_pattern(inst_rx,   "institution")

          # Bare person name: only in columns whose name suggests they hold names
          if (!"person_name" %in% names(ner_found) &&
              grepl(name_col_rx, fc, perl=TRUE)) {
            bare_mi <- which(grepl(bare_name_rx, vals, perl=TRUE) &
              !grepl(not_name_rx, vals, perl=TRUE))
            if (length(bare_mi) > 0)
              ner_found[["person_name"]] <- list(
                col=fc, rows=head(bare_mi, 6L),
                texts=head(vals[bare_mi], 6L))
          }

          # Full composite QI: age + sex + occupation in same cell (RED)
          if (!"composite_qi" %in% names(ner_found)) {
            qi_mi <- which(
              grepl(qi_age_rx, vals, perl=TRUE) &
              grepl(qi_sex_rx, vals, perl=TRUE) &
              grepl(qi_occ_rx, vals, perl=TRUE))
            if (length(qi_mi) > 0)
              ner_found[["composite_qi"]] <- list(
                col=fc, rows=head(qi_mi, 6L),
                texts=head(vals[qi_mi], 6L))
          }

          # Partial QI: age + sex only (no occupation) - AMBER
          # Common in clinical "Detail" columns
          if (!"composite_qi" %in% names(ner_found) &&
              !"partial_qi" %in% names(ner_found)) {
            pqi_mi <- which(
              grepl(qi_age_rx, vals, perl=TRUE) &
              grepl(qi_sex_rx, vals, perl=TRUE))
            if (length(pqi_mi) > 0)
              ner_found[["partial_qi"]] <- list(
                col=fc, rows=head(pqi_mi, 6L),
                texts=head(vals[pqi_mi], 6L))
          }
        }

        if (length(ner_found) > 0) {
          # Build evidence from the first/most severe pattern found
          severity <- c(person_name=5L, composite_qi=4L, partial_qi=3L,
                        institution=2L, uk_place=1L)
          best <- names(sort(severity[names(ner_found)], decreasing=TRUE))[1]
          nf   <- ner_found[[best]]

          pattern_labels <- c(
            person_name  = "Person name (bare or with title)",
            composite_qi = "Quasi-identifier composite (age + sex + occupation)",
            partial_qi   = "Quasi-identifier (age + sex)",
            institution  = "Healthcare institution name",
            uk_place     = "UK place name"
          )
          all_patterns <- paste(sapply(names(ner_found), function(p)
            pattern_labels[p]), collapse="; ")

          ev_lines <- lapply(seq_along(nf$texts), function(i)
            list(lineno=i,
                 text=paste0("[", nf$col, "] ", substr(nf$texts[i], 1, 120)),
                 flag=TRUE))

          # partial_qi alone = AMBER; any other pattern = RED
          ner_outcome <- if (best == "partial_qi" && length(ner_found) == 1L)
            "AMBER" else "RED"
          hits <- append(hits, list(list(
            rule="TAB-024", outcome=ner_outcome,
            detail=paste0(
              length(ner_found), " named-entity pattern type(s) detected in free-text columns: ",
              all_patterns, ". ",
              "Column '", nf$col, "' contains ", pattern_labels[best], " pattern(s)."),
            evidence=list(type="lines", lines=ev_lines,
              caption=paste0("Sample matching cells from '", nf$col,
                "' \u2014 ", pattern_labels[best]))
          )))
        }
      }
    }
  }, error=function(e) NULL)  # silently skip if NER/suppression check fails

  hits
}