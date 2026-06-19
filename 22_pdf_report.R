# PDF governance report generator (base R grid)
# Auto-split from app.R

# ============================================================
# REVIEW REPORT PDF GENERATOR  (base-R grid, no extra packages)
# ============================================================

# ============================================================
# RULES BASELINE - governance helpers
# ============================================================

# Compute a deterministic MD5 over the full active rule configuration.
# Covers: every rule definition, every weight, every threshold, the
# sensitive phenotype list. Writing to a tempfile lets us use

# ============================================================
# PDF REPORT GENERATOR


# ── Enhanced basic PDF report - pure base R grid, no external packages ──────
# ── Enhanced basic PDF report - pure base R grid, no external packages ──────
generate_review_pdf_basic <- function(res, decisions, out_path,
                                       file_dir      = FILE_DIR_DEFAULT,
                                       cfg           = list(),
                                       default_cfg   = list(),
                                       cfg_changes   = list(),
                                       batch_score   = NULL,
                                       aira_reviews  = NULL,
                                       aira_batch    = NULL) {
  library(grid)

  # ── Brand colours ──────────────────────────────────────────────────────────
  DARK <- "#003366"; BLUE <- "#0066A1"
  RC <- "#C62828"; RBG <- "#FFEBEE"
  AC <- "#E65100"; ABG <- "#FFF3CD"
  GC <- "#2E7D32"; GBG <- "#E8F5E9"
  GREY <- "#F5F5F5"; LGR <- "#E0E0E0"; DGR <- "#666666"

  # ── Layout constants (npc = 0-1, 0=bottom-left) ───────────────────────────
  ML  <- 0.046           # left margin
  MR  <- 0.954           # right margin
  CW  <- MR - ML         # content width
  TOP <- 0.953           # content top (just below header)
  BOT <- 0.048           # content bottom (just above footer)

  # ── State: y-cursor measured downward from TOP ────────────────────────────
  page_num <- 0L
  cur_y    <- 0.0        # distance below TOP (npc)
  max_y    <- TOP - BOT  # max usable depth per page

  ycur  <- function() TOP - cur_y
  move  <- function(d) cur_y <<- cur_y + d

  out_col <- function(o) switch(toupper(o %||% ""),
    RED=RC, AMBER=AC, GREEN=GC, UNCERTAIN=BLUE, BLUE)
  out_bg <- function(o) switch(toupper(o %||% ""),
    RED=RBG, AMBER=ABG, GREEN=GBG, GREY)

  # ── AIRA visual styling ──────────────────────────────────────────────────
  # After the 2026-04-28 redesign, the AI banner uses a single neutral
  # colour scheme regardless of risk level - matching the onscreen
  # design. The rule-engine RAG bar carries the verdict signal; the AI
  # panel is observational and should not prime reviewers with
  # red/amber/green semantics. Helpers retained as functions (rather
  # than collapsed to constants) so future audits of risk-driven
  # styling have a single point to inspect.
  aira_col <- function(rl) DARK   # neutral border / pill background
  aira_bg  <- function(rl) "#F4F7FB"  # neutral panel fill

  # Map a risk level to its observation-style label. Mirrors
  # .disclosure_risk_label in 26_server.R - kept in sync manually since
  # the two files cannot share helpers (PDF writer runs synchronously
  # at report-generation time, not in the server reactive context).
  aira_label <- function(rl) {
    switch(toupper(rl %||% "UNCERTAIN"),
      "LOW"          = "Minor observations",
      "MEDIUM"       = "Notable observations",
      "HIGH"         = "Significant observations",
      "INSUFFICIENT" = "Insufficient evidence",
                       "Unable to assess")
  }

  # ── Safe JSON parse for disclosure_review responses (v6, v2, v1) ─────────
  # Shape-detects like render_aira_review_banner in 26_server.R:
  #   - v6:  has engine_alignment (current shape - dataset_recognition,
  #          structure_summary, anomalies, engine_alignment, reviewer_focus)
  #   - v2:  risk_level + assessment + reasoning + (concerns|blind_spots)
  #   - v1:  risk_level + assessment + reasoning
  # Returns list(parsed=<list>|NULL, err=<string>|NULL). Never throws.
  # parsed$shape is "v6" or "v2"/"v1" so the renderer can choose layout.
  .parse_aira_review_json <- function(text) {
    if (!is.character(text) || length(text) != 1L || !nzchar(text))
      return(list(parsed = NULL, err = "empty text"))
    if (!requireNamespace("jsonlite", quietly = TRUE))
      return(list(parsed = NULL, err = "jsonlite not available"))
    parsed <- tryCatch(
      jsonlite::fromJSON(text, simplifyVector = FALSE),
      error = function(e) NULL)
    if (is.null(parsed) || !is.list(parsed))
      return(list(parsed = NULL, err = "not valid JSON"))

    # Shape detection. v6 has engine_alignment; v3/v4/v5 have risk_level
    # + assessment + reasoning. Anything else is genuinely malformed.
    is_v6 <- is.list(parsed$engine_alignment)
    is_v5_or_earlier <- !is.null(parsed$risk_level) &&
                        !is.null(parsed$assessment) &&
                        !is.null(parsed$reasoning)
    if (!is_v6 && !is_v5_or_earlier)
      return(list(parsed = NULL, err = "JSON missing required keys"))

    if (is_v6) {
      # engine_alignment.agrees -> yes|no|cannot_assess
      ea <- parsed$engine_alignment
      agrees <- tolower(as.character(ea$agrees %||% "")[1L])
      if (!agrees %in% c("yes","no","cannot_assess")) agrees <- "cannot_assess"
      parsed$engine_alignment$agrees <- agrees

      # anomalies -> list of {column, observation}; tolerate string-lists
      anomalies <- parsed$anomalies
      if (is.null(anomalies) || !is.list(anomalies)) {
        anomalies <- list()
      } else {
        anomalies <- lapply(anomalies, function(a) {
          if (is.list(a))
            list(column = a$column %||% NA_character_,
                 observation = as.character(a$observation %||% "")[1L])
          else
            list(column = NA_character_, observation = as.character(a)[1L])
        })
        anomalies <- Filter(function(a) nzchar(a$observation), anomalies)
      }
      parsed$anomalies <- anomalies

      # dataset_recognition - default to not-recognised if shape is off
      dr <- parsed$dataset_recognition
      if (!is.list(dr) || !isTRUE(dr$recognised)) {
        parsed$dataset_recognition <- list(recognised = FALSE,
          name = NULL, confidence = NULL, evidence = NULL)
      }

      parsed$shape <- "v6"
      return(list(parsed = parsed, err = NULL))
    }

    # v1/v2 path (unchanged shape, preserved for rollback / cached reviews)
    rl <- toupper(as.character(parsed$risk_level)[1L])
    if (!rl %in% c("LOW","MEDIUM","HIGH","UNCERTAIN","INSUFFICIENT"))
      rl <- "UNCERTAIN"
    parsed$risk_level <- rl
    parsed$is_v2 <- !is.null(parsed$concerns) ||
                    !is.null(parsed$blind_spots) ||
                    !is.null(parsed$reviewer_focus)
    if (isTRUE(parsed$is_v2)) {
      if (!is.null(parsed$concerns) && !is.list(parsed$concerns))
        parsed$concerns <- as.list(as.character(parsed$concerns))
      if (!is.null(parsed$blind_spots) && !is.list(parsed$blind_spots))
        parsed$blind_spots <- as.list(as.character(parsed$blind_spots))
    }
    parsed$shape <- "v2"
    list(parsed = parsed, err = NULL)
  }

  # v6 engine-alignment label (mirrors .disclosure_alignment_label).
  .aira_alignment_label <- function(agrees) {
    switch(tolower(agrees %||% "cannot_assess"),
      "yes"           = "AI agrees with engine",
      "no"            = "AI disagrees with engine",
      "cannot_assess" = "AI cannot assess",
                        "AI cannot assess")
  }
  # v6 confidence label (mirrors .disclosure_recognition_confidence_label).
  .aira_confidence_label <- function(conf) {
    switch(tolower(conf %||% ""),
      "high"   = "high confidence",
      "medium" = "medium confidence",
      "low"    = "low confidence",
                 "unspecified confidence")
  }

  # Map a column-classification category string to display label + colour.
  # Categories rendered: direct_id, quasi_id, sensitive, unknown.
  # "non_identifying" rows are dropped by the caller (per user request -
  # reviewers only need details of columns with some element of risk).
  .aira_col_cat_display <- function(cat) {
    cat <- tolower(as.character(cat %||% "unknown")[1L])
    switch(cat,
      direct_id       = list(label="DIRECT ID",       col=RC),
      quasi_id        = list(label="QUASI-ID",        col=AC),
      sensitive       = list(label="SENSITIVE",       col="#7B1FA2"),
      non_identifying = list(label="NON-IDENTIFYING", col=GC),
      list(label="UNKNOWN", col=DGR))
  }

  # Compact one-line note shown for non-ok AIRA responses (timeout,
  # unavailable, disabled, malformed). Keeps the report honest about
  # what was attempted without cluttering it with full banners.
  .draw_aira_compact_note <- function(resp) {
    status <- resp$status %||% ""
    label <- switch(status,
      "timeout"     = "AI review: timed out",
      "unavailable" = "AI review: unavailable",
      "disabled"    = "AI review: disabled",
      "malformed"   = "AI review: malformed output",
      "AI review: unknown status")
    reason <- resp$reason %||% ""
    txt <- if (nzchar(reason))
      paste0("\u24d8  ", label, " \u00b7 ", substr(reason, 1L, 80L))
    else
      paste0("\u24d8  ", label)
    draw_para(txt, cex=.66, col=DGR, face="italic", indent=.006)
    move(.002)
  }

  # Per-file AIRA disclosure-review block. Rendered after rule cards and
  # before the reviewer decision strip. Handles three cases:
  #   1. ok + parseable JSON -> full structured block (v1 or v2 layout)
  #   2. ok + unparseable JSON -> header + raw text in muted block
  #   3. non-ok status -> compact one-liner (timeout/unavailable/etc)
  #   4. status == "skipped_parse_only" -> "AI review not applicable"
  .draw_aira_review_block <- function(resp) {
    if (is.null(resp)) return()
    status <- resp$status %||% ""

    # New skipped state from 2026-04-28: file content was unreadable
    # so AI dispatch was skipped. Show a brief muted note.
    if (identical(status, "skipped_parse_only")) {
      hh <- .024
      check_space(hh + .004)
      grid.rect(x=ML+CW/2, y=ycur()-hh/2, width=CW, height=hh,
                gp=gpar(fill="#FAFAFA", col=LGR, lwd=.4))
      grid.rect(x=ML+.003, y=ycur()-hh/2, width=.007, height=hh,
                gp=gpar(fill="#ECEFF1", col=NA))
      grid.text("AI OBSERVATIONS",
                x=ML+.015, y=ycur()-hh/2, just="left",
                gp=gpar(cex=.72, fontface="bold", col=DGR))
      move(hh)
      draw_para(
        "AI review not applicable for this file: rule engine could not read its content.",
        cex=.66, col=DGR, face="italic", indent=.006)
      move(.006)
      return()
    }

    if (!identical(status, "ok") && !identical(status, "malformed")) {
      .draw_aira_compact_note(resp)
      return()
    }

    # Header strip - matches onscreen banner styling. Neutral palette
    # (DARK navy on light blue-grey) regardless of risk level.
    hh <- .024
    check_space(hh + .004)
    grid.rect(x=ML+CW/2, y=ycur()-hh/2, width=CW, height=hh,
              gp=gpar(fill=aira_bg(NULL), col=DARK, lwd=.5))
    grid.rect(x=ML+.003, y=ycur()-hh/2, width=.007, height=hh,
              gp=gpar(fill=DARK, col=NA))
    grid.text("AI OBSERVATIONS",
              x=ML+.015, y=ycur()-hh/2, just="left",
              gp=gpar(cex=.72, fontface="bold", col=DARK))

    parsed <- .parse_aira_review_json(resp$text %||% "")

    if (!is.null(parsed$parsed) && identical(parsed$parsed$shape, "v6")) {
      # ── v6 layout ────────────────────────────────────────────────────
      # Engine-alignment pill (not a risk pill); structure_summary as the
      # factual lead line; dataset_recognition callout when claimed;
      # anomalies list; engine_alignment rationale; reviewer_focus.
      p  <- parsed$parsed
      ea <- p$engine_alignment %||% list()
      pill_label <- .aira_alignment_label(ea$agrees %||% "cannot_assess")
      pw <- .16
      grid.rect(x=MR-pw/2-.004, y=ycur()-hh/2, width=pw, height=hh*.7,
                gp=gpar(fill="#E2EAF0", col=NA))
      grid.text(pill_label,
                x=MR-pw/2-.004, y=ycur()-hh/2, just="centre",
                gp=gpar(cex=.56, fontface="bold", col=DARK))
      move(hh)

      # structure_summary - factual lead line
      ss <- as.character(p$structure_summary %||% "")[1L]
      if (nzchar(ss))
        draw_para(ss, cex=.74, col="black", face="bold", indent=.006)

      # dataset_recognition callout (only when AI claims recognition)
      dr <- p$dataset_recognition %||% list()
      if (isTRUE(dr$recognised)) {
        nm   <- as.character(dr$name %||% "(unnamed dataset)")[1L]
        conf <- .aira_confidence_label(dr$confidence)
        evid <- as.character(dr$evidence %||% "")[1L]
        move(.003)
        draw_para(sprintf("\u2605 Recognised: %s (%s)", nm, conf),
                  cex=.68, col=DARK, face="bold", indent=.006)
        if (nzchar(evid))
          draw_para(evid, cex=.64, col=DGR, face="italic", indent=.012)
      }

      # anomalies (often empty - render nothing when so)
      anomalies <- p$anomalies %||% list()
      if (is.list(anomalies) && length(anomalies) > 0L) {
        move(.003)
        draw_para(sprintf("Anomalies (%d)", length(anomalies)),
                  cex=.62, col=DARK, face="bold", indent=.006)
        for (a in anomalies) {
          col <- a$column
          col <- if (is.null(col) || (length(col)==1L && is.na(col)) ||
                     !nzchar(as.character(col)[1L])) NULL
                 else as.character(col)[1L]
          obs <- as.character(a$observation %||% "")[1L]
          line <- if (!is.null(col)) sprintf("- %s: %s", col, obs)
                  else sprintf("- %s", obs)
          draw_para(line, cex=.66, col="#333333", indent=.012)
        }
      }

      # engine_alignment rationale - explanatory paragraph
      rationale <- as.character(ea$rationale %||% "")[1L]
      if (nzchar(rationale)) {
        move(.003)
        draw_para(rationale, cex=.70, col="#333333", indent=.006)
      }

      # reviewer_focus
      rf <- as.character(p$reviewer_focus %||% "")[1L]
      if (nzchar(rf)) {
        move(.003)
        draw_para("SUGGESTED REVIEWER FOCUS", cex=.62, col=DARK,
                  face="bold", indent=.006)
        draw_para(rf, cex=.68, col="#222222", indent=.012)
      }

      # Footer advisory
      move(.002)
      draw_para(
        "Advisory only \u2014 the rule-engine classification remains the authoritative check.",
        cex=.60, col=DGR, face="italic", indent=.006)
      move(.006)
      return(invisible())
    }

    if (!is.null(parsed$parsed)) {
      # Observation pill on the right of the header. Single neutral
      # colour scheme. Pill width sized to fit the longest label
      # ("Significant observations" / "Insufficient evidence").
      p <- parsed$parsed
      rl <- p$risk_level
      pill_label <- aira_label(rl)
      pw <- .14
      grid.rect(x=MR-pw/2-.004, y=ycur()-hh/2, width=pw, height=hh*.7,
                gp=gpar(fill="#E2EAF0", col=NA))
      grid.text(pill_label,
                x=MR-pw/2-.004, y=ycur()-hh/2, just="centre",
                gp=gpar(cex=.58, fontface="bold", col=DARK))
      move(hh)

      # Assessment (1-2 sentences, bold)
      assessment <- as.character(p$assessment %||% "")
      if (nzchar(assessment))
        draw_para(assessment, cex=.74, col="black", face="bold", indent=.006)

      # ── v2-specific sections ──────────────────────────────────────────
      # Only render when v2 is detected. v1 responses go straight from
      # assessment to reasoning to column classifications (preserving
      # the original v1 layout for any cached v1 responses or rollback).
      if (isTRUE(p$is_v2)) {

        # Concerns: array of flag names with optional explanations.
        # Render as a section with small label header and flag entries.
        conc <- p$concerns
        if (is.list(conc) && length(conc) > 0L) {
          ce <- p$concern_explanations
          if (!is.list(ce)) ce <- list()
          move(.003)
          draw_para("CONCERNS", cex=.62, col=DARK, face="bold", indent=.006)
          for (flag in conc) {
            flag_name <- as.character(flag)[1L]
            if (!nzchar(flag_name)) next
            explanation <- as.character(ce[[flag_name]] %||% "")[1L]
            line <- if (nzchar(explanation))
              sprintf("- %s - %s", flag_name, explanation)
            else
              sprintf("- %s", flag_name)
            draw_para(line, cex=.66, col="#333333", indent=.012)
          }
        }

        # Blind spots: things the AI could not evaluate. Muted italic.
        bs <- p$blind_spots
        if (is.list(bs) && length(bs) > 0L) {
          move(.003)
          draw_para("NOT EVALUATED BY AI", cex=.62, col=DARK,
                    face="bold", indent=.006)
          for (s in bs) {
            t <- as.character(s)[1L]
            if (!nzchar(t)) next
            draw_para(sprintf("- %s", t), cex=.64, col=DGR,
                      face="italic", indent=.012)
          }
        }

        # Reviewer focus: prose paragraph in a highlighted block.
        rf <- as.character(p$reviewer_focus %||% "")[1L]
        if (nzchar(rf)) {
          move(.003)
          draw_para("SUGGESTED REVIEWER FOCUS", cex=.62, col=DARK,
                    face="bold", indent=.006)
          draw_para(rf, cex=.68, col="#222222", indent=.012)
        }
      }

      # Reasoning (paragraph - both v1 and v2 have this field)
      reasoning <- as.character(p$reasoning %||% "")
      if (nzchar(reasoning)) {
        move(.003)
        draw_para(reasoning, cex=.70, col="#333333", indent=.006)
      }

      # Column classifications - filter to risky only (direct_id,
      # quasi_id, sensitive, unknown). non_identifying columns dropped
      # per user request: reviewers only want details of columns with
      # some element of risk.
      cc <- p$column_classifications
      if (is.list(cc) && length(cc) > 0L) {
        risky <- list()
        n_nonid <- 0L
        for (cn in names(cc)) {
          cat <- tolower(as.character(cc[[cn]])[1L] %||% "unknown")
          if (identical(cat, "non_identifying")) {
            n_nonid <- n_nonid + 1L
          } else {
            risky[[length(risky) + 1L]] <- list(name = cn, cat = cat)
          }
        }

        if (length(risky) > 0L) {
          move(.002)
          hdr_txt <- sprintf("Columns with potential risk (%d of %d):",
                             length(risky), length(cc))
          draw_para(hdr_txt, cex=.68, col=BLUE, face="bold", indent=.006)

          # Group by category for a more scannable layout, in severity order
          cat_order <- c("direct_id", "quasi_id", "sensitive", "unknown")
          for (cat in cat_order) {
            grp <- Filter(function(x) identical(x$cat, cat), risky)
            if (length(grp) == 0L) next
            d <- .aira_col_cat_display(cat)
            # Category header line
            hl <- .016
            check_space(hl)
            grid.rect(x=ML+.012, y=ycur()-hl/2,
                      width=.015, height=hl*.7,
                      gp=gpar(fill=d$col, col=NA))
            grid.text(d$label,
                      x=ML+.032, y=ycur()-hl/2, just="left",
                      gp=gpar(cex=.62, fontface="bold", col=d$col))
            # Inline column names after the tag
            names_txt <- paste(vapply(grp, function(x)
              substr(x$name, 1L, 40L), character(1)),
              collapse=", ")
            grid.text(names_txt,
                      x=ML+.095, y=ycur()-hl/2, just="left",
                      gp=gpar(cex=.64, fontfamily="mono", col="black"))
            move(hl)
          }

          if (n_nonid > 0L) {
            draw_para(sprintf("(%d column(s) classified as non-identifying - not shown)",
                              n_nonid),
                      cex=.60, col=DGR, face="italic", indent=.012)
          }
        } else if (n_nonid > 0L) {
          # All columns classified non_identifying
          draw_para(sprintf("All %d column(s) classified as non-identifying by AI.",
                            n_nonid),
                    cex=.68, col=GC, face="italic", indent=.006)
        }
      }

      # Footer advisory
      move(.002)
      draw_para(
        "Advisory only \u2014 the rule-engine classification remains the authoritative check.",
        cex=.60, col=DGR, face="italic", indent=.006)

    } else {
      # Parse failed or status == "malformed". Show the raw text in a
      # muted block. Reviewer can still extract meaning from prose.
      grid.text("Unstructured",
                x=MR-.050, y=ycur()-hh/2, just="centre",
                gp=gpar(cex=.62, fontface="bold", col=DGR))
      move(hh)
      draw_para(
        "AI did not return structured output; showing raw response.",
        cex=.64, col=DGR, face="italic", indent=.006)
      raw_txt <- substr(as.character(resp$text %||% ""), 1L, 1500L)
      if (nzchar(raw_txt))
        draw_para(raw_txt, cex=.66, col="#333333", indent=.008)
      move(.002)
      draw_para(
        "Advisory only \u2014 the rule-engine classification remains the authoritative check.",
        cex=.60, col=DGR, face="italic", indent=.006)
    }
    move(.006)
  }

  # Batch AIRA summary block, shown on the cover page. Compact - just
  # a header strip and the summary text (free-form prose from AIRA,
  # 1-5 sentences). Only renders when resp$status == "ok" with non-
  # empty text; non-ok statuses render the compact one-liner.
  .draw_aira_batch_block <- function(resp) {
    if (is.null(resp)) return()
    status <- resp$status %||% ""

    if (!identical(status, "ok")) {
      .draw_aira_compact_note(resp)
      return()
    }
    txt <- trimws(as.character(resp$text %||% ""))
    if (!nzchar(txt)) return()

    # Header strip
    hh <- .024
    check_space(hh + .004)
    grid.rect(x=ML+CW/2, y=ycur()-hh/2, width=CW, height=hh,
              gp=gpar(fill="#F5F8FC", col=BLUE, lwd=.5))
    grid.rect(x=ML+.003, y=ycur()-hh/2, width=.007, height=hh,
              gp=gpar(fill=BLUE, col=NA))
    grid.text("AI BATCH SUMMARY",
              x=ML+.015, y=ycur()-hh/2, just="left",
              gp=gpar(cex=.72, fontface="bold", col=DARK))
    move(hh)

    draw_para(substr(txt, 1L, 2000L), cex=.72, col="black", indent=.006)
    move(.002)
    draw_para(
      "Advisory only \u2014 the rule-engine classification remains the authoritative check.",
      cex=.60, col=DGR, face="italic", indent=.006)
    move(.006)
  }

  # ── Page chrome ────────────────────────────────────────────────────────────
  new_page <- function() {
    grid.newpage()
    page_num <<- page_num + 1L
    cur_y    <<- 0.0
    # Header
    grid.rect(x=.5, y=.980, width=1, height=.040,
              gp=gpar(fill=DARK, col=NA))
    grid.text("Aridhia Airlock Checker \u2014 Airlock Review Report  |  CONFIDENTIAL",
              x=ML+.005, y=.980, just="left",
              gp=gpar(col="white", fontface="bold", cex=.72))
    grid.text(format(Sys.time(), "%Y-%m-%d %H:%M"),
              x=MR-.005, y=.980, just="right",
              gp=gpar(col="white", cex=.65))
    # Left accent
    grid.rect(x=.012, y=.5, width=.022, height=1,
              gp=gpar(fill=BLUE, col=NA))
    # Footer
    grid.rect(x=.5, y=.022, width=1, height=.036,
              gp=gpar(fill=GREY, col=NA))
    grid.text(paste0("Page ", page_num),
              x=MR-.005, y=.022, just="right",
              gp=gpar(col=DGR, cex=.60))
    grid.text("Airlock Checker  |  Aridhia Informatics",
              x=ML+.030, y=.022, just="left",
              gp=gpar(col=DGR, cex=.60))
  }

  check_space <- function(needed) {
    if (!is.numeric(needed) || is.na(needed)) return()
    if (cur_y + needed > max_y) new_page()
  }

  # ── Section header ─────────────────────────────────────────────────────────
  section_hdr <- function(title, level=1) {
    h <- if (level==1) .030 else .023
    check_space(h + .006)
    if (level==1) {
      grid.rect(x=.5, y=ycur()-h/2, width=1, height=h,
                gp=gpar(fill=DARK, col=NA))
      grid.text(title, x=ML+.008, y=ycur()-h/2, just="left",
                gp=gpar(col="white", fontface="bold", cex=.92))
    } else {
      grid.lines(x=c(ML,MR), y=c(ycur()-.001, ycur()-.001),
                 gp=gpar(col=BLUE, lwd=1.5))
      grid.text(title, x=ML+.003, y=ycur()-h/2+.002, just="left",
                gp=gpar(col=DARK, fontface="bold", cex=.83))
    }
    move(h + .005)
  }

  # ── Wrapped text block ─────────────────────────────────────────────────────
  draw_para <- function(text, cex=.76, col="black", face="plain",
                         indent=0) {
    text <- if (is.null(text) || length(text) == 0) "" else as.character(text)
    if (is.na(text)) text <- ""
    if (!nzchar(trimws(text))) return()
    chars <- max(40L, floor((CW - indent) * 145))
    lns   <- strwrap(text, width=chars)
    lh    <- .0185 * (cex/.76)
    check_space(length(lns) * lh + .002)
    for (ln in lns) {
      grid.text(ln, x=ML+indent+.003, y=ycur()-lh/2, just="left",
                gp=gpar(cex=cex, col=col, fontface=face))
      move(lh)
    }
  }

  # ── Compact table ──────────────────────────────────────────────────────────
  # rows: list of lists; widths: npc widths summing to CW
  draw_table <- function(headers, rows, widths=NULL, rh=.0195,
                          hdr_col=DARK, cex=.70) {
    nc <- length(headers)
    if (is.null(widths)) widths <- rep(CW/nc, nc)
    total_h <- rh * (1 + length(rows))
    check_space(total_h + .004)

    # Header
    grid.rect(x=ML+CW/2, y=ycur()-rh/2, width=CW, height=rh,
              gp=gpar(fill=hdr_col, col=LGR, lwd=.3))
    xc <- ML
    for (ci in seq_along(headers)) {
      grid.text(as.character(headers[[ci]]),
                x=xc+.004, y=ycur()-rh/2, just="left",
                gp=gpar(col="white", fontface="bold", cex=cex))
      xc <- xc + widths[ci]
    }
    move(rh)

    # Rows
    for (ri in seq_along(rows)) {
      bg <- if (ri %% 2 == 0) GREY else "white"
      grid.rect(x=ML+CW/2, y=ycur()-rh/2, width=CW, height=rh,
                gp=gpar(fill=bg, col=LGR, lwd=.2))
      xc <- ML
      for (ci in seq_along(rows[[ri]])) {
        raw <- as.character(rows[[ri]][[ci]] %||% "")
        if (length(raw) == 0 || is.na(raw)) raw <- ""
        # Clip to column width
        max_c <- max(4L, floor((widths[ci]-.010) * 145 * cex/.70))
        if (is.na(max_c)) max_c <- 40L
        val   <- if (nchar(raw) > max_c)
                   paste0(substr(raw, 1, max_c-1), "\u2026") else raw
        grid.text(val, x=xc+.004, y=ycur()-rh/2, just="left",
                  gp=gpar(cex=cex, col="black"))
        xc <- xc + widths[ci]
      }
      move(rh)
    }
    move(.004)
  }

  # ── Evidence block (table or lines) ───────────────────────────────────────
  draw_evidence <- function(ev) {
    if (is.null(ev)) return()
    cap <- ev$caption %||% ""
    if (nzchar(cap)) draw_para(cap, cex=.67, col=DGR, face="italic", indent=.010)

    if (identical(ev$type,"table") && !is.null(ev$data) && is.data.frame(ev$data)) {
      df  <- ev$data
      # Sanitise: convert integer64 and truncate cell values
      df  <- as.data.frame(lapply(df, function(col) {
        if (inherits(col, "integer64")) as.character(col)
        else substr(as.character(col), 1, 60)
      }), stringsAsFactors=FALSE, check.names=FALSE)
      nc  <- ncol(df)
      if (nc == 0) return()

      # ── Wide-table handling ────────────────────────────────────────────
      # Base-R grid tables don't wrap or paginate columns. Beyond a small
      # number of columns the cells overlap into an illegible smear. Cap at
      # the number that renders legibly, always keeping the flagged column
      # (the one that triggered the rule) visible, and note the remainder.
      MAX_COLS <- 7L
      flag_cols <- as.character(ev$flag_cols %||% character(0))
      n_hidden  <- 0L
      if (nc > MAX_COLS) {
        all_names <- names(df)
        # Always keep flagged columns; fill the rest with leading columns
        # (which for ACRO/pivot evidence carry the row labels), preserving
        # original left-to-right order in the final display.
        keep <- intersect(flag_cols, all_names)
        remaining <- setdiff(all_names, keep)
        fill_n <- max(0L, MAX_COLS - length(keep))
        keep <- c(keep, head(remaining, fill_n))
        keep <- all_names[all_names %in% keep]   # restore original order
        n_hidden <- nc - length(keep)
        df <- df[, keep, drop=FALSE]
        nc <- ncol(df)
      }

      col_w <- rep(CW/nc, nc)
      ev_rows <- lapply(seq_len(min(nrow(df), 8)), function(i)
        as.list(df[i, , drop=FALSE]))
      draw_table(as.list(names(df)), ev_rows,
                 widths=col_w, rh=.0170, hdr_col=BLUE, cex=.66)
      if (n_hidden > 0L)
        draw_para(
          sprintf("(+%d more column(s) not shown - table too wide for report; %s)",
                  n_hidden,
                  if (length(intersect(flag_cols, names(df))) > 0L)
                    "flagged column(s) retained"
                  else "see source file for full structure"),
          cex=.60, col=DGR, face="italic", indent=.012)

    } else if (identical(ev$type,"lines") && !is.null(ev$lines)) {
      lh <- .0160
      for (ln in head(ev$lines, 10)) {
        flag   <- isTRUE(ln$flag)
        lineno <- formatC(as.integer(ln$lineno %||% 0), width=4)
        txt    <- substr(as.character(ln$text %||% ""), 1, 110)
        check_space(lh)
        grid.rect(x=ML+CW/2, y=ycur()-lh/2, width=CW, height=lh,
                  gp=gpar(fill=if(flag)"#FFF9C4" else "white", col=LGR, lwd=.15))
        grid.text(paste0(lineno, "  ", txt),
                  x=ML+.005, y=ycur()-lh/2, just="left",
                  gp=gpar(cex=.62, fontfamily="mono", col="black"))
        move(lh)
      }
    }
  }

  # ── Rule card (one hit) ────────────────────────────────────────────────────
  draw_rule_card <- function(h) {
    outcome <- toupper(h$outcome %||% "")
    cc  <- out_col(outcome); cb <- out_bg(outcome)
    rid <- h$rule %||% ""
    rd  <- RULES[[gsub("-","",rid)]]
    lbl <- rd$label %||% rid
    chk <- rd$check %||% ""
    det <- h$detail %||% ""
    rem <- get_remediation(rid, det) %||% ""
    ev  <- h$evidence

    # Card header
    hh <- .026
    check_space(hh + .004)
    grid.rect(x=ML+CW/2, y=ycur()-hh/2, width=CW, height=hh,
              gp=gpar(fill=cb, col=cc, lwd=.5))
    grid.rect(x=ML+.003, y=ycur()-hh/2, width=.007, height=hh,
              gp=gpar(fill=cc, col=NA))
    grid.text(paste0("[", outcome, "]  ", rid, "  \u2014  ", lbl),
              x=ML+.015, y=ycur()-hh/2, just="left",
              gp=gpar(cex=.80, fontface="bold", col=cc))
    move(hh)

    # Tests / Finding / Evidence / Remediation
    if (nzchar(chk))
      draw_para(paste0("Tests: ", chk), cex=.69, col=DGR,
                face="italic", indent=.008)
    if (nzchar(det))
      draw_para(paste0("Finding: ", det), cex=.74, indent=.008)
    if (!is.null(ev)) {
      draw_para("Evidence:", cex=.71, col=BLUE, face="bold", indent=.008)
      draw_evidence(ev)
    }
    if (nzchar(rem))
      draw_para(paste0("\U0001F527 Remediation: ", rem),
                cex=.70, col=GC, indent=.008)

    # Card bottom rule
    move(.004)
    check_space(.002)
    grid.lines(x=c(ML,MR), y=c(ycur(),ycur()),
               gp=gpar(col=LGR, lwd=.5))
    move(.004)
  }

  # ── Compute batch score if not supplied ────────────────────────────────────
  if (is.null(batch_score)) {
    batch_score <- tryCatch(
      calculate_batch_score(res,
        check_linkage_risk(res, min_categories=3L, min_shared_files=2L)),
      error=function(e) NULL)
  }

  # ── Decision integrity (for the four-eyes second reviewer) ─────────────────
  # The second reviewer checks the first reviewer's completed work before
  # egress is authorised. They need, at a glance: is every file decided
  # (review complete?), how many approved / rejected / pending, and - the
  # four-eyes scrutiny trigger - how many files were APPROVED DESPITE a flag
  # (a RED classification or an ACRO fail/review hit that the first reviewer
  # nonetheless cleared). Those are the decisions warranting a second look.
  #
  # A decision counts as "approve" if its recorded outcome is GREEN/AMBER
  # (egress permitted) rather than RED (rejected). "approved-despite-flag"
  # is an approve decision on a file whose original classification was RED,
  # or whose hits include an ACRO fail (ACR-001) or ACRO review (ACR-002).
  .compute_decision_integrity <- function(res, decisions) {
    n_total   <- length(res)
    n_decided <- 0L; n_approved <- 0L; n_rejected <- 0L; n_pending <- 0L
    despite   <- list()   # files approved despite a flag

    for (f in res) {
      cls <- toupper(f$classification %||% "")
      dec <- decisions[[f$file]] %||% list()
      out <- toupper(as.character(dec$outcome %||% "")[1L])

      if (!nzchar(out)) { n_pending <- n_pending + 1L; next }
      n_decided <- n_decided + 1L

      is_reject <- out == "RED"
      if (is_reject) { n_rejected <- n_rejected + 1L }
      else           { n_approved <- n_approved + 1L }

      # Approved-despite-flag: an approve decision on a flagged file.
      if (!is_reject) {
        hit_rules <- vapply(f$hits %||% list(),
          function(h) toupper(h$rule %||% ""), character(1))
        acro_flag <- any(hit_rules %in% c("ACR-001","ACR-002"))
        was_red   <- cls == "RED"
        if (was_red || acro_flag) {
          despite[[length(despite) + 1L]] <- list(
            file = basename(f$file %||% ""),
            orig = cls,
            acro = acro_flag,
            outcome = out)
        }
      }
    }

    list(
      n_total          = n_total,
      n_decided        = n_decided,
      n_approved       = n_approved,
      n_rejected       = n_rejected,
      n_pending        = n_pending,
      complete         = (n_pending == 0L && n_total > 0L),
      despite_flag     = despite,
      n_despite_flag   = length(despite))
  }
  di <- tryCatch(.compute_decision_integrity(res, decisions),
                 error = function(e) NULL)

  # ── Open device ────────────────────────────────────────────────────────────
  grDevices::pdf(out_path, width=8.27, height=11.69,
                 title="Airlock Checker - Egress Review Report")
  on.exit(grDevices::dev.off(), add=TRUE)

  # ══════════════════════════════════════════════════════════════════════════
  # PAGE 1 - COVER
  # ══════════════════════════════════════════════════════════════════════════
  new_page()

  # Title block
  grid.text("Airlock Egress Review Report",
            x=ML+.005, y=ycur()-.024, just="left",
            gp=gpar(fontface="bold", cex=1.38, col=DARK))
  move(.030)
  grid.text("Airlock Checker \u2014 Automated Disclosure Assessment",
            x=ML+.005, y=ycur()-.014, just="left",
            gp=gpar(cex=.78, col=BLUE))
  move(.020)
  # Metadata row
  meta <- list(
    c("Timestamp",    format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    c("Source folder",file_dir),
    c("App version",  "v2 2026-03-31")
  )
  for (m in meta) {
    grid.text(m[1], x=ML+.005, y=ycur()-.009, just="left",
              gp=gpar(cex=.68, fontface="bold", col=DARK))
    grid.text(m[2], x=ML+.180, y=ycur()-.009, just="left",
              gp=gpar(cex=.68, fontfamily="mono"))
    move(.016)
  }
  grid.lines(x=c(ML,MR), y=c(ycur()-.002,ycur()-.002),
             gp=gpar(col=BLUE, lwd=1.5))
  move(.010)

  # ── Decision integrity band (four-eyes second-reviewer headline) ───────────
  # Replaces the former oversized risk-score card. The second reviewer's
  # first question is "is this review complete and sound", not "what's the
  # score", so the headline is the completion state and the decision counts,
  # with the approved-despite-flag count set apart as the scrutiny trigger.
  # The automated risk score is demoted to a small figure in the stats row.
  sc <- batch_score
  if (!is.null(di)) {
    complete  <- isTRUE(di$complete)
    band_col  <- if (!complete) AC else if (di$n_despite_flag > 0L) AC else GC
    band_bg   <- if (!complete) ABG else if (di$n_despite_flag > 0L) ABG else GBG

    # ── Headline: completion state ──
    band_h <- .040
    check_space(band_h + .004)
    grid.rect(x=ML+CW/2, y=ycur()-band_h/2, width=CW, height=band_h,
              gp=gpar(fill=band_bg, col=band_col, lwd=.9))
    grid.rect(x=ML+.003, y=ycur()-band_h/2, width=.008, height=band_h,
              gp=gpar(fill=band_col, col=NA))
    headline <- if (complete)
      sprintf("Review complete \u2014 %d of %d decisions recorded",
              di$n_decided, di$n_total)
    else
      sprintf("Review incomplete \u2014 %d of %d decisions pending; NOT ready for egress",
              di$n_pending, di$n_total)
    grid.text(headline, x=ML+.018, y=ycur()-band_h*.36, just="left",
              gp=gpar(cex=.92, fontface="bold", col=band_col))
    sub <- if (di$n_despite_flag > 0L)
      sprintf("%d approval(s) override a flag \u2014 second-review scrutiny required",
              di$n_despite_flag)
    else if (complete)
      "No approvals override a flag"
    else
      "Complete all decisions before second-review sign-off"
    grid.text(sub, x=ML+.018, y=ycur()-band_h*.74, just="left",
              gp=gpar(cex=.68, col=DGR))
    move(band_h + .008)

    # ── Decision-count tiles ──
    tiles <- list(
      list("APPROVED",  as.character(di$n_approved), GC,  "egress permitted"),
      list("REJECTED",  as.character(di$n_rejected), RC,  "egress refused"),
      list("PENDING",   as.character(di$n_pending),
           if (di$n_pending > 0L) AC else DGR, "no decision yet"),
      list("APPROVED DESPITE FLAG", as.character(di$n_despite_flag),
           if (di$n_despite_flag > 0L) AC else GC, "first-reviewer overrides")
    )
    tile_h <- .064; gap <- .006
    ntile <- length(tiles); tw <- (CW - gap*(ntile-1)) / ntile
    check_space(tile_h + .004)
    base_y <- ycur()
    for (i in seq_along(tiles)) {
      ti <- tiles[[i]]
      tx <- ML + (i-1)*(tw+gap)
      # Emphasise the despite-flag tile with a heavier border when non-zero
      is_trigger <- (i == 4L && di$n_despite_flag > 0L)
      grid.rect(x=tx+tw/2, y=base_y-tile_h/2, width=tw, height=tile_h,
                gp=gpar(fill=if(is_trigger) ABG else GREY,
                        col=ti[[3]], lwd=if(is_trigger) 1.1 else .4))
      grid.text(ti[[1]], x=tx+.006, y=base_y-.012, just="left",
                gp=gpar(cex=.52, fontface="bold", col=DGR))
      grid.text(ti[[2]], x=tx+.006, y=base_y-.036, just="left",
                gp=gpar(cex=1.30, fontface="bold", col=ti[[3]]))
      grid.text(ti[[4]], x=tx+.006, y=base_y-.056, just="left",
                gp=gpar(cex=.50, col=DGR))
    }
    move(tile_h + .010)

    # ── Approved-despite-flag detail (only when present) ──
    if (di$n_despite_flag > 0L) {
      draw_para("Files approved despite a flag (review these first):",
                cex=.68, col=AC, face="bold", indent=.004)
      for (d in di$despite_flag) {
        why <- if (isTRUE(d$acro) && identical(d$orig, "RED"))
                 "was RED + ACRO-flagged"
               else if (isTRUE(d$acro)) "ACRO fail/review"
               else "was RED"
        draw_para(sprintf("- %s  (%s, approved as %s)", d$file, why, d$outcome),
                  cex=.66, col="#333333", indent=.012)
      }
      move(.004)
    }

    # ── Demoted stats row: automated score + severity + coverage ──
    # The risk score lives here now, as one modest figure among peers,
    # not as the page headline. ACRO attestation context could be added
    # here in future. Kept compact - this is supporting context for the
    # second reviewer, who leads on the decision integrity above.
    if (!is.null(sc)) {
      total <- sc$total %||% 0L
      tc    <- out_col(sc$tl)
      stat_items <- list(
        list("AUTOMATED RISK", paste0(total, "/100"), tc,
             sc$tl_label %||% ""),
        list("RULE SEVERITY", as.character(sc$severity_score %||% 0), DARK,
             paste0(length(sc$top_hits %||% list()), " hit(s)")),
        list("COVERAGE", paste0(sc$coverage_pct %||% 100, "%"), DARK,
             paste0((sc$n_files%||%0)-(sc$n_limited%||%0), "/",
                    (sc$n_files%||%0), " inspected"))
      )
      sh <- .046; sgap <- .006
      ns <- length(stat_items); sw <- (CW - sgap*(ns-1)) / ns
      check_space(sh + .004)
      sb_y <- ycur()
      for (i in seq_along(stat_items)) {
        st <- stat_items[[i]]
        sx <- ML + (i-1)*(sw+sgap)
        grid.rect(x=sx+sw/2, y=sb_y-sh/2, width=sw, height=sh,
                  gp=gpar(fill="#FAFBFC", col=LGR, lwd=.3))
        grid.text(st[[1]], x=sx+.006, y=sb_y-.010, just="left",
                  gp=gpar(cex=.50, fontface="bold", col=DGR))
        grid.text(st[[2]], x=sx+.006, y=sb_y-.028, just="left",
                  gp=gpar(cex=.92, fontface="bold", col=st[[3]]))
        note_lns <- strwrap(st[[4]], 30)
        grid.text(note_lns[[1]] %||% "", x=sx+.006, y=sb_y-.040, just="left",
                  gp=gpar(cex=.48, col=DGR))
      }
      move(sh + .008)
    }
  }

  # ── Top contributing rules - rendered below scorecard using cursor ──────────
  if (!is.null(sc)) {
    top <- sc$top_hits %||% list()
    if (length(top) > 0) {
      check_space(.018 + length(head(top,4)) * .016 + .004)
      draw_para("Top contributing rules:",
                cex=.68, col=DARK, face="bold")
      for (ti in seq_along(head(top, 4))) {
        h  <- top[[ti]]
        hc <- out_col(h$outcome %||% "")
        lh <- .016
        check_space(lh)
        grid.text(
          paste0("[", h$outcome %||% "", "]  ", h$rule %||% "",
                 "  ", substr(h$file %||% "", 1, 28),
                 "  +", h$weight %||% 0),
          x=ML+.010, y=ycur()-lh/2, just="left",
          gp=gpar(cex=.68, col=hc))
        move(lh)
      }
      move(.006)
    }
  }

  # ── AIRA batch summary (if generated) ──────────────────────────────────────
  # Placed above File Review Summary so reviewers see the AI's narrative
  # overview before scanning the per-file table. Silent if aira_batch is
  # NULL (AIRA disabled, or batch summary never requested).
  .draw_aira_batch_block(aira_batch)

  # ── File review summary table ──────────────────────────────────────────────
  section_hdr("File Review Summary", level=2)
  f_rows <- lapply(res, function(f) {
    cls <- f$classification %||% "?"
    dec <- decisions[[f$file]] %||% list()
    fin <- dec$outcome %||% cls
    ovr <- isTRUE(dec$overridden)
    flg <- paste(sapply(
      Filter(function(h) h$outcome %in% c("RED","AMBER"), f$hits %||% list()),
      `[[`, "rule"), collapse=", ")
    list(paste0("[",cls,"] ",basename(f$file %||% "")),
         f$type_label %||% f$file_type %||% "",
         paste0(round((f$size_bytes%||%0)/1024,1)," KB"),
         if(nzchar(flg)) flg else "\u2014",
         paste0(fin, if(ovr)" OVR" else ""),
         substr(dec$note %||% "\u2014", 1, 38))
  })
  draw_table(list("File","Type","Size","Rules Fired","Decision","Note"),
             f_rows,
             widths=c(.255,.118,.064,.175,.085,.225),
             rh=.0190, cex=.68)
  draw_para(
    "\u24d8 Score reflects automated rule assessment only. OVR = reviewer override.",
    cex=.62, col=DGR, face="italic")

  # ══════════════════════════════════════════════════════════════════════════
  # RED / AMBER FILES
  # ══════════════════════════════════════════════════════════════════════════
  red_amber <- Filter(function(f) f$classification %in%
                        c("RED","AMBER","UNCERTAIN"), res)
  green_f   <- Filter(function(f) f$classification == "GREEN", res)

  if (length(red_amber) > 0) {
    section_hdr(sprintf("Per-File Detail \u2014 %d RED/AMBER File(s)",
                        length(red_amber)))

    for (f in red_amber) {
      cls <- f$classification %||% "?"
      cc  <- out_col(cls); cb <- out_bg(cls)
      dec <- decisions[[f$file]] %||% list()
      fin <- dec$outcome %||% cls; fc <- out_col(fin)
      ovr <- isTRUE(dec$overridden)
      sz  <- round((f$size_bytes%||%0)/1024, 1)

      # File header
      fh <- .034
      check_space(fh + .004)
      grid.rect(x=ML+CW/2, y=ycur()-fh/2, width=CW, height=fh,
                gp=gpar(fill=cb, col=cc, lwd=.8))
      grid.rect(x=ML+.003, y=ycur()-fh/2, width=.007, height=fh,
                gp=gpar(fill=cc, col=NA))
      grid.text(paste0("[",cls,"]  ",f$file%||%""),
                x=ML+.015, y=ycur()-fh*.32, just="left",
                gp=gpar(cex=.90, fontface="bold", col=cc))
      grid.text(paste0("Type: ",f$type_label%||%f$file_type,
                       "  |  Size: ",sz," KB",
                       "  |  Rules: ",length(f$hits%||%list())),
                x=ML+.015, y=ycur()-fh*.76, just="left",
                gp=gpar(cex=.68, col=DGR))
      move(fh + .004)

      # Column names
      cols <- f$col_names %||% character(0)
      if (length(cols)>0) {
        shown <- paste(cols[seq_len(min(length(cols),28))], collapse=", ")
        if (length(cols)>28) shown <- paste0(shown,"\u2026")
        draw_para(paste0("Columns (",length(cols),"): ",shown),
                  cex=.66, col=DGR, indent=.006)
      }

      # Rule cards (RED/AMBER hits)
      hits    <- f$hits %||% list()
      flagged <- Filter(function(h) h$outcome %in% c("RED","AMBER","UNCERTAIN"), hits)
      clean   <- Filter(function(h) h$outcome == "GREEN", hits)

      if (length(flagged)>0) {
        move(.004)
        for (h in flagged) draw_rule_card(h)
      }
      if (length(clean)>0)
        draw_para(paste0("Clean rules: ",
                         paste(sapply(clean,`[[`,"rule"),collapse=", ")),
                  cex=.70, col=GC, indent=.006)

      # AIRA disclosure-review block (if AI review was generated for
      # this file). Keyed on filepath (falling back to file) to match
      # the storage key used by the dispatch observer in 26_server.R.
      if (!is.null(aira_reviews)) {
        r_key <- f$filepath %||% f$file
        resp  <- if (!is.null(r_key)) aira_reviews[[r_key]] else NULL
        if (!is.null(resp)) {
          move(.003)
          .draw_aira_review_block(resp)
        }
      }

      # Reviewer decision strip
      note <- dec$note %||% ""
      dec_txt <- paste0("Reviewer: [",fin,"]",
                        if(ovr)" OVERRIDE" else "",
                        "  |  Logged: ", dec$timestamp%||%"\u2014")
      if (nzchar(note)) dec_txt <- paste0(dec_txt,"\n  Note: ",note)
      dec_lns <- strwrap(dec_txt, 115)
      dh <- length(dec_lns)*.018 + .008
      check_space(dh + .006)
      grid.rect(x=ML+CW/2, y=ycur()-dh/2, width=CW, height=dh,
                gp=gpar(fill=if(ovr)ABG else GREY, col=if(ovr)AC else LGR, lwd=.5))
      for (di in seq_along(dec_lns))
        grid.text(dec_lns[di], x=ML+.008, y=ycur()-di*.018+.012,
                  just="left", gp=gpar(cex=.72))
      move(dh + .010)
    }
  }

  # ══════════════════════════════════════════════════════════════════════════
  # GREEN SUMMARY
  # ══════════════════════════════════════════════════════════════════════════
  if (length(green_f)>0) {
    section_hdr(sprintf("GREEN Files \u2014 Passed (%d)", length(green_f)),
                level=2)
    g_rows <- lapply(green_f, function(f) {
      dec <- decisions[[f$file]] %||% list()
      rules <- paste(sapply(f$hits%||%list(),`[[`,"rule"), collapse=", ")
      list(basename(f$file%||%""),
           f$type_label%||%f$file_type%||%"",
           paste0(round((f$size_bytes%||%0)/1024,1)," KB"),
           substr(rules,1,50),
           dec$outcome%||%"GREEN",
           substr(dec$note%||%"\u2014",1,38))
    })
    draw_table(list("File","Type","Size","Rules","Decision","Note"),
               g_rows, widths=c(.235,.118,.064,.210,.082,.213),
               rh=.0190, cex=.68)
  }

  # ════════════════════════════════════════════════════════════════════════════
  # RULE CONFIGURATION - uses pre-computed cfg_changes from Apply observer
  # ════════════════════════════════════════════════════════════════════════════
  # cfg_changes is a named list accumulated at Apply time, one entry per key:
  #   list(label, rules, default_val, applied_val, applied_at)
  # This correctly handles multiple Apply clicks - each key reflects the LAST
  # applied value; keys reset to default are removed from the list.
  n_changes <- length(cfg_changes %||% list())

  section_hdr("Rule Configuration", level=2)

  if (n_changes == 0L) {
    grid.rect(x=ML, y=ycur()-0.004, width=CW, height=0.024,
              just=c("left","top"),
              gp=gpar(fill="#E8F5E9", col="#2E7D32", lwd=0.8))
    grid.text("\u2713  All rule parameters used at installation defaults. No configuration changes were made.",
              x=ML+0.010, y=ycur()-0.016, just=c("left","centre"),
              gp=gpar(fontsize=7.5, col="#2E7D32", fontface="bold"))
    move(0.030)
  } else {
    grid.rect(x=ML, y=ycur()-0.004, width=CW, height=0.024,
              just=c("left","top"),
              gp=gpar(fill="#FFF3CD", col="#E65100", lwd=0.8))
    grid.text(paste0("\u26a0  ", n_changes, " rule parameter(s) changed from installation defaults in this session."),
              x=ML+0.010, y=ycur()-0.016, just=c("left","centre"),
              gp=gpar(fontsize=7.5, col="#E65100", fontface="bold"))
    move(0.030)
    chg_list <- cfg_changes %||% list()
    chg_rows <- lapply(chg_list, function(ch)
      list(ch$label %||% "?",
           ch$default_val %||% "\u2014",
           ch$applied_val %||% "\u2014",
           ch$rules       %||% "",
           ch$applied_at  %||% ""))
    draw_table(
      list("Parameter", "Default", "Applied value", "Rules", "Time"),
      chg_rows,
      widths  = c(.275, .105, .155, .230, .155),
      rh      = .0185, cex = .67, hdr_col = AC
    )
  }

  a <- cfg %||% list()
  phs <- a$sensitive_phenotypes %||% sensitive_phenotypes
  draw_para(paste0("Active sensitive phenotypes (", length(phs), "): ",
                   paste(phs, collapse=", ")), cex=.67, col=DGR)
  section_hdr("Report Verification", level=2)
  draw_table(list("Field","Value"),
    list(list("Generated",    format(Sys.time(),"%Y-%m-%d %H:%M:%S")),
         list("Source folder",file_dir),
         list("Files assessed",as.character(length(res))),
         list("Generator",   "R base grid \u2014 grDevices::pdf()"),
         list("App version", "v2 2026-03-31")),
    widths=c(.260,.660), rh=.0185, cex=.70)
  draw_para(
    paste0("Generated programmatically by Airlock Checker. ",
           "Score reflects automated rule-based assessment only. ",
           "Final egress authority rests with the workspace administrator."),
    cex=.67, col=DGR)

  invisible(out_path)
}