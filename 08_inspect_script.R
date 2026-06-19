# Script and notebook inspector
# Auto-split from app.R — do not edit the monolithic file

inspect_script <- function(filepath, cfg=list()) {
  sens_ph <- cfg$sensitive_phenotypes %||% sensitive_phenotypes

  hits <- list()
  tryCatch({
    raw_lines <- readLines(filepath, warn=FALSE)
    src       <- paste(raw_lines, collapse="\n")
    sl        <- tolower(src)

    mk_line_ev <- function(matching_linenos, caption="") {
      lines_data <- lapply(matching_linenos, function(ln) {
        list(lineno=ln,
             text=if(ln<=length(raw_lines)) raw_lines[ln] else "",
             flag=TRUE)
      })
      # Add 1 line context before/after each hit, unmarked
      ctx_nos <- unique(sort(unlist(lapply(matching_linenos, function(ln)
        intersect(seq_len(length(raw_lines)),
                  c(ln-1, ln, ln+1))))))
      ctx_lines <- lapply(ctx_nos, function(ln)
        list(lineno=ln,
             text=if(ln<=length(raw_lines)) raw_lines[ln] else "",
             flag=ln %in% matching_linenos))
      list(type="lines", lines=ctx_lines, caption=caption)
    }

    # SCR-001: participant identifier patterns
    eid_pats <- c("participant_id",
                  "imaging_id", "sample_id\\s*=", "subject_id\\s*=", "patient_id\\s*=")
    for (p in eid_pats) {
      match_lines <- which(grepl(p, tolower(raw_lines), perl=TRUE))
      if (length(match_lines) > 0) {
        hits <- append(hits, list(list(rule="SCR-001", outcome="RED",
          detail=paste0("Participant identifier pattern in script: '",
                        sub("\\\\b|\\\\s.*","",p), "'"),
          evidence=mk_line_ev(head(match_lines,5),
            paste0("Lines matching identifier pattern '",
                   sub("\\\\b|\\\\s.*","",p), "'"))
        )))
        break
      }
    }

    # SCR-003: Sensitive phenotype in active coding context
    # Only fires when a phenotype term appears in a meaningful coding construct.
    # Word boundaries enforced in ALL contexts including quoted strings to prevent
    # matching terms embedded in base64 data, long identifiers, or binary content.
    # Lines containing base64/binary data (long unspaced encoded strings) are
    # pre-filtered entirely before phenotype matching.
    scr003_min_len <- cfg$scr003_min_term_length %||% 3L
    code_lines   <- raw_lines[!grepl("^\\s*(#|//)", raw_lines)]
    code_lower   <- tolower(code_lines)
    # Remove lines that are clearly base64 / binary data
    b64_pat      <- '[a-zA-Z0-9+/=]{80,}'
    code_lower   <- code_lower[!grepl(b64_pat, code_lower, perl=TRUE)]
    # Contexts: word boundaries required inside quoted strings (\\b prevents
    # matching substrings like 'hiv' inside long encoded data or identifiers)
    ctx_q  <- '("[^"]*\\b%s\\b[^"]*")|(\047[^\047]*\\b%s\\b[^\047]*\047)'
    ctx_d  <- '(\\$%s\\b)'
    ctx_a  <- '(\\b%s\\s*[=<,\\)])'
    ctx_v  <- '(filter|select|mutate|group_by|summarise|summarize).*\\b%s\\b'
    coding_ctx <- paste(ctx_q, ctx_d, ctx_a, ctx_v, sep='|')
    for (sp in sens_ph) {
      if (nchar(sp) < scr003_min_len) next
      pat <- sprintf(coding_ctx, sp, sp, sp, sp, sp)
      match_lines <- which(grepl(pat, code_lower, perl=TRUE))
      if (length(match_lines) > 0) {
        # Reconstruct original line numbers: first remove comment lines,
        # then remove b64 lines, then map the remaining match indices back
        noncomment_idx <- which(!grepl("^\\s*(#|//)", raw_lines))
        kept           <- noncomment_idx[!grepl(b64_pat,
                            tolower(raw_lines[noncomment_idx]), perl=TRUE)]
        orig_nos <- kept[head(match_lines, 5)]
        orig_nos <- orig_nos[!is.na(orig_nos)]
        hits <- append(hits, list(list(rule="SCR-003", outcome="RED",
          detail=paste0("Sensitive phenotype '", sp,
            "' used in active coding context (string/filter/assignment)"),
          evidence=mk_line_ev(orig_nos,
            paste0("Lines where '", sp, "' appears in coding context"))
        )))
        break
      }
    }

    # SCR-002: Inline data blocks
    # Search raw_lines (vector) not sl (single string) so we get real line numbers.
    # For Rmd files, skip YAML front matter (lines between the opening --- pairs).
    code_for_scr002 <- raw_lines
    if (tolower(tools::file_ext(filepath)) %in% c("rmd","qmd")) {
      yaml_markers <- which(trimws(raw_lines) == "---")
      if (length(yaml_markers) >= 2)
        code_for_scr002 <- raw_lines[-(yaml_markers[1]:yaml_markers[2])]
    }
    data_lines <- which(grepl("data\\.frame\\(|tibble\\(|pd\\.dataframe\\(",
                               tolower(code_for_scr002), perl=TRUE))
    if (length(data_lines) > 0) {
      # Map back to original line numbers if YAML was stripped
      offset <- if (tolower(tools::file_ext(filepath)) %in% c("rmd","qmd") &&
                    length(which(trimws(raw_lines) == "---")) >= 2) {
        yaml_markers <- which(trimws(raw_lines) == "---")
        yaml_markers[2]
      } else 0L
      orig_data_lines <- data_lines + offset
      hits <- append(hits, list(list(rule="SCR-002", outcome="AMBER",
        detail="Script contains inline data construction (data.frame/tibble/DataFrame)",
        evidence=mk_line_ev(head(orig_data_lines, 5),
          "Lines containing inline data construction")
      )))
    }

  }, error=function(e) {
    hits <<- append(hits, list(list(rule="PARSE", outcome="UNCERTAIN",
      detail=paste0("Could not read script: ", conditionMessage(e)))))
  })

  # SCR-006: rendered outputs in Jupyter notebooks
  if (tolower(tools::file_ext(filepath)) == "ipynb") {
    nb_hits <- inspect_notebook_outputs(filepath, cfg)
    hits <- c(hits, nb_hits)
  }

  hits
}


# WAVE 1 INSPECTORS
# ============================================================

# ── SCR-006: Rendered notebook outputs ───────────────────────────────────────
# Called from inspect_script when ext == "ipynb"
inspect_notebook_outputs <- function(filepath, cfg=list()) {
  flag_outputs <- cfg$scr006_flag_outputs %||% TRUE
  pii_scan     <- cfg$scr006_pii_scan     %||% TRUE
  sens_ph      <- cfg$sensitive_phenotypes %||% sensitive_phenotypes
  id_pats      <- cfg$id_patterns          %||% participant_id_patterns

  if (!flag_outputs) return(list())

  lines <- tryCatch(readLines(filepath, warn=FALSE), error=function(e) character(0))
  if (length(lines) == 0) return(list())
  full <- paste(lines, collapse="\n")

  # Detect non-empty outputs: "output_type" only appears inside populated output arrays
  has_outputs <- grepl('"output_type"', full, fixed=TRUE)
  if (!has_outputs) return(list())

  # Count output cells for detail text
  n_out <- length(gregexpr('"output_type"', full, fixed=TRUE)[[1]])

  # Extract text content from output blocks for PII scan
  # Lines between "text": [ ... ] in output sections
  out_text <- paste(
    regmatches(full, gregexpr('"text":\\s*\\[[^\\]]*\\]', full, perl=TRUE))[[1]],
    collapse=" ")
  out_text <- gsub('["\\\\n\\\\t]', ' ', out_text)

  hits <- list()
  hits <- append(hits, list(list(rule="SCR-006", outcome="AMBER",
    detail=paste0(n_out, " cell output(s) found. Notebook was not cleared before egress — ",
      "execution results are embedded. Review output content carefully."))))

  if (pii_scan && nchar(out_text) > 0) {
    # Participant identifiers in output
    id_hits <- id_pats[sapply(id_pats, function(p)
      grepl(gsub("^\\^|\\$$","",p), out_text, ignore.case=TRUE))]
    if (length(id_hits) > 0)
      hits <- append(hits, list(list(rule="SCR-006", outcome="RED",
        detail=paste0("Participant identifier field name(s) found in notebook output: ",
          paste(head(id_hits,5), collapse=", ")))))

    # Sensitive phenotypes in output
    ph_hits <- sens_ph[sapply(sens_ph, function(p)
      grepl(paste0("\\b",p,"\\b"), out_text, ignore.case=TRUE, perl=TRUE))]
    if (length(ph_hits) > 0)
      hits <- append(hits, list(list(rule="SCR-006", outcome="RED",
        detail=paste0("Sensitive phenotype term(s) in notebook output: ",
          paste(head(ph_hits,5), collapse=", ")))))
  }

  hits
}
