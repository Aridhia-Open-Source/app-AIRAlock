# Document inspectors (PDF, Office, binary)
# Auto-split from app.R — do not edit the monolithic file

inspect_document <- function(filepath, cfg=list()) {
  sens_ph     <- cfg$sensitive_phenotypes %||% sensitive_phenotypes
  rest_flds   <- cfg$restricted_fields   %||% restricted_fields
  id_pats     <- cfg$id_patterns         %||% participant_id_patterns
  count_thr   <- cfg$count_threshold     %||% 5L
  eid_digits  <- cfg$img002_eid_digits   %||% 7L
  size_thr_mb <- (cfg$size_threshold_gb  %||% 5) * 1024

  hits  <- list()
  sz_mb <- file.info(filepath)$size / 1024^2
  if (!is.na(sz_mb) && sz_mb >= size_thr_mb)
    hits <- append(hits, list(list(rule="TAB-008", outcome="AMBER",
      detail=paste0("PDF size (", round(sz_mb,1), " MB) exceeds threshold"))))

  hdr_lines <- tryCatch(readLines(filepath, n=80, warn=FALSE),
                        error=function(e) character(0))
  if (grepl("/Encrypt", paste(hdr_lines, collapse=" "), fixed=TRUE)) {
    hits <- append(hits, list(list(rule="PDF-001", outcome="RED",
      detail="PDF contains /Encrypt — file is password-protected. Contents cannot be assessed.")))
    return(hits)
  }

  # Text extraction via pdftools
  if (!PDFTOOLS_OK) {
    return(append(hits, list(list(rule="PDF-006", outcome="AMBER",
      detail=paste0(
        "The pdftools R package is not installed. PDF text extraction is unavailable. ",
        "Run dependencies.R in the workspace Terminal, then restart the app.")))))
  }

  txt_lines <- extract_pdf_text(filepath)
  full_text <- if (!is.null(txt_lines)) paste(txt_lines, collapse="\n") else ""

  if (is.null(txt_lines)) {
    return(append(hits, list(list(rule="PDF-006", outcome="AMBER",
      detail="PDF text extraction failed — manual review required."))))
  }
  if (length(txt_lines) == 0 || nchar(trimws(full_text)) == 0) {
    sz_kb <- round(file.info(filepath)$size/1024, 1)
    return(append(hits, list(list(rule="PDF-006", outcome="AMBER",
      detail=paste0("No text extracted from PDF (", sz_kb, " KB) — likely image-only (scanned) PDF.")))))
  }

  n_pages <- sum(grepl("\f", txt_lines, fixed=TRUE)) + 1L
  n_words <- length(unlist(strsplit(trimws(full_text), "[[:space:]]+")))

  # PDF-002: Participant identifiers
  eid_pat  <- paste0("\\b[0-9]{", eid_digits, "}\\b")
  eid_hits <- regmatches(full_text, gregexpr(eid_pat, full_text, perl=TRUE))[[1]]
  if (length(eid_hits) > 0)
    hits <- append(hits, list(list(rule="PDF-002", outcome="RED",
      detail=paste0(length(eid_hits), " ", eid_digits, "-digit pattern(s): ",
        paste(head(unique(eid_hits),4), collapse=", ")),
      evidence=mk_text_ev(txt_lines, eid_pat,
        paste0(eid_digits, "-digit identifier patterns in PDF text")))))

  nhs_pat <- "\\b[0-9]{3}[[:space:]-][0-9]{3}[[:space:]-][0-9]{4}\\b"
  nhs_v   <- regmatches(full_text, gregexpr(nhs_pat, full_text, perl=TRUE))[[1]]
  if (length(nhs_v) > 0)
    hits <- append(hits, list(list(rule="PDF-002", outcome="RED",
      detail=paste0(length(nhs_v), " NHS number pattern(s): ",
        paste(head(unique(nhs_v),3), collapse=", ")),
      evidence=mk_text_ev(txt_lines, nhs_pat, "Lines with NHS number patterns"))))

  for (p in id_pats) {
    term <- gsub("^\\^|\\$$","",p)
    if (grepl(term, full_text, ignore.case=TRUE))
      hits <- append(hits, list(list(rule="PDF-002", outcome="RED",
        detail=paste0("Identifier field name '", term, "' in PDF text"),
        evidence=mk_text_ev(txt_lines, term,
          paste0("Lines mentioning '", term, "'")))))
  }

  # PDF-003: Sensitive phenotypes
  for (sp in sens_ph) {
    pat <- paste0("\\b", sp, "\\b")
    if (grepl(pat, full_text, ignore.case=TRUE, perl=TRUE))
      hits <- append(hits, list(list(rule="PDF-003", outcome="RED",
        detail=paste0("Sensitive phenotype '", sp, "' in PDF"),
        evidence=mk_text_ev(txt_lines, pat,
          paste0("Lines mentioning '", sp, "'")))))
  }

  # PDF-004: Unmasked small counts
  count_pat <- paste0(
    "(?i)\\b(n|n_cases|cases|count|total|freq|participants|subjects)",
    "[[:space:]]*[=:][[:space:]]*([1-", count_thr-1, "])(?![0-9])")
  cnt_v <- regmatches(full_text, gregexpr(count_pat, full_text, perl=TRUE))[[1]]
  if (length(cnt_v) > 0)
    hits <- append(hits, list(list(rule="PDF-004", outcome="RED",
      detail=paste0(length(cnt_v), " unmasked count(s) below ", count_thr, ": ",
        paste(head(unique(trimws(cnt_v)),4), collapse=", ")),
      evidence=mk_text_ev(txt_lines, count_pat, "Lines with unmasked count values"))))

  # PDF-005: Restricted fields
  for (f in rest_flds) {
    fpat <- paste0("\\b", gsub("_","[[:space:]_-]",f), "\\b")
    if (grepl(fpat, full_text, ignore.case=TRUE, perl=TRUE))
      hits <- append(hits, list(list(rule="PDF-005", outcome="AMBER",
        detail=paste0("Restricted field '", f, "' in PDF"),
        evidence=mk_text_ev(txt_lines, fpat,
          paste0("Lines mentioning restricted field '", f, "'")))))
  }

  # PDF-NER: Named entity recognition in extracted PDF text (TAB-024 logic)
  # Uses the same configurable vocabulary as the tabular NER rule.
  ner_on         <- isTRUE(cfg$ner_enabled        %||% DEFAULT_CFG$ner_enabled)
  ner_titles     <- cfg$ner_person_titles  %||% DEFAULT_CFG$ner_person_titles
  ner_places     <- cfg$ner_geo_places      %||% DEFAULT_CFG$ner_geo_places
  ner_insts      <- cfg$ner_inst_patterns   %||% DEFAULT_CFG$ner_inst_patterns
  ner_occs       <- cfg$ner_occ_patterns    %||% DEFAULT_CFG$ner_occ_patterns
  ner_exclusions <- cfg$ner_name_exclusions %||% DEFAULT_CFG$ner_name_exclusions

  if (ner_on && nchar(trimws(full_text)) > 0) {
    title_rx    <- paste0("(?i)\\b(", paste(ner_titles, collapse="|"),
      ")\\.?\\s+[A-Z][a-z]+(?:\\s+[A-Z][a-z]+)?")
    place_rx    <- paste0("(?i)\\b(", paste(ner_places, collapse="|"), ")\\b")
    inst_rx     <- paste0("(?i)\\b(", paste(ner_insts,  collapse="|"), ")\\b")
    qi_age_rx   <- "\\b[0-9]{1,3}[- ]?(?:year|yr|y/o|years)"
    qi_sex_rx   <- "(?i)\\b(male|female|man|woman|boy|girl)\\b"
    qi_occ_rx   <- if (length(ner_occs) > 0)
      paste0("(?i)\\b(", paste(ner_occs, collapse="|"), ")\\b") else "(?!x)x"
    not_name_rx <- if (length(ner_exclusions) > 0)
      paste0("(?i)^(", paste(ner_exclusions, collapse="|"), ")\\b") else "(?!x)x"
    bare_name_rx <- "\\b[A-Z][a-z]{1,20}\\s+[A-Z][a-z]{1,20}\\b"

    pdf_ner_hits <- list()
    plab <- c(person_name="Person name", composite_qi="Quasi-identifier composite",
              partial_qi="Quasi-identifier (age + sex)",
              institution="Institution name", uk_place="Geographic place name")

    chk_pdf <- function(rx, pname, lines) {
      if (pname %in% names(pdf_ner_hits)) return()
      mi <- which(grepl(rx, lines, perl=TRUE, ignore.case=FALSE))
      if (length(mi) > 0)
        pdf_ner_hits[[pname]] <<- head(mi, 6L)
    }

    chk_pdf(title_rx, "person_name", txt_lines)
    # Bare names: only flag if not matched by exclusion list
    if (!"person_name" %in% names(pdf_ner_hits)) {
      bare_mi <- which(grepl(bare_name_rx, txt_lines, perl=TRUE) &
        !grepl(not_name_rx, txt_lines, perl=TRUE))
      if (length(bare_mi) > 0)
        pdf_ner_hits[["person_name"]] <- head(bare_mi, 6L)
    }
    chk_pdf(place_rx,  "uk_place",     txt_lines)
    chk_pdf(inst_rx,   "institution",  txt_lines)
    # Full composite QI
    if (!"composite_qi" %in% names(pdf_ner_hits)) {
      qi_mi <- which(grepl(qi_age_rx, txt_lines, perl=TRUE) &
        grepl(qi_sex_rx, txt_lines, perl=TRUE) &
        grepl(qi_occ_rx, txt_lines, perl=TRUE))
      if (length(qi_mi) > 0) pdf_ner_hits[["composite_qi"]] <- head(qi_mi, 6L)
    }
    # Partial QI (age + sex only)
    if (!"composite_qi" %in% names(pdf_ner_hits) &&
        !"partial_qi"   %in% names(pdf_ner_hits)) {
      pqi_mi <- which(grepl(qi_age_rx, txt_lines, perl=TRUE) &
        grepl(qi_sex_rx, txt_lines, perl=TRUE))
      if (length(pqi_mi) > 0) pdf_ner_hits[["partial_qi"]] <- head(pqi_mi, 6L)
    }

    if (length(pdf_ner_hits) > 0) {
      sev  <- c(person_name=5L, composite_qi=4L, partial_qi=3L,
                institution=2L, uk_place=1L)
      best <- names(sort(sev[names(pdf_ner_hits)], decreasing=TRUE))[1]
      best_lines <- pdf_ner_hits[[best]]
      all_labels <- paste(sapply(names(pdf_ner_hits),
        function(p) plab[p]), collapse="; ")
      ner_outcome <- if (best == "partial_qi" && length(pdf_ner_hits) == 1L)
        "AMBER" else "RED"
      hits <- append(hits, list(list(
        rule="TAB-024", outcome=ner_outcome,
        detail=paste0(
          length(pdf_ner_hits), " named-entity pattern type(s) in PDF text: ",
          all_labels, ". Highest severity: ", plab[best], "."),
        evidence=list(type="lines",
          lines=lapply(seq_along(best_lines), function(i)
            list(lineno=best_lines[i],
                 text=substr(trimws(txt_lines[best_lines[i]]), 1, 140),
                 flag=TRUE)),
          caption=paste0(plab[best], " — matching lines from PDF text")))))
    }
  }

  # PDF-007: Per-page tables (approximated by page count + word density)
  if (n_pages > 1 && n_words / n_pages > 200)
    hits <- append(hits, list(list(rule="PDF-007", outcome="GREEN",
      detail=paste0("PDF contains ", n_pages, " pages (~", n_words, " words). ",
        "No specific disclosure risks detected."))))

  if (length(hits) == 0)
    hits <- list(list(rule="PDF-007", outcome="GREEN",
      detail=paste0("PDF (", n_pages, " page(s), ~", n_words, " words): clean.")))
  hits
}


# ── Office documents: .docx / .odt ──────────────────────────────────────────
inspect_office <- function(filepath, cfg=list()) {
  sens_ph     <- cfg$sensitive_phenotypes %||% sensitive_phenotypes
  rest_flds   <- cfg$restricted_fields   %||% restricted_fields
  id_pats     <- cfg$id_patterns         %||% participant_id_patterns
  count_thr   <- cfg$count_threshold     %||% 5L
  eid_digits  <- cfg$img002_eid_digits   %||% 7L
  size_thr_mb <- (cfg$size_threshold_gb  %||% 5) * 1024

  hits <- list()
  ext  <- tolower(tools::file_ext(filepath))
  sz_mb <- file.info(filepath)$size / 1024^2
  if (!is.na(sz_mb) && sz_mb >= size_thr_mb)
    hits <- append(hits, list(list(rule="TAB-008", outcome="AMBER",
      detail=paste0("Document size (", round(sz_mb,1), " MB) exceeds threshold"))))

  if (ext == "doc")
    return(append(hits, list(list(rule="DOC-006", outcome="AMBER",
      detail="Legacy .doc format — convert to .docx to enable automated inspection."))))

  xml_entry <- if (ext == "docx") "word/document.xml" else "content.xml"
  tmp_dir   <- tempfile()
  dir.create(tmp_dir, recursive=TRUE, showWarnings=FALSE)
  on.exit(unlink(tmp_dir, recursive=TRUE), add=TRUE)

  extracted <- tryCatch(
    unzip(filepath, files=xml_entry, exdir=tmp_dir, overwrite=TRUE),
    error=function(e) NULL)
  xml_path <- file.path(tmp_dir, xml_entry)
  if (is.null(extracted) || !file.exists(xml_path))
    return(append(hits, list(list(rule="DOC-001", outcome="RED",
      detail=paste0("'", basename(filepath),
        "' could not be unzipped — likely encrypted or password-protected.")))))

  xml_raw  <- tryCatch(paste(readLines(xml_path, warn=FALSE), collapse=" "),
                       error=function(e) "")
  full_text <- trimws(gsub("[[:space:]]+"," ", gsub("<[^>]+>"," ", xml_raw)))
  if (nchar(full_text)==0)
    return(append(hits, list(list(rule="DOC-006", outcome="AMBER",
      detail="No text extracted from document XML."))))

  # Build line vector for evidence (wrap at ~120 chars)
  words  <- unlist(strsplit(full_text, " "))
  doc_lns <- character(0); cur <- ""
  for (w in words) {
    if (nchar(cur)+nchar(w)+1 > 120) { doc_lns <- c(doc_lns, cur); cur <- w }
    else cur <- if (nchar(cur)==0) w else paste(cur, w)
  }
  if (nchar(cur)>0) doc_lns <- c(doc_lns, cur)
  n_words <- length(words)

  # DOC-002: Participant identifiers
  eid_pat <- paste0("\\b[0-9]{", eid_digits, "}\\b")
  eid_v   <- regmatches(full_text, gregexpr(eid_pat, full_text, perl=TRUE))[[1]]
  if (length(eid_v)>0)
    hits <- append(hits, list(list(rule="DOC-002", outcome="RED",
      detail=paste0(length(eid_v), " ", eid_digits, "-digit pattern(s): ",
        paste(head(unique(eid_v),4), collapse=", ")),
      evidence=mk_text_ev(doc_lns, eid_pat,
        paste0(eid_digits, "-digit identifier patterns")))))

  nhs_pat <- "\\b[0-9]{3}[[:space:]-][0-9]{3}[[:space:]-][0-9]{4}\\b"
  nhs_v   <- regmatches(full_text, gregexpr(nhs_pat, full_text, perl=TRUE))[[1]]
  if (length(nhs_v)>0)
    hits <- append(hits, list(list(rule="DOC-002", outcome="RED",
      detail=paste0(length(nhs_v), " NHS number pattern(s): ",
        paste(head(unique(nhs_v),3), collapse=", ")),
      evidence=mk_text_ev(doc_lns, nhs_pat, "Lines with NHS patterns"))))

  for (p in id_pats) {
    term <- gsub("^\\^|\\$$","",p)
    if (grepl(term, full_text, ignore.case=TRUE))
      hits <- append(hits, list(list(rule="DOC-002", outcome="RED",
        detail=paste0("Identifier term '",term,"' in document"),
        evidence=mk_text_ev(doc_lns, term,
          paste0("Lines mentioning '",term,"'")))))
  }

  # DOC-003: Sensitive phenotypes
  for (sp in sens_ph) {
    pat <- paste0("\\b",sp,"\\b")
    if (grepl(pat, full_text, ignore.case=TRUE, perl=TRUE))
      hits <- append(hits, list(list(rule="DOC-003", outcome="RED",
        detail=paste0("Sensitive phenotype '",sp,"' in document"),
        evidence=mk_text_ev(doc_lns, pat, paste0("Lines mentioning '",sp,"'")))))
  }

  # DOC-004: Unmasked counts
  count_pat <- paste0(
    "(?i)\\b(n|n_cases|cases|count|total|participants|subjects)",
    "[[:space:]]*[=:][[:space:]]*([1-",count_thr-1,"])(?![0-9])")
  cnt_v <- regmatches(full_text, gregexpr(count_pat, full_text, perl=TRUE))[[1]]
  if (length(cnt_v)>0)
    hits <- append(hits, list(list(rule="DOC-004", outcome="RED",
      detail=paste0(length(cnt_v), " unmasked count(s) below ", count_thr, ": ",
        paste(head(unique(trimws(cnt_v)),4), collapse=", ")),
      evidence=mk_text_ev(doc_lns, count_pat, "Lines with unmasked count values"))))

  # DOC-005: Restricted fields
  for (f in rest_flds) {
    fpat <- paste0("\\b",gsub("_","[[:space:]_-]",f),"\\b")
    if (grepl(fpat, full_text, ignore.case=TRUE, perl=TRUE))
      hits <- append(hits, list(list(rule="DOC-005", outcome="AMBER",
        detail=paste0("Restricted field '",f,"' in document"),
        evidence=mk_text_ev(doc_lns, fpat,
          paste0("Lines mentioning '",f,"'")))))
  }

  if (length(hits)==0)
    hits <- list(list(rule="DOC-007", outcome="GREEN",
      detail=paste0("Document (~",n_words," words): clean.")))
  hits
}

inspect_binary <- function(filepath)
  list(list(rule="BIN-001", outcome="RED",
    detail=paste0("'", tools::file_ext(filepath),
      "' is not in the approved egress format list.")))

