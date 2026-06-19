# Text-format inspectors (JSON, XML, Markdown, HTML)
# Auto-split from app.R — do not edit the monolithic file

# ── JSON ─────────────────────────────────────────────────────────────────────
inspect_json <- function(filepath, cfg=list()) {
  sens_ph     <- cfg$sensitive_phenotypes %||% sensitive_phenotypes
  id_pats     <- cfg$id_patterns         %||% participant_id_patterns
  eid_digits  <- cfg$img002_eid_digits   %||% 7L
  json_rec_thr<- cfg$json_record_threshold %||% 50L
  size_thr_mb <- (cfg$size_threshold_gb  %||% 5) * 1024

  hits  <- list()
  sz_mb <- file.info(filepath)$size / 1024^2
  if (!is.na(sz_mb) && sz_mb >= size_thr_mb)
    hits <- append(hits, list(list(rule="TAB-008", outcome="AMBER",
      detail=paste0("JSON file size (", round(sz_mb,1), " MB) exceeds threshold"))))

  read <- safe_read_text(filepath)
  if (read$status == "empty") {
    return(append(hits, list(list(rule="JSON-005", outcome="GREEN",
      detail="Empty JSON file."))))}
  if (read$status == "binary") {
    return(append(hits, list(list(rule="PARSE", outcome="UNCERTAIN",
      detail=sprintf("File appears to be binary (%d NUL byte(s), %.0f%% non-printable) — not valid JSON.",
                     read$n_nul, 100 * read$pct_binary)))))
  }
  if (read$status == "unreadable") {
    return(append(hits, list(list(rule="PARSE", outcome="UNCERTAIN",
      detail="File contents could not be decoded as text."))))
  }
  src <- read$src
  lns <- read$lns

  # JSON-001: Credential keys — one hit per credential type found
  cred_pats <- list(
    "API key"      = '"(api_key|apikey|api-key)"[[:space:]]*:',
    "Password"     = '"(password|passwd|pwd)"[[:space:]]*:',
    "Access token" = '"(access_token|auth_token|bearer_token)"[[:space:]]*:',
    "Secret"       = '"(secret|client_secret|private_key)"[[:space:]]*:',
    "OAuth token"  = '"(oauth_token|refresh_token)"[[:space:]]*:'
  )
  for (cname in names(cred_pats)) {
    p <- cred_pats[[cname]]
    if (grepl(p, src, ignore.case=TRUE, perl=TRUE))
      hits <- append(hits, list(list(rule="JSON-001", outcome="RED",
        detail=paste0("Credential key found: ", cname),
        evidence=mk_text_ev(lns, p,
          paste0("Lines containing '", cname, "' key")))))
  }

  # JSON-002: Participant identifiers
  eid_pat  <- paste0("\\b[0-9]{", eid_digits, "}\\b")
  eid_hits <- regmatches(src, gregexpr(eid_pat, src, perl=TRUE))[[1]]
  if (length(eid_hits) > 0)
    hits <- append(hits, list(list(rule="JSON-002", outcome="RED",
      detail=paste0(length(eid_hits), " occurrence(s) of ", eid_digits,
        "-digit pattern: ", paste(head(unique(eid_hits),4), collapse=", ")),
      evidence=mk_text_ev(lns, eid_pat,
        paste0(eid_digits, "-digit identifier patterns")))))

  nhs_pat <- "\\b[0-9]{3}[[:space:]-][0-9]{3}[[:space:]-][0-9]{4}\\b"
  nhs_v   <- regmatches(src, gregexpr(nhs_pat, src, perl=TRUE))[[1]]
  if (length(nhs_v) > 0)
    hits <- append(hits, list(list(rule="JSON-002", outcome="RED",
      detail=paste0(length(nhs_v), " NHS number pattern(s): ",
        paste(head(unique(nhs_v),3), collapse=", ")),
      evidence=mk_text_ev(lns, nhs_pat, "Lines with NHS number patterns"))))

  for (p in id_pats) {
    term <- gsub("^\\^|\\$$", "", p)
    kpat <- paste0('"', term, '"[[:space:]]*:')
    if (grepl(kpat, src, ignore.case=TRUE, perl=TRUE))
      hits <- append(hits, list(list(rule="JSON-002", outcome="RED",
        detail=paste0("Participant identifier key: '", term, "'"),
        evidence=mk_text_ev(lns, kpat,
          paste0("Lines containing key '", term, "'")))))
  }

  # JSON-003: Sensitive phenotypes
  for (sp in sens_ph) {
    pat <- paste0("\\b", sp, "\\b")
    if (grepl(pat, src, ignore.case=TRUE, perl=TRUE))
      hits <- append(hits, list(list(rule="JSON-003", outcome="RED",
        detail=paste0("Sensitive phenotype '", sp, "' in JSON"),
        evidence=mk_text_ev(lns, pat,
          paste0("Lines mentioning '", sp, "'")))))
  }

  # JSON-004: Large record array
  n_obj <- length(gregexpr("[}][,][[:space:]]*[{]", src, perl=TRUE)[[1]])
  if (n_obj > json_rec_thr)
    hits <- append(hits, list(list(rule="JSON-004", outcome="AMBER",
      detail=paste0("~", n_obj+1, " array objects detected (threshold: ", json_rec_thr, ")"),
      evidence=mk_text_ev(lns, "^[[:space:]]*[{]",
        paste0("Sample of array object openings (~", n_obj+1, " total)"),
        n_matches=4L))))

  if (length(hits) == 0)
    hits <- list(list(rule="JSON-005", outcome="GREEN",
      detail=paste0("JSON (~", nchar(src), " chars): no credentials, identifiers, ",
        "phenotypes, or large record arrays detected.")))
  hits
}

# ── XML ──────────────────────────────────────────────────────────────────────
inspect_xml <- function(filepath, cfg=list()) {
  sens_ph     <- cfg$sensitive_phenotypes %||% sensitive_phenotypes
  id_pats     <- cfg$id_patterns         %||% participant_id_patterns
  eid_digits  <- cfg$img002_eid_digits   %||% 7L
  xml_rec_thr <- cfg$xml_record_threshold %||% 50L
  size_thr_mb <- (cfg$size_threshold_gb  %||% 5) * 1024

  hits  <- list()
  sz_mb <- file.info(filepath)$size / 1024^2
  if (!is.na(sz_mb) && sz_mb >= size_thr_mb)
    hits <- append(hits, list(list(rule="TAB-008", outcome="AMBER",
      detail=paste0("XML file size (", round(sz_mb,1), " MB) exceeds threshold"))))

  read <- safe_read_text(filepath)
  if (read$status == "empty")
    return(append(hits, list(list(rule="XML-005", outcome="GREEN",
      detail="Empty XML file."))))
  if (read$status == "binary")
    return(append(hits, list(list(rule="PARSE", outcome="UNCERTAIN",
      detail=sprintf("File appears to be binary (%d NUL byte(s), %.0f%% non-printable) — not valid XML.",
                     read$n_nul, 100 * read$pct_binary)))))
  if (read$status == "unreadable")
    return(append(hits, list(list(rule="PARSE", outcome="UNCERTAIN",
      detail="File contents could not be decoded as text."))))
  src <- read$src
  lns <- read$lns

  # XML-001: Health data standards
  health_pats <- list(
    "FHIR"     = "fhir[.]hl7[.]org|<Patient>|<Bundle>|<Observation>|<Condition>|resourceType.*Patient",
    "HL7 v2/v3"= "hl7[.]org|<HL7|MSH[|]|PID[|]|<Patient_ID>",
    "CDA/CCD"  = "ClinicalDocument|urn:hl7-org:v3|<ClinicalDocument",
    "DICOM SR" = "DicomAttribute|<DicomObject|dcm4che",
    "openEHR"  = "openehr[.]org|<COMPOSITION|<OBSERVATION"
  )
  for (hname in names(health_pats)) {
    p <- health_pats[[hname]]
    if (grepl(p, src, ignore.case=TRUE, perl=TRUE))
      hits <- append(hits, list(list(rule="XML-001", outcome="RED",
        detail=paste0("Health data standard detected: ", hname,
          " — encodes individual patient records"),
        evidence=mk_text_ev(lns, p,
          paste0("Lines matching ", hname, " marker")))))
  }

  # Plain text for content checks
  txt_lns <- gsub("<[^>]+>", " ", lns)
  full_text <- trimws(gsub("[[:space:]]+", " ",
                           paste(txt_lns, collapse=" ")))

  # XML-002: Participant identifiers
  eid_pat  <- paste0("\\b[0-9]{", eid_digits, "}\\b")
  eid_hits <- regmatches(full_text, gregexpr(eid_pat, full_text, perl=TRUE))[[1]]
  if (length(eid_hits) > 0)
    hits <- append(hits, list(list(rule="XML-002", outcome="RED",
      detail=paste0(length(eid_hits), " ", eid_digits, "-digit pattern(s): ",
        paste(head(unique(eid_hits),4), collapse=", ")),
      evidence=mk_text_ev(txt_lns, eid_pat,
        paste0(eid_digits, "-digit patterns in element content")))))

  nhs_pat <- "\\b[0-9]{3}[[:space:]-][0-9]{3}[[:space:]-][0-9]{4}\\b"
  nhs_v   <- regmatches(full_text, gregexpr(nhs_pat, full_text, perl=TRUE))[[1]]
  if (length(nhs_v) > 0)
    hits <- append(hits, list(list(rule="XML-002", outcome="RED",
      detail=paste0(length(nhs_v), " NHS number pattern(s): ",
        paste(head(unique(nhs_v),3), collapse=", ")),
      evidence=mk_text_ev(lns, nhs_pat, "Lines with NHS patterns"))))

  for (p in id_pats) {
    term <- gsub("^\\^|\\$$", "", p)
    # Check both element names and attribute values
    epat <- paste0("(<", term, "[>[:space:]]|name=\"[^\"]*", term, "[^\"]*\")")
    if (grepl(epat, src, ignore.case=TRUE, perl=TRUE))
      hits <- append(hits, list(list(rule="XML-002", outcome="RED",
        detail=paste0("Identifier element/attribute '", term, "' in XML"),
        evidence=mk_text_ev(lns, epat,
          paste0("Lines with '", term, "' element or attribute")))))
  }

  # XML-003: Sensitive phenotypes (in plain text content)
  for (sp in sens_ph) {
    pat <- paste0("\\b", sp, "\\b")
    if (grepl(pat, full_text, ignore.case=TRUE, perl=TRUE))
      hits <- append(hits, list(list(rule="XML-003", outcome="RED",
        detail=paste0("Sensitive phenotype '", sp, "' in XML content"),
        evidence=mk_text_ev(txt_lns, pat,
          paste0("Lines mentioning '", sp, "'")))))
  }

  # XML-004: Large repeating record structure
  tag_m <- regmatches(src, gregexpr("<([a-zA-Z][a-zA-Z0-9_:-]*)[^/]",
                                     src, perl=TRUE))[[1]]
  if (length(tag_m) > 0) {
    tags      <- gsub("<([a-zA-Z][a-zA-Z0-9_:-]*).*", "\\1", tag_m)
    tag_freq  <- sort(table(tags), decreasing=TRUE)
    top_tag   <- names(tag_freq)[1]
    top_count <- as.integer(tag_freq[1])
    if (top_count > xml_rec_thr)
      hits <- append(hits, list(list(rule="XML-004", outcome="AMBER",
        detail=paste0("<", top_tag, "> repeats ", top_count,
          " times (threshold: ", xml_rec_thr, ")"),
        evidence=mk_text_ev(lns, paste0("<", top_tag, "[>[:space:]]"),
          paste0("Sample of <", top_tag, "> elements"),
          n_matches=4L))))
  }

  if (length(hits) == 0) {
    n_words <- length(unlist(strsplit(full_text, "[[:space:]]+")))
    hits <- list(list(rule="XML-005", outcome="GREEN",
      detail=paste0("XML (~", n_words, " words): no health standards, identifiers, ",
        "phenotypes, or large record structures.")))
  }
  hits
}

# ── Markdown ──────────────────────────────────────────────────────────────────
inspect_markdown <- function(filepath, cfg=list()) {
  sens_ph     <- cfg$sensitive_phenotypes %||% sensitive_phenotypes
  id_pats     <- cfg$id_patterns         %||% participant_id_patterns
  count_thr   <- cfg$count_threshold     %||% 5L
  eid_digits  <- cfg$img002_eid_digits   %||% 7L
  md_row_thr  <- cfg$htm_table_rows      %||% 20L
  size_thr_mb <- (cfg$size_threshold_gb  %||% 5) * 1024

  hits  <- list()
  sz_mb <- file.info(filepath)$size / 1024^2
  if (!is.na(sz_mb) && sz_mb >= size_thr_mb)
    hits <- append(hits, list(list(rule="TAB-008", outcome="AMBER",
      detail=paste0("Markdown size (", round(sz_mb,1), " MB) exceeds threshold"))))

  read <- safe_read_text(filepath)
  if (read$status == "empty")
    return(append(hits, list(list(rule="MD-004", outcome="GREEN",
      detail="Empty markdown file."))))
  if (read$status == "binary")
    return(append(hits, list(list(rule="PARSE", outcome="UNCERTAIN",
      detail=sprintf("File appears to be binary (%d NUL byte(s), %.0f%% non-printable) — not valid Markdown.",
                     read$n_nul, 100 * read$pct_binary)))))
  if (read$status == "unreadable")
    return(append(hits, list(list(rule="PARSE", outcome="UNCERTAIN",
      detail="File contents could not be decoded as text."))))
  lns <- read$lns

  # Exclude fenced code blocks from PII/phenotype checks
  in_fence    <- FALSE
  prose_idx   <- integer(0)
  for (i in seq_along(lns)) {
    if (grepl("^```", lns[i])) { in_fence <- !in_fence; next }
    if (!in_fence) prose_idx <- c(prose_idx, i)
  }
  prose_lns <- lns[prose_idx]
  prose     <- paste(prose_lns, collapse="\n")

  # MD-001: Participant identifiers (prose only)
  eid_pat <- paste0("\\b[0-9]{", eid_digits, "}\\b")
  eid_v   <- regmatches(prose, gregexpr(eid_pat, prose, perl=TRUE))[[1]]
  if (length(eid_v) > 0)
    hits <- append(hits, list(list(rule="MD-001", outcome="RED",
      detail=paste0(length(eid_v), " ", eid_digits, "-digit pattern(s): ",
        paste(head(unique(eid_v),4), collapse=", ")),
      evidence=mk_text_ev(lns, eid_pat,
        paste0(eid_digits, "-digit identifier patterns (code blocks excluded)")))))

  nhs_pat <- "\\b[0-9]{3}[[:space:]-][0-9]{3}[[:space:]-][0-9]{4}\\b"
  nhs_v   <- regmatches(prose, gregexpr(nhs_pat, prose, perl=TRUE))[[1]]
  if (length(nhs_v) > 0)
    hits <- append(hits, list(list(rule="MD-001", outcome="RED",
      detail=paste0(length(nhs_v), " NHS number pattern(s): ",
        paste(head(unique(nhs_v),3), collapse=", ")),
      evidence=mk_text_ev(lns, nhs_pat, "Lines with NHS number patterns"))))

  for (p in id_pats) {
    term <- gsub("^\\^|\\$$", "", p)
    if (grepl(term, prose, ignore.case=TRUE))
      hits <- append(hits, list(list(rule="MD-001", outcome="RED",
        detail=paste0("Identifier term '", term, "' in markdown prose"),
        evidence=mk_text_ev(lns, term,
          paste0("Lines mentioning '", term, "'")))))
  }

  # Unmasked counts in prose
  count_pat <- paste0(
    "(?i)\\b(n|n_cases|cases|count|total|participants|subjects)",
    "[[:space:]]*[=:][[:space:]]*([1-", count_thr-1, "])(?![0-9])")
  cnt_v <- regmatches(prose, gregexpr(count_pat, prose, perl=TRUE))[[1]]
  if (length(cnt_v) > 0)
    hits <- append(hits, list(list(rule="MD-001", outcome="RED",
      detail=paste0(length(cnt_v), " unmasked count(s) below ", count_thr, ": ",
        paste(head(unique(trimws(cnt_v)),4), collapse=", ")),
      evidence=mk_text_ev(lns, count_pat, "Lines with unmasked count values"))))

  # MD-002: Sensitive phenotypes (prose only)
  for (sp in sens_ph) {
    pat <- paste0("\\b", sp, "\\b")
    if (grepl(pat, prose, ignore.case=TRUE, perl=TRUE))
      hits <- append(hits, list(list(rule="MD-002", outcome="RED",
        detail=paste0("Sensitive phenotype '", sp, "' in markdown"),
        evidence=mk_text_ev(lns, pat,
          paste0("Lines mentioning '", sp, "' (code blocks excluded)"),
          n_matches=5L))))
  }

  # MD-003: Large pipe table
  tbl_lns <- which(grepl("^[[:space:]]*[|]", lns) &
                   !grepl("^[[:space:]]*[|][-:]+[|]", lns))
  if (length(tbl_lns) > md_row_thr)
    hits <- append(hits, list(list(rule="MD-003", outcome="AMBER",
      detail=paste0(length(tbl_lns), " pipe table row(s) (threshold: ", md_row_thr, ")"),
      evidence=list(type="lines", caption="Sample table rows",
        lines=lapply(head(tbl_lns, 6), function(i)
          list(lineno=i, text=lns[[i]], flag=TRUE))))))

  if (length(hits) == 0) {
    n_words <- length(unlist(strsplit(prose, "[[:space:]]+")))
    hits <- list(list(rule="MD-004", outcome="GREEN",
      detail=paste0("Markdown (~", n_words, " prose words, ",
        length(tbl_lns), " table row(s)): clean.")))
  }
  hits
}

# ── HTML / web pages ─────────────────────────────────────────────────────────
inspect_webpage <- function(filepath, cfg=list()) {
  sens_ph     <- cfg$sensitive_phenotypes %||% sensitive_phenotypes
  rest_flds   <- cfg$restricted_fields   %||% restricted_fields
  id_pats     <- cfg$id_patterns         %||% participant_id_patterns
  count_thr   <- cfg$count_threshold     %||% 5L
  eid_digits  <- cfg$img002_eid_digits   %||% 7L
  htm_row_thr <- cfg$htm_table_rows      %||% 20L
  size_thr_mb <- (cfg$size_threshold_gb  %||% 5) * 1024

  hits  <- list()
  sz_mb <- file.info(filepath)$size / 1024^2
  if (!is.na(sz_mb) && sz_mb >= size_thr_mb)
    hits <- append(hits, list(list(rule="TAB-008", outcome="AMBER",
      detail=paste0("HTML size (", round(sz_mb,1), " MB) exceeds threshold"))))

  read <- safe_read_text(filepath)
  if (read$status == "empty") {
    return(append(hits, list(list(rule="HTM-007", outcome="GREEN",
      detail="Empty HTML file."))))}
  if (read$status == "binary") {
    return(append(hits, list(list(rule="PARSE", outcome="UNCERTAIN",
      detail=sprintf("File appears to be binary (%d NUL byte(s), %.0f%% non-printable) — not valid HTML.",
                     read$n_nul, 100 * read$pct_binary)))))}
  if (read$status == "unreadable") {
    return(append(hits, list(list(rule="PARSE", outcome="UNCERTAIN",
      detail="File contents could not be decoded as text."))))}
  src <- read$src
  lns <- read$lns

  # HTM-001: JavaScript / interactive content
  js_checks <- list(
    "<script> block"       = "<script[^>]*>",
    "Plotly.js"            = "plotly",
    "D3.js"                = "d3[.]js|d3[.]min[.]js|new d3",
    "Highcharts"           = "highcharts",
    "Observable/OJS"       = "observable",
    "Shiny embedded"       = "window[.]shiny|shinyapp",
    "inline event handler" = "onclick=|onload=|onchange="
  )
  for (jname in names(js_checks)) {
    p <- js_checks[[jname]]
    if (grepl(p, src, ignore.case=TRUE, perl=TRUE))
      hits <- append(hits, list(list(rule="HTM-001", outcome="RED",
        detail=paste0("Interactive content: ", jname,
          " — may embed raw data in JavaScript variables"),
        evidence=mk_text_ev(lns, p,
          paste0("Lines containing ", jname), n_matches=3L))))
  }

  # HTM-006: Large table
  n_tr <- length(gregexpr("<tr[[:space:]>]", src, ignore.case=TRUE, perl=TRUE)[[1]])
  if (n_tr > htm_row_thr) {
    tr_lns <- which(grepl("<tr[[:space:]>]", lns, ignore.case=TRUE, perl=TRUE))
    hits <- append(hits, list(list(rule="HTM-006", outcome="AMBER",
      detail=paste0("~", n_tr, " table rows (threshold: ", htm_row_thr, ")"),
      evidence=list(type="lines", caption="Sample table row lines",
        lines=lapply(head(tr_lns,5), function(i)
          list(lineno=i, text=lns[[i]], flag=TRUE))))))
  }

  # Strip scripts/styles for text checks
  clean <- gsub("<style[^>]*>.*?</style>", " ", src, ignore.case=TRUE, perl=TRUE)
  clean <- gsub("<script[^>]*>.*?</script>", " ", clean, ignore.case=TRUE, perl=TRUE)
  txt_lns   <- gsub("<[^>]+>", " ", strsplit(clean, "\n")[[1]])
  full_text <- trimws(gsub("[[:space:]]+"," ", paste(txt_lns, collapse=" ")))

  # HTM-002: Participant identifiers
  eid_pat <- paste0("\\b[0-9]{", eid_digits, "}\\b")
  eid_v   <- regmatches(full_text, gregexpr(eid_pat, full_text, perl=TRUE))[[1]]
  if (length(eid_v) > 0)
    hits <- append(hits, list(list(rule="HTM-002", outcome="RED",
      detail=paste0(length(eid_v), " ", eid_digits,"-digit pattern(s): ",
        paste(head(unique(eid_v),4), collapse=", ")),
      evidence=mk_text_ev(txt_lns, eid_pat,
        paste0(eid_digits, "-digit patterns in HTML text")))))

  nhs_pat <- "\\b[0-9]{3}[[:space:]-][0-9]{3}[[:space:]-][0-9]{4}\\b"
  nhs_v   <- regmatches(full_text, gregexpr(nhs_pat, full_text, perl=TRUE))[[1]]
  if (length(nhs_v) > 0)
    hits <- append(hits, list(list(rule="HTM-002", outcome="RED",
      detail=paste0(length(nhs_v), " NHS number pattern(s)"),
      evidence=mk_text_ev(txt_lns, nhs_pat, "Lines with NHS patterns"))))

  for (p in id_pats) {
    term <- gsub("^\\^|\\$$","",p)
    if (grepl(term, full_text, ignore.case=TRUE))
      hits <- append(hits, list(list(rule="HTM-002", outcome="RED",
        detail=paste0("Identifier term '", term, "' in HTML text"),
        evidence=mk_text_ev(txt_lns, term, paste0("Lines mentioning '",term,"'")))))
  }

  # HTM-003: Sensitive phenotypes
  for (sp in sens_ph) {
    pat <- paste0("\\b", sp, "\\b")
    if (grepl(pat, full_text, ignore.case=TRUE, perl=TRUE))
      hits <- append(hits, list(list(rule="HTM-003", outcome="RED",
        detail=paste0("Sensitive phenotype '", sp, "' in HTML"),
        evidence=mk_text_ev(txt_lns, pat, paste0("Lines mentioning '",sp,"'")))))
  }

  # HTM-004: Unmasked counts
  count_pat <- paste0(
    "(?i)\\b(n|n_cases|cases|count|total|participants|subjects)",
    "[[:space:]]*[=:][[:space:]]*([1-", count_thr-1, "])(?![0-9])")
  cnt_v <- regmatches(full_text, gregexpr(count_pat, full_text, perl=TRUE))[[1]]
  if (length(cnt_v) > 0)
    hits <- append(hits, list(list(rule="HTM-004", outcome="RED",
      detail=paste0(length(cnt_v), " unmasked count(s) below ", count_thr),
      evidence=mk_text_ev(txt_lns, count_pat, "Lines with unmasked count values"))))

  # HTM-005: Restricted fields
  for (f in rest_flds) {
    fpat <- paste0("\\b", gsub("_","[[:space:]_-]",f), "\\b")
    if (grepl(fpat, full_text, ignore.case=TRUE, perl=TRUE))
      hits <- append(hits, list(list(rule="HTM-005", outcome="AMBER",
        detail=paste0("Restricted field '", f, "' in HTML"),
        evidence=mk_text_ev(txt_lns, fpat,
          paste0("Lines mentioning '",f,"'")))))
  }

  if (length(hits)==0) {
    n_words <- length(unlist(strsplit(full_text, "[[:space:]]+")))
    hits <- list(list(rule="HTM-007", outcome="GREEN",
      detail=paste0("HTML (~", n_words, " words, ~", n_tr, " table row(s)): clean.")))
  }
  hits
}
