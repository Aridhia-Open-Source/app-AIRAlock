# Rule engine: run_dte, batch scoring, linkage risk
# Auto-split from app.R - do not edit the monolithic file

# ============================================================
# PARSE-CLASS RULES
# ============================================================
# Rules that indicate the inspector could not meaningfully read the
# file's content at all. A file whose ONLY hits are in this set has no
# parsed content for the AI to assess, so AIRA dispatch is skipped and
# the file shows "AI review not applicable" in the per-file card.
#
# This list is DELIBERATELY NARROW. Earlier versions included rules
# like BIN-001, ARC-002, COL-001, PDF-006 etc on the assumption that
# these meant "couldn't read content". On audit, most of them fire on
# perfectly valid files where the AI absolutely should engage:
#
#   - BIN-001 fires on any binary with a disallowed extension; that's
#     a policy violation, not a parse failure
#   - ARC-002 fires on every standard archive routinely as a "please
#     unpack and re-check" reminder; the archive is fine
#   - COL-001 fires on every parquet/feather file routinely; the file
#     is fine, AI can reason about column names
#   - PDF-006 fires for scanned (image-only) PDFs; the file is real
#     and the AI can still flag obvious image-PDF concerns
#   - DOC-006 fires for legacy .doc files; the file is real
#   - DCM-006, STAT-002, NII-004 sometimes fire purely because a
#     dependency is missing; that's an environmental issue, not a
#     parse failure of the file itself
#
# So the set is now just the generic PARSE rule, which only fires when
# the engine itself caught an error before any inspector could run.
# That is the only case where the AI genuinely has nothing to work
# with.
PARSE_CLASS_RULES <- c(
  "PARSE"   # generic engine-level parse failure (engine caught error
            # before any inspector could produce findings)
)

# is_parse_only_result(r): TRUE if every hit on r is a PARSE-class rule.
# Such files should not be sent to AIRA for disclosure review - there is
# nothing for the AI to evaluate beyond the fact that the file is
# unreadable, which the rule engine has already said.
is_parse_only_result <- function(r) {
  hits <- r$hits
  if (!is.list(hits) || length(hits) == 0L) return(FALSE)
  rules <- vapply(hits, function(h) as.character(h$rule %||% ""),
                  character(1))
  # Only TRUE if at least one hit exists AND every hit is parse-class.
  # An empty rules vector or any non-parse-class hit means there's
  # something for the AI to react to.
  length(rules) > 0L && all(rules %in% PARSE_CLASS_RULES)
}


# ============================================================
# RULE WEIGHTS  (used by calculate_batch_score)
# Scale: 10 = direct identity/re-identification risk
#         8 = strong disclosure risk
#         6 = moderate disclosure risk
#         4 = structural / lower risk
#         2 = tooling limitation / informational
#         0 = clean pass (no weight)
#        -2 = positive signal (actively safe output)
# ============================================================
RULE_WEIGHTS <- c(
  # Direct identity / re-identification
  "TAB-001"=10, "TAB-002"=10, "TAB-003"=10,
  "TAB-009"=10, "TAB-010"=10,
  "GEN-001"=10, "GEN-002"=10,
  "GEN-004"=10, "GEN-006"=10, "GEN-008"=10, "GEN-010"=10, "GEN-011"=10, "GEN-012"=10,
  "GEN-005"=4,  "GEN-007"=2,  "GEN-009"=3,
  "GEN-013"=4,  "GEN-014"=3,  "GEN-015"=3,
  "XML-001"=10,
  "JSON-001"=10, "JSON-002"=10,
  "SCR-001"=10,
  # Strong disclosure risk
  "PDF-001"=8,  "DOC-001"=8,
  "SCR-003"=8,
  "PDF-002"=8,  "DOC-002"=8,  "HTM-002"=8,
  "JSON-003"=8, "XML-002"=8,  "MD-001"=8,
  "IMG-002"=8,  "IMG-004"=8,
  # Moderate disclosure risk
  "TAB-004"=6,  "TAB-005"=6,
  "SCR-002"=6,  "SCR-006"=6,
  "PDF-003"=6,  "PDF-004"=6,
  "DOC-003"=6,  "DOC-004"=6,
  "HTM-001"=6,  "HTM-003"=6,  "HTM-004"=6,
  "JSON-004"=6, "XML-004"=6,  "MD-002"=6,
  "SER-001"=6,  "SER-002"=6,  "DAT-001"=6,
  "ARC-001"=6,
  # Structural / lower risk
  "TAB-006"=4,  "TAB-007"=4,  "TAB-011"=4,  "TAB-012"=6,  "TAB-014"=4,
  # TAB-013 secondary suppression RED - high weight
  "TAB-013"=8,
  # TAB-015 k-anonymity below threshold - serious identity risk (GREEN overridden to 0 in scoring)
  "TAB-015"=8,
  # TAB-016 k-anonymity cannot estimate - tooling limitation
  "TAB-016"=2,
  "PDF-005"=4,  "DOC-005"=4,  "HTM-005"=4,
  "HTM-006"=4,  "XML-003"=4,  "MD-003"=4,
  "IMG-001"=4,  "ARC-002"=4,
  # Tooling limitation / informational
  "TAB-008"=2,  "BIN-001"=2,  "COL-001"=2,
  "PDF-006"=2,  "DOC-006"=2,
  # Statistical files
  "TAB-017"=6, "STAT-001"=0, "STAT-002"=2,
  # Archive manifest rules
  "ARC-003"=8, "ARC-004"=4, "ARC-005"=2,
  # NIfTI imaging
  "NII-001"=8, "NII-002"=6, "NII-003"=4, "NII-004"=2, "NII-005"=0,
  # DICOM - direct identifiers / burned-in (RED)
  "DCM-001"=10, "DCM-002"=10,
  # DICOM - structural risk (AMBER)
  "DCM-003"=6,  "DCM-004"=4,  "DCM-005"=4,
  # DICOM - parse failure (AMBER)
  "DCM-006"=2,
  # Clean pass - no weight
  "IMG-003"=2,  "GEN-003"=0,
  "DCM-007"=0,
  "JSON-005"=0, "XML-005"=0,
  "HTM-007"=0,  "DOC-007"=0,
  "PDF-007"=0,  "MD-004"=0,
  # Positive signal - subtract from score
  "GEN-003_pos"=-2,   # placeholder; GEN-003 is handled as 0 above
  # ACRO/SACRO integration
  "ACR-001"=8,   # SDC check failed - strong disclosure risk
  "ACR-002"=5,   # Manual review required - medium
  "ACR-003"=0,   # SDC checks passed - positive signal
  "ACR-004"=3,   # Config discrepancy - low
  "ACR-005"=2,   # Output file not in batch - low
  "ACR-006"=4,   # Checksum mismatch - medium
  "ACR-007"=0    # Session metadata - informational
)

# ============================================================
# BATCH RISK SCORE
# ============================================================
calculate_batch_score <- function(res, linkage_risk=NULL) {
  if (length(res) == 0) return(NULL)

  n_files <- length(res)

  # ── 1. Rule severity ────────────────────────────────────────
  # Sum weights of every hit across all files. GREEN hits in
  # clean-pass rules don't add weight (weight = 0). Cap at 80
  # to leave headroom for batch structure penalties.
  severity_raw <- 0L
  top_hits <- list()   # top weighted hits for breakdown display
  for (r in res) {
    for (h in r$hits) {
      w <- RULE_WEIGHTS[h$rule]   # single [ returns NA for unknown rules, not error
      # TAB-015 weight depends on outcome: 8 for RED/AMBER, 0 for GREEN
      if (!is.na(w) && h$rule == "TAB-015" && isTRUE(h$outcome == "GREEN")) w <- 0L
      w <- if (is.na(w) || length(w)==0) 0L else as.integer(w)
      if (w == 0L) next
      severity_raw <- severity_raw + w
      top_hits <- c(top_hits, list(list(
        rule=h$rule, outcome=h$outcome, weight=w, file=r$file
      )))
    }
  }
  severity_score <- min(80L, severity_raw)

  # ── 2. Batch structure ──────────────────────────────────────
  n_red   <- sum(sapply(res, function(r) r$classification == "RED"))
  n_amber <- sum(sapply(res, function(r) r$classification == "AMBER"))
  n_green <- sum(sapply(res, function(r) r$classification == "GREEN"))

  struct_score   <- 0L
  struct_reasons <- character(0)

  if (!is.null(linkage_risk) && length(linkage_risk$findings) > 0) {
    types <- sapply(linkage_risk$findings, `[[`, "type")
    if ("shared_columns" %in% types) {
      struct_score   <- struct_score + 15L
      struct_reasons <- c(struct_reasons, "+15  shared column names (join-key risk)")
    }
    if ("qi_combination" %in% types) {
      struct_score   <- struct_score + 10L
      struct_reasons <- c(struct_reasons, "+10  quasi-identifier combination across files")
    }
  }
  if (n_files > 5) {
    struct_score   <- struct_score + 5L
    struct_reasons <- c(struct_reasons,
      paste0("+5   batch size (", n_files, " files \u2014 larger surface area)"))
  }
  if (n_red == 0 && n_amber == 0 && n_green == n_files) {
    struct_score   <- struct_score - 10L
    struct_reasons <- c(struct_reasons, "-10  all files GREEN (clean batch)")
  }
  struct_score <- max(-10L, struct_score)

  # ── 3. Coverage ─────────────────────────────────────────────
  # Files where a tooling limitation prevented full inspection
  tool_limited_rules <- c("PDF-006","DOC-006","BIN-001","ARC-002","COL-001","STAT-002","NII-004")
  n_limited <- sum(sapply(res, function(r)
    any(sapply(r$hits, function(h)
      h$rule %in% tool_limited_rules && h$outcome %in% c("AMBER","UNCERTAIN","RED")
    ))
  ))
  coverage_pct <- round((n_files - n_limited) / n_files * 100)

  # ── Composite ───────────────────────────────────────────────
  total <- as.integer(min(100L, max(0L, severity_score + struct_score)))

  # Traffic light bands
  tl <- if (total <= 15) "GREEN"
        else if (total <= 40) "AMBER"
        else if (total <= 70) "AMBER_RED"
        else "RED"

  tl_label <- switch(tl,
    GREEN     = "Low Risk",
    AMBER     = "Moderate Risk",
    AMBER_RED = "Elevated Risk",
    RED       = "High Risk"
  )
  tl_colour <- switch(tl,
    GREEN     = GREEN_C,
    AMBER     = AMBER_C,
    AMBER_RED = "#BF360C",
    RED       = RED_C
  )
  tl_bg <- switch(tl,
    GREEN     = GREEN_BG,
    AMBER     = AMBER_BG,
    AMBER_RED = "#FBE9E7",
    RED       = RED_BG
  )

  rev_time <- switch(tl,
    GREEN     = "5 \u2013 10 min",
    AMBER     = "15 \u2013 25 min",
    AMBER_RED = "30 \u2013 45 min \u2014 review all evidence panels",
    RED       = "45+ min \u2014 consider returning to researcher"
  )

  # Sort top hits for display (descending weight, first 6)
  if (length(top_hits) > 1)
    top_hits <- top_hits[order(sapply(top_hits, `[[`, "weight"), decreasing=TRUE)]
  top_hits <- head(top_hits, 6)

  list(
    total          = total,
    severity_score = severity_score,
    severity_raw   = severity_raw,
    struct_score   = struct_score,
    struct_reasons = struct_reasons,
    tl             = tl,
    tl_label       = tl_label,
    tl_colour      = tl_colour,
    tl_bg          = tl_bg,
    rev_time       = rev_time,
    n_files        = n_files,
    n_red          = n_red,
    n_amber        = n_amber,
    n_green        = n_green,
    n_limited      = n_limited,
    coverage_pct   = coverage_pct,
    top_hits       = top_hits
  )
}

check_linkage_risk <- function(res, min_categories=3L, min_shared_files=2L) {
  # Only consider tabular files with captured column names
  tab_res <- Filter(function(r) r$file_type == "tabular" && length(r$col_names) > 0, res)
  if (length(tab_res) < 2) return(NULL)

  findings <- list()

  # ── 1. Shared column name detection (join-key risk) ──────────────────────
  # Any column appearing in 2+ files could be a linking key
  all_cols   <- lapply(tab_res, function(r) r$col_names)
  names(all_cols) <- sapply(tab_res, function(r) r$file)
  col_counts <- table(unlist(all_cols))
  shared     <- names(col_counts[col_counts >= min_shared_files])
  # Filter out trivially common column names that aren't meaningful keys
  trivial    <- c("x","y","n","id","value","name","label","type","code","date",
                  "flag","row","col","index","count","total","result","status","notes")
  shared     <- shared[!shared %in% trivial]

  if (length(shared) > 0) {
    shared_map <- lapply(shared, function(col)
      names(all_cols)[sapply(all_cols, function(cs) col %in% cs)])
    names(shared_map) <- shared

    findings <- c(findings, list(list(
      type    = "shared_columns",
      level   = "RED",
      title   = "Shared column name(s) detected - potential join key",
      detail  = paste0(length(shared), " column name(s) appear in 2 or more files: ",
        paste(head(shared, 5), collapse=", "),
        if(length(shared)>5) paste0(" (+", length(shared)-5, " more)") else "",
        ". Files sharing a common identifier column can be linked to re-identify participants."),
      shared_map = shared_map
    )))
  }

  # ── 2. Quasi-identifier combination risk ─────────────────────────────────
  # Categorise every column across all tabular files
  batch_cols  <- unique(unlist(all_cols))
  cat_hits    <- list()
  for (cat_name in names(QI_CATEGORIES)) {
    terms <- QI_CATEGORIES[[cat_name]]
    matched <- batch_cols[sapply(batch_cols, function(col)
      any(sapply(terms, function(t)
        grepl(paste0("\\b", t, "\\b"), col, ignore.case=TRUE, perl=TRUE))))]
    if (length(matched) > 0) cat_hits[[cat_name]] <- matched
  }

  n_categories <- length(cat_hits)
  if (n_categories >= min_categories) {
    cat_summary <- paste(sapply(names(cat_hits), function(cn)
      paste0(cn, " (", paste(head(cat_hits[[cn]],2), collapse=","), ")")),
      collapse="; ")
    findings <- c(findings, list(list(
      type    = "qi_combination",
      level   = "AMBER",
      title   = paste0("Quasi-identifier combination (", n_categories, " categories)"),
      detail  = paste0("Columns across this batch span ", n_categories,
        " quasi-identifier categories: ", cat_summary,
        ". The combination may allow re-identification even if each file is individually safe."),
      categories = cat_hits
    )))
  }

  if (length(findings) == 0) return(NULL)

  list(
    findings      = findings,
    n_tab_files   = length(tab_res),
    file_names    = sapply(tab_res, function(r) r$file)
  )
}

# ============================================================
# CONTENT EXCERPT + TYPE FRAMING for AIRA (disclosure_review_v4+)
# ============================================================
# v3 only populated structured profiles for tabular/statistical files,
# leaving every other file type (genomic, script, document, etc.) with
# nothing for AIRA to inspect. AIRA correctly responded with
# INSUFFICIENT but the verdict was misleading - the inspectors had
# already read content for their rules; we simply weren't passing it
# through.
#
# v4 adds two channels to the result object:
#   $content_excerpt : a textual excerpt of file content for non-
#                      tabular files (or NULL when none is available)
#   $type_framing    : a 1-3 sentence orientation describing the file
#                      type for AIRA (always populated)
#
# The excerpt is bounded by AIRA_CONTENT_EXCERPT_MAX_BYTES (4096 by
# default). The framing is a static lookup keyed by file_type. Both
# travel with the result object the same way column_profiles and
# sample_rows do.
#
# IMPORTANT (R-AIRA-05 amended): AIRA may receive any content the rule
# engine itself read. The excerpt extractor below performs ONE bounded
# read per file using readLines/readBin on the same paths the
# inspectors already touched - it does not open a new content-reading
# pathway, just exposes content the engine has already consumed.

AIRA_CONTENT_EXCERPT_MAX_BYTES <- 4096L

# Per-file-type framing strings. These tell AIRA what kind of file it
# is looking at and what disclosure-relevant features to focus on.
# Keep each framing 1-3 sentences. Add new types here when the engine
# learns to handle them.
AIRA_TYPE_FRAMING <- list(
  tabular = paste(
    "This is a tabular data file (CSV/TSV/Excel-style).",
    "Disclosure-relevant content lives in cell values: direct",
    "identifiers, quasi-identifier combinations, free-text fields,",
    "and small-count cells. Both column names and sample row values",
    "are informative.",
    sep = " "
  ),
  statistical = paste(
    "This is a statistical software data file (SPSS/SAS/Stata). Like",
    "a tabular file but with embedded variable labels, value labels,",
    "and metadata. Disclosure risks are similar to tabular files.",
    sep = " "
  ),
  genomic = paste(
    "This is a genomic data file (VCF, GWAS summary, BAM/SAM). VCF",
    "header lines start with '##'; the column header line starts",
    "with '#CHROM' and lists fixed columns followed by per-sample",
    "genotype columns. Per-sample columns are the",
    "disclosure-relevant feature - their presence indicates",
    "individual-level data; their absence indicates aggregate",
    "variant statistics. INFO field annotations (NS=, AN=) may",
    "imply cohort size.",
    sep = " "
  ),
  script = paste(
    "This is a source code file (R, Python, SQL, shell, etc.). The",
    "code itself is rarely the disclosure concern; look for embedded",
    "data (hardcoded values, string literals containing identifiers),",
    "credentials/connection strings, commented-out result tables,",
    "and references to sensitive datasets or fields.",
    sep = " "
  ),
  document = paste(
    "This is a document file (PDF, Word). Disclosure-relevant content",
    "appears in extracted text: free-text descriptions, names in",
    "headers/footers, embedded tables, identifiers in form fields.",
    "PDFs may also contain images, attachments, or form fields not",
    "visible in the text excerpt - declare these as blind spots.",
    sep = " "
  ),
  json = paste(
    "This is a JSON file. Disclosure-relevant content lives in",
    "string values (especially within objects keyed by 'name', 'id',",
    "'address', etc.) and in nested arrays of records. The structure",
    "may be a single object, an array of records, or NDJSON",
    "(line-delimited JSON).",
    sep = " "
  ),
  xml = paste(
    "This is an XML file. Disclosure-relevant content lives in",
    "element text and attribute values. Look for elements named for",
    "identifiers (Name, ID, Address) and patient/participant",
    "structures. Schemas may indicate clinical document standards",
    "(HL7, FHIR, CDISC ODM).",
    sep = " "
  ),
  markdown = paste(
    "This is a Markdown file. Disclosure-relevant content appears in",
    "prose (free-text descriptions, names), embedded tables, code",
    "blocks containing data, and inline references. Treat as document",
    "for risk purposes.",
    sep = " "
  ),
  webpage = paste(
    "This is an HTML file. Disclosure-relevant content lives in",
    "rendered text (between tags), in attribute values (especially",
    "form input names/values), and in embedded tables. Embedded",
    "scripts and tracking pixels are not in the excerpt.",
    sep = " "
  ),
  archive = paste(
    "This is an archive file (zip, tar, etc.). The excerpt lists the",
    "files contained inside. Disclosure risk depends on the contents",
    "of the contained files, which would need separate assessment",
    "after extraction. Flag as a blind spot if extraction was not",
    "performed.",
    sep = " "
  ),
  office = paste(
    "This is an Office document. The disclosure profile depends on",
    "the format: spreadsheets (.xlsx/.xls) carry data in cells",
    "across one or more sheets - the excerpt lists sheet names and",
    "column counts; word-processor documents (.docx) carry prose",
    "and embedded tables - the excerpt is the document text with",
    "formatting stripped. Embedded images, equations, and revision",
    "history are not in the excerpt and should be flagged as a blind",
    "spot.",
    sep = " "
  ),
  serialised = paste(
    "This is a serialised data file (RDS, RData, Pickle). The",
    "excerpt shows what the engine could discern about its contents",
    "without deserialising. Serialised formats can carry arbitrary",
    "objects including data frames, models, or raw data; the safe",
    "default is to treat them as opaque unless the engine identified",
    "the contained structure.",
    sep = " "
  ),
  database = paste(
    "This is a database file (SQLite, etc.). The excerpt may show",
    "table names and schema. Disclosure risk depends on the rows",
    "in the contained tables, which the engine has not enumerated.",
    sep = " "
  ),
  columnar = paste(
    "This is a columnar data file (Parquet, Feather, ORC). The",
    "excerpt shows column names and types from the file metadata.",
    "Cell values are not extracted; treat row-level content as not",
    "evaluated.",
    sep = " "
  ),
  dicom = paste(
    "This is a DICOM medical imaging file. Disclosure risks are",
    "concentrated in the metadata header tags - PatientName,",
    "PatientID, PatientBirthDate, ReferringPhysicianName,",
    "AccessionNumber, StudyDate, AcquisitionDate, and similar.",
    "The header tags are extracted into the content excerpt as",
    "'(GROUP,ELEMENT)  TagName  =  Value' lines. Pixel data is",
    "not in the excerpt - it cannot carry record-level disclosure",
    "risk in the same way as text fields, but may carry burned-in",
    "patient information visible in the rendered image (a separate",
    "concern, declare as a blind spot).",
    sep = " "
  ),
  nifti = paste(
    "This is a NIfTI neuroimaging file. The content excerpt shows",
    "the NIfTI header fields including descrip (80 chars),",
    "aux_file (24 chars), and intent_name (16 chars) - these text",
    "fields can carry subject identifiers from the originating",
    "scanner. Voxel/volume data is not in the excerpt; for",
    "head/brain scans, voxel data may also carry facial",
    "reconstruction risk (declare as a blind spot if relevant).",
    sep = " "
  ),
  image = paste(
    "This is an image file. For raster formats (PNG, JPEG, TIFF,",
    "BMP) disclosure risks come from embedded EXIF metadata (GPS,",
    "camera serial, timestamps) and from any text/identifiers",
    "visible in the image content itself - neither is represented",
    "in this excerpt. For SVG images, the file is XML and the",
    "excerpt shows the markup; SVGs may contain embedded text",
    "elements that ARE disclosure-relevant (chart labels showing",
    "patient identifiers, annotations, embedded data attributes).",
    sep = " "
  ),
  binary = paste(
    "This is a binary file with no recognised text structure. The",
    "engine could not extract a textual sample; assessment is based",
    "purely on file metadata (name, extension, size). Choose",
    "INSUFFICIENT - you genuinely have no content to assess.",
    sep = " "
  )
)

# Read a bounded textual excerpt for AIRA. Dispatches by file type:
#  - text-like files: readLines, joined to a single string, capped at
#    AIRA_CONTENT_EXCERPT_MAX_BYTES (4096 bytes default).
#  - archive: directory listing (best effort).
#  - document: PDF text extraction; .docx unzip + content.xml read
#  - dicom: header tags via oro.dicom (with PixelData filtered out)
#  - nifti: header text fields via app's read_nifti_header()
#  - office: sheet listing (.xlsx)
#  - columnar: arrow schema (.parquet)
#  - SVG (image type by extension, but XML by content): text-like path
#  - tabular / statistical: no excerpt - sample_rows + column_profiles
#    are richer.
#  - other image types / binary: no excerpt - the framing tells AIRA
#    what to do.
#
# Returns NULL on any error or unsupported type. Never throws.
.read_content_excerpt <- function(filepath, ftype,
                                  max_bytes = AIRA_CONTENT_EXCERPT_MAX_BYTES) {
  text_like <- c("genomic", "script", "json", "xml",
                 "markdown", "webpage")

  tryCatch({
    # Special case: SVG files get classified as 'image' by detect_file_type
    # (because of the .svg extension) but are XML text by content. Send
    # them through the text-like path so AIRA sees the markup. This
    # check goes BEFORE the ftype dispatch so it catches SVGs even when
    # someone classifies them differently.
    ext <- tolower(tools::file_ext(filepath))
    if (ext == "svg") {
      n_lines <- max(80L, as.integer(max_bytes / 40L))
      lines <- readLines(filepath, n = n_lines, warn = FALSE)
      txt <- paste(lines, collapse = "\n")
      if (nchar(txt) > max_bytes) {
        txt <- paste0(substr(txt, 1L, max_bytes - 1L), "\u2026")
      }
      return(txt)
    }

    # Special case: .docx files are zip archives containing
    # word/document.xml. Extract the document text from there rather
    # than reading the raw bytes (which is binary noise). detect_file_type
    # routes .docx to ftype="office", so this check is keyed on extension
    # not type.
    if (ext == "docx") {
      tmp <- tryCatch(
        utils::unzip(filepath, files = "word/document.xml",
                     exdir = tempdir(), overwrite = TRUE),
        error = function(e) character(0))
      if (length(tmp) > 0L && file.exists(tmp[1])) {
        on.exit(unlink(tmp[1]), add = TRUE)
        # word/document.xml is XML wrapping <w:t>text</w:t> elements.
        # We don't need a full XML parser - just strip tags to get
        # readable text. AIRA reads markup-stripped prose better than
        # raw XML.
        xml_lines <- readLines(tmp[1], warn = FALSE)
        xml_text  <- paste(xml_lines, collapse = "\n")
        # Strip XML tags. Crude but effective: any <...> sequence is
        # markup. Cell separators inside <w:t> elements survive as
        # adjacent text.
        plain <- gsub("<[^>]+>", " ", xml_text)
        # Normalise whitespace.
        plain <- gsub("\\s+", " ", plain)
        plain <- trimws(plain)
        if (!nzchar(plain)) return(NULL)
        if (nchar(plain) > max_bytes) {
          plain <- paste0(substr(plain, 1L, max_bytes - 1L), "\u2026")
        }
        return(plain)
      }
      return(NULL)
    }

    if (ftype %in% text_like) {
      # Read up to ~max_bytes worth. readLines doesn't take a byte
      # cap directly, so estimate generously: ~80 chars/line average,
      # so max_bytes/40 gives an upper bound on lines (with a floor).
      n_lines <- max(80L, as.integer(max_bytes / 40L))
      lines <- readLines(filepath, n = n_lines, warn = FALSE)
      txt <- paste(lines, collapse = "\n")
      if (nchar(txt) > max_bytes) {
        txt <- paste0(substr(txt, 1L, max_bytes - 1L), "\u2026")
      }
      return(txt)
    }
    if (ftype == "document") {
      # Document = PDF/Word. Use pdftools if available, else pdftotext
      # fallback. R-FILE-02. For .doc/.docx without a converter, fall
      # back to readLines on the raw bytes (will be largely binary noise,
      # but PDF /Title and similar may surface).
      txt <- ""
      if (ext == "pdf") {
        if (requireNamespace("pdftools", quietly = TRUE)) {
          pages <- tryCatch(pdftools::pdf_text(filepath),
                            error = function(e) character(0))
          txt <- paste(pages, collapse = "\n")
        } else if (nzchar(Sys.which("pdftotext"))) {
          tmp <- tempfile(fileext = ".txt")
          on.exit(unlink(tmp), add = TRUE)
          ok <- tryCatch({
            system2("pdftotext", c("-layout", "-q",
                                   shQuote(filepath), shQuote(tmp)),
                    stdout = FALSE, stderr = FALSE)
            TRUE
          }, error = function(e) FALSE)
          if (isTRUE(ok) && file.exists(tmp)) {
            txt <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
          }
        }
      } else {
        # .doc/.docx and similar - try a raw read; it'll be partial
        # but may surface metadata strings.
        lines <- readLines(filepath, n = 100, warn = FALSE)
        txt <- paste(lines, collapse = "\n")
      }
      if (!nzchar(txt)) return(NULL)
      if (nchar(txt) > max_bytes) {
        txt <- paste0(substr(txt, 1L, max_bytes - 1L), "\u2026")
      }
      return(txt)
    }
    if (ftype == "archive") {
      # Best-effort listing; uses utils::unzip(list = TRUE) for zips.
      if (ext == "zip") {
        listing <- tryCatch(utils::unzip(filepath, list = TRUE),
                            error = function(e) NULL)
        if (is.data.frame(listing) && nrow(listing) > 0L) {
          lines <- vapply(seq_len(min(nrow(listing), 50L)), function(i) {
            sprintf("%s (%s bytes)",
                    listing$Name[i],
                    format(listing$Length[i], scientific = FALSE))
          }, character(1))
          if (nrow(listing) > 50L) {
            lines <- c(lines, sprintf("(... and %d more entries)",
                                      nrow(listing) - 50L))
          }
          txt <- paste(lines, collapse = "\n")
          if (nchar(txt) > max_bytes) {
            txt <- paste0(substr(txt, 1L, max_bytes - 1L), "\u2026")
          }
          return(txt)
        }
      }
      return(NULL)
    }
    if (ftype == "office") {
      # Multi-sheet listing for .xlsx. Sheet names + per-sheet column count.
      # (.docx is handled by the special case above before reaching here.)
      if (ext %in% c("xlsx", "xls") &&
          requireNamespace("readxl", quietly = TRUE)) {
        sheets <- tryCatch(readxl::excel_sheets(filepath),
                           error = function(e) character(0))
        if (length(sheets) == 0L) return(NULL)
        lines <- vapply(sheets, function(sn) {
          cols <- tryCatch(
            ncol(readxl::read_excel(filepath, sheet = sn, n_max = 0)),
            error = function(e) NA_integer_)
          sprintf("Sheet '%s': %s columns", sn,
                  if (is.na(cols)) "?" else as.character(cols))
        }, character(1))
        return(paste(lines, collapse = "\n"))
      }
      return(NULL)
    }
    if (ftype == "columnar") {
      # Parquet/feather - column names and types from metadata, if
      # the relevant package is available. Best-effort, returns NULL
      # if no reader is available.
      if (ext == "parquet" && requireNamespace("arrow", quietly = TRUE)) {
        sch <- tryCatch(arrow::open_dataset(filepath)$schema,
                        error = function(e) NULL)
        if (!is.null(sch)) {
          fields <- vapply(seq_len(sch$num_fields), function(i) {
            f <- sch$field(i)
            sprintf("%s: %s", f$name, f$type$ToString())
          }, character(1))
          return(paste(fields, collapse = "\n"))
        }
      }
      return(NULL)
    }
    if (ftype == "dicom") {
      # DICOM header tags. Disclosure risk concentrates here:
      # PatientName (0010,0010), PatientID (0010,0020),
      # PatientBirthDate (0010,0030), ReferringPhysicianName
      # (0008,0090), AccessionNumber (0008,0050), plus study /
      # series / institution context. We render the full header as
      # tab-aligned "GROUP,ELEMENT  Name  Value" lines so AIRA sees
      # both the standard identifier tags AND any unusual tags the
      # rule engine did not check.
      #
      # Pixel data and other large binary tags are filtered out -
      # they are bytes, not narrative content, and would blow the
      # excerpt budget. The standard PixelData tag is (7FE0,0010);
      # we filter on tag name as a defensive check too.
      if (!requireNamespace("oro.dicom", quietly = TRUE)) return(NULL)
      hdr <- tryCatch(
        oro.dicom::readDICOMFile(filepath, pixelData = FALSE)$hdr,
        error = function(e) NULL)
      if (is.null(hdr) || !is.data.frame(hdr) || nrow(hdr) == 0L) {
        return(NULL)
      }
      # Defensive filter: drop pixel-data and other large binary tags.
      drop_names <- c("PixelData", "OverlayData", "EncapsulatedDocument",
                      "WaveformData", "RedPaletteColorLookupTableData",
                      "GreenPaletteColorLookupTableData",
                      "BluePaletteColorLookupTableData", "IconImageSequence")
      keep <- !(toupper(hdr$name) %in% toupper(drop_names))
      hdr  <- hdr[keep, , drop = FALSE]
      if (nrow(hdr) == 0L) return(NULL)
      # Build "GROUP,ELEMENT  Name  Value" lines; values truncated
      # cell-wise so a long PatientComments doesn't dominate.
      cell_cap <- 200L
      lines <- vapply(seq_len(nrow(hdr)), function(i) {
        grp <- as.character(hdr$group[i]   %||% "")
        el  <- as.character(hdr$element[i] %||% "")
        nm  <- as.character(hdr$name[i]    %||% "")
        val <- as.character(hdr$value[i]   %||% "")
        if (nchar(val) > cell_cap) {
          val <- paste0(substr(val, 1L, cell_cap - 1L), "\u2026")
        }
        sprintf("(%s,%s)  %s  =  %s", grp, el, nm, val)
      }, character(1))
      txt <- paste(lines, collapse = "\n")
      if (nchar(txt) > max_bytes) {
        txt <- paste0(substr(txt, 1L, max_bytes - 1L), "\u2026")
      }
      return(txt)
    }
    if (ftype == "nifti") {
      # NIfTI header text fields. The disclosure-relevant fields are
      # descrip (80 chars), aux_file (24 chars), and intent_name
      # (16 chars) plus dimensions and datatype. The app's
      # read_nifti_header() helper returns these as a list. If the
      # helper is not in scope (called outside the running app), we
      # fall back to NULL rather than reimplementing the parser.
      if (!exists("read_nifti_header", mode = "function")) return(NULL)
      hdr <- tryCatch(read_nifti_header(filepath),
                      error = function(e) NULL)
      if (is.null(hdr)) return(NULL)
      lines <- c(
        sprintf("ndim:        %s", as.character(hdr$ndim    %||% "?")),
        sprintf("dim3:        %s",
                paste(hdr$dim3 %||% integer(0), collapse = " x ")),
        sprintf("datatype:    %s", as.character(hdr$datatype %||% "?")),
        sprintf("bitpix:      %s", as.character(hdr$bitpix   %||% "?")),
        sprintf("descrip:     %s", as.character(hdr$descrip  %||% "")),
        sprintf("aux_file:    %s", as.character(hdr$aux      %||% "")),
        sprintf("intent_name: %s", as.character(hdr$intent_name %||% ""))
      )
      txt <- paste(lines, collapse = "\n")
      if (nchar(txt) > max_bytes) {
        txt <- paste0(substr(txt, 1L, max_bytes - 1L), "\u2026")
      }
      return(txt)
    }
    # tabular, statistical, image, binary, database, serialised:
    # no excerpt - either covered by other channels (sample_rows)
    # or genuinely opaque.
    NULL
  }, error = function(e) NULL)
}

# Look up the file-type framing string. Defaults to a generic message
# if the type is unknown.
.lookup_type_framing <- function(ftype) {
  fr <- AIRA_TYPE_FRAMING[[as.character(ftype)]]
  if (is.null(fr) || !nzchar(fr)) {
    return(paste("This is a file of an unrecognised type. The engine",
                 "applied generic checks; the disclosure profile depends",
                 "on what the file actually contains."))
  }
  fr
}


# ============================================================
# COLUMN PROFILES + SAMPLE ROWS for AIRA (disclosure_review_v3+)
# ============================================================
# These functions extract a bounded structural view of a tabular file
# for inclusion in the AIRA disclosure review prompt. The AIRA module
# (27_aira.R) defines truncation constants (AIRA_SAMPLE_HEAD_ROWS,
# AIRA_SAMPLE_TAIL_ROWS, AIRA_SAMPLE_CELL_MAX_CHARS) which are read here
# at runtime - so loading order matters: 17_engine.R must source after
# 27_aira.R, which is the case in app.R.
#
# Both functions return NULL on any error - profiles/samples are
# advisory; their absence triggers AIRA's INSUFFICIENT path rather than
# a hard failure. The rule engine's existing classification is
# unaffected by profile/sample extraction.

# Build a per-column profile from a data frame already loaded into
# memory. Returns a named list of fixed-shape per-column profiles, in
# the same order as names(df). Never throws; per-column failures land
# as NULL profile entries.
.build_column_profiles <- function(df) {
  if (!is.data.frame(df) || ncol(df) == 0L) return(list())

  out <- list()
  for (cn in names(df)) {
    out[[cn]] <- tryCatch({
      col <- df[[cn]]
      total <- length(col)
      nulls <- sum(is.na(col))
      non_na <- col[!is.na(col)]

      # Type detection - keep the categories the AIRA prompt understands.
      # readxl returns POSIXct for datetimes; readr/read.csv return
      # character for most things.
      ctype <- if (inherits(col, "Date")) "Date"
               else if (inherits(col, "POSIXct") || inherits(col, "POSIXt")) "POSIXct"
               else if (is.logical(col)) "logical"
               else if (is.integer(col)) "integer"
               else if (is.numeric(col)) "numeric"
               else if (is.character(col) || is.factor(col)) "character"
               else "unknown"

      # Distinct count - bounded to avoid pathological cases on huge
      # high-cardinality columns. For our 1000-row sample read this is
      # always fast, but guard anyway.
      distinct <- if (length(non_na) == 0L) 0L
                  else length(unique(non_na))

      # Range or length stats - never both.
      mn <- NA_real_; mx <- NA_real_
      mnl <- NA_integer_; mxl <- NA_integer_; mlnl <- NA_real_
      if (ctype %in% c("integer","numeric") && length(non_na) > 0L) {
        mn <- suppressWarnings(min(as.numeric(non_na), na.rm = TRUE))
        mx <- suppressWarnings(max(as.numeric(non_na), na.rm = TRUE))
        if (!is.finite(mn)) mn <- NA_real_
        if (!is.finite(mx)) mx <- NA_real_
      } else if (ctype == "character" && length(non_na) > 0L) {
        lens <- nchar(as.character(non_na))
        if (length(lens) > 0L) {
          mnl  <- suppressWarnings(min(lens, na.rm = TRUE))
          mxl  <- suppressWarnings(max(lens, na.rm = TRUE))
          mlnl <- suppressWarnings(mean(lens, na.rm = TRUE))
          if (!is.finite(mnl))  mnl  <- NA_integer_  else mnl <- as.integer(mnl)
          if (!is.finite(mxl))  mxl  <- NA_integer_  else mxl <- as.integer(mxl)
          if (!is.finite(mlnl)) mlnl <- NA_real_
        }
      }

      list(
        type           = ctype,
        distinct_count = as.integer(distinct),
        null_count     = as.integer(nulls),
        total_count    = as.integer(total),
        min            = mn,
        max            = mx,
        min_length     = mnl,
        max_length     = mxl,
        mean_length    = mlnl
      )
    }, error = function(e) NULL)
  }
  out
}

# Build the head/tail sample data frame from a data frame already loaded
# into memory. Cells are truncated cell-wise to cell_max_chars. Returns
# a data.frame with .position column ("head"/"tail") in the leftmost
# position, or NULL if the input is unsuitable. n_head and n_tail
# default to AIRA_SAMPLE_HEAD_ROWS / AIRA_SAMPLE_TAIL_ROWS if defined,
# otherwise to 5L.
.build_sample_rows <- function(df,
                               n_head        = NULL,
                               n_tail        = NULL,
                               cell_max_chars = NULL) {
  if (!is.data.frame(df) || ncol(df) == 0L || nrow(df) == 0L) return(NULL)

  # Defaults - read AIRA constants if loaded, otherwise sensible fallbacks.
  if (is.null(n_head))
    n_head <- if (exists("AIRA_SAMPLE_HEAD_ROWS")) AIRA_SAMPLE_HEAD_ROWS else 5L
  if (is.null(n_tail))
    n_tail <- if (exists("AIRA_SAMPLE_TAIL_ROWS")) AIRA_SAMPLE_TAIL_ROWS else 5L
  if (is.null(cell_max_chars))
    cell_max_chars <- if (exists("AIRA_SAMPLE_CELL_MAX_CHARS"))
                        AIRA_SAMPLE_CELL_MAX_CHARS else 80L

  n_head <- as.integer(n_head)
  n_tail <- as.integer(n_tail)
  cell_max_chars <- as.integer(cell_max_chars)

  nr <- nrow(df)
  # If file is shorter than head + tail combined, use the whole file as
  # head with no tail block. This avoids overlap and weird "head row
  # appears in tail too" artefacts.
  if (nr <= n_head + n_tail) {
    head_idx <- seq_len(nr)
    tail_idx <- integer(0)
  } else {
    head_idx <- seq_len(n_head)
    tail_idx <- seq.int(nr - n_tail + 1L, nr)
  }

  # Coerce all columns to character cell-by-cell with truncation.
  truncate_cell <- function(v) {
    s <- if (is.na(v)) NA_character_ else as.character(v)
    if (!is.na(s) && nchar(s) > cell_max_chars) {
      s <- paste0(substr(s, 1L, cell_max_chars - 1L), "\u2026")
    }
    s
  }

  build_block <- function(idx, label) {
    if (length(idx) == 0L) return(NULL)
    sub <- df[idx, , drop = FALSE]
    out <- data.frame(
      .position = rep(label, nrow(sub)),
      stringsAsFactors = FALSE
    )
    for (cn in names(df)) {
      out[[cn]] <- vapply(sub[[cn]], truncate_cell, character(1))
    }
    out
  }

  head_block <- build_block(head_idx, "head")
  tail_block <- build_block(tail_idx, "tail")

  if (is.null(head_block) && is.null(tail_block)) return(NULL)
  if (is.null(head_block)) return(tail_block)
  if (is.null(tail_block)) return(head_block)
  rbind(head_block, tail_block)
}

# Read a tabular file into a small data frame for profiling and
# sampling. Caps at n_max_for_profile rows (default 1000) so the engine
# does not load huge files just to compute profiles. Returns NULL on
# any error - the caller treats absence as "no profile available".
.read_for_profile <- function(filepath,
                              n_max_for_profile = 1000L) {
  tryCatch({
    ext <- tolower(tools::file_ext(filepath))
    df <- if (ext %in% c("xlsx","xls")) {
      if (!requireNamespace("readxl", quietly = TRUE)) return(NULL)
      readxl::read_excel(filepath, n_max = n_max_for_profile)
    } else {
      # Use read.csv with conservative settings. stringsAsFactors=FALSE
      # so character columns stay character. check.names=FALSE so we
      # do not silently mangle column names.
      read.csv(filepath,
               nrows            = n_max_for_profile,
               stringsAsFactors = FALSE,
               check.names      = FALSE,
               na.strings       = c("", "NA", "N/A", "n/a", "null"))
    }
    if (!is.data.frame(df)) return(NULL)
    # Lowercase + trim column names to match the col_names used elsewhere.
    names(df) <- tolower(trimws(names(df)))
    df
  }, error = function(e) NULL)
}


# Decide which columns warrant full profile detail in the AIRA prompt
# (disclosure_review_v6+). Returns a character vector of column names.
#
# A column is "interesting" - and gets a full profile - if any of:
#   1. It's referenced by a non-GREEN rule hit (the engine flagged it)
#   2. It has high distinct-count ratio (>0.95) - potential identifier
#      the engine may not have caught
#   3. It has very low cardinality (<=5) - potential enum or sensitive
#      low-count categorical
#   4. The column name is short or generic (var1, var2, x, y, col1, ...)
#      - the engine relies on column names; AIRA's added value is when
#      names are unhelpful
#
# Everything else gets name-only treatment in the prompt - typically
# the bulk of numeric measurement columns in scientific datasets, which
# don't need their range/distinct stats restated when AIRA already sees
# the values in the sample rows.
#
# This is for output-token-efficiency in the v6 prompt: the breast
# cancer file goes from ~32 column profiles to 2 or 3, dropping prompt
# size meaningfully without losing signal AIRA actually uses.
.select_interesting_columns_v6 <- function(col_names, profiles, hits) {
  if (length(col_names) == 0L) return(character(0))

  # Names referenced in non-GREEN hits.
  flagged_names <- character(0)
  for (h in hits %||% list()) {
    out <- toupper(as.character(h$outcome %||% ""))
    if (out %in% c("RED","AMBER","UNCERTAIN")) {
      # Hits may carry a 'column' field, or may reference columns
      # in detail/evidence. Be conservative: pick up any column name
      # mentioned in the hit's character fields.
      hit_cols <- character(0)
      if (!is.null(h$column))     hit_cols <- c(hit_cols, as.character(h$column))
      if (!is.null(h$columns))    hit_cols <- c(hit_cols, as.character(h$columns))
      detail <- as.character(h$detail %||% "")
      if (nzchar(detail)) {
        # Match column names that appear in the detail string.
        for (cn in col_names) {
          # Word-boundary match to avoid 'id' matching inside 'guid'.
          if (grepl(paste0("\\b", gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", cn), "\\b"),
                    detail, perl = TRUE)) {
            hit_cols <- c(hit_cols, cn)
          }
        }
      }
      flagged_names <- c(flagged_names, hit_cols)
    }
  }
  flagged_names <- unique(flagged_names[flagged_names %in% col_names])

  # Generic/uninformative column names that defeat regex-based identifier
  # detection. AIRA can read the values for these even when the engine
  # gives up on the name.
  generic_pattern <- "^(var|col|x|y|z|v|f|field|column|c|item)[0-9_]*$|^[a-z]$|^[a-z][0-9]$"

  interesting <- character(0)
  for (cn in col_names) {
    if (cn %in% flagged_names) {
      interesting <- c(interesting, cn)
      next
    }
    if (grepl(generic_pattern, cn, ignore.case = TRUE)) {
      interesting <- c(interesting, cn)
      next
    }
    prof <- profiles[[cn]]
    if (is.null(prof)) next
    total <- prof$total_count    %||% 0L
    distinct <- prof$distinct_count %||% 0L
    if (total >= 20L) {
      ratio <- distinct / total
      if (!is.na(ratio) && ratio > 0.95) {
        interesting <- c(interesting, cn)
        next
      }
      if (distinct > 0L && distinct <= 5L) {
        interesting <- c(interesting, cn)
        next
      }
    }
  }

  unique(interesting)
}


run_dte <- function(filepath, cfg=list()) {
  ftype <- detect_file_type(filepath)
  hits  <- switch(ftype,
    tabular    = inspect_tabular(filepath, cfg),
    genomic    = inspect_genomic(filepath, cfg),
    script     = inspect_script(filepath, cfg),
    archive    = inspect_archive(filepath, cfg),
    image      = inspect_image(filepath, cfg),
    document   = inspect_document(filepath, cfg),
    json       = inspect_json(filepath, cfg),
    xml        = inspect_xml(filepath, cfg),
    markdown   = inspect_markdown(filepath, cfg),
    webpage    = inspect_webpage(filepath, cfg),
    office     = inspect_office(filepath, cfg),
    serialised = inspect_serialised(filepath, cfg),
    database   = inspect_database(filepath),
    columnar   = inspect_columnar(filepath, cfg),
    dicom      = inspect_dicom(filepath, cfg),
    nifti      = inspect_nifti(filepath, cfg),
    statistical= inspect_statistical(filepath, cfg),
    acro_results = .run_dte_acro(filepath, cfg),
    binary     = inspect_binary(filepath)
  )
  outcomes <- sapply(hits, `[[`, "outcome")
  cl <- if ("RED" %in% outcomes) "RED" else if ("UNCERTAIN" %in% outcomes) "UNCERTAIN" else
        if ("AMBER" %in% outcomes) "AMBER" else "GREEN"
  score <- if (cl=="RED") 0 else if (cl=="UNCERTAIN") 30 else if (cl=="AMBER")
    max(20, 70 - sum(outcomes=="AMBER")*15 + sum(outcomes=="GREEN")*5) else 100

  # Capture column names for tabular and statistical files (used by linkage risk checker)
  col_names <- if (ftype %in% c("tabular","statistical")) {
    tryCatch({
      ext <- tolower(tools::file_ext(filepath))
      nms <- if (ext %in% c("xlsx","xls")) {
        names(readxl::read_excel(filepath, n_max=0))
      } else {
        # Use base-R read.csv header-only read (no readr dependency)
        hdr <- read.csv(filepath, nrows=1, check.names=FALSE, stringsAsFactors=FALSE)
        names(hdr)
      }
      tolower(trimws(nms))
    }, error=function(e) character(0))
  } else character(0)

  # Column profiles + sample rows for AIRA (disclosure_review_v3+).
  # Tabular and statistical files only - other types return NULL and
  # AIRA's user_builder renders the "no sample available" line. The
  # whole block is wrapped in tryCatch defensively; profile extraction
  # never blocks the rule engine's classification.
  #
  # Two guards:
  #  1. Skip pivot/multi-index CSVs (ACRO crosstab/pivot_table outputs).
  #     Their shape has no meaningful flat column structure to profile and
  #     .read_for_profile / .build_sample_rows can throw on it
  #     ("replacement has 0 rows, data has 5"). The inspector already
  #     recognises these via TAB-025; profiling must skip them too.
  #  2. Wrap in tryCatch so any profiling failure degrades to empty
  #     profiles rather than escaping run_dte and surfacing as a generic
  #     "Engine failure" in the caller. Profiles are advisory for AIRA,
  #     exactly like content_excerpt below.
  column_profiles <- list()
  sample_rows     <- NULL
  is_pivot_csv <- tryCatch(
    ftype == "tabular" &&
      exists(".is_pivot_multiindex_csv", mode = "function") &&
      .is_pivot_multiindex_csv(filepath),
    error = function(e) FALSE)
  if (ftype %in% c("tabular","statistical") && length(col_names) > 0L &&
      !isTRUE(is_pivot_csv)) {
    tryCatch({
      df_for_profile <- .read_for_profile(filepath)
      if (!is.null(df_for_profile)) {
        column_profiles <- .build_column_profiles(df_for_profile)
        sample_rows     <- .build_sample_rows(df_for_profile)
      }
    }, error = function(e) {
      log_event("WARN", "profile_build_failed",
                file = basename(filepath), message = conditionMessage(e))
      column_profiles <<- list()
      sample_rows     <<- NULL
    })
  }

  # Content excerpt + type framing for AIRA (disclosure_review_v4+).
  # Excerpt is for non-tabular text-like files (genomic, script, json,
  # etc.). Tabular files use sample_rows instead. Framing is always
  # populated from a static lookup. Both wrapped defensively.
  content_excerpt <- tryCatch(.read_content_excerpt(filepath, ftype),
                              error = function(e) NULL)
  type_framing    <- .lookup_type_framing(ftype)

  list(file=basename(filepath), filepath=filepath, file_type=ftype,
       type_label=type_label(ftype), size_bytes=file.info(filepath)$size,
       classification=cl, score=score, hits=hits, col_names=col_names,
       column_profiles=column_profiles, sample_rows=sample_rows,
       content_excerpt=content_excerpt, type_framing=type_framing)
}


# ============================================================
# ACRO/SACRO integration - engine-level functions
# ============================================================

# Minimal run_dte dispatch for ACRO results.json files. Like every other
# inspect_* branch in run_dte's switch, this returns ONLY a hits list -
# run_dte wraps it into the full result (setting file_type, classification,
# size_bytes etc. from the standard tail). The real ACRO inspection happens
# at batch level in acro_batch_integrate(), which has access to all batch
# filenames for correlation; here we only emit a placeholder session hit so
# the file classifies GREEN and shows as recognised until the batch pass
# replaces these hits with the full session findings.
#
# IMPORTANT: must return a bare list-of-hits, NOT a full result list. The
# switch assigns this to `hits`, then run_dte runs sapply(hits, [[, "outcome").
# Returning a result list (with $file, $classification, ...) makes that
# sapply index "outcome" into a scalar string -> "subscript out of bounds".
.run_dte_acro <- function(filepath, cfg) {
  list(list(
    rule    = "ACR-007",
    outcome = "GREEN",
    detail  = "ACRO session metadata \u2014 full analysis runs at batch level"
  ))
}

# acro_batch_integrate: called from the assessment observer in 26_server.R
# AFTER the per-file run_dte() loop and BEFORE res_data() is set.
# Finds ACRO results files in the batch, runs inspect_acro() with batch
# context, and injects per-output ACRO hits into correlated file results.
acro_batch_integrate <- function(results, cfg) {
  # Step 1: Find ACRO results files in the batch
  acro_indices <- which(vapply(results, function(r) {
    isTRUE(r$is_acro) || identical(r$file_type, "acro_results")
  }, logical(1)))

  if (length(acro_indices) == 0L) return(results)

  # Step 2: Collect basenames of all non-ACRO files in the batch
  non_acro_indices <- setdiff(seq_along(results), acro_indices)
  batch_basenames <- vapply(results[non_acro_indices], function(r) {
    basename(r$file %||% r$filepath %||% "")
  }, character(1))

  # Step 3: Run inspect_acro on each ACRO results file
  all_acro_data <- list()

  for (idx in acro_indices) {
    r <- results[[idx]]
    fp <- r$filepath %||% r$file

    acro_result <- tryCatch(
      inspect_acro(fp, cfg = cfg, batch_basenames = batch_basenames),
      error = function(e) {
        list(
          hits = list(list(
            rule    = "PARSE",
            outcome = "UNCERTAIN",
            detail  = paste0("ACRO batch integration failed: ", conditionMessage(e))
          )),
          acro_data = NULL
        )
      }
    )

    # package_id groups the results.json and its member files into one
    # ACRO package. Derived from the results file path so multiple ACRO
    # sessions in one batch stay distinct. package_role distinguishes the
    # session-metadata file from the reviewable member outputs.
    pkg_id <- paste0("acropkg_", basename(dirname(fp %||% "")), "_",
                     gsub("[^a-zA-Z0-9]", "_", basename(fp %||% "results")))

    # Replace the placeholder result with the full inspection
    results[[idx]]$hits           <- acro_result$hits
    results[[idx]]$classification <- .acro_classify_session(acro_result$hits)
    results[[idx]]$acro_data      <- acro_result$acro_data
    results[[idx]]$package_id     <- pkg_id
    results[[idx]]$package_role   <- "metadata"

    if (!is.null(acro_result$acro_data)) {
      # Attach the package id so the renderer can find this session's
      # members, and record which declared members are present vs missing
      # in the batch (member-vs-infrastructure: only files under results
      # are members; results.json/config.json/checksums are infrastructure).
      acro_result$acro_data$package_id <- pkg_id
      declared <- names(acro_result$acro_data$file_map %||% list())
      present  <- declared[declared %in% batch_basenames]
      missing  <- declared[!declared %in% batch_basenames]
      acro_result$acro_data$members_declared <- declared
      acro_result$acro_data$members_present  <- present
      acro_result$acro_data$members_missing  <- missing
      results[[idx]]$acro_data <- acro_result$acro_data
      all_acro_data <- c(all_acro_data, list(acro_result$acro_data))
    }
  }

  # Step 4: Inject per-output ACRO hits into correlated files
  for (ad in all_acro_data) {
    if (is.null(ad$file_map)) next

    for (fname in names(ad$file_map)) {
      output_uids <- ad$file_map[[fname]]

      # Find the matching result by basename
      match_idx <- which(vapply(results[non_acro_indices], function(r) {
        identical(basename(r$file %||% ""), fname) ||
        identical(basename(r$filepath %||% ""), fname)
      }, logical(1)))

      if (length(match_idx) == 0L) next
      global_idx <- non_acro_indices[match_idx[1L]]

      # Collect all ACRO hits for this file from all matching outputs
      acro_hits <- list()
      acro_metadata <- list()
      for (uid in output_uids) {
        out_data <- ad$outputs[[uid]]
        if (is.null(out_data)) next
        acro_hits <- c(acro_hits, out_data$hits)
        acro_metadata <- c(acro_metadata, list(list(
          uid            = out_data$uid,
          status         = out_data$status,
          type           = out_data$type,
          method         = out_data$method,
          command        = out_data$command,
          summary        = out_data$summary,
          comments       = out_data$comments,
          comment_status = out_data$comment_status,
          exception      = out_data$exception,
          has_exception  = out_data$has_exception
        )))
      }

      if (length(acro_hits) > 0L) {
        # Append ACRO hits to the file's existing hits
        results[[global_idx]]$hits <- c(results[[global_idx]]$hits, acro_hits)

        # Store ACRO metadata for rendering (researcher comments, exceptions)
        results[[global_idx]]$acro_outputs <- acro_metadata

        # Tag this file as a member of the package so the renderer can
        # group it under the session card.
        results[[global_idx]]$package_id   <- ad$package_id
        results[[global_idx]]$package_role <- "member"

        # Re-derive classification: ACRO can only escalate, never downgrade
        results[[global_idx]]$classification <- .acro_reclassify(
          results[[global_idx]]$classification,
          acro_hits
        )
      }
    }
  }

  results
}

# Classify the ACRO results.json file itself based on its session-level hits.
.acro_classify_session <- function(hits) {
  outcomes <- vapply(hits, function(h) h$outcome %||% "GREEN", character(1))
  if ("RED" %in% outcomes) return("RED")
  if ("AMBER" %in% outcomes) return("AMBER")
  if ("UNCERTAIN" %in% outcomes) return("UNCERTAIN")
  "GREEN"
}

# Reclassify a file after ACRO hits are injected. Conservative direction:
# ACRO can only escalate, never downgrade.
.acro_reclassify <- function(original_class, acro_hits) {
  acro_outcomes <- vapply(acro_hits, function(h) h$outcome %||% "GREEN", character(1))
  worst_acro <- if ("RED" %in% acro_outcomes) "RED"
                else if ("AMBER" %in% acro_outcomes) "AMBER"
                else if ("UNCERTAIN" %in% acro_outcomes) "UNCERTAIN"
                else "GREEN"

  severity_order <- c("GREEN" = 1L, "AMBER" = 2L, "UNCERTAIN" = 3L, "RED" = 4L)
  orig_sev <- severity_order[[original_class %||% "GREEN"]] %||% 1L
  acro_sev <- severity_order[[worst_acro]] %||% 1L

  if (acro_sev > orig_sev) worst_acro else original_class
}

# acro_group_results: compute a package-grouped VIEW of the flat result
# list for rendering. Does NOT mutate the canonical res_data() list - the
# decision/exclusion logic still operates on the flat list. This only
# reorganises for display.
#
# Returns a list with:
#   $packages   - list of package groups, each:
#       $package_id  - the grouping id
#       $metadata    - the results.json result (role "metadata")
#       $members     - list of member file results (role "member"), in
#                      the order ACRO declared them
#       $acro_data   - the session data (config, checklist, summary,
#                      members_present / members_missing)
#   $standalone - list of results not belonging to any ACRO package
#
# Files are matched to packages by their $package_id tag (set in
# acro_batch_integrate). A package always has exactly one metadata result
# (the results.json) and zero or more member results. Member ordering
# follows the ACRO declared order (file_map names) so the package reads
# the way the researcher's session was structured.
acro_group_results <- function(results) {
  if (length(results) == 0L) {
    return(list(packages = list(), standalone = list()))
  }

  pkg_ids <- vapply(results, function(r) {
    as.character(r$package_id %||% "")[1L]
  }, character(1))
  roles <- vapply(results, function(r) {
    as.character(r$package_role %||% "")[1L]
  }, character(1))

  # No ACRO packages at all -> everything is standalone.
  if (!any(nzchar(pkg_ids))) {
    return(list(packages = list(), standalone = results))
  }

  packages <- list()
  used     <- rep(FALSE, length(results))

  # One package per distinct metadata result.
  meta_idx <- which(roles == "metadata" & nzchar(pkg_ids))
  for (mi in meta_idx) {
    pid <- pkg_ids[mi]
    meta_result <- results[[mi]]
    ad <- meta_result$acro_data

    # Member results carrying this package_id.
    member_idx <- which(pkg_ids == pid & roles == "member")

    # Order members by ACRO's declared order where possible.
    declared <- ad$members_declared %||% character(0)
    member_results <- results[member_idx]
    if (length(member_results) > 0L && length(declared) > 0L) {
      member_names <- vapply(member_results, function(r)
        basename(r$file %||% r$filepath %||% ""), character(1))
      ord <- order(match(member_names, declared, nomatch = length(declared) + 1L))
      member_results <- member_results[ord]
      member_idx     <- member_idx[ord]
    }

    used[mi] <- TRUE
    used[member_idx] <- TRUE

    packages[[length(packages) + 1L]] <- list(
      package_id = pid,
      metadata   = meta_result,
      members    = member_results,
      acro_data  = ad
    )
  }

  standalone <- results[!used]

  list(packages = packages, standalone = standalone)
}