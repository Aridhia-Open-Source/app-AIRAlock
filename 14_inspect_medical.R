# Medical image inspectors (NIfTI, DICOM)
# Auto-split from app.R - do not edit the monolithic file

# ── NIfTI header reader (pure R binary, no pixel data loaded) ────────────────
read_nifti_header <- function(filepath) {
  # Safe raw-to-string: filters null bytes first to avoid rawToChar errors
  # on vectors containing embedded nulls followed by non-null bytes
  raw_str <- function(rv) trimws(rawToChar(rv[rv != as.raw(0L)]))
  is_gz <- grepl("[.]nii[.]gz$", filepath, ignore.case=TRUE)
  con <- tryCatch(
    if (is_gz) gzfile(filepath, "rb") else file(filepath, "rb"),
    error=function(e) NULL)
  if (is.null(con)) return(NULL)
  on.exit(tryCatch(close(con), error=function(e) NULL))
  hdr_raw <- tryCatch(readBin(con, "raw", n=352), error=function(e) raw(0))
  if (length(hdr_raw) < 348) return(NULL)
  # Detect endianness - sizeof_hdr must be 348 (NIfTI-1) or 540 (NIfTI-2)
  sh_le <- readBin(hdr_raw[1:4], "integer", n=1, size=4, endian="little")
  sh_be <- readBin(hdr_raw[1:4], "integer", n=1, size=4, endian="big")
  if      (sh_le %in% c(348L, 540L)) endian <- "little"
  else if (sh_be %in% c(348L, 540L)) endian <- "big"
  else return(NULL)
  sizeof_hdr <- if (endian == "little") sh_le else sh_be
  # NIfTI-1 field offsets (R 1-indexed = C 0-indexed + 1):
  #   dim[8]       C offset  40 -> R  41:56   (8 x int16)
  #   descrip[80]  C offset 148 -> R 149:228  (80 chars)
  #   aux_file[24] C offset 228 -> R 229:252  (24 chars)
  #   intent_name  C offset 328 -> R 329:344  (16 chars)
  dims <- tryCatch(
    readBin(hdr_raw[41:56], "integer", n=8, size=2, endian=endian),
    error=function(e) integer(8))
  descrip     <- raw_str(hdr_raw[149:228])
  aux_file    <- raw_str(hdr_raw[229:252])
  intent_name <- raw_str(hdr_raw[329:344])
  # datatype at C offset 70 -> R bytes 71:72 (int16)
  # bitpix   at C offset 72 -> R bytes 73:74 (int16)
  datatype <- tryCatch(
    readBin(hdr_raw[71:72], "integer", n=1, size=2, endian=endian),
    error=function(e) 4L)
  bitpix <- tryCatch(
    readBin(hdr_raw[73:74], "integer", n=1, size=2, endian=endian),
    error=function(e) 16L)
  # vox_offset at C offset 108 -> R bytes 109:112 (float32)
  # Specifies where voxel data begins; must be >= 352 for .nii files.
  # Some files have NIfTI extensions between header and data.
  vox_off <- tryCatch({
    vo <- readBin(hdr_raw[109:112], "double", n=1, size=4, endian=endian)
    as.integer(max(352, vo))
  }, error=function(e) 352L)
  list(ndim=dims[1], dim=dims[2:8], dim3=dims[2:4], descrip=descrip,
       aux=aux_file, intent_name=intent_name, nifti2=(sizeof_hdr == 540L),
       datatype=datatype, bitpix=bitpix, endian=endian, vox_off=vox_off)
}

inspect_nifti <- function(filepath, cfg=list()) {
  name     <- tolower(basename(filepath))
  id_pats  <- cfg$id_patterns %||% participant_id_patterns
  nhs_pat  <- "[0-9]{3}[[:space:]][0-9]{3}[[:space:]][0-9]{4}"
  dob_pat  <- "[0-9]{2}[/\\-][0-9]{2}[/\\-][0-9]{2,4}"
  hdr <- tryCatch(read_nifti_header(filepath), error=function(e) NULL)
  if (is.null(hdr))
    return(list(list(rule="NII-004", outcome="AMBER",
      detail="NIfTI header could not be read. File may be corrupt, use an unsupported encoding, or be a paired .img file.")))

  hits <- list()
  meta_fields <- c(hdr$descrip, hdr$aux, hdr$intent_name)
  meta_fields <- meta_fields[nzchar(meta_fields)]

  # NII-001 - identifiers in header metadata
  id_matches <- character(0)
  for (fld in meta_fields) {
    if (grepl(nhs_pat, fld)) id_matches <- c(id_matches, paste0("NHS number pattern in: '", fld, "'"))
    if (grepl(dob_pat, fld))  id_matches <- c(id_matches, paste0("DOB pattern in: '", fld, "'"))
    for (p in id_pats)
      if (grepl(p, fld, ignore.case=TRUE))
        id_matches <- c(id_matches, paste0("Identifier pattern ('", p, "') in: '", fld, "'"))
  }
  if (length(id_matches) > 0) {
    ev <- list(type="lines",
               lines=lapply(seq_along(id_matches), function(.i)
                 list(lineno=.i, text=id_matches[[.i]], flag=TRUE)),
               caption="Identifier patterns in NIfTI header fields")
    hits <- append(hits, list(list(rule="NII-001", outcome="RED",
      detail=paste0("NIfTI header metadata contains identifier patterns in ",
        length(id_matches), " field(s). Header fields may carry patient information from the originating scanner."),
      evidence=ev)))
  }

  # NII-002 - head/brain imaging indicators
  # Filename: split on separators (handles BIDS _ . - conventions),
  # then exact-match segments to avoid t1_diabetes, bold_results, func_outcome etc.
  name_segs   <- tolower(strsplit(name, "[^a-zA-Z0-9]+")[[1]])
  name_segs   <- name_segs[nzchar(name_segs)]
  exact_head  <- c("t1w","t2w","brain","head","fmri","bold",
                   "anat","func","struct","mprage","dwi","dti","cranial",
                   "skull","neuro","epi")
  prefix_head <- c("brain","mni","neuro","cranial","skull","mprage","fmri","dwi","bold")
  # Header free-text: grepl is fine (less false-positive risk in structured metadata)
  hdr_pat     <- "brain|head|fmri|t1w|t2w|bold|anat|struct|func|mni|cranial|skull|neuro|mprage|dwi"
  is_head <- any(name_segs %in% exact_head) ||
    any(sapply(prefix_head, function(p) any(startsWith(name_segs, p)))) ||
    any(sapply(meta_fields, function(f) grepl(hdr_pat, f, ignore.case=TRUE)))
  if (is_head)
    hits <- append(hits, list(list(rule="NII-002", outcome="AMBER",
      detail=paste0("File name or header indicates head/brain imaging ('", basename(filepath), "'). ",
        "3D brain volumes can be surface-rendered to reconstruct a recognisable facial likeness. ",
        "De-facing must be applied and verified before egress."))))

  # NII-003 - 3D volume without head indicators
  if (!is_head && !any(sapply(hits, function(h) h$rule=="NII-001")) &&
      !is.na(hdr$ndim) && hdr$ndim >= 3 && all(hdr$dim[1:3] > 0))
    hits <- append(hits, list(list(rule="NII-003", outcome="AMBER",
      detail=paste0("3D NIfTI volume (", paste(hdr$dim[1:3], collapse="\u00d7"),
        " voxels). No head/brain indicators found in filename or header. ",
        "Confirm anatomy, assess facial reconstruction risk, and document in reviewer note."))))

  # NII-005 - green pass
  if (length(hits) == 0)
    hits <- list(list(rule="NII-005", outcome="GREEN",
      detail=paste0("NIfTI header parsed. No identifier patterns found in metadata fields. ",
        if (length(meta_fields)>0) paste0("Fields checked: ", paste(meta_fields, collapse="; ")) else
          "All metadata fields are empty.",
        " Manual imaging review still required.")))
  hits
}


inspect_dicom <- function(filepath, cfg=list()) {
  hits <- list()
  count_thr <- cfg$count_threshold %||% 5L

  # oro.dicom availability check
  if (!ORODICOM_OK) {
    return(list(list(rule="DCM-006", outcome="AMBER",
      detail=paste0(
        "oro.dicom R package is not installed. DICOM metadata cannot be inspected. ",
        "Run dependencies.R in the workspace Terminal, then restart the app."))))
  }

  # ── Magic-byte pre-flight ──────────────────────────────────────────────────
  # oro.dicom's C reader segfaults on certain malformed inputs (notably files
  # under ~256 bytes with junk in the preamble area). Even with tryCatch around
  # readDICOMFile, segfaults take down the whole R process - tryCatch can only
  # catch R-level errors, not native-library crashes.
  #
  # Defence: validate the DICOM magic before delegating to oro.dicom. The DICOM
  # standard (PS3.10) requires bytes 128:131 to be the literal ASCII "DICM"
  # immediately after a 128-byte preamble. Any file lacking this signature is
  # not a valid Part 10 DICOM file and must not be passed to the parser.
  #
  # Files smaller than 132 bytes cannot contain the preamble + magic and are
  # rejected the same way. (The minimum useful DICOM file is far larger; 132
  # is the absolute floor below which the file cannot even be syntactically
  # well-formed.)
  has_dicom_magic <- tryCatch({
    if (!file.exists(filepath)) FALSE
    else {
      sz <- file.info(filepath)$size
      if (is.na(sz) || sz < 132L) FALSE
      else {
        con <- file(filepath, "rb")
        on.exit(close(con), add=TRUE)
        bytes <- readBin(con, "raw", n=132L)
        if (length(bytes) < 132L) FALSE
        else identical(rawToChar(bytes[129:132]), "DICM")
      }
    }
  }, error = function(e) FALSE, warning = function(w) FALSE)

  if (!isTRUE(has_dicom_magic)) {
    return(list(list(rule="DCM-006", outcome="AMBER",
      detail=paste0(
        "File has a .dcm extension but does not contain the DICOM magic ",
        "marker (bytes 129-132 are not 'DICM'). The file is either malformed, ",
        "truncated, or not a Part 10 DICOM file. Manual review required ",
        "before egress."))))
  }

  # ── Parse DICOM header ──────────────────────────────────────────────────────
  hdr <- tryCatch(
    oro.dicom::readDICOMFile(filepath, pixelData=FALSE)$hdr,
    error = function(e) NULL
  )

  if (is.null(hdr) || !is.data.frame(hdr) || nrow(hdr) == 0) {
    return(list(list(rule="DCM-006", outcome="AMBER",
      detail=paste0(
        "DICOM file could not be parsed. The file may be corrupted, use a ",
        "proprietary transfer syntax, or be a DICOMDIR index file. ",
        "Manual review required before egress."))))
  }

  # Helper: retrieve tag value by group+element (e.g. "00100010")
  get_tag <- function(group, element) {
    grp <- toupper(sprintf("%04X", as.integer(paste0("0x", group))))
    el  <- toupper(sprintf("%04X", as.integer(paste0("0x", element))))
    rows <- hdr[toupper(hdr$group) == grp & toupper(hdr$element) == el, ]
    if (nrow(rows) == 0) return("")
    val <- trimws(paste(rows$value, collapse=" "))
    val
  }

  # ── Evidence helper - builds a tag table ──────────────────────────────────
  mk_tag_ev <- function(tag_rows, caption="") {
    if (length(tag_rows) == 0) return(NULL)
    df <- as.data.frame(do.call(rbind, tag_rows), stringsAsFactors=FALSE)
    names(df) <- c("Tag address", "Tag name", "Value")
    list(type="table", data=df, flag_cols="Value", caption=caption)
  }

  # ── DCM-001: Direct patient identifier tags ─────────────────────────────────
  id_tags <- list(
    list(name="PatientName",           group="0010", element="0010"),
    list(name="PatientID",             group="0010", element="0020"),
    list(name="PatientBirthDate",      group="0010", element="0030"),
    list(name="ReferringPhysicianName",group="0008", element="0090"),
    list(name="AccessionNumber",       group="0008", element="0050")
  )
  # Common anonymisation placeholder values - these indicate the tag has
  # already been de-identified and should not trigger DCM-001
  anon_placeholders <- c(
    "^(?=[0-9a-f]*[a-f])[0-9a-f]{8,}$", # hex hash with at least one a-f
    "^0+$",                        # zero / 000000
    "^anon(ymous|ymized|ymised)?$",# Anonymized, Anonymous, Anon
    "^de-?identified?$",           # Deidentified, De-identified
    "^redacted$",
    "^removed$",
    "^unknown$",
    "^not[[:space:]]provided$",
    "^n/a$",
    "^\\.$",                       # single dot (common DICOM anon convention)
    "^[*]+$"                       # asterisks
  )

  is_placeholder <- function(v) {
    vl <- trimws(tolower(v))
    any(sapply(anon_placeholders, function(p) grepl(p, vl, perl=TRUE)))
  }

  filled_id <- Filter(function(t) {
    v <- get_tag(t$group, t$element)
    nzchar(v) && !is_placeholder(v)
  }, id_tags)

  if (length(filled_id) > 0) {
    tag_summary <- paste(sapply(filled_id, function(t) {
      v <- get_tag(t$group, t$element)
      paste0(t$name, ": ", substr(v, 1, 40))
    }), collapse="; ")
    ev_rows <- lapply(filled_id, function(t) {
      v <- get_tag(t$group, t$element)
      list(paste0("(",t$group,",",t$element,")"), t$name, substr(v,1,80))
    })
    hits <- append(hits, list(list(rule="DCM-001", outcome="RED",
      detail=paste0(
        length(filled_id), " direct identifier tag(s) contain non-empty values: ",
        tag_summary),
      evidence=mk_tag_ev(ev_rows, "Identifier tags with non-empty values"))))
  }

  # ── DCM-002: Burned-in annotation ──────────────────────────────────────────
  bia <- toupper(trimws(get_tag("0028", "0301")))
  if (bia == "YES") {
    hits <- append(hits, list(list(rule="DCM-002", outcome="RED",
      detail="BurnedInAnnotation (0028,0301) = YES. Patient demographics are embedded in the image pixels and cannot be removed by tag stripping.",
      evidence=mk_tag_ev(
        list(list("(0028,0301)", "BurnedInAnnotation", "YES")),
        "Burned-in annotation tag"))))
  }

  # ── DCM-003: Head imaging / facial reconstruction risk ─────────────────────
  modality     <- toupper(trimws(get_tag("0008", "0060")))
  body_part    <- toupper(trimws(get_tag("0018", "0015")))
  series_desc  <- toupper(trimws(get_tag("0008", "103E")))
  study_desc   <- toupper(trimws(get_tag("0008", "1030")))
  combined_desc <- paste(body_part, series_desc, study_desc)

  head_modalities <- c("MR", "CT")
  head_patterns   <- c("HEAD","BRAIN","SKULL","FACE","CRANIAL","CRANIUM",
                        "NEURO","CEREBR","FACIAL","ORBITS")

  is_head_modality <- modality %in% head_modalities
  is_head_region   <- any(sapply(head_patterns,
    function(p) grepl(p, combined_desc, fixed=TRUE)))

  if (is_head_modality && is_head_region) {
    dcm3_rows <- Filter(function(r) nzchar(r[[3]]), list(
      list("(0008,0060)", "Modality",         modality),
      list("(0018,0015)", "BodyPartExamined",  body_part),
      list("(0008,103E)", "SeriesDescription", series_desc),
      list("(0008,1030)", "StudyDescription",  study_desc)
    ))
    hits <- append(hits, list(list(rule="DCM-003", outcome="AMBER",
      detail=paste0(
        "Modality: ", modality,
        if (nzchar(body_part))    paste0(" | Body part: ", body_part)    else "",
        if (nzchar(series_desc))  paste0(" | Series: ",   series_desc)   else "",
        ". Head imaging with MR or CT modality - facial reconstruction risk. ",
        "Manual specialist review required."),
      evidence=mk_tag_ev(dcm3_rows, "Head imaging identification tags"))))
  } else if (is_head_modality && !is_head_region && nzchar(body_part) == FALSE) {
    # Modality is MR or CT but body part not tagged - conservative AMBER
    hits <- append(hits, list(list(rule="DCM-003", outcome="AMBER",
      detail=paste0(
        "Modality: ", modality, " - body part tag (0018,0015) is absent. ",
        "Cannot confirm this is not a head scan. Manual review required to rule out ",
        "facial reconstruction risk."),
      evidence=mk_tag_ev(
        list(list("(0008,0060)", "Modality", modality),
             list("(0018,0015)", "BodyPartExamined", "(absent)")),
        "Modality present but body part not tagged"))))
  }

  # ── DCM-004: Study/content date at day precision ────────────────────────────
  study_date   <- trimws(get_tag("0008", "0020"))
  content_date <- trimws(get_tag("0008", "0023"))
  acq_date     <- trimws(get_tag("0008", "0022"))

  date_hits <- Filter(function(d) nzchar(d$val) && nchar(gsub("[^0-9]","",d$val)) == 8,
    list(
      list(tag="StudyDate (0008,0020)",   val=study_date),
      list(tag="ContentDate (0008,0023)", val=content_date),
      list(tag="AcquisitionDate (0008,0022)", val=acq_date)
    ))

  if (length(date_hits) > 0) {
    date_summary <- paste(sapply(date_hits,
      function(d) paste0(d$tag, " = ", d$val)), collapse="; ")
    date_ev_rows <- lapply(date_hits, function(d) {
      addr <- regmatches(d$tag, regexpr("\\([0-9,]+\\)", d$tag))
      list(if(length(addr)>0) addr else d$tag, d$tag, d$val)
    })
    hits <- append(hits, list(list(rule="DCM-004", outcome="AMBER",
      detail=paste0(
        "Day-precision date(s) present: ", date_summary, ". ",
        "Truncate to year only (YYYY0101) or apply a consistent date shift."),
      evidence=mk_tag_ev(date_ev_rows, "Date tags at day precision"))))
  }

  # ── DCM-005: Institution / equipment identifiers ────────────────────────────
  site_tags <- list(
    list(name="InstitutionName (0008,0080)",    group="0008", element="0080"),
    list(name="InstitutionAddress (0008,0081)", group="0008", element="0081"),
    list(name="StationName (0008,1010)",        group="0008", element="1010")
  )
  filled_site <- Filter(function(t) nzchar(get_tag(t$group, t$element)), site_tags)

  if (length(filled_site) > 0) {
    site_ev_rows <- lapply(filled_site, function(t) {
      v <- get_tag(t$group, t$element)
      list(paste0("(",t$group,",",t$element,")"), t$name, substr(v,1,80))
    })
    site_summary <- paste(sapply(filled_site, function(t) {
      v <- get_tag(t$group, t$element)
      paste0(t$name, ": ", substr(v, 1, 40))
    }), collapse="; ")
    hits <- append(hits, list(list(rule="DCM-005", outcome="AMBER",
      detail=paste0(
        "Site/equipment identifier(s) present: ", site_summary),
      evidence=mk_tag_ev(site_ev_rows, "Institution and equipment identifier tags"))))
  }

  # ── DCM-007: GREEN pass if no RED/AMBER fired ───────────────────────────────
  # Count slices as supplementary info
  n_frames <- suppressWarnings(as.integer(trimws(get_tag("0028", "0008"))))
  slices_note <- if (!is.na(n_frames) && n_frames > 1)
    paste0(" (", n_frames, " frames/slices)") else ""

  if (!any(sapply(hits, function(h) h$outcome %in% c("RED","AMBER")))) {
    hits <- append(hits, list(list(rule="DCM-007", outcome="GREEN",
      detail=paste0(
        "Tags parsed successfully. No direct identifier tags, no burned-in annotation. ",
        "Modality: ", if (nzchar(modality)) modality else "not tagged",
        slices_note, ". ",
        if (is_head_modality)
          "Note: this is a head-modality file - pixel-level facial reconstruction risk requires separate manual review."
        else
          "Metadata check complete."))))
  }

  hits
}