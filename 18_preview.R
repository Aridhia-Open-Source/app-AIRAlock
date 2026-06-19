# File preview functions
# Auto-split from app.R

# ============================================================
# FILE PREVIEW FUNCTIONS
# ============================================================

# Detect preview capability by file type
can_preview <- function(ftype, filepath) {
  ext <- tolower(tools::file_ext(filepath))
  ftype %in% c("tabular","script","genomic","image","document","office","webpage",
               "json","xml","markdown","serialised","database","columnar","dicom",
               "nifti","statistical") ||
    ext %in% c("txt","log","json","yaml","yml","md","html","xml","tsv","csv","vcf","r","py","rmd")
}

# Build preview content (returns a list with type + rendered HTML/data)
build_preview <- function(filepath) {
  ext   <- tolower(tools::file_ext(filepath))
  ftype <- detect_file_type(filepath)
  fname <- basename(filepath)
  sz    <- file.info(filepath)$size

  # ── Tabular: CSV / TSV / Excel ───────────────────────────────────────────
  if (ftype == "tabular" || ext %in% c("csv","tsv","xlsx","xls")) {
    df <- tryCatch({
      if (ext %in% c("xlsx","xls"))
        readxl::read_excel(filepath, n_max=200)
      else
        readr::read_csv(filepath, n_max=200, show_col_types=FALSE,
                        col_types=readr::cols(.default=readr::col_character()))
    }, error=function(e) NULL)
    if (is.null(df)) return(list(type="error", msg="Could not parse file"))
    list(type="tabular", data=df,
         meta=paste0(nrow(df), " rows shown  ·  ", ncol(df),
                     " columns  ·  ", round(sz/1024,1), " KB"))
  }

  # ── VCF: parse header + variant rows ─────────────────────────────────────
  else if (ftype == "genomic" || ext == "vcf") {
    lines <- tryCatch(readLines(filepath, n=120, warn=FALSE),
                      error=function(e) character(0))
    meta_lines <- lines[grepl("^##", lines)]
    data_lines <- lines[!grepl("^##", lines)]
    hdr_line   <- data_lines[grepl("^#CHROM", data_lines)]
    var_lines  <- data_lines[!grepl("^#", data_lines)]

    # Parse variant rows into a data frame
    df_vars <- NULL
    if (length(hdr_line)>0 && length(var_lines)>0) {
      cols <- strsplit(sub("^#","",hdr_line[1]), "\t")[[1]]
      rows <- lapply(head(var_lines,50), function(l) strsplit(l,"\t")[[1]])
      max_c <- max(sapply(rows, length))
      rows_pad <- lapply(rows, function(r) c(r, rep("",max_c-length(r))))
      df_vars <- as.data.frame(do.call(rbind,rows_pad), stringsAsFactors=FALSE)
      if (length(cols)==ncol(df_vars)) names(df_vars) <- cols
    }
    list(type="vcf",
         meta_lines=meta_lines,
         data=df_vars,
         meta=paste0(length(meta_lines), " header lines  ·  ",
                     length(var_lines), " variants  ·  ",
                     round(sz/1024,1), " KB"))
  }

  # ── Script: R, Python, shell, Rmd ────────────────────────────────────────
  else if (ftype == "script" || ext %in% c("r","py","sh","rmd","ipynb","sql","md")) {
    lines <- tryCatch(readLines(filepath, warn=FALSE), error=function(e) character(0))
    list(type="script",
         lines=lines,
         lang=switch(ext, r="r", rmd="r", py="python", sh="bash",
                     sql="sql", md="markdown", "text"),
         meta=paste0(length(lines), " lines  ·  ", round(sz/1024,1), " KB"))
  }

  # ── Image: SVG (render directly), PNG/JPEG (base64) ──────────────────────
  else if (ftype == "image" || ext %in% c("png","jpg","jpeg","gif","svg","webp","bmp")) {
    if (ext == "svg") {
      svg_src <- tryCatch(paste(readLines(filepath,warn=FALSE),collapse="\n"),
                          error=function(e) "")
      list(type="svg", src=svg_src,
           meta=paste0("SVG vector image  ·  ", round(sz/1024,1), " KB"))
    } else {
      b64 <- tryCatch({
        raw_bytes <- readBin(filepath, "raw", n=sz)
        base64enc::base64encode(raw_bytes)
      }, error=function(e) NULL)
      mime <- switch(ext, jpg="jpeg", jpeg="jpeg", png="png",
                     gif="gif", webp="webp", bmp="bmp", "png")
      list(type="image",
           src=if(!is.null(b64)) paste0("data:image/",mime,";base64,",b64) else NULL,
           meta=paste0(toupper(ext), " image  ·  ", round(sz/1024,1), " KB"))
    }
  }

  # ── PDF: extract text via pdftools ─────────────────────────────────────────
  else if (ext == "pdf") {
    hdr <- tryCatch(readLines(filepath, n=80, warn=FALSE), error=function(e) character(0))
    if (grepl("/Encrypt", paste(hdr, collapse=" "), fixed=TRUE)) {
      list(type="script", lines=c("[Password-protected or encrypted PDF — content cannot be previewed]"),
           lang="text", meta=paste0("Encrypted PDF  \u00b7  ", round(sz/1024,1), " KB"))
    } else if (!PDFTOOLS_OK) {
      list(type="script",
           lines=c("[pdftools package not installed — run dependencies.R to enable PDF preview]"),
           lang="text", meta=paste0("PDF document  \u00b7  ", round(sz/1024,1), " KB"))
    } else {
      txt_lines <- extract_pdf_text(filepath)
      if (is.null(txt_lines) || length(txt_lines) == 0)
        txt_lines <- c("[No text could be extracted — this may be a scanned (image-only) PDF]")
      n_pages <- sum(grepl("\f", txt_lines, fixed=TRUE)) + 1L
      txt_lines <- gsub("\f", "\n--- [page break] ---", txt_lines, fixed=TRUE)
      list(type="script", lines=txt_lines[1:min(300, length(txt_lines))], lang="text",
           meta=paste0("PDF document  \u00b7  ", n_pages, " page(s)  \u00b7  ",
                       round(sz/1024,1), " KB  \u00b7  first 300 lines shown"))
    }
  }

  # ── Plain text / logs / config ───────────────────────────────────────────
  else if (ext %in% c("html","htm")) {
    raw    <- tryCatch(readLines(filepath, warn=FALSE, n=500), error=function(e) character(0))
    src    <- paste(raw, collapse="\n")
    has_js <- grepl("<script|plotly|highcharts|d3[.]js", src, ignore.case=TRUE)
    n_tr   <- length(gregexpr("<tr[[:space:]>]", src, ignore.case=TRUE, perl=TRUE)[[1]])
    meta   <- paste0("HTML  \u00b7  ", length(raw), " source lines  \u00b7  ",
      round(sz/1024,1), " KB",
      if (has_js) "  \u00b7  \u26a0 contains scripts" else "",
      if (n_tr > 0) paste0("  \u00b7  ~", n_tr, " table rows") else "",
      if (length(raw) == 500) "  \u00b7  first 500 lines shown" else "")
    list(type="script", lines=raw, lang="html", meta=meta)
  }

  else if (ext %in% c("json","xml","md","markdown","yaml","yml","txt","log","tsv")) {
    raw <- tryCatch(readLines(filepath, n=500, warn=FALSE),
                    error=function(e) character(0))
    lang <- switch(ext,
      json="json", xml="xml", md="markdown", markdown="markdown",
      yaml="yaml", yml="yaml", "text")
    # For JSON: show pretty-printed preview with record count hint
    meta_extra <- if (ext == "json") {
      src <- paste(raw, collapse="
")
      n_obj <- length(gregexpr("[}][,][[:space:]]*[{]", src, perl=TRUE)[[1]])
      if (n_obj > 0) paste0("  ·  ~", n_obj+1, " array objects") else ""
    } else if (ext == "xml") {
      src <- paste(raw, collapse="
")
      has_fhir <- grepl("fhir|HL7|ClinicalDocument|<Patient>", src, ignore.case=TRUE)
      if (has_fhir) "  ·  ⚠ health data standard" else ""
    } else if (ext %in% c("md","markdown")) {
      n_tbl <- sum(grepl("^[[:space:]]*[|]", raw) & !grepl("^[[:space:]]*[|][-:]+[|]", raw))
      if (n_tbl > 0) paste0("  ·  ", n_tbl, " table rows") else ""
    } else ""
    list(type="script", lines=raw, lang=lang,
         meta=paste0(toupper(ext), "  ·  ", length(raw), " lines  ·  ",
           round(sz/1024,1), " KB", meta_extra))
  }

  # ── Office documents (.docx / .odt) ─────────────────────────────────────
  else if (ext %in% c("docx","odt","doc")) {
    if (ext == "doc") {
      return(list(type="script",
        lines=c("[Legacy .doc format — convert to .docx to enable preview]"),
        lang="text", meta=paste0("Legacy Word document  ·  ", round(sz/1024,1), " KB")))
    }
    xml_entry <- if (ext == "docx") "word/document.xml" else "content.xml"
    tmp_dir   <- tempfile()
    dir.create(tmp_dir, recursive=TRUE, showWarnings=FALSE)
    extracted <- tryCatch(
      unzip(filepath, files=xml_entry, exdir=tmp_dir, overwrite=TRUE),
      error=function(e) NULL)
    xml_path <- file.path(tmp_dir, xml_entry)
    if (is.null(extracted) || !file.exists(xml_path)) {
      unlink(tmp_dir, recursive=TRUE)
      return(list(type="script",
        lines=c("[Could not open document — may be encrypted or password-protected]"),
        lang="text", meta=paste0(toupper(ext), "  ·  ", round(sz/1024,1), " KB")))
    }
    xml_raw  <- tryCatch(paste(readLines(xml_path, warn=FALSE), collapse=" "), error=function(e) "")
    plain    <- trimws(gsub("[[:space:]]+", " ", gsub("<[^>]+>", " ", xml_raw)))
    unlink(tmp_dir, recursive=TRUE)
    if (nchar(plain) == 0) plain <- "[No text extracted]"
    # Wrap into lines of ~100 chars for display
    words    <- unlist(strsplit(plain, " "))
    lines    <- character(0); cur <- ""
    for (w in words) {
      if (nchar(cur) + nchar(w) + 1 > 100) { lines <- c(lines, cur); cur <- w }
      else cur <- if (nchar(cur)==0) w else paste(cur, w)
    }
    if (nchar(cur) > 0) lines <- c(lines, cur)
    list(type="script", lines=head(lines, 300), lang="text",
         meta=paste0(toupper(ext), " document  ·  ~",
           length(unlist(strsplit(plain,"[[:space:]]+")))," words  ·  ",
           round(sz/1024,1)," KB  ·  first 300 lines shown"))
  }

  # ── Serialised R objects ──────────────────────────────────────────────────
  else if (ext %in% c("rds","rdata","rda")) {
    sz_mb <- sz / 1024^2
    type_desc <- switch(ext,
      rds   = "Single R object (.rds)",
      rdata = , rda = "R workspace / multiple objects (.RData/.rda)",
      "Serialised R object")
    lines <- c(
      paste0("File type : ", type_desc),
      paste0("Size      : ", round(sz_mb, 2), " MB"),
      "",
      "[Content cannot be previewed without deserialisation]",
      "",
      "This file must be manually reviewed before egress.",
      "Open in R and inspect the object with str() or summary()",
      "to confirm it does not contain participant-level data.",
      "",
      "Suggested R commands:",
      paste0('  obj <- readRDS("', basename(filepath), '")'),
      "  str(obj)",
      "  # Check for data frames with many rows:",
      "  if (is.data.frame(obj)) cat(nrow(obj), 'rows,', ncol(obj), 'cols')"
    )
    list(type="script", lines=lines, lang="text",
         meta=paste0(type_desc, "  ·  ", round(sz_mb,2), " MB"))
  }

  # ── SQLite databases ──────────────────────────────────────────────────────
  else if (ext %in% c("db","sqlite","sqlite3")) {
    sz_mb  <- sz / 1024^2
    magic  <- tryCatch(rawToChar(readBin(filepath,"raw",n=15)),
                       error=function(e) "")
    confirmed <- grepl("SQLite format", magic, fixed=TRUE)
    lines <- c(
      paste0("File type : SQLite database (.", ext, ")"),
      paste0("Size      : ", round(sz_mb, 2), " MB"),
      paste0("Confirmed : ", if (confirmed) "Yes — SQLite magic bytes detected" else "Magic bytes unclear"),
      "",
      "[Database content cannot be previewed here]",
      "",
      "This file is classified RED. Databases can contain",
      "multiple tables of participant-level data and cannot",
      "be egressed without explicit inspection of all tables.",
      "",
      "To inspect in R (if RSQLite is available):",
      paste0('  con <- DBI::dbConnect(RSQLite::SQLite(), "', basename(filepath), '")'),
      "  DBI::dbListTables(con)",
      "  DBI::dbDisconnect(con)"
    )
    list(type="script", lines=lines, lang="text",
         meta=paste0("SQLite database  ·  ", round(sz_mb,2), " MB",
                     if (confirmed) "  ·  confirmed" else "  ·  unconfirmed"))
  }

  # ── Columnar formats (Parquet / Feather / Arrow) ──────────────────────────
  else if (ext %in% c("parquet","feather","arrow")) {
    sz_mb  <- sz / 1024^2
    magic4 <- tryCatch(readBin(filepath,"raw",n=8), error=function(e) raw(0))
    magic_s <- tryCatch(rawToChar(magic4[1:min(4,length(magic4))]),
                        error=function(e) "")
    fmt <- if (magic_s == "PAR1") "Parquet"
           else if (magic_s == "FEA1") "Feather v1"
           else if (grepl("ARROW", rawToChar(magic4), fixed=TRUE)) "Arrow IPC / Feather v2"
           else paste0(toupper(ext), " (format unconfirmed)")
    lines <- c(
      paste0("File type : ", fmt),
      paste0("Extension : .", ext),
      paste0("Size      : ", round(sz_mb, 2), " MB"),
      "",
      "[Content cannot be previewed without the arrow package]",
      "",
      "Columnar formats can hold large research datasets.",
      "Manual review is required before egress.",
      "",
      "To inspect in R (if arrow package is available):",
      paste0('  tbl <- arrow::read_parquet("', basename(filepath), '")'),
      "  # or: arrow::read_feather(...) for Feather files",
      "  dplyr::glimpse(tbl)",
      "  nrow(tbl)"
    )
    list(type="script", lines=lines, lang="text",
         meta=paste0(fmt, "  ·  ", round(sz_mb,2), " MB"))
  }

  # ── DICOM medical image ──────────────────────────────────────────────────
  else if (ftype == "dicom" || ext %in% c("dcm","dicom")) {
    if (!ORODICOM_OK) {
      return(list(type="script",
        lines=c("[oro.dicom not installed — run dependencies.R to enable DICOM preview]"),
        lang="text", meta=paste0("DICOM  \u00b7  ", round(sz/1024,1), " KB")))
    }
    # Read header
    hdr <- tryCatch(
      oro.dicom::readDICOMFile(filepath, pixelData=FALSE)$hdr,
      error=function(e) NULL)

    if (is.null(hdr)) {
      return(list(type="script",
        lines=c("[DICOM file could not be parsed]"),
        lang="text", meta=paste0("DICOM  \u00b7  ", round(sz/1024,1), " KB")))
    }

    get_tag_pv <- function(group, element) {
      grp <- toupper(sprintf("%04X", as.integer(paste0("0x", group))))
      el  <- toupper(sprintf("%04X", as.integer(paste0("0x", element))))
      rows <- hdr[toupper(hdr$group)==grp & toupper(hdr$element)==el,]
      if (nrow(rows)==0) return("")
      trimws(paste(rows$value, collapse=" "))
    }

    modality   <- get_tag_pv("0008","0060")
    patient_nm <- get_tag_pv("0010","0010")
    study_date <- get_tag_pv("0008","0020")
    desc       <- get_tag_pv("0008","103E")
    rows_px    <- suppressWarnings(as.integer(get_tag_pv("0028","0010")))
    cols_px    <- suppressWarnings(as.integer(get_tag_pv("0028","0011")))
    n_frames   <- suppressWarnings(as.integer(get_tag_pv("0028","0008")))

    # Try to read and render pixel data as PNG
    img_b64 <- tryCatch({
      dcm  <- oro.dicom::readDICOMFile(filepath, pixelData=TRUE)
      pix  <- dcm$img
      if (is.null(pix) || length(pix) == 0) stop("no pixel data")

      # For multi-frame, take the middle frame
      if (length(dim(pix)) == 3) {
        mid  <- ceiling(dim(pix)[3] / 2)
        pix  <- pix[,,mid]
      }

      # Normalise to 0–1
      pmin <- min(pix, na.rm=TRUE)
      pmax <- max(pix, na.rm=TRUE)
      if (pmax > pmin) pix <- (pix - pmin) / (pmax - pmin)

      # Render to temp PNG and base64 encode
      tmp <- tempfile(fileext=".png")
      on.exit(unlink(tmp), add=TRUE)
      grDevices::png(tmp, width=512, height=512, bg="black")
      graphics::par(mar=c(0,0,0,0))
      graphics::image(t(pix[nrow(pix):1,]), col=grDevices::grey(seq(0,1,length=256)),
                      axes=FALSE, asp=1)
      grDevices::dev.off()
      base64enc::base64encode(tmp)
    }, error=function(e) NULL)

    # ── Fallback: extract embedded JPEG for compressed transfer syntaxes ──────
    # oro.dicom cannot decode JPEG-compressed pixel data (e.g. XC photographs,
    # JPEG Baseline transfer syntax). Extract the raw JPEG from the file bytes.
    if (is.null(img_b64)) {
      img_b64 <- tryCatch({
        sz_file <- file.info(filepath)$size
        raw_bytes <- as.integer(readBin(filepath, "raw", n=sz_file))
        n <- length(raw_bytes)

        # Find first JPEG SOI (FF D8 FF) after byte 1000 (skip DICOM header)
        soi_idx <- which(
          raw_bytes[seq_len(n-2)]     == 0xff &
          raw_bytes[seq_len(n-2) + 1] == 0xd8 &
          raw_bytes[seq_len(n-2) + 2] == 0xff
        )
        soi_idx <- soi_idx[soi_idx > 1000L]
        if (length(soi_idx) == 0) stop("no JPEG SOI found")
        soi <- soi_idx[1]

        # Find last JPEG EOI (FF D9)
        eoi_idx <- which(
          raw_bytes[seq_len(n-1)]     == 0xff &
          raw_bytes[seq_len(n-1) + 1] == 0xd9
        )
        eoi_idx <- eoi_idx[eoi_idx > soi]
        if (length(eoi_idx) == 0) stop("no JPEG EOI found")
        eoi <- tail(eoi_idx, 1) + 1L  # inclusive of the 0xd9 byte

        # Extract JPEG bytes and encode
        jpeg_raw <- readBin(filepath, "raw", n=sz_file)[soi:eoi]
        paste0("data:image/jpeg;base64,",
               base64enc::base64encode(jpeg_raw))
      }, error=function(e) NULL)
      # Flag that this is already a data URI (not a PNG to prefix later)
      jpeg_direct <- !is.null(img_b64)
    } else {
      jpeg_direct <- FALSE
    }

    if (!is.null(img_b64) && !jpeg_direct)
      img_b64 <- paste0("data:image/png;base64,", img_b64)

    # Build tag summary for metadata display
    tag_lines <- c(
      if (nzchar(modality))   paste0("Modality         : ", modality),
      if (nzchar(patient_nm)) paste0("PatientName      : ", patient_nm),
      if (nzchar(study_date)) paste0("StudyDate        : ", study_date),
      if (nzchar(desc))       paste0("SeriesDescription: ", desc),
      if (!is.na(rows_px) && !is.na(cols_px))
        paste0("Image dimensions : ", cols_px, " \u00d7 ", rows_px, " px"),
      if (!is.na(n_frames) && n_frames > 1)
        paste0("Frames/slices    : ", n_frames)
    )

    meta <- paste0("DICOM  \u00b7  ",
      if (nzchar(modality)) paste0(modality, "  \u00b7  ") else "",
      round(sz/1024,1), " KB",
      if (!is.na(n_frames) && n_frames > 1) paste0("  \u00b7  ", n_frames, " frames") else "")

    if (!is.null(img_b64)) {
      list(type="dicom_image",
           src=img_b64,
           tag_lines=tag_lines,
           meta=meta)
    } else {
      list(type="script",
           lines=c("[Pixel data could not be rendered — JPEG-compressed transfer syntax]",
                   "[Use a DICOM viewer or de-identification tool to inspect this file]",
                   "", tag_lines),
           lang="text", meta=meta)
    }
  }

  else if (ftype == "nifti") {
    # Read header (fast, no pixel data)
    hdr <- tryCatch(read_nifti_header(filepath), error=function(e) NULL)

    # Attempt pixel slice render — direct binary read avoids oro.nifti vox_offset issues.
    # nibabel writes vox_offset=0 (valid per spec, treated as 352), but oro.nifti
    # may not apply the fallback. We read the decompressed stream sequentially:
    # skip 352 bytes (348 header + 4 extension), then read the voxel data.
    img_b64 <- if (!is.null(hdr) && !is.null(hdr$dim3) && all(hdr$dim3 > 0)) {
      tryCatch({
        # Datatype -> readBin parameters
        dtype_map <- list(
          `2`=list(sz=1L,  tp="integer", sg=FALSE),  # UINT8
          `4`=list(sz=2L,  tp="integer", sg=TRUE),   # INT16
          `8`=list(sz=4L,  tp="integer", sg=TRUE),   # INT32
          `16`=list(sz=4L, tp="double",  sg=TRUE),   # FLOAT32
          `64`=list(sz=8L, tp="double",  sg=TRUE),   # FLOAT64
          `256`=list(sz=1L,tp="integer", sg=TRUE),   # INT8
          `512`=list(sz=2L,tp="integer", sg=FALSE)   # UINT16
        )
        dt <- dtype_map[[as.character(hdr$datatype)]]
        if (is.null(dt)) dt <- list(sz=2L, tp="integer", sg=TRUE)

        # Read entire file into memory then decompress — avoids gzfile connection
        # buffering issues. memDecompress is base R (no packages needed).
        is_gz    <- grepl("[.]nii[.]gz$", filepath, ignore.case=TRUE)
        raw_file <- readBin(filepath, "raw", n=file.info(filepath)$size)
        raw_data <- if (is_gz) memDecompress(raw_file, type="gzip") else raw_file

        # Data starts at vox_offset; treat 0 as 352 (NIfTI spec default)
        voff <- if (!is.null(hdr$vox_off) && hdr$vox_off >= 352L) hdr$vox_off else 352L
        nx <- hdr$dim3[1]; ny <- hdr$dim3[2]; nz <- hdr$dim3[3]
        n_vox   <- nx * ny * nz
        n_bytes <- n_vox * dt$sz
        if (length(raw_data) < voff + n_bytes)
          stop("file too small for declared dimensions")

        pix_raw <- raw_data[seq(voff + 1L, voff + n_bytes)]
        pix_vec <- readBin(pix_raw, dt$tp, n=n_vox, size=dt$sz,
                           endian=hdr$endian, signed=dt$sg)
        arr <- array(as.numeric(pix_vec), dim=c(nx, ny, nz))

        # Mid axial slice, y-flipped for standard orientation
        mid_z <- max(1L, floor(nz / 2))
        sl    <- arr[, ny:1, mid_z]

        # Percentile windowing (1st-99th) for good contrast.
        # pmax/pmin strip matrix attributes in some R builds — re-wrap explicitly.
        p_lo <- as.numeric(quantile(sl, 0.01, na.rm=TRUE))
        p_hi <- as.numeric(quantile(sl, 0.99, na.rm=TRUE))
        sl <- if (p_hi > p_lo)
          matrix(pmax(0, pmin(1, (sl - p_lo) / (p_hi - p_lo))), nrow=nx, ncol=ny)
        else
          matrix(0.5, nrow=nx, ncol=ny)

        tmp <- tempfile(fileext=".png")
        on.exit(unlink(tmp), add=TRUE)
        grDevices::png(tmp, width=512, height=512, bg="black")
        graphics::par(mar=c(0,0,0,0))
        graphics::image(sl, col=grDevices::grey(seq(0, 1, length.out=256)),
                        axes=FALSE, asp=1)
        grDevices::dev.off()
        paste0("data:image/png;base64,", base64enc::base64encode(tmp))
      }, error=function(e) stop(paste("NIfTI render:", conditionMessage(e))))
    } else NULL

    # Metadata rows from binary header
    meta_rows <- if (!is.null(hdr)) list(
      c("Format",       if (isTRUE(hdr$nifti2)) "NIfTI-2" else "NIfTI-1"),
      c("Dimensions",   if (!is.null(hdr$dim3)) paste(hdr$dim3, collapse=" \u00d7 ") else "\u2014"),
      c("N dimensions", as.character(hdr$ndim)),
      c("Description",  if (nzchar(hdr$descrip)) hdr$descrip else "(empty)"),
      c("Aux file",     if (nzchar(hdr$aux)) hdr$aux else "(empty)"),
      c("Intent name",  if (nzchar(hdr$intent_name)) hdr$intent_name else "(empty)")
    ) else NULL

    list(type=if (!is.null(img_b64)) "nifti_image" else "nifti",
         src=img_b64,
         meta_rows=meta_rows,
         filepath=filepath,
         meta=paste0("NIfTI  \u00b7  ", round(sz/1024,1), " KB",
                     if (grepl("[.]gz$", filepath)) "  \u00b7  gzip-compressed" else "",
                     if (!is.null(img_b64)) "  \u00b7  axial mid-slice" else
                       "  \u00b7  header only"))
  }
  else if (ftype == "statistical") {
    if (!HAVEN_OK) {
      return(list(type="statistical", data=NULL,
        error="haven not installed \u2014 run dependencies.R to enable Stata/SAS/SPSS preview",
        meta=paste0(toupper(ext), "  \u00b7  ", round(sz/1024,1), " KB")))
    }
    df <- tryCatch({
      if      (ext == "dta")      haven::read_dta(filepath)
      else if (ext == "sav")      haven::read_sav(filepath)
      else if (ext == "sas7bdat") haven::read_sas(filepath)
      else NULL
    }, error=function(e) NULL)
    if (is.null(df)) {
      return(list(type="statistical", data=NULL,
        error=paste0("Could not read ", toupper(ext), " file."),
        meta=paste0(toupper(ext), "  \u00b7  ", round(sz/1024,1), " KB")))
    }
    var_labels <- do.call(rbind, lapply(names(df), function(col) {
      lbl <- attr(df[[col]], "label")
      if (!is.null(lbl) && nzchar(trimws(as.character(lbl))))
        data.frame(col=col, label=as.character(lbl), stringsAsFactors=FALSE)
      else NULL
    }))
    df_plain <- as.data.frame(lapply(df, function(col) {
      if (inherits(col, "haven_labelled")) as.character(haven::as_factor(col))
      else if (is.numeric(col)) col
      else as.character(col)
    }), stringsAsFactors=FALSE)
    list(type="statistical",
         data=head(df_plain, 200),
         var_labels=var_labels,
         meta=paste0(toupper(ext), "  \u00b7  ",
                     nrow(df), " rows  \u00b7  ", ncol(df), " cols  \u00b7  ",
                     round(sz/1024,1), " KB",
                     if (!is.null(var_labels) && nrow(var_labels)>0)
                       paste0("  \u00b7  ", nrow(var_labels), " variable labels") else ""))
  }

  else {
    list(type="unsupported",
         meta=paste0(toupper(tools::file_ext(filepath)),
                     " - preview not available  \u00b7  ", round(sz/1024,1), " KB"))
  }
}

# Render preview content as Shiny HTML
