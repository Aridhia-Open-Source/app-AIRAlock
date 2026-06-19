# Image inspector (PNG, JPEG, SVG, etc.)
# Auto-split from app.R — do not edit the monolithic file

inspect_image <- function(filepath, cfg=list()) {
  eid_digits    <- cfg$img002_eid_digits   %||% 7L
  patient_check <- cfg$img002_patient_word %||% TRUE
  extra_words   <- cfg$img002_extra_words  %||% character(0)
  plot_keywords <- cfg$img004_keywords     %||% "circos|igv|oncoplot|tmb|mutational"

  hits <- list()
  name <- tolower(basename(filepath))
  ext  <- tolower(tools::file_ext(filepath))

  if (ext == "svg") {
    src    <- tryCatch(paste(readLines(filepath, warn=FALSE), collapse="\n"), error=function(e) "")
    svg_ev <- list(type="svg", src=src, caption=paste0("SVG preview - ", basename(filepath)))
    if (grepl("onclick|javascript|<script|plotly|highcharts|d3\\.js|interactive", src, ignore.case=TRUE))
      hits <- append(hits, list(list(rule="IMG-001", outcome="RED",
        detail="SVG contains JavaScript/interactive elements (onclick, <script>, Plotly/D3 references)",
        evidence=svg_ev)))
    eid_pat <- paste0("\\b[0-9]{", eid_digits, "}\\b")
    if (grepl(eid_pat, src))
      hits <- append(hits, list(list(rule="IMG-002", outcome="RED",
        detail=paste0("SVG text layer contains ", eid_digits, "-digit numbers consistent with participant identifier values"),
        evidence=svg_ev)))
    if (patient_check && grepl("\\bpatient\\b", src, ignore.case=TRUE))
      hits <- append(hits, list(list(rule="IMG-002", outcome="RED",
        detail="SVG text layer contains the word 'patient' - potential participant annotation",
        evidence=svg_ev)))
    for (w in extra_words[nchar(trimws(extra_words)) > 0]) {
      wpat <- paste0("\\b", trimws(w), "\\b")
      if (grepl(wpat, src, ignore.case=TRUE))
        hits <- append(hits, list(list(rule="IMG-002", outcome="RED",
          detail=paste0("SVG text layer contains configured trigger word '", trimws(w), "'"),
          evidence=svg_ev)))
    }
    if (grepl(plot_keywords, name))
      hits <- append(hits, list(list(rule="IMG-004", outcome="AMBER",
        detail=paste0("Filename suggests restricted genetic visualisation: '", basename(filepath), "'"),
        evidence=svg_ev)))
  } else {
    # Raster image (PNG, JPEG, BMP, TIFF) \u2014 pixel content cannot be read
    # without OCR. Check filename only, then always flag AMBER.
    if (grepl(plot_keywords, name))
      hits <- append(hits, list(list(rule="IMG-004", outcome="AMBER",
        detail=paste0("Filename suggests restricted genetic visualisation: '", basename(filepath), "'"))))
    if (grepl("participant|patient|subject", name))
      hits <- append(hits, list(list(rule="IMG-002", outcome="RED",
        detail=paste0("Filename contains participant identifier reference: '", basename(filepath), "'"))))
    # Always add AMBER \u2014 raster pixel content cannot be automatically inspected.
    # Text, annotations, or demographic information burned into pixels are invisible
    # without OCR. Reviewer must visually inspect the image before approving egress.
    hits <- append(hits, list(list(rule="IMG-003", outcome="AMBER",
      detail=paste0(
        toupper(tools::file_ext(filepath)), " raster image \u2014 pixel content cannot be automatically inspected. ",
        "Text, patient annotations, or demographic information burned into the image are not ",
        "detectable without OCR. Reviewer must visually inspect this file before approving egress."))))
  }
  hits
}
