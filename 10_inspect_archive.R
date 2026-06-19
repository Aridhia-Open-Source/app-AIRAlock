# Archive inspector (ZIP, TAR, etc.)
# Auto-split from app.R — do not edit the monolithic file

inspect_archive <- function(filepath, cfg=list()) {
  name <- tolower(basename(filepath))
  ext  <- tolower(tools::file_ext(filepath))
  hits <- list()

  # ARC-001 — encryption markers in filename
  if (grepl("encrypt|password|protected|aes", name)) {
    hits <- append(hits, list(list(rule="ARC-001", outcome="RED",
      detail="Archive filename indicates encryption or password protection — contents cannot be inspected")))
    return(hits)
  }

  # Try to read manifest without extracting
  arc_thresh <- cfg$arc005_max_files %||% 50L
  hi_risk_ext   <- c("dta","sas7bdat","sav","db","sqlite","sqlite3","vcf","bam","rds","rdata","rda","nii")
  hi_risk_pat   <- paste0("\\.(", paste(hi_risk_ext, collapse="|"), ")(\\.gz)?$")
  scan_ext      <- c("csv","xlsx","xls","tsv","r","rmd","py","sql","ipynb","qmd")
  scan_pat      <- paste0("\\.(", paste(scan_ext, collapse="|"), ")$")

  manifest <- tryCatch({
    if (ext %in% c("zip","jar","xlsx","docx")) {
      fl <- unzip(filepath, list=TRUE)
      fl$Name
    } else if (ext %in% c("tar","tgz","gz","bz2")) {
      untar(filepath, list=TRUE)
    } else character(0)
  }, error=function(e) character(0))

  if (length(manifest) > 0) {
    mn_lower <- tolower(manifest)

    # ARC-003 — high-risk file types in manifest
    hi_matches <- manifest[grepl(hi_risk_pat, mn_lower, perl=TRUE)]
    if (length(hi_matches) > 0) {
      hi_ev_lines <- head(hi_matches, 20)
      ev <- list(type="lines",
               lines=lapply(seq_along(hi_ev_lines), function(.i)
                 list(lineno=.i, text=hi_ev_lines[[.i]], flag=TRUE)),
               caption=paste0("High-risk files in archive manifest (",
                              length(hi_matches), " file(s))"))
      hits <- append(hits, list(list(rule="ARC-003", outcome="RED",
        detail=paste0("Archive contains ", length(hi_matches),
          " high-risk file(s) that cannot be inspected through the archive wrapper: ",
          paste(head(basename(hi_matches), 5), collapse=", "),
          if (length(hi_matches) > 5) paste0(" (+", length(hi_matches)-5, " more)") else ""),
        evidence=ev)))
    }

    # ARC-004 — tabular/script files (only if ARC-003 not already RED)
    if (!any(sapply(hits, function(h) h$rule == "ARC-003"))) {
      scan_matches <- manifest[grepl(scan_pat, mn_lower, perl=TRUE)]
      if (length(scan_matches) > 0) {
        scan_ev_lines <- head(scan_matches, 20)
        ev <- list(type="lines",
               lines=lapply(seq_along(scan_ev_lines), function(.i)
                 list(lineno=.i, text=scan_ev_lines[[.i]], flag=TRUE)),
               caption=paste0("Tabular/script files in manifest (",
                              length(scan_matches), " file(s))"))
        hits <- append(hits, list(list(rule="ARC-004", outcome="AMBER",
          detail=paste0("Archive contains ", length(scan_matches),
            " tabular or script file(s) requiring individual inspection: ",
            paste(head(basename(scan_matches), 5), collapse=", "),
            if (length(scan_matches) > 5) paste0(" (+", length(scan_matches)-5, " more)") else ""),
          evidence=ev)))
      }
    }

    # ARC-005 — large archive (only if no higher-priority rule fired)
    if (length(hits) == 0 && length(manifest) > arc_thresh) {
      hits <- append(hits, list(list(rule="ARC-005", outcome="AMBER",
        detail=paste0("Archive contains ", length(manifest),
          " files (threshold: ", arc_thresh, "). Large archives may obscure content volume."))))
    }
  }

  # ARC-002 — fallback if no specific rule fired
  if (length(hits) == 0) {
    man_note <- if (length(manifest) > 0)
      paste0(" Manifest lists ", length(manifest), " file(s).")
    else
      " Manifest could not be read automatically."
    hits <- append(hits, list(list(rule="ARC-002", outcome="AMBER",
      detail=paste0("Standard archive — contents must be unpacked and each file individually ",
        "assessed by Airlock Checker before a decision.", man_note))))
  }

  hits
}
