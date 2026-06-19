# Statistical file inspector (Stata, SAS, SPSS)
# Auto-split from app.R — do not edit the monolithic file

inspect_statistical <- function(filepath, cfg=list()) {
  ext  <- tolower(tools::file_ext(filepath))
  name <- basename(filepath)

  if (!HAVEN_OK)
    return(list(list(rule="STAT-002", outcome="AMBER",
      detail="haven R package is not installed. Install it via dependencies.R to enable inspection of Stata/SAS/SPSS files.")))

  df <- tryCatch({
    if      (ext == "dta")       haven::read_dta(filepath)
    else if (ext == "sav")       haven::read_sav(filepath)
    else if (ext == "sas7bdat")  haven::read_sas(filepath)
    else stop("Unknown statistical extension")
  }, error=function(e) NULL)

  if (is.null(df))
    return(list(list(rule="STAT-001", outcome="AMBER",
      detail=paste0("Could not read statistical file '", name, "'. ",
        "File may be corrupt, use a version not supported by haven, or be password-protected."))))

  # Write to temp CSV so all existing tabular rules run unchanged
  tmp <- tempfile(fileext=".csv")
  on.exit(tryCatch(unlink(tmp), error=function(e) NULL), add=TRUE)
  df_capped <- if (nrow(df) > 500) df[seq_len(500), ] else df
  tryCatch(write.csv(df_capped, tmp, row.names=FALSE), error=function(e) NULL)

  hits <- if (file.exists(tmp))
    tryCatch(inspect_tabular(tmp, cfg), error=function(e) list())
  else list()

  # TAB-017 — value labels and variable labels in haven metadata
  sens_ph <- cfg$sensitive_phenotypes %||% sensitive_phenotypes
  ph_pat  <- paste(sens_ph, collapse="|")
  label_hits <- character(0)

  for (col in names(df)) {
    var_lbl <- attr(df[[col]], "label")
    if (!is.null(var_lbl) && nzchar(trimws(as.character(var_lbl)))) {
      lbl_str <- as.character(var_lbl)
      if (grepl(ph_pat, lbl_str, ignore.case=TRUE))
        label_hits <- c(label_hits, paste0("Variable label '", col, "': ", lbl_str))
    }
    val_lbls <- attr(df[[col]], "labels")
    if (!is.null(val_lbls) && length(names(val_lbls)) > 0) {
      for (ln in names(val_lbls))
        if (grepl(ph_pat, ln, ignore.case=TRUE))
          label_hits <- c(label_hits, paste0("Value label in '", col, "': ", ln))
    }
  }

  if (length(label_hits) > 0) {
    ev <- list(type="lines",
               lines=lapply(seq_along(label_hits), function(.i)
                 list(lineno=.i, text=label_hits[[.i]], flag=TRUE)),
               caption="Sensitive phenotype terms in embedded statistical label metadata")
    hits <- append(hits, list(list(rule="TAB-017", outcome="RED",
      detail=paste0("Embedded label metadata contains ", length(label_hits),
        " sensitive phenotype reference(s). Label metadata exposes sensitive categories ",
        "even when column values are numeric codes."),
      evidence=ev)))
  }

  hits
}
