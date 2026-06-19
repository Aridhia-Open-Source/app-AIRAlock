# File type detection, labels, and icons
# Auto-split from app.R - do not edit the monolithic file

# ============================================================
# FILE TYPE DETECTION
# ============================================================
detect_file_type <- function(filepath) {
  ext  <- tolower(tools::file_ext(filepath))
  name <- tolower(basename(filepath))
  if (ext %in% c("csv","tsv","txt","xlsx","xls","ods")) {
    if (grepl("vcf|gwas|variant|genomic|plink|bim|fam", name) && ext=="txt") return("genomic")
    return("tabular")
  }
  if (ext %in% c("vcf","bcf","bim","fam","bed","pgen","psam","pvar","sam")) return("genomic")
  if (ext %in% c("r","rmd","py","ipynb","jl","sh","qmd","sql")) return("script")
  if (ext == "nii") return("nifti")
  # .nii.gz - tools::file_ext() returns "gz"; detect by full name before archive check
  if (ext == "gz" && grepl("[.]nii[.]gz$", name)) return("nifti")
  if (ext %in% c("zip","tar","gz","tgz","bz2","rar","7z")) return("archive")
  if (ext %in% c("png","jpg","jpeg","svg","tiff","bmp")) return("image")
  if (ext %in% c("pdf")) return("document")
  if (ext %in% c("json")) {
    # ACRO results.json detection must precede generic JSON. ACRO results
    # ARE valid JSON but should dispatch to the ACRO inspector, not the
    # generic JSON inspector. is_acro_results_file() is defined in
    # 28_inspect_acro.R and parses the file's structure to confirm the
    # ACRO schema (version + config.safe_threshold + results). Guarded so
    # detection still works if the ACRO module is not sourced.
    if (exists("is_acro_results_file", mode = "function") &&
        is_acro_results_file(filepath)) {
      return("acro_results")
    }
    return("json")
  }
  if (ext %in% c("xml")) return("xml")
  if (ext %in% c("md","markdown")) return("markdown")
  if (ext %in% c("html","htm")) return("webpage")
  if (ext %in% c("docx","odt","odp")) return("office")
  if (ext %in% c("doc")) return("office")
  if (ext %in% c("rds","rdata","rda")) return("serialised")
  if (ext %in% c("db","sqlite","sqlite3")) return("database")
  if (ext %in% c("parquet","feather","arrow")) return("columnar")
  if (ext %in% c("dcm","dicom")) return("dicom")
  if (ext %in% c("dta","sav","sas7bdat")) return("statistical")
  return("binary")
}

type_label <- function(t) switch(t,
  tabular="Tabular Data", genomic="Genomic", script="Script / Notebook",
  archive="Archive", image="Image / Plot", document="PDF Document",
  json="JSON Data", xml="XML Document", markdown="Markdown",
  webpage="HTML / Web Page", office="Office Document (.docx/.odt)", serialised="Serialised R Object", database="Database File",
  columnar="Columnar Data (Parquet/Feather)",
  dicom="Medical Image (DICOM)", nifti="Medical Image (NIfTI)", statistical="Statistical Data (Stata/SAS/SPSS)",
  acro_results="ACRO Results (SDC Session)", binary="Binary / Unknown", "Unknown")

type_icon <- function(t) switch(t,
  tabular="CSV", genomic="DNA", script="CODE", archive="ZIP",
  image="IMG", document="PDF", office="DOC", webpage="HTM",
  json="JSON", xml="XML", markdown="MD", serialised="RDS", database="SQL",
  columnar="PAR", dicom="DCM", nifti="NII", statistical="STAT",
  acro_results="ACRO", binary="BIN", "???")