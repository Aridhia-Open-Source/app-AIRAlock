# ============================================================
# Airlock Checker — Robustness Fixture Generator
# ============================================================
# Writes pathological test fixtures into tests/fixtures/robustness/.
# These exercise inspector error handling, not rule logic.
#
# Called automatically by run_tests.R alongside build_fixtures.R.
# ============================================================

build_robustness_fixtures <- function(fixture_dir) {
  rdir <- file.path(fixture_dir, "robustness")
  if (!dir.exists(rdir)) dir.create(rdir, recursive = TRUE, showWarnings = FALSE)

  # ── Empty files ────────────────────────────────────────────────────────
  # Zero bytes: no headers, no rows — most parsers treat this differently.
  writeBin(raw(0), file.path(rdir, "empty.csv"))
  writeBin(raw(0), file.path(rdir, "empty.vcf"))
  writeBin(raw(0), file.path(rdir, "empty.json"))
  writeBin(raw(0), file.path(rdir, "empty.R"))

  # ── Single byte / very small ──────────────────────────────────────────
  writeBin(charToRaw("x"), file.path(rdir, "one_byte.csv"))
  writeLines("", file.path(rdir, "blank_line.csv"))
  writeLines("header_only", file.path(rdir, "header_only.csv"))

  # ── Truncated mid-record ──────────────────────────────────────────────
  # CSV truncated in the middle of a row (no final newline, short field)
  cat("id,name,age\n1,Alice,30\n2,Bob,25\n3,Char",
      file = file.path(rdir, "truncated.csv"))

  # VCF truncated inside the header
  cat("##fileformat=VCFv4.2\n##INFO=<ID=AF,Number=A,Type=Float,Descri",
      file = file.path(rdir, "truncated.vcf"))

  # JSON with unclosed brace
  cat('{"name":"test","values":[1,2,3',
      file = file.path(rdir, "truncated.json"))

  # ── Wrong extension (binary content as text format) ───────────────────
  # 1024 random bytes with .csv extension — must not be read as CSV
  set.seed(1)
  writeBin(as.raw(sample(0:255, 1024, replace = TRUE)),
           file.path(rdir, "binary_as.csv"))
  writeBin(as.raw(sample(0:255, 1024, replace = TRUE)),
           file.path(rdir, "binary_as.json"))
  writeBin(as.raw(sample(0:255, 1024, replace = TRUE)),
           file.path(rdir, "binary_as.vcf"))

  # ── Invalid UTF-8 ─────────────────────────────────────────────────────
  # Byte sequences that are not valid UTF-8
  bad_utf8 <- c(charToRaw("col1,col2\n"),
                as.raw(c(0xFF, 0xFE, 0xC3, 0x28, 0x80, 0x81)),
                charToRaw(",valid\n"))
  writeBin(bad_utf8, file.path(rdir, "bad_utf8.csv"))

  # ── Embedded NUL bytes in text content ────────────────────────────────
  nul_content <- c(charToRaw("col1,col2\nhello"),
                   as.raw(0x00),
                   charToRaw("world,value\n"))
  writeBin(nul_content, file.path(rdir, "embedded_nul.csv"))

  # ── Mismatched row lengths ────────────────────────────────────────────
  writeLines(c("a,b,c", "1,2,3", "4,5", "6,7,8,9,10", "11,12,13"),
             file.path(rdir, "mismatched_rows.csv"))

  # ── Duplicate column names ────────────────────────────────────────────
  writeLines(c("id,value,value,value",
               "1,10,20,30", "2,40,50,60"),
             file.path(rdir, "duplicate_cols.csv"))

  # ── Pathologically wide (200 columns, 10 rows) ────────────────────────
  # Kept modest — true 10k-col fixtures belong in stress mode
  wide_hdr <- paste(sprintf("col%03d", 1:200), collapse = ",")
  wide_row <- paste(sample(1:100, 200, replace = TRUE), collapse = ",")
  writeLines(c(wide_hdr, rep(wide_row, 10)),
             file.path(rdir, "wide_200cols.csv"))

  # ── Single-column, many rows ──────────────────────────────────────────
  writeLines(c("x", as.character(1:1000)),
             file.path(rdir, "single_col_1000rows.csv"))

  # ── Column with all NA ────────────────────────────────────────────────
  writeLines(c("a,b,c",
               paste(rep(c("NA,,NA"), 20), collapse = "\n")),
             file.path(rdir, "all_na.csv"))

  # ── Column with a single unique value ─────────────────────────────────
  writeLines(c("category,n", paste0("X,", rep(5, 20))),
             file.path(rdir, "all_same.csv"))

  # ── Extremely long cell value ─────────────────────────────────────────
  long_val <- paste(rep("x", 50000), collapse = "")
  writeLines(c("id,notes", paste0("1,", long_val)),
             file.path(rdir, "long_cell.csv"))

  # ── Filename with spaces ──────────────────────────────────────────────
  # Some code paths split on spaces — this catches that
  writeLines(c("a,b", "1,2"),
             file.path(rdir, "file with spaces.csv"))

  # ── Script robustness ─────────────────────────────────────────────────
  # R script that is itself malformed
  writeLines(c("library(dplyr)", "x <- function(y) {", "  return(y"),
             file.path(rdir, "malformed.R"))
  # Very long single line
  writeLines(paste0("x <- c(", paste(1:10000, collapse=","), ")"),
             file.path(rdir, "long_line.R"))

  # ── Archive edge cases ────────────────────────────────────────────────
  # Not a real zip — just ZIP magic bytes followed by garbage
  fake_zip <- c(as.raw(c(0x50, 0x4B, 0x03, 0x04)),  # PK\003\004
                as.raw(sample(0:255, 500, replace = TRUE)))
  writeBin(fake_zip, file.path(rdir, "corrupt.zip"))

  # ── Genomic edge cases ────────────────────────────────────────────────
  # VCF with header only, no variants
  writeLines(c("##fileformat=VCFv4.2",
               "#CHROM\tPOS\tID\tREF\tALT\tQUAL\tFILTER\tINFO"),
             file.path(rdir, "header_only.vcf"))

  # VCF with malformed info field
  writeLines(c("##fileformat=VCFv4.2",
               "#CHROM\tPOS\tID\tREF\tALT\tQUAL\tFILTER\tINFO",
               "chr1\tnotanumber\trs1\tA\tG\t100\tPASS\tmalformed==="),
             file.path(rdir, "malformed.vcf"))

  # ======================================================================
  # Phase 2: every remaining inspector gets the same 3-fixture pattern
  # (empty, binary-as-format, truncated) plus targeted extras.
  # ======================================================================

  # ── XML ───────────────────────────────────────────────────────────────
  writeBin(raw(0), file.path(rdir, "empty.xml"))
  set.seed(10)
  writeBin(as.raw(sample(0:255, 1024, replace=TRUE)),
           file.path(rdir, "binary_as.xml"))
  cat('<?xml version="1.0"?>\n<root><item>unclo',
      file = file.path(rdir, "truncated.xml"))
  # XML with mismatched tags (valid-looking but parse error)
  writeLines(c("<?xml version=\"1.0\"?>", "<a><b></a></b>"),
             file.path(rdir, "mismatched_tags.xml"))

  # ── Markdown ──────────────────────────────────────────────────────────
  writeBin(raw(0), file.path(rdir, "empty.md"))
  set.seed(11)
  writeBin(as.raw(sample(0:255, 1024, replace=TRUE)),
           file.path(rdir, "binary_as.md"))
  # Truncated fenced code block (no closing ```)
  cat("# Title\n\nSome intro.\n\n```r\nx <- c(1,2,3)\nsum(x",
      file = file.path(rdir, "truncated.md"))

  # ── HTML / webpage ────────────────────────────────────────────────────
  writeBin(raw(0), file.path(rdir, "empty.html"))
  set.seed(12)
  writeBin(as.raw(sample(0:255, 1024, replace=TRUE)),
           file.path(rdir, "binary_as.html"))
  cat("<!DOCTYPE html>\n<html><head><title>Unclo",
      file = file.path(rdir, "truncated.html"))
  # Deeply nested HTML to stress DOM parsing
  nested_open  <- paste(rep("<div>", 500), collapse="")
  nested_close <- paste(rep("</div>", 500), collapse="")
  writeLines(c("<!DOCTYPE html><html><body>",
               nested_open, "content", nested_close,
               "</body></html>"),
             file.path(rdir, "deeply_nested.html"))

  # ── PDF ───────────────────────────────────────────────────────────────
  writeBin(raw(0), file.path(rdir, "empty.pdf"))
  # Random bytes without the %PDF- header
  set.seed(13)
  writeBin(as.raw(sample(0:255, 2048, replace=TRUE)),
           file.path(rdir, "binary_as.pdf"))
  # PDF header + garbage (has the magic bytes but nothing parseable)
  pdf_fake <- c(charToRaw("%PDF-1.4\n"),
                as.raw(sample(0:255, 500, replace=TRUE)))
  writeBin(pdf_fake, file.path(rdir, "corrupt.pdf"))
  # Truncated immediately after header
  cat("%PDF-1.4\n%", file = file.path(rdir, "truncated.pdf"))

  # ── DOCX / PPTX / XLSX (all ZIP-based OOXML) ──────────────────────────
  writeBin(raw(0), file.path(rdir, "empty.docx"))
  writeBin(raw(0), file.path(rdir, "empty.xlsx"))
  writeBin(raw(0), file.path(rdir, "empty.pptx"))
  # ZIP magic but invalid OOXML structure
  ooxml_fake <- c(as.raw(c(0x50, 0x4B, 0x03, 0x04)),
                  as.raw(sample(0:255, 500, replace=TRUE)))
  writeBin(ooxml_fake, file.path(rdir, "corrupt.docx"))
  writeBin(ooxml_fake, file.path(rdir, "corrupt.xlsx"))
  writeBin(ooxml_fake, file.path(rdir, "corrupt.pptx"))

  # ── Images (PNG / JPEG) ───────────────────────────────────────────────
  writeBin(raw(0), file.path(rdir, "empty.png"))
  writeBin(raw(0), file.path(rdir, "empty.jpg"))
  # PNG signature + garbage
  png_fake <- c(as.raw(c(0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A)),
                as.raw(sample(0:255, 200, replace=TRUE)))
  writeBin(png_fake, file.path(rdir, "corrupt.png"))
  # JPEG SOI marker + garbage
  jpg_fake <- c(as.raw(c(0xFF, 0xD8, 0xFF, 0xE0)),
                as.raw(sample(0:255, 200, replace=TRUE)))
  writeBin(jpg_fake, file.path(rdir, "corrupt.jpg"))
  # Wrong extension — PNG bytes with .jpg extension
  writeBin(png_fake, file.path(rdir, "png_as.jpg"))

  # ── DICOM ─────────────────────────────────────────────────────────────
  writeBin(raw(0), file.path(rdir, "empty.dcm"))
  # DICOM preamble (128 zeros) + "DICM" magic + nothing else
  dicom_stub <- c(raw(128), charToRaw("DICM"))
  writeBin(dicom_stub, file.path(rdir, "header_only.dcm"))
  # Wrong magic — 128 zeros + wrong 4 bytes
  dicom_wrong_magic <- c(raw(128), charToRaw("XXXX"),
                         as.raw(sample(0:255, 100, replace=TRUE)))
  writeBin(dicom_wrong_magic, file.path(rdir, "wrong_magic.dcm"))
  # No preamble at all — just random bytes
  set.seed(14)
  writeBin(as.raw(sample(0:255, 1024, replace=TRUE)),
           file.path(rdir, "binary_as.dcm"))

  # ── NIfTI ─────────────────────────────────────────────────────────────
  writeBin(raw(0), file.path(rdir, "empty.nii"))
  # First 4 bytes should be 348 (int32) — put wrong value instead
  nifti_wrong_sz <- c(writeBin(999L, raw(), size = 4, endian = "little"),
                      raw(344))
  writeBin(nifti_wrong_sz, file.path(rdir, "wrong_header_size.nii"))
  # Truncated — only 50 bytes where 348 are expected
  writeBin(as.raw(sample(0:255, 50, replace=TRUE)),
           file.path(rdir, "truncated.nii"))

  # ── Statistical (SAS / Stata / SPSS) ──────────────────────────────────
  writeBin(raw(0), file.path(rdir, "empty.sas7bdat"))
  writeBin(raw(0), file.path(rdir, "empty.dta"))
  writeBin(raw(0), file.path(rdir, "empty.sav"))
  set.seed(15)
  writeBin(as.raw(sample(0:255, 1024, replace=TRUE)),
           file.path(rdir, "binary_as.sas7bdat"))
  writeBin(as.raw(sample(0:255, 1024, replace=TRUE)),
           file.path(rdir, "binary_as.dta"))
  writeBin(as.raw(sample(0:255, 1024, replace=TRUE)),
           file.path(rdir, "binary_as.sav"))

  # ── Database files (SQLite / DuckDB) ──────────────────────────────────
  writeBin(raw(0), file.path(rdir, "empty.sqlite"))
  writeBin(raw(0), file.path(rdir, "empty.db"))
  # SQLite magic header ("SQLite format 3\0") + garbage after
  sqlite_fake <- c(charToRaw("SQLite format 3"), as.raw(0),
                   as.raw(sample(0:255, 500, replace=TRUE)))
  writeBin(sqlite_fake, file.path(rdir, "corrupt.sqlite"))

  # ── Columnar (Parquet / Arrow) ────────────────────────────────────────
  writeBin(raw(0), file.path(rdir, "empty.parquet"))
  # Parquet magic is "PAR1" at start and end
  parquet_fake <- c(charToRaw("PAR1"),
                    as.raw(sample(0:255, 500, replace=TRUE)),
                    charToRaw("PAR1"))
  writeBin(parquet_fake, file.path(rdir, "corrupt.parquet"))

  # ── RDS (R serialised) ────────────────────────────────────────────────
  writeBin(raw(0), file.path(rdir, "empty.rds"))
  # readRDS on random bytes raises cryptic errors
  set.seed(16)
  writeBin(as.raw(sample(0:255, 1024, replace=TRUE)),
           file.path(rdir, "binary_as.rds"))

  # ── Archives (ZIP / TAR / 7Z) extended ────────────────────────────────
  writeBin(raw(0), file.path(rdir, "empty.zip"))
  writeBin(raw(0), file.path(rdir, "empty.tar"))
  writeBin(raw(0), file.path(rdir, "empty.7z"))
  # TAR with wrong format (not 512-byte aligned)
  writeBin(as.raw(sample(0:255, 100, replace=TRUE)),
           file.path(rdir, "corrupt.tar"))

  # ── Unknown binary (for inspect_binary fallback) ──────────────────────
  set.seed(17)
  writeBin(as.raw(sample(0:255, 2048, replace=TRUE)),
           file.path(rdir, "random.dat"))
  writeBin(raw(0), file.path(rdir, "empty.dat"))

  # ── Cross-cutting: compression / encoding mismatches ──────────────────
  # File with gzip magic but .csv extension
  gzip_magic <- as.raw(c(0x1F, 0x8B))
  writeBin(c(gzip_magic, as.raw(sample(0:255, 200, replace=TRUE))),
           file.path(rdir, "gzipped_as.csv"))
  # UTF-16 BOM + CSV-like content (would look empty to readLines if not handled)
  utf16_csv <- c(as.raw(c(0xFF, 0xFE)),   # UTF-16 LE BOM
                 charToRaw("a,b\n1,2\n"))
  writeBin(utf16_csv, file.path(rdir, "utf16_bom.csv"))

  cat(sprintf("  built %d robustness fixtures in %s\n",
              length(list.files(rdir)), rdir))
  invisible(TRUE)
}

# Call if sourced directly
if (sys.nframe() == 0) {
  fixture_dir <- if (!is.null(getOption("fixture_dir")))
    getOption("fixture_dir") else "fixtures"
  build_robustness_fixtures(fixture_dir)
}
