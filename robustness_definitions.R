# ============================================================
# Airlock Checker — Robustness Test Definitions
# ============================================================
# These tests assert that inspectors survive pathological inputs.
# Success criteria:
#   1. Inspector does NOT throw an error
#   2. Returns a list (possibly empty, possibly with UNCERTAIN hits)
#   3. Completes within max_duration_ms
#
# Unlike unit tests, these do not assert on specific rule firing.
# Their purpose is to catch crashes and hangs, not classification logic.
#
# Appended to TEST_CASES by run_tests.R. Each has category="robustness".
# ============================================================

ROBUSTNESS_CASES <- list(

  # ── Empty files ────────────────────────────────────────────────────────
  list(id = "robust_tabular_empty",          category = "robustness",
       inspector = "inspect_tabular",
       fixture = "robustness/empty.csv",     max_duration_ms = 5000),
  list(id = "robust_genomic_empty",          category = "robustness",
       inspector = "inspect_genomic",
       fixture = "robustness/empty.vcf",     max_duration_ms = 5000),
  list(id = "robust_json_empty",             category = "robustness",
       inspector = "inspect_json",
       fixture = "robustness/empty.json",    max_duration_ms = 5000),
  list(id = "robust_script_empty",           category = "robustness",
       inspector = "inspect_script",
       fixture = "robustness/empty.R",       max_duration_ms = 5000),

  # ── Tiny / almost-empty ────────────────────────────────────────────────
  list(id = "robust_tabular_one_byte",       category = "robustness",
       inspector = "inspect_tabular",
       fixture = "robustness/one_byte.csv",  max_duration_ms = 5000),
  list(id = "robust_tabular_blank_line",     category = "robustness",
       inspector = "inspect_tabular",
       fixture = "robustness/blank_line.csv", max_duration_ms = 5000),
  list(id = "robust_tabular_header_only",    category = "robustness",
       inspector = "inspect_tabular",
       fixture = "robustness/header_only.csv", max_duration_ms = 5000),

  # ── Truncated ──────────────────────────────────────────────────────────
  list(id = "robust_tabular_truncated",      category = "robustness",
       inspector = "inspect_tabular",
       fixture = "robustness/truncated.csv", max_duration_ms = 5000),
  list(id = "robust_genomic_truncated",      category = "robustness",
       inspector = "inspect_genomic",
       fixture = "robustness/truncated.vcf", max_duration_ms = 5000),
  list(id = "robust_json_truncated",         category = "robustness",
       inspector = "inspect_json",
       fixture = "robustness/truncated.json", max_duration_ms = 5000),

  # ── Binary content with text extension ─────────────────────────────────
  list(id = "robust_tabular_binary",         category = "robustness",
       inspector = "inspect_tabular",
       fixture = "robustness/binary_as.csv", max_duration_ms = 10000),
  list(id = "robust_genomic_binary",         category = "robustness",
       inspector = "inspect_genomic",
       fixture = "robustness/binary_as.vcf", max_duration_ms = 10000),
  list(id = "robust_json_binary",            category = "robustness",
       inspector = "inspect_json",
       fixture = "robustness/binary_as.json", max_duration_ms = 10000),

  # ── Invalid UTF-8 / embedded NUL ───────────────────────────────────────
  list(id = "robust_tabular_bad_utf8",       category = "robustness",
       inspector = "inspect_tabular",
       fixture = "robustness/bad_utf8.csv",  max_duration_ms = 5000),
  list(id = "robust_tabular_embedded_nul",   category = "robustness",
       inspector = "inspect_tabular",
       fixture = "robustness/embedded_nul.csv", max_duration_ms = 5000),

  # ── Structural edge cases ──────────────────────────────────────────────
  list(id = "robust_tabular_mismatched_rows", category = "robustness",
       inspector = "inspect_tabular",
       fixture = "robustness/mismatched_rows.csv", max_duration_ms = 5000),
  list(id = "robust_tabular_duplicate_cols", category = "robustness",
       inspector = "inspect_tabular",
       fixture = "robustness/duplicate_cols.csv", max_duration_ms = 5000),
  list(id = "robust_tabular_wide_200cols",   category = "robustness",
       inspector = "inspect_tabular",
       fixture = "robustness/wide_200cols.csv", max_duration_ms = 15000),
  list(id = "robust_tabular_single_col",     category = "robustness",
       inspector = "inspect_tabular",
       fixture = "robustness/single_col_1000rows.csv", max_duration_ms = 10000),
  list(id = "robust_tabular_all_na",         category = "robustness",
       inspector = "inspect_tabular",
       fixture = "robustness/all_na.csv",    max_duration_ms = 5000),
  list(id = "robust_tabular_all_same",       category = "robustness",
       inspector = "inspect_tabular",
       fixture = "robustness/all_same.csv",  max_duration_ms = 5000),
  list(id = "robust_tabular_long_cell",      category = "robustness",
       inspector = "inspect_tabular",
       fixture = "robustness/long_cell.csv", max_duration_ms = 10000),

  # ── Filename edge cases ────────────────────────────────────────────────
  list(id = "robust_file_with_spaces",       category = "robustness",
       inspector = "inspect_tabular",
       fixture = "robustness/file with spaces.csv", max_duration_ms = 5000),

  # ── Script edge cases ──────────────────────────────────────────────────
  list(id = "robust_script_malformed",       category = "robustness",
       inspector = "inspect_script",
       fixture = "robustness/malformed.R",   max_duration_ms = 5000),
  list(id = "robust_script_long_line",       category = "robustness",
       inspector = "inspect_script",
       fixture = "robustness/long_line.R",   max_duration_ms = 10000),

  # ── Archive edge cases ─────────────────────────────────────────────────
  list(id = "robust_archive_corrupt_zip",    category = "robustness",
       inspector = "inspect_archive",
       fixture = "robustness/corrupt.zip",   max_duration_ms = 10000),

  # ── Genomic edge cases ─────────────────────────────────────────────────
  list(id = "robust_genomic_header_only",    category = "robustness",
       inspector = "inspect_genomic",
       fixture = "robustness/header_only.vcf", max_duration_ms = 5000),
  list(id = "robust_genomic_malformed",      category = "robustness",
       inspector = "inspect_genomic",
       fixture = "robustness/malformed.vcf", max_duration_ms = 5000),

  # ── XML ────────────────────────────────────────────────────────────────
  list(id = "robust_xml_empty",              category = "robustness",
       inspector = "inspect_xml",
       fixture = "robustness/empty.xml",    max_duration_ms = 5000),
  list(id = "robust_xml_binary",             category = "robustness",
       inspector = "inspect_xml",
       fixture = "robustness/binary_as.xml", max_duration_ms = 10000),
  list(id = "robust_xml_truncated",          category = "robustness",
       inspector = "inspect_xml",
       fixture = "robustness/truncated.xml", max_duration_ms = 5000),
  list(id = "robust_xml_mismatched_tags",    category = "robustness",
       inspector = "inspect_xml",
       fixture = "robustness/mismatched_tags.xml", max_duration_ms = 5000),

  # ── Markdown ───────────────────────────────────────────────────────────
  list(id = "robust_markdown_empty",         category = "robustness",
       inspector = "inspect_markdown",
       fixture = "robustness/empty.md",     max_duration_ms = 5000),
  list(id = "robust_markdown_binary",        category = "robustness",
       inspector = "inspect_markdown",
       fixture = "robustness/binary_as.md", max_duration_ms = 10000),
  list(id = "robust_markdown_truncated",     category = "robustness",
       inspector = "inspect_markdown",
       fixture = "robustness/truncated.md", max_duration_ms = 5000),

  # ── HTML / webpage ─────────────────────────────────────────────────────
  list(id = "robust_webpage_empty",          category = "robustness",
       inspector = "inspect_webpage",
       fixture = "robustness/empty.html",   max_duration_ms = 5000),
  list(id = "robust_webpage_binary",         category = "robustness",
       inspector = "inspect_webpage",
       fixture = "robustness/binary_as.html", max_duration_ms = 10000),
  list(id = "robust_webpage_truncated",      category = "robustness",
       inspector = "inspect_webpage",
       fixture = "robustness/truncated.html", max_duration_ms = 5000),
  list(id = "robust_webpage_deeply_nested",  category = "robustness",
       inspector = "inspect_webpage",
       fixture = "robustness/deeply_nested.html", max_duration_ms = 15000),

  # ── PDF ────────────────────────────────────────────────────────────────
  list(id = "robust_pdf_empty",              category = "robustness",
       inspector = "inspect_document",
       fixture = "robustness/empty.pdf",    max_duration_ms = 10000,
       required_pkgs = "pdftools"),
  list(id = "robust_pdf_binary",             category = "robustness",
       inspector = "inspect_document",
       fixture = "robustness/binary_as.pdf", max_duration_ms = 10000,
       required_pkgs = "pdftools"),
  list(id = "robust_pdf_corrupt",            category = "robustness",
       inspector = "inspect_document",
       fixture = "robustness/corrupt.pdf",  max_duration_ms = 10000,
       required_pkgs = "pdftools"),
  list(id = "robust_pdf_truncated",          category = "robustness",
       inspector = "inspect_document",
       fixture = "robustness/truncated.pdf", max_duration_ms = 10000,
       required_pkgs = "pdftools"),

  # ── Office (DOCX/XLSX/PPTX) ────────────────────────────────────────────
  list(id = "robust_docx_empty",             category = "robustness",
       inspector = "inspect_office",
       fixture = "robustness/empty.docx",   max_duration_ms = 10000),
  list(id = "robust_docx_corrupt",           category = "robustness",
       inspector = "inspect_office",
       fixture = "robustness/corrupt.docx", max_duration_ms = 10000),
  list(id = "robust_pptx_empty",             category = "robustness",
       inspector = "inspect_office",
       fixture = "robustness/empty.pptx",   max_duration_ms = 10000),
  list(id = "robust_pptx_corrupt",           category = "robustness",
       inspector = "inspect_office",
       fixture = "robustness/corrupt.pptx", max_duration_ms = 10000),

  # ── Images ─────────────────────────────────────────────────────────────
  list(id = "robust_image_empty_png",        category = "robustness",
       inspector = "inspect_image",
       fixture = "robustness/empty.png",    max_duration_ms = 5000),
  list(id = "robust_image_empty_jpg",        category = "robustness",
       inspector = "inspect_image",
       fixture = "robustness/empty.jpg",    max_duration_ms = 5000),
  list(id = "robust_image_corrupt_png",      category = "robustness",
       inspector = "inspect_image",
       fixture = "robustness/corrupt.png",  max_duration_ms = 5000),
  list(id = "robust_image_corrupt_jpg",      category = "robustness",
       inspector = "inspect_image",
       fixture = "robustness/corrupt.jpg",  max_duration_ms = 5000),
  list(id = "robust_image_wrong_ext",        category = "robustness",
       inspector = "inspect_image",
       fixture = "robustness/png_as.jpg",   max_duration_ms = 5000),

  # ── DICOM ──────────────────────────────────────────────────────────────
  list(id = "robust_dicom_empty",            category = "robustness",
       inspector = "inspect_dicom",
       fixture = "robustness/empty.dcm",    max_duration_ms = 10000,
       required_pkgs = "oro.dicom"),
  list(id = "robust_dicom_header_only",      category = "robustness",
       inspector = "inspect_dicom",
       fixture = "robustness/header_only.dcm", max_duration_ms = 10000,
       required_pkgs = "oro.dicom"),
  list(id = "robust_dicom_wrong_magic",      category = "robustness",
       inspector = "inspect_dicom",
       fixture = "robustness/wrong_magic.dcm", max_duration_ms = 10000,
       required_pkgs = "oro.dicom"),
  list(id = "robust_dicom_binary",           category = "robustness",
       inspector = "inspect_dicom",
       fixture = "robustness/binary_as.dcm", max_duration_ms = 10000,
       required_pkgs = "oro.dicom"),

  # ── NIfTI ──────────────────────────────────────────────────────────────
  list(id = "robust_nifti_empty",            category = "robustness",
       inspector = "inspect_nifti",
       fixture = "robustness/empty.nii",    max_duration_ms = 5000),
  list(id = "robust_nifti_wrong_header_sz",  category = "robustness",
       inspector = "inspect_nifti",
       fixture = "robustness/wrong_header_size.nii", max_duration_ms = 5000),
  list(id = "robust_nifti_truncated",        category = "robustness",
       inspector = "inspect_nifti",
       fixture = "robustness/truncated.nii", max_duration_ms = 5000),

  # ── Statistical (SAS / Stata / SPSS) ──────────────────────────────────
  list(id = "robust_stat_empty_sas",         category = "robustness",
       inspector = "inspect_statistical",
       fixture = "robustness/empty.sas7bdat", max_duration_ms = 5000,
       required_pkgs = "haven"),
  list(id = "robust_stat_empty_dta",         category = "robustness",
       inspector = "inspect_statistical",
       fixture = "robustness/empty.dta",    max_duration_ms = 5000,
       required_pkgs = "haven"),
  list(id = "robust_stat_empty_sav",         category = "robustness",
       inspector = "inspect_statistical",
       fixture = "robustness/empty.sav",    max_duration_ms = 5000,
       required_pkgs = "haven"),
  list(id = "robust_stat_binary_sas",        category = "robustness",
       inspector = "inspect_statistical",
       fixture = "robustness/binary_as.sas7bdat", max_duration_ms = 5000,
       required_pkgs = "haven"),
  list(id = "robust_stat_binary_dta",        category = "robustness",
       inspector = "inspect_statistical",
       fixture = "robustness/binary_as.dta", max_duration_ms = 5000,
       required_pkgs = "haven"),
  list(id = "robust_stat_binary_sav",        category = "robustness",
       inspector = "inspect_statistical",
       fixture = "robustness/binary_as.sav", max_duration_ms = 5000,
       required_pkgs = "haven"),

  # ── Database files ─────────────────────────────────────────────────────
  list(id = "robust_db_empty_sqlite",        category = "robustness",
       inspector = "inspect_database",
       fixture = "robustness/empty.sqlite", max_duration_ms = 5000),
  list(id = "robust_db_empty_db",            category = "robustness",
       inspector = "inspect_database",
       fixture = "robustness/empty.db",     max_duration_ms = 5000),
  list(id = "robust_db_corrupt_sqlite",      category = "robustness",
       inspector = "inspect_database",
       fixture = "robustness/corrupt.sqlite", max_duration_ms = 5000),

  # ── Columnar ───────────────────────────────────────────────────────────
  list(id = "robust_columnar_empty",         category = "robustness",
       inspector = "inspect_columnar",
       fixture = "robustness/empty.parquet", max_duration_ms = 5000),
  list(id = "robust_columnar_corrupt",       category = "robustness",
       inspector = "inspect_columnar",
       fixture = "robustness/corrupt.parquet", max_duration_ms = 5000),

  # ── RDS ────────────────────────────────────────────────────────────────
  list(id = "robust_rds_empty",              category = "robustness",
       inspector = "inspect_serialised",
       fixture = "robustness/empty.rds",    max_duration_ms = 5000),
  list(id = "robust_rds_binary",             category = "robustness",
       inspector = "inspect_serialised",
       fixture = "robustness/binary_as.rds", max_duration_ms = 5000),

  # ── Archives extended ──────────────────────────────────────────────────
  list(id = "robust_archive_empty_zip",      category = "robustness",
       inspector = "inspect_archive",
       fixture = "robustness/empty.zip",    max_duration_ms = 5000),
  list(id = "robust_archive_empty_tar",      category = "robustness",
       inspector = "inspect_archive",
       fixture = "robustness/empty.tar",    max_duration_ms = 5000),
  list(id = "robust_archive_empty_7z",       category = "robustness",
       inspector = "inspect_archive",
       fixture = "robustness/empty.7z",     max_duration_ms = 5000),
  list(id = "robust_archive_corrupt_tar",    category = "robustness",
       inspector = "inspect_archive",
       fixture = "robustness/corrupt.tar",  max_duration_ms = 5000),

  # ── Unknown binary / dat ───────────────────────────────────────────────
  list(id = "robust_binary_random_dat",      category = "robustness",
       inspector = "inspect_binary",
       fixture = "robustness/random.dat",   max_duration_ms = 5000),
  list(id = "robust_binary_empty_dat",       category = "robustness",
       inspector = "inspect_binary",
       fixture = "robustness/empty.dat",    max_duration_ms = 5000),

  # ── Cross-cutting: compression / encoding mismatches ──────────────────
  list(id = "robust_tabular_gzipped",        category = "robustness",
       inspector = "inspect_tabular",
       fixture = "robustness/gzipped_as.csv", max_duration_ms = 5000),
  list(id = "robust_tabular_utf16_bom",      category = "robustness",
       inspector = "inspect_tabular",
       fixture = "robustness/utf16_bom.csv", max_duration_ms = 5000)
)
