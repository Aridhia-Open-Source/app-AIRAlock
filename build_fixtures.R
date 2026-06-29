# ============================================================
# Airlock Checker — Test Fixture Builder
# ============================================================
# Generates all synthetic test files into FIXTURE_DIR.
# Re-run whenever fixtures need refreshing.
# Called automatically by run_tests.R before the test loop.
# ============================================================

build_fixtures <- function(fixture_dir) {
  if (!dir.exists(fixture_dir))
    dir.create(fixture_dir, recursive = TRUE, showWarnings = FALSE)

  write_csv_lines <- function(name, lines) {
    writeLines(lines, file.path(fixture_dir, name))
  }
  write_bytes <- function(name, bytes) {
    writeBin(bytes, file.path(fixture_dir, name))
  }

  # ── TAB-001: Identifier column headers ──────────────────────────────────
  write_csv_lines("tab001_participant_id.csv", c(
    "participant_id,age,sex",
    "P001,45,M",
    "P002,52,F",
    "P003,38,M"
  ))
  write_csv_lines("tab001_clean.csv", c(
    "age_band,sex,n",
    "40-49,M,25",
    "50-59,F,31"
  ))

  # ── TAB-003: Per-participant / high-cardinality data ────────────────────
  write_csv_lines("tab003_unique_rows.csv", c(
    paste("row_key", "value", sep = ","),
    paste0("key_", sprintf("%04d", 1:15), ",", sample(100:999, 15))
  ))

  # ── TAB-004: Sensitive phenotype columns ────────────────────────────────
  write_csv_lines("tab004_hiv.csv", c(
    "age,sex,hiv_status",
    "45,M,positive",
    "52,F,negative"
  ))

  # ── TAB-005: Unmasked small counts ──────────────────────────────────────
  write_csv_lines("tab005_small_count.csv", c(
    "category,n",
    "A,3",
    "B,12",
    "C,8"
  ))
  write_csv_lines("tab005_safe.csv", c(
    "category,n",
    "A,5",
    "B,12",
    "C,8"
  ))
  write_csv_lines("tab005_not_count_col.csv", c(
    "category,score",
    "A,3",
    "B,12",
    "C,8"
  ))

  # ── TAB-006: Raw UKB field codes ────────────────────────────────────────
  write_csv_lines("tab006_ukb_raw.csv", c(
    "eid,f.31.0.0,f.34.0.0",
    "1000001,Male,1960",
    "1000002,Female,1955"
  ))

  # ── TAB-008: Oversized file — skip (size-dependent, handled in runner) ──

  # ── TAB-009: Percentage back-calculation risk ───────────────────────────
  write_csv_lines("tab009_pct_backcalc.csv", c(
    "group,n,pct",
    "A,100,2.0",
    "B,100,15.5",
    "C,100,8.3"
  ))

  # ── TAB-010: Rare category exposure ─────────────────────────────────────
  write_csv_lines("tab010_rare_category.csv", c(
    "diagnosis,age,sex",
    paste0("common,", sample(30:70, 20, TRUE), ",", sample(c("M","F"), 20, TRUE)),
    "rare_disease_x,45,M",
    "rare_disease_y,52,F"
  ))

  # ── TAB-011: Over-precise continuous variables ──────────────────────────
  # Keep below TAB-015 k-anon threshold (min_rows=10) to avoid that path
  # interfering. 4 rows is enough to satisfy TAB-011's min(3L, n_rows) check.
  write_csv_lines("tab011_day_dates.csv", c(
    "region,event_date,n",
    "north,2024-03-15,12",
    "south,2024-04-22,18",
    "east,2024-05-08,15",
    "west,2024-06-11,20"
  ))

  # ── TAB-012: Free text / narrative columns ──────────────────────────────
  write_csv_lines("tab012_notes.csv", c(
    "patient_idx,age,notes",
    paste0("1,45,\"Patient presented with lower back pain, referred for imaging.\""),
    paste0("2,52,\"Follow-up visit; condition stable since last review.\""),
    paste0("3,38,\"Initial consultation for ongoing headaches.\"")
  ))

  # ── TAB-013: Secondary suppression risk ─────────────────────────────────
  write_csv_lines("tab013_secondary_supp.csv", c(
    "group,cases,total",
    "A,10,50",
    "B,<5,50",
    "C,15,50",
    "Total,30,150"
  ))

  # ── TAB-015: k-anonymity below threshold ────────────────────────────────
  # Small dataset where combining age+sex+postcode gives unique rows
  set.seed(42)
  ka_lines <- c("age,sex,postcode,outcome")
  for (i in 1:20) {
    ka_lines <- c(ka_lines, sprintf("%d,%s,SW%dA,%d",
      sample(25:85, 1), sample(c("M","F"), 1), i, sample(0:1, 1)))
  }
  write_csv_lines("tab015_k_unique.csv", ka_lines)

  # ── TAB-018: PII patterns in cell values ────────────────────────────────
  # Valid NHS number (mod-11): 943 476 5919
  write_csv_lines("tab018_nhs_number.csv", c(
    "record_idx,notes,value",
    "1,\"Contact: 943 476 5919\",100",
    "2,clean note,200",
    "3,another clean,150"
  ))
  write_csv_lines("tab018_email.csv", c(
    "record_idx,notes",
    "1,\"Contact: researcher@example.com\"",
    "2,\"No email here\""
  ))

  # ── TAB-019: UK postcode in cell values ─────────────────────────────────
  write_csv_lines("tab019_postcode.csv", c(
    "record_idx,address",
    "1,\"Flat 3, SW1A 1AA\"",
    "2,\"House 5, EC2M 7PP\""
  ))

  # ── TAB-023: Suppression back-calculation ──────────────────────────────
  # Single suppressed cell in a row with a known total
  write_csv_lines("tab023_backcalc.csv", c(
    "group,n",
    "A,15",
    "B,<5",
    "C,12",
    "Total,32"
  ))

  # ── TAB-024: Free-text NER — person name pattern ───────────────────────
  write_csv_lines("tab024_person_name.csv", c(
    "record_idx,note",
    "1,\"Dr John Smith reviewed the case on Tuesday.\"",
    "2,\"Mrs Sarah Jones declined the referral.\"",
    "3,\"Follow-up scheduled.\""
  ))

  # ── SCR-001: Embedded participant identifiers in R script ──────────────
  writeLines(c(
    "# Analysis script",
    "library(dplyr)",
    "participant_id <- c('P001', 'P002', 'P003')",
    "df <- read.csv('data.csv')",
    "summary(df)"
  ), file.path(fixture_dir, "scr001_ids.R"))

  # ── SCR-003: Sensitive phenotype in active coding context ───────────────
  writeLines(c(
    "# Analysis script",
    "library(dplyr)",
    "df %>% filter(hiv == 'positive') %>% summarise(n = n())"
  ), file.path(fixture_dir, "scr003_hiv_filter.R"))

  # Clean script — no triggers
  writeLines(c(
    "# Simple analysis",
    "library(ggplot2)",
    "ggplot(mtcars, aes(mpg, wt)) + geom_point()"
  ), file.path(fixture_dir, "scr_clean.R"))

  # ── GEN-001: VCF with per-sample genotype columns ──────────────────────
  writeLines(c(
    "##fileformat=VCFv4.2",
    "##FORMAT=<ID=GT,Number=1,Type=String,Description=\"Genotype\">",
    "#CHROM\tPOS\tID\tREF\tALT\tQUAL\tFILTER\tINFO\tFORMAT\tSAMPLE1\tSAMPLE2",
    "1\t100\trs1\tA\tG\t99\tPASS\t.\tGT\t0/1\t1/1",
    "1\t200\trs2\tC\tT\t99\tPASS\t.\tGT\t0/0\t0/1"
  ), file.path(fixture_dir, "gen001_per_sample.vcf"))

  # ── GEN-003: GWAS summary statistics (clean) ───────────────────────────
  # Generate 1200 rows of synthetic GWAS summary stats. Must be >= 1000 rows
  # to avoid triggering GEN-014 (low-variant warning). CSV format because
  # inspect_tabular uses read_csv unconditionally for .csv/.tsv/.txt.
  set.seed(42)
  n_variants <- 1200L
  gwas_rows <- data.frame(
    snp  = sprintf("rs%d", seq(1000000L, length.out = n_variants)),
    chr  = sample(1:22, n_variants, replace = TRUE),
    bp   = sample(1e5:2e8, n_variants, replace = FALSE),
    beta = round(rnorm(n_variants, 0, 0.05), 4),
    se   = round(runif(n_variants, 0.01, 0.08), 4),
    p    = signif(runif(n_variants, 1e-4, 0.99), 3)
  )
  write.csv(gwas_rows, file.path(fixture_dir, "gen003_gwas.csv"),
            row.names = FALSE, quote = FALSE)

  # ── GEN-006: VCF with PEDIGREE metadata ────────────────────────────────
  writeLines(c(
    "##fileformat=VCFv4.2",
    "##PEDIGREE=<Child=CHILD_01,Mother=MOTHER_01,Father=FATHER_01>",
    "##SAMPLE=<ID=SAMPLE1,Description=\"Proband\">",
    "#CHROM\tPOS\tID\tREF\tALT\tQUAL\tFILTER\tINFO",
    "1\t100\trs1\tA\tG\t99\tPASS\t."
  ), file.path(fixture_dir, "gen006_pedigree.vcf"))

  # ── GEN-010: PLINK BED binary (magic bytes) ────────────────────────────
  write_bytes("gen010_plink.bed",
    as.raw(c(0x6c, 0x1b, 0x01, rep(0x00, 30))))

  # ── JSON-001: Credentials in JSON ──────────────────────────────────────
  writeLines(c(
    "{",
    '  "database": "research_db",',
    '  "api_key": "sk-proj-abc123def456",',
    '  "port": 5432',
    "}"
  ), file.path(fixture_dir, "json001_api_key.json"))

  writeLines(c(
    "{",
    '  "database": "research_db",',
    '  "port": 5432,',
    '  "host": "localhost"',
    "}"
  ), file.path(fixture_dir, "json_clean.json"))

  # ── XML-001: FHIR health data standard ─────────────────────────────────
  writeLines(c(
    '<?xml version="1.0"?>',
    '<Bundle xmlns="http://hl7.org/fhir">',
    '  <entry>',
    '    <resource>',
    '      <Patient>',
    '        <id value="12345"/>',
    '      </Patient>',
    '    </resource>',
    '  </entry>',
    '</Bundle>'
  ), file.path(fixture_dir, "xml001_fhir.xml"))

  # ── MD-001: Identifier in markdown ─────────────────────────────────────
  writeLines(c(
    "# Study Report",
    "",
    "Patient ID 9876543 was enrolled on 2024-03-15.",
    "",
    "Results showed improvement."
  ), file.path(fixture_dir, "md001_patient_id.md"))

  # ── HTM-001: HTML with interactive script ──────────────────────────────
  writeLines(c(
    "<!DOCTYPE html>",
    "<html><head><title>Report</title>",
    "<script src='plotly-latest.min.js'></script>",
    "</head><body>",
    "<div id='plot'></div>",
    "<script>Plotly.newPlot('plot', data);</script>",
    "</body></html>"
  ), file.path(fixture_dir, "htm001_plotly.html"))

  # ── DAT-001: SQLite database (magic bytes) ─────────────────────────────
  sqlite_magic <- c(charToRaw("SQLite format 3"), as.raw(0x00))
  write_bytes("dat001_sqlite.db", c(sqlite_magic, as.raw(rep(0x00, 100))))

  # ── SER-001: Small RDS (inspected via filename/size heuristic) ─────────
  tmp_obj <- list(a = 1:10, b = letters[1:5])
  saveRDS(tmp_obj, file.path(fixture_dir, "ser_model_fit.rds"))

  # ── BIN-001: Unknown binary extension ──────────────────────────────────
  write_bytes("bin001_unknown.xyz", as.raw(c(0xde, 0xad, 0xbe, 0xef)))

  invisible(list.files(fixture_dir))
}

# If sourced directly (not from run_tests.R), build into ./fixtures
if (sys.nframe() == 0L) {
  build_fixtures(file.path(getwd(), "fixtures"))
  cat("Fixtures built.\n")
}
