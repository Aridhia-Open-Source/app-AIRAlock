# ============================================================
# Airlock Checker — Test Case Definitions
# ============================================================
# Each test is a list with:
#   id            — unique test identifier
#   rule          — rule ID being tested (for grouping in report)
#   fixture       — fixture filename (relative to FIXTURE_DIR)
#                   OR NULL if generated at test time
#   inspector     — inspector function to call
#                   ("inspect_tabular", "inspect_script", etc.)
#                   OR "run_dte" for end-to-end
#   cfg           — config overrides (list) or NULL for defaults
#   expect_fire   — TRUE if rule should fire, FALSE if it should NOT
#   expect_outcome — "RED" / "AMBER" / "GREEN" / NA (any)
#   expect_detail  — optional substring that must appear in detail
#   required_pkgs  — character vector of optional packages this test
#                    needs; test is SKIPPED if any are missing
# ============================================================

test_cases <- list(

  # ── TAB-001: Identifier column headers ──────────────────────────────
  list(id = "tab001_fires_on_participant_id",
       rule = "TAB-001", fixture = "tab001_participant_id.csv",
       inspector = "inspect_tabular", cfg = NULL,
       expect_fire = TRUE, expect_outcome = "RED",
       expect_detail = "participant_id"),
  list(id = "tab001_clean_file",
       rule = "TAB-001", fixture = "tab001_clean.csv",
       inspector = "inspect_tabular", cfg = NULL,
       expect_fire = FALSE),

  # ── TAB-003: Per-participant data ───────────────────────────────────
  list(id = "tab003_fires_on_unique_rows",
       rule = "TAB-003", fixture = "tab003_unique_rows.csv",
       inspector = "inspect_tabular", cfg = NULL,
       expect_fire = TRUE, expect_outcome = "RED"),

  # ── TAB-004: Sensitive phenotype columns ────────────────────────────
  list(id = "tab004_fires_on_hiv_column",
       rule = "TAB-004", fixture = "tab004_hiv.csv",
       inspector = "inspect_tabular", cfg = NULL,
       expect_fire = TRUE, expect_outcome = "RED",
       expect_detail = "hiv"),

  # ── TAB-005: Unmasked small counts ──────────────────────────────────
  list(id = "tab005_fires_on_small_n",
       rule = "TAB-005", fixture = "tab005_small_count.csv",
       inspector = "inspect_tabular", cfg = NULL,
       expect_fire = TRUE, expect_outcome = "RED"),
  list(id = "tab005_safe_when_all_above_threshold",
       rule = "TAB-005", fixture = "tab005_safe.csv",
       inspector = "inspect_tabular", cfg = NULL,
       expect_fire = FALSE),
  list(id = "tab005_ignores_non_count_columns",
       rule = "TAB-005", fixture = "tab005_not_count_col.csv",
       inspector = "inspect_tabular", cfg = NULL,
       expect_fire = FALSE),

  # ── TAB-006: Raw UKB field codes ────────────────────────────────────
  list(id = "tab006_fires_on_ukb_fields",
       rule = "TAB-006", fixture = "tab006_ukb_raw.csv",
       inspector = "inspect_tabular", cfg = NULL,
       expect_fire = TRUE, expect_outcome = "RED"),

  # ── TAB-009: Percentage back-calculation ────────────────────────────
  list(id = "tab009_fires_on_pct_x_n",
       rule = "TAB-009", fixture = "tab009_pct_backcalc.csv",
       inspector = "inspect_tabular", cfg = NULL,
       expect_fire = TRUE, expect_outcome = "RED"),

  # ── TAB-010: Rare category exposure ─────────────────────────────────
  list(id = "tab010_fires_on_rare_category",
       rule = "TAB-010", fixture = "tab010_rare_category.csv",
       inspector = "inspect_tabular", cfg = NULL,
       expect_fire = TRUE, expect_outcome = "RED"),

  # ── TAB-011: Over-precise continuous variables ──────────────────────
  list(id = "tab011_fires_on_day_dates",
       rule = "TAB-011", fixture = "tab011_day_dates.csv",
       inspector = "inspect_tabular", cfg = NULL,
       expect_fire = TRUE, expect_outcome = "AMBER"),

  # ── TAB-012: Free text columns ──────────────────────────────────────
  list(id = "tab012_fires_on_notes_column",
       rule = "TAB-012", fixture = "tab012_notes.csv",
       inspector = "inspect_tabular", cfg = NULL,
       expect_fire = TRUE, expect_outcome = "AMBER"),

  # ── TAB-013: Secondary suppression risk ─────────────────────────────
  list(id = "tab013_fires_on_supp_with_total",
       rule = "TAB-013", fixture = "tab013_secondary_supp.csv",
       inspector = "inspect_tabular", cfg = NULL,
       expect_fire = TRUE, expect_outcome = "RED"),

  # ── TAB-015: k-anonymity below threshold ────────────────────────────
  list(id = "tab015_fires_on_unique_combinations",
       rule = "TAB-015", fixture = "tab015_k_unique.csv",
       inspector = "inspect_tabular", cfg = NULL,
       expect_fire = TRUE, expect_outcome = "RED"),

  # ── TAB-018: PII value patterns ─────────────────────────────────────
  list(id = "tab018_fires_on_nhs_number",
       rule = "TAB-018", fixture = "tab018_nhs_number.csv",
       inspector = "inspect_tabular", cfg = NULL,
       expect_fire = TRUE, expect_outcome = "RED",
       expect_detail = "NHS number"),
  list(id = "tab018_fires_on_email",
       rule = "TAB-018", fixture = "tab018_email.csv",
       inspector = "inspect_tabular", cfg = NULL,
       expect_fire = TRUE, expect_outcome = "RED",
       expect_detail = "Email"),

  # ── TAB-019: Postcode in cell values ────────────────────────────────
  list(id = "tab019_fires_on_full_postcode",
       rule = "TAB-019", fixture = "tab019_postcode.csv",
       inspector = "inspect_tabular", cfg = NULL,
       expect_fire = TRUE, expect_outcome = "RED"),

  # ── TAB-023: Suppression back-calculation ──────────────────────────
  list(id = "tab023_fires_on_backcalc",
       rule = "TAB-023", fixture = "tab023_backcalc.csv",
       inspector = "inspect_tabular", cfg = NULL,
       expect_fire = TRUE, expect_outcome = "RED",
       expect_detail = "back-calculation"),

  # ── TAB-024: Free-text NER ──────────────────────────────────────────
  list(id = "tab024_fires_on_person_name",
       rule = "TAB-024", fixture = "tab024_person_name.csv",
       inspector = "inspect_tabular", cfg = NULL,
       expect_fire = TRUE, expect_outcome = "RED",
       expect_detail = "person name"),

  # ── SCR-001: Participant IDs in script ──────────────────────────────
  list(id = "scr001_fires_on_participant_id",
       rule = "SCR-001", fixture = "scr001_ids.R",
       inspector = "inspect_script", cfg = NULL,
       expect_fire = TRUE, expect_outcome = "RED"),
  list(id = "scr001_clean_script",
       rule = "SCR-001", fixture = "scr_clean.R",
       inspector = "inspect_script", cfg = NULL,
       expect_fire = FALSE),

  # ── SCR-003: Sensitive phenotype in coding context ──────────────────
  list(id = "scr003_fires_on_hiv_filter",
       rule = "SCR-003", fixture = "scr003_hiv_filter.R",
       inspector = "inspect_script", cfg = NULL,
       expect_fire = TRUE, expect_outcome = "RED",
       expect_detail = "hiv"),
  list(id = "scr003_clean_script",
       rule = "SCR-003", fixture = "scr_clean.R",
       inspector = "inspect_script", cfg = NULL,
       expect_fire = FALSE),

  # ── GEN-001: VCF per-sample genotypes ───────────────────────────────
  list(id = "gen001_fires_on_per_sample_vcf",
       rule = "GEN-001", fixture = "gen001_per_sample.vcf",
       inspector = "inspect_genomic", cfg = NULL,
       expect_fire = TRUE, expect_outcome = "RED"),

  # ── GEN-003: GWAS summary (clean) ───────────────────────────────────
  list(id = "gen003_fires_green_on_gwas",
       rule = "GEN-003", fixture = "gen003_gwas.csv",
       inspector = "inspect_tabular", cfg = NULL,
       expect_fire = TRUE, expect_outcome = "GREEN"),

  # ── GEN-006: PEDIGREE metadata ──────────────────────────────────────
  list(id = "gen006_fires_on_pedigree",
       rule = "GEN-006", fixture = "gen006_pedigree.vcf",
       inspector = "inspect_genomic", cfg = NULL,
       expect_fire = TRUE, expect_outcome = "RED"),

  # ── GEN-010: PLINK BED magic bytes ──────────────────────────────────
  list(id = "gen010_fires_on_plink_bed",
       rule = "GEN-010", fixture = "gen010_plink.bed",
       inspector = "inspect_genomic", cfg = NULL,
       expect_fire = TRUE, expect_outcome = "RED"),

  # ── JSON-001: Credentials in JSON ───────────────────────────────────
  list(id = "json001_fires_on_api_key",
       rule = "JSON-001", fixture = "json001_api_key.json",
       inspector = "inspect_json", cfg = NULL,
       expect_fire = TRUE, expect_outcome = "RED",
       expect_detail = "API key"),
  list(id = "json001_clean_config",
       rule = "JSON-001", fixture = "json_clean.json",
       inspector = "inspect_json", cfg = NULL,
       expect_fire = FALSE),

  # ── XML-001: FHIR health data standard ──────────────────────────────
  list(id = "xml001_fires_on_fhir",
       rule = "XML-001", fixture = "xml001_fhir.xml",
       inspector = "inspect_xml", cfg = NULL,
       expect_fire = TRUE, expect_outcome = "RED",
       expect_detail = "FHIR"),

  # ── MD-001: Identifier in markdown ──────────────────────────────────
  list(id = "md001_fires_on_7_digit_id",
       rule = "MD-001", fixture = "md001_patient_id.md",
       inspector = "inspect_markdown", cfg = NULL,
       expect_fire = TRUE, expect_outcome = "RED"),

  # ── HTM-001: Interactive HTML ───────────────────────────────────────
  list(id = "htm001_fires_on_plotly",
       rule = "HTM-001", fixture = "htm001_plotly.html",
       inspector = "inspect_webpage", cfg = NULL,
       expect_fire = TRUE, expect_outcome = "RED"),

  # ── DAT-001: SQLite database ────────────────────────────────────────
  list(id = "dat001_fires_on_sqlite_magic",
       rule = "DAT-001", fixture = "dat001_sqlite.db",
       inspector = "inspect_database", cfg = NULL,
       expect_fire = TRUE, expect_outcome = "RED"),

  # ── SER-001: Small serialised R object ──────────────────────────────
  list(id = "ser001_fires_on_rds",
       rule = "SER-001", fixture = "ser_model_fit.rds",
       inspector = "inspect_serialised", cfg = NULL,
       expect_fire = TRUE, expect_outcome = "AMBER"),

  # ── BIN-001: Unknown binary ─────────────────────────────────────────
  list(id = "bin001_fires_on_unknown_ext",
       rule = "BIN-001", fixture = "bin001_unknown.xyz",
       inspector = "inspect_binary", cfg = NULL,
       expect_fire = TRUE, expect_outcome = "RED"),

  # ── End-to-end: run_dte dispatcher sanity checks ────────────────────
  list(id = "e2e_tab001_classified_red",
       rule = "E2E", fixture = "tab001_participant_id.csv",
       inspector = "run_dte", cfg = NULL,
       expect_fire = TRUE, expect_outcome = "RED"),
  list(id = "e2e_gwas_high_cardinality_rs_trips_tab003",
       rule = "E2E", fixture = "gen003_gwas.csv",
       inspector = "run_dte", cfg = NULL,
       expect_fire = TRUE, expect_outcome = "RED",
       note = "Known engine behaviour: unique rs IDs in GWAS summary fire TAB-003 as high-cardinality quasi-identifiers. Documented as a real false-positive to track for future engine improvement."),
  list(id = "e2e_json_clean",
       rule = "E2E", fixture = "json_clean.json",
       inspector = "run_dte", cfg = NULL,
       expect_fire = TRUE, expect_outcome = "GREEN")
)
