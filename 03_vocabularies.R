# Vocabulary lists, QI categories, and DEFAULT_CFG
# Auto-split from app.R - do not edit the monolithic file


AIRA_BASE_URL_DEFAULT <- {
  app_hostname <- Sys.getenv("APP_HOSTNAME", unset = "")

  if (nzchar(app_hostname)) {
    paste0(
      "https://api.",
      sub("^[^.]+\\.", "", app_hostname),
      "/api/aira/v1"
    )
  } else {
    "https://api.uksouth.saas.aridhia.io/api/aira/v1"
  }
}


# ============================================================
# QUASI-IDENTIFIER CATEGORIES (for cross-file linkage detection)
# ============================================================
QI_CATEGORIES <- list(
  Demographics   = c("age","sex","gender","ethnicity","race","nationality","marital_status"),
  Geography      = c("postcode","zip_code","lsoa","msoa","oa_code","region","country",
                     "site","centre","location","latitude","longitude",
                     "easting","northing","uprn","address","ward","district"),
  Identity       = c("name","firstname","first_name","lastname","last_name","surname",
                     "initials","dob","date_of_birth","birth_date","nhs_number",
                     "chi_number","mrn","hospital_number","passport"),
  ClinicalDate   = c("admission_date","discharge_date","diagnosis_date","event_date",
                     "date_of_death","dod","death_date","procedure_date",
                     "appointment_date","visit_date","referral_date"),
  Diagnosis      = c("icd","icd10","snomed","opcs","read_code","diagnosis","condition",
                     "phenotype","disease","disorder","comorbidity"),
  Occupation     = c("occupation","job_title","employer","employment_status","income"),
  DerivedTemporal = c("time_since","age_at","days_since","months_since","years_since",
                       "follow_up","fu_time","survival_time","event_time","duration",
                       "distance_from","distance_to","travel_time")
)

sensitive_phenotypes <- c(
  "hiv","aids","mental_health","psychiatric","depression","anxiety",
  "schizophrenia","bipolar","substance_use","alcohol_use","drug_use",
  "sexual_orientation","gender_identity","self_harm","suicide",
  "domestic_abuse","termination","abortion","eating_disorder","anorexia"
)

restricted_fields <- c(
  "systolic_bp","diastolic_bp","brain","mri","dxa","ocular",
  "retinal","ophthalmology","carotid","abdominal_mri","brain_mri",
  "cardiac_mri","body_composition"
)

participant_id_patterns <- c(
  "^participant_id$","^imaging_id$","^sample_id$",
  "^subject_id$","^pid$","^record_id$","^person_id$","^patient_id$"
)

gwas_cols <- c("snp","chr","bp","beta","se","p","or","a1","a2","freq","rsid","pos","af")

# TAB-012: Free text column name patterns
free_text_patterns <- c(
  "^notes$","^note$","^comments$","^comment$","^description$","^narrative$",
  "^text$","^free_text$","^free_text_",".*_notes$",".*_comments$",".*_text$",
  "^history$","^clinical_history$","^reason$","^details$","^remarks$",
  "^summary$","^annotation$","^memo$","^observation$","^rationale$",
  "^justification$","^feedback$","^additional_info$","^other_info$"
)

# TAB-014: Derived temporal/spatial identifier patterns
derived_id_patterns <- c(
  "^time_since","^age_at_","^days_","^months_since","^years_since",
  "^weeks_since","^distance_from","^duration_","^interval_",
  "^time_to_","^time_from_","_since_diagnosis","_since_recruitment",
  "_from_centre","_from_site","_from_baseline","^survival_time",
  "^follow_up_","^fu_time","^event_time"
)

# ── Installation defaults - global scope so PDF diff always compares
# against hardcoded values, never a session-mutated copy.
DEFAULT_CFG <- list(
  id_patterns          = participant_id_patterns,
  sensitive_phenotypes = sensitive_phenotypes,
  restricted_fields    = restricted_fields,
  gwas_cols            = gwas_cols,
  count_threshold      = 5L,
  tab003_min_rows      = 10L,
  tab003_cardinality   = 0.85,
  size_threshold_gb    = 5,
  gen003_min_cols      = 4L,
  img002_eid_digits    = 7L,
  img002_patient_word  = TRUE,
  img002_extra_words   = character(0),
  img004_keywords      = "circos|igv|oncoplot|tmb|mutational",
  scr003_min_term_length = 3L,
  htm_table_rows       = 20L,
  json_record_threshold= 50L,
  xml_record_threshold = 50L,
  scr006_flag_outputs  = TRUE,
  scr006_pii_scan      = TRUE,
  robj_large_mb        = 100L,
  robj_model_names     = c("model","fit","lm","glm","cox","surv",
                           "rf","xgb","result","summary","output"),
  columnar_outcome     = "AMBER",
  free_text_patterns   = free_text_patterns,
  derived_id_patterns  = derived_id_patterns,
  kanon_enabled        = TRUE,
  kanon_max_rows       = 10000L,
  kanon_max_qi_cols    = 6L,
  # TAB-023: suppression consistency
  suppression_markers  = c("<5","<10","*","**","[c]","[s]","-","--","x","X","z","Z","0s"),
  total_row_patterns   = c("total","all","overall","grand.total","sum","subtotal","combined",
                           "aggregate","any","n"),
  # TAB-024 NER vocabulary - all lists configurable via the settings panel.
  # Defaults are illustrative UK English examples.
  ner_enabled          = TRUE,
  ner_person_titles    = c(
    "mr","mrs","ms","miss","dr","prof","sir","rev","mx",  # English
    "m","mme","mlle","pr",                                 # French
    "herr","frau",                                         # German/Dutch
    "sr","sra","srta","dra"                                # Spanish/Portuguese
  ),
  ner_geo_places       = c(
    # UK examples - replace or extend for your geography (one per line in settings)
    "london","manchester","birmingham","leeds","glasgow","liverpool",
    "bristol","sheffield","edinburgh","cardiff","belfast","newcastle",
    "nottingham","leicester","coventry","oxford","cambridge","exeter",
    "york","bath","kent","surrey","essex","norfolk","suffolk",
    "cornwall","devon","dorset","hampshire","lancashire","yorkshire"
  ),
  ner_inst_patterns    = c(
    # Generic healthcare/institution terms
    "hospital","clinic","infirmary","centre","center","surgery","practice",
    "ward","unit","department","institute","foundation","trust","authority",
    # UK-specific (remove via settings if not applicable)
    "nhs","royal"
  ),
  ner_occ_patterns     = c(
    # Occupation keywords for composite QI detection - extend for your population
    "teacher","nurse","doctor","engineer","driver","manager","director",
    "student","retired","farmer","police","soldier","lawyer","accountant",
    "cleaner","carer","worker","officer","consultant","therapist","pharmacist",
    "midwife","surgeon","dentist","paramedic","radiographer","porter"
  ),
  ner_name_exclusions  = c(
    # Exclude these two-word capitalised phrases from bare person name detection
    "Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday",
    "January","February","March","April","June","July","August",
    "September","October","November","December",
    "North","South","East","West","Central","Upper","Lower","New","Old",
    "National","General","Regional","District","Primary","Secondary"
  ),
    # AIRA configuration - feature disabled by default. To enable:
    #   1. Ensure dependencies.R has installed ellmer/future/promises/R.utils
    #   2. Set WORKSPACE_API_KEY in the workspace environment
    #   3. Set aira$enabled = TRUE (via config UI or by editing the saved JSON)
    aira = list(
      enabled         = TRUE,
      base_url        = AIRA_BASE_URL_DEFAULT,
      model           = "workspace-chat",
      timeout_s_file  = 500L,    # per-file calls: short prompts, fast response
      timeout_s_batch = 500L,    # batch calls: larger prompts need more headroom
      capture_prompts = FALSE,   # opt-in: write full (system,user,response)
                                 # triples to aira_capture/ for model testing.
                                 # OFF by default; persists prompt content.
      use_cases = list(
        batch_summary     = list(enabled = TRUE, prompt_version = "batch_summary_v1"),
        disclosure_review = list(enabled = TRUE, prompt_version = "disclosure_review_v1")
      )
    )
)
