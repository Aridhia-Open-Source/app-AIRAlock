# Remediation guidance per rule ID
# Auto-split from app.R - do not edit the monolithic file

# ============================================================
# REMEDIATION GUIDANCE
# One entry per rule ID - actionable fix instructions for researchers.
# get_remediation() interpolates detail text where useful.
# ============================================================
REMEDIATION <- list(

  # ── Tabular ──────────────────────────────────────────────────────────────
  "TAB-001" = "Remove or hash the identifier column(s) flagged above before resubmitting. If a linking key is genuinely needed for downstream analysis, replace values with a synthetic pseudonym that cannot be reverse-mapped to individuals.",
  "TAB-002" = "Review and redact or replace the cell values flagged above. Use a consistent replacement token (e.g. [REDACTED]) rather than blank cells, which can shift row indices and break downstream joins. Check for NHS numbers, dates of birth, and email address patterns specifically.",
  "TAB-003" = "Aggregate to group level before egress. If per-participant output is required under your data access agreement, this must be approved separately - contact your data governance lead. Ensure no stratum in the aggregated output contains fewer than 5 individuals.",
  "TAB-004" = "Rename or remove columns whose names reference sensitive phenotype categories. If coded column names are necessary, replace plain-language phenotype terms with ICD/SNOMED codes or internally defined numeric identifiers.",
  "TAB-005" = "Apply small-number suppression: replace any count value below the threshold with '<5' (or the threshold configured for this workspace). Check percentage and rate columns - if the denominator is known, even suppressed counts can be back-calculated from a percentage.",
  "TAB-006" = "Rename columns to analysis-level names that do not expose source schema structure. Column names like `eid`, `f.31.0.0`, or database field names should be replaced with descriptive analysis labels.",
  "TAB-007" = "Remove free-text narrative columns before egress. If the content is essential to the analysis, request a separate manual review of the free-text fields through your data governance process.",
  "TAB-008" = "Review whether the full file is needed. Subset to the rows and columns directly relevant to the research output. If the full dataset is required, confirm this is within scope of your data access agreement.",
  "TAB-009" = "Suppress percentage values that imply a count below the threshold alongside the count itself. Options: (1) round percentages to fewer decimal places so the implied count spans a wider range; (2) apply a minimum group size rule and do not report percentages for strata smaller than 10. Recalculate derived rates and confidence intervals after suppression.",
  "TAB-010" = "Merge rare categories into an 'Other' group until all reported categories contain at least the suppression threshold of individuals. If the rare category cannot be merged, suppress both the category label and its count, and note in the output that suppression has been applied.",
  "TAB-011" = "Truncate continuous variables to appropriate precision before egress: dates to year only (YYYY); postcodes to outward code (e.g. SW1A, not SW1A 1AA); ages to whole years. If higher precision is scientifically required, confirm with your reviewer and data governance lead before resubmitting.",
  "TAB-012" = "Review all free text columns before egress. Options: (1) remove the column entirely if it is not essential to the output; (2) submit a sample of rows for manual reviewer inspection prior to egress; (3) run automated named-entity recognition to identify and redact PII before including the column. Do not assume free text is safe because structured columns are clean - clinical narrative commonly contains names, dates, and location references.",
  "TAB-013" = "Apply secondary suppression to cells where the arithmetic reveals a suppressed count. Options: (1) suppress additional cells in the same row or column until no single arithmetic path recovers a count below the threshold; (2) replace exact row and column totals with rounded or banded totals (e.g. report totals to the nearest 10); (3) use interval suppression - report '5-9' instead of an exact value adjacent to suppressed cells. After applying secondary suppression, verify that no combination of visible cells allows back-calculation of any suppressed value.",
  "TAB-014" = "Review derived temporal and spatial columns before egress. Options: (1) round to coarser units - age-at-recruitment to nearest year, time-since-diagnosis to nearest 3-month quarter, distances to nearest 10 km band; (2) cap extreme values that identify rare events (e.g. censor survival times at the 95th percentile); (3) if the derived variable is a direct function of a date, confirm the source date cannot be reconstructed from it. Derived variables are not de-identified by default - they carry the precision of their inputs.",
  "TAB-015" = "Apply suppression or aggregation to raise k to at least the suppression threshold. Options: (1) merge rare category combinations - group strata with fewer than the threshold into an 'Other' category; (2) reduce quasi-identifier precision - round age to 5-year bands, truncate dates to year or quarter, replace full postcodes with outward codes, use broader ethnic group categories; (3) remove the least necessary quasi-identifier column entirely. After each change, re-run the assessment to confirm k has improved. Check TAB-013 (secondary suppression) after any change to count or total columns.",
  "TAB-016" = "Manually identify which columns in this file are quasi-identifiers (age, sex, ethnicity, geography, dates, diagnoses, occupation, or computed derivatives). Compute k-anonymity in R using: df %>% group_by(across(c(qi_col_1, qi_col_2, ...))) %>% summarise(n=n()) %>% arrange(n). NHS SDC guidance recommends k \u2265 5 before egress. If this file is large, assess a representative stratified sample. Ensure the result is documented in your submission note for the reviewer.",

  # ── Script / Notebook ─────────────────────────────────────────────────────
  "SCR-001" = "Remove hardcoded identifier values from the script. If specific participant IDs are needed for reproducibility documentation, store them in a separate configuration file that is not included in the egress batch.",
  "SCR-002" = "Remove inline data construction from the script. Load data from a separately reviewed file, or replace the inline data block with synthetic placeholder values that demonstrate the code structure without containing real data.",
  "SCR-003" = "Replace plain-language sensitive phenotype labels in code with coded identifiers (ICD codes, SNOMED codes, or internal project codes). If the terms appear only in comments, remove or generalise the comments.",
  "SCR-006" = "Clear all cell outputs before egress. In Jupyter: use 'Kernel > Restart and Clear Output'. In RMarkdown: regenerate the document from source without including raw data chunks. Check that no cell output contains tabular data or participant-level plots.",

  # ── Genomic ───────────────────────────────────────────────────────────────
  "GEN-001" = "Replace per-sample genotype columns with summary statistics (allele frequencies, p-values, effect sizes). Individual genotype data requires explicit approval under most genomic data access agreements and should not be included in standard research outputs.",
  "GEN-002" = "Remove phenotype annotations from sample metadata rows or columns. If phenotype associations are needed in the output, provide them as aggregate summary statistics rather than per-sample labels.",
  "GEN-003" = "GWAS summary statistics are a standard approved output. Verify that no per-sample rows are present, that sample size N is reported at the study level only, and that minor allele frequency filters have been applied to suppress rare variant information.",
  "GEN-004" = "Apply a minor allele frequency filter (MAF ≥ 0.01) using bcftools or PLINK before egress: 'bcftools filter -i \"MAF>=0.01\" input.vcf.gz -o filtered.vcf.gz'. Alternatively, convert to summary statistics (allele frequencies, effect sizes) rather than exporting variant-level data.",
  "GEN-005" = "Review the implied sample size against the approved cohort size in your data access agreement. If the sample size should not be disclosed, strip NS= and AN= annotations from the INFO field using 'bcftools annotate --remove INFO/NS,INFO/AN'.",
  "GEN-006" = "Remove all ##PEDIGREE= and ##SAMPLE= meta-information lines before egress. Use 'bcftools reheader' or grep to strip these lines: 'grep -vE \"^##PEDIGREE|^##SAMPLE\" input.vcf > output.vcf'. Verify removal before resubmission.",
  "GEN-007" = "If per-sample depth and quality fields are not required for the analysis, strip DP and GQ from the FORMAT field: 'bcftools annotate --remove FORMAT/DP,FORMAT/GQ'. If these fields are required, document the justification in your governance note.",
  "GEN-008" = "The .fam file contains individual-level data and cannot be egressed as-is. Options: (1) remove the file entirely if it is not needed for the receiving analysis; (2) replace individual IDs with pseudonyms using a mapping table held outside the workspace; (3) remove the phenotype column (column 6) if phenotype status is not required. Pedigree structure (columns 3-4) must also be reviewed for family reidentification risk.",
  "GEN-009" = "Review the .bim file under your data access agreement. If the variant panel is considered sensitive, consider providing only summary-level variant statistics rather than the full map. Check whether any rsIDs correspond exclusively to rare disease loci that could narrow the study population.",
  "GEN-010" = "The .bed binary genotype file cannot be egressed. It must be either: (1) converted to summary statistics using PLINK ('plink --bfile input --freq --out output'); or (2) explicitly approved under your data access agreement with specialist review. Do not attempt to circumvent this by renaming the file.",
  "GEN-011" = "The .pgen PLINK2 binary genotype file cannot be egressed without specialist review. Convert to summary statistics using PLINK2 ('plink2 --pfile input --freq --out output') or obtain explicit approval under your data access agreement.",
  "GEN-012" = "The .psam file contains individual-level phenotype and covariate data. Remove or pseudonymise individual IDs and review covariate columns for disclosure risk before resubmission. Treat as equivalent to a .fam file plus additional clinical covariates.",
  "GEN-013" = "Sample size below 100 in GWAS summary statistics raises reconstruction risk. Verify the N column reflects the full discovery cohort, not a subgroup. If the study is genuinely small, consult your data governance lead before approving egress - additional suppression (e.g. p-value floor, MAF filter increase) may be required.",
  "GEN-014" = "Fewer than 1,000 variants in a file presented as GWAS summary statistics is unusual. Confirm this is an intentional filtered subset (e.g. a target gene region) and document the filtering rationale in your governance note. If the low row count is unexpected, ask the researcher to confirm the file is complete.",
  "GEN-015" = "Extreme p-values in small cohorts may identify individual contributors. Consider applying a p-value floor (e.g. replace p < 1×10⁻³⁰ with p = 1×10⁻³⁰) or discuss with your data governance lead whether the signal requires additional protection.",

  # ── Archive ───────────────────────────────────────────────────────────────
  "ARC-001" = "Extract, remediate, and repackage the archive contents. Each file within the archive must independently pass disclosure checks before the archive can be approved.",
  "ARC-002" = "Re-create the archive without encryption so that automated content inspection can proceed. If encryption is required for transit, apply it after review approval.",
  "ARC-003" = "Extract the archive and submit each high-risk file individually to the Airlock Checker for full disclosure assessment. Do not approve egress of an archive containing uninspected high-risk file types.",
  "ARC-004" = "Extract the archive and submit each tabular or script file individually to the Airlock Checker. Archives that bundle inspectable files cannot be reviewed as a unit.",
  "ARC-005" = "Review the complete archive manifest before approving. Consider whether all files are necessary for egress, and extract and assess the highest-risk content individually.",
  "TAB-018" = "Remove or pseudonymise the identified PII values before submission. For NHS numbers: replace with a pseudonym or research ID using a consistent mapping table kept outside the workspace. For NI/CHI numbers: remove entirely or replace with a research ID. For email addresses: remove the column or replace values with a domain-only representation if the domain is needed.",
  "TAB-019" = "Replace full postcodes with the outward code only (e.g. SW1A rather than SW1A 1AA) or with LSOA/MSOA codes if geographic granularity is needed for analysis. Full postcodes identify approximately 15 households and must not appear in egress outputs unless explicitly approved.",
  "TAB-020" = "Remove phone number columns from the export, or replace values with a pseudonymised reference if the column is needed for linkage purposes. Phone numbers are direct identifiers under GDPR and UK data protection law.",
  "TAB-021" = "Remove the column or replace exact dates of birth with year of birth or 5-year age bands. Exact dates of birth combined with any other demographic variable significantly increase reidentification risk.",
  "TAB-023" = "Secondary suppression must be applied to prevent back-calculation. Options: (1) suppress additional cells so that no suppressed value is uniquely determined by the row or column total; (2) remove the total row/column from the output; (3) apply noise/rounding to totals so the arithmetic constraint is broken. After applying secondary suppression, re-run the assessment to confirm the rule no longer fires.",
  "TAB-024" = "Remove or pseudonymise free-text fields before egress. Options: (1) delete the column entirely if the information is not needed by the receiving analyst; (2) replace with coded values (ICD codes, SNOMED terms, occupation codes) rather than prose; (3) apply NLP de-identification using a validated tool (e.g. MedCAT, spaCy NER) and verify the output before submission. Document which de-identification approach was used in your governance note.",
  "TAB-022" = "Informational profile only. Review high-uniqueness columns (>80%) for potential identifier risk. Columns with 100% uniqueness may be direct identifiers even if not flagged by name heuristics.",
  "TAB-017" = "Use haven::zap_labels() to strip all variable and value label metadata before exporting. Alternatively, export as CSV (labels are not preserved in CSV), ensure no sensitive category terms are embedded in the metadata, then resubmit.",
  "STAT-001" = "Ensure the statistical file has passed all tabular disclosure checks. Use haven::read_dta/sav/sas() and review the data frame before egress. Remove sensitive columns, suppress small counts, and apply k-anonymity checks.",
  "STAT-002" = "Install the haven R package (run dependencies.R) then re-run the assessment to enable full tabular inspection of this statistical file.",
  "NII-001" = "Strip identifier metadata from the NIfTI header using tools such as fslhd with manual editing, or re-export from your analysis pipeline with header fields cleared. Verify that descrip, aux_file, and intent_name fields are empty or contain only scan parameters.",
  "NII-002" = "Apply a validated de-facing tool (pydeface, mri_deface, fsl_deface) to remove facial surface data from the 3D volume before egress. Retain the de-facing script or tool provenance for the governance record.",
  "NII-003" = "Confirm the body part and assess facial reconstruction risk. Apply de-facing if the volume includes head anatomy. Document the anatomical region and any de-identification steps in the reviewer note.",
  "NII-004" = "Verify the file is a valid NIfTI-1 or NIfTI-2 file. For paired formats, ensure both .hdr and .img files are present. If the file is valid but unreadable, request a re-export from the source pipeline.",
  "NII-005" = "NIfTI header metadata check passed. Manual review of imaging content is still required to confirm no facial reconstruction risk exists. Use the Preview button to inspect the file if oro.nifti is installed.",

  # ── Binary ────────────────────────────────────────────────────────────────
  "BIN-001" = "Convert to an approved egress format before resubmitting. If the binary format is essential and cannot be converted, submit a written justification to your reviewer explaining why the format is necessary and what the file contains.",

  # ── Image ─────────────────────────────────────────────────────────────────
  "IMG-001" = "Export as a static image (PNG, PDF, or SVG without embedded JavaScript). If using ggplot2 or matplotlib, use standard static export functions. Interactive Shiny or Plotly outputs must be converted to static equivalents before egress.",
  "IMG-002" = "Remove participant identifiers from all plot labels, axis tick labels, titles, and annotations. Replace with aggregate group labels, anonymised codes, or remove entirely. Regenerate the figure from the updated analysis.",
  "IMG-003" = "Visually inspect the image to confirm it contains no patient names, demographic information, clinical identifiers, or burned-in annotations. If clean, approve with a note confirming manual inspection was performed. If identifiable content is found, remove it from the source analysis and regenerate the figure.",
  "IMG-004" = "Remove or replace images containing identifiable individuals. Use schematic diagrams, averaged composites, or images where faces and identifying features have been appropriately obscured.",

  # ── PDF ───────────────────────────────────────────────────────────────────
  "PDF-001" = "Remove password protection before resubmission. Use your PDF editor or the command `qpdf --decrypt input.pdf output.pdf` to produce an unprotected copy for review.",
  "PDF-002" = "Redact the identifier values flagged above using a PDF editor (Adobe Acrobat, LibreOffice, or `pdftk`). Ensure redaction permanently removes the underlying text - do not use image overlays or black rectangles that leave text in the PDF structure.",
  "PDF-003" = "Replace or redact sensitive phenotype labels in the source document and regenerate the PDF. If the terms appear in a table or figure generated from analysis code, update the labels in the code and re-export.",
  "PDF-004" = "Apply small-number suppression in the source analysis and regenerate the PDF. Replace counts below the threshold with '<5'. Review all tables for back-calculability from row and column totals.",
  "PDF-005" = "Confirm that restricted field data is covered by your data access agreement before resubmitting. If it is, provide written confirmation to your reviewer. If it is not, remove the restricted content from the document.",
  "PDF-006" = "If this is an image-only (scanned) PDF, run OCR to produce a text-searchable version, or provide the source document in a text-based format (.docx, .html, .md). If this is a text PDF and the error is unexpected, contact your workspace administrator.",
  "PDF-007" = "No action required. Standard PDF document.",

  # ── Office documents ──────────────────────────────────────────────────────
  "DOC-001" = "Remove password protection and resubmit the unencrypted file for review.",
  "DOC-002" = "Use Find & Replace (Ctrl+H) to locate and remove or redact the identifier values flagged above. Save as a new file. If the document was generated from a script, update the script to suppress identifiers and regenerate.",
  "DOC-003" = "Replace sensitive phenotype terms in the document with coded equivalents, or remove them if they are not essential to the output. If the document was generated from a script, update labels at source.",
  "DOC-004" = "Apply small-number suppression in the source analysis and regenerate the document. Replace counts below the threshold with '<5'. Check that totals and derived statistics are not back-calculable.",
  "DOC-005" = "Confirm restricted field coverage with your data governance lead before resubmitting. Provide written confirmation to your reviewer if approved.",
  "DOC-006" = "Convert the .doc file to .docx format using Word or LibreOffice (File > Save As > .docx) and resubmit. The .doc binary format cannot be automatically inspected.",
  "DOC-007" = "No action required. Standard document.",

  # ── HTML ──────────────────────────────────────────────────────────────────
  "HTM-001" = "Export as static HTML (no JavaScript) or convert to PDF before resubmitting. In RMarkdown, use `output: pdf_document` or set `runtime: static`. Remove Plotly, D3, Shiny, and Observable dependencies - use static ggplot2 or matplotlib equivalents instead.",
  "HTM-002" = "Remove identifier values from the HTML source or analysis script and regenerate the file. Check table cell values, axis labels, and any inline text that references participant IDs.",
  "HTM-003" = "Replace sensitive phenotype labels with coded equivalents in the analysis script and regenerate the HTML output.",
  "HTM-004" = "Apply small-number suppression in the source analysis and regenerate the report. Values below the threshold should be replaced with '<5' before the HTML is produced.",
  "HTM-005" = "Confirm restricted field coverage before resubmitting and notify your reviewer in writing.",
  "HTM-006" = "Review whether the embedded table contains individual-level rows. If so, aggregate before including in the report. If it is a summary table with one row per group, confirm this to the reviewer in your submission note.",
  "HTM-007" = "No action required. Standard HTML output.",

  # ── JSON ──────────────────────────────────────────────────────────────────
  "JSON-001" = "Remove the credential value(s) immediately and rotate any exposed keys or tokens as a security precaution - treat them as compromised. Credentials must never appear in research outputs. If configuration values are needed for reproducibility, store them in a separate secrets file excluded from the egress batch.",
  "JSON-002" = "Remove or hash identifier fields from the JSON. If the identifiers are array keys or object fields, replace values with synthetic pseudonyms. If the file was generated by a script, update the serialisation step to exclude identifier fields.",
  "JSON-003" = "Remove or replace sensitive phenotype keys and values in the JSON. If the file is generated programmatically, update the field naming conventions to use coded identifiers.",
  "JSON-004" = "Replace the record array with aggregate summary statistics. If individual-record JSON output is required under your data access agreement, this must be explicitly approved - contact your data governance lead.",
  "JSON-005" = "No action required. Standard JSON output.",

  # ── XML ───────────────────────────────────────────────────────────────────
  "XML-001" = "Individual patient records in FHIR, HL7, or CDA format require explicit clinical governance approval before egress. This is not a standard research output pathway. Contact your data governance team and information governance lead before resubmitting.",
  "XML-002" = "Remove or anonymise identifier elements and attribute values. If the XML was exported from a database or API, update the export configuration to exclude patient identifier fields.",
  "XML-003" = "Replace sensitive phenotype element content with coded values (ICD, SNOMED) or remove the elements if they are not essential to the output.",
  "XML-004" = "Aggregate the data to summary format before egress. An XML file with hundreds of repeating patient-level records requires the same approval as individual-level tabular data.",
  "XML-005" = "No action required. Standard XML output.",

  # ── Markdown ──────────────────────────────────────────────────────────────
  "MD-001" = "Remove identifier values, NHS numbers, and unmasked counts from the document text. If the document was generated from analysis code, update the source and regenerate. Apply small-number suppression to any count values below the threshold.",
  "MD-002" = "Replace sensitive phenotype terms in the document text with coded equivalents, or remove them if they serve only as contextual labels that do not need to appear in the final output.",
  "MD-003" = "Review whether the embedded table contains individual-level rows. Aggregate to group level if so, or confirm to your reviewer that each row represents a summary group, not an individual.",
  "MD-004" = "No action required. Standard markdown document.",

  # ── Serialised / database / columnar ─────────────────────────────────────
  "SER-001" = "Convert the serialised R object to a plain-text format (CSV, JSON, or Parquet) before egress. Serialised objects can contain arbitrary R data structures including raw datasets that are not visible in a standard file preview.",
  "SER-002" = "Convert to a plain-text format before egress. If the serialised object contains only model parameters or summary statistics, export those specifically rather than the full object.",
  "DAT-001" = "Export the specific query results as a CSV file rather than egressing the full database file. A SQLite database may contain multiple tables including raw data that was not intended for egress.",
  "COL-001" = "If this file contains summary data, consider converting to CSV for transparency and reviewability. If it contains individual-level records, aggregate before egress or confirm approval under your data access agreement.",

  # ── DICOM ─────────────────────────────────────────────────────────────────
  "DCM-001" = "Strip all direct identifier tags before egress using a DICOM anonymisation tool. The minimum required: PatientName (0010,0010) set to a pseudonym or empty; PatientID (0010,0020) replaced with a study-specific pseudonymous code; PatientBirthDate (0010,0030) removed or shifted; ReferringPhysicianName (0008,0090) emptied; AccessionNumber (0008,0050) removed or replaced. Consider using a validated DICOM de-identification profile (DICOM PS3.15 Annex E) rather than manual tag editing. Re-run the Airlock Checker after de-identification to confirm all identifier tags are cleared.",
  "DCM-002" = "Burned-in annotations cannot be removed by tag stripping. The image pixels themselves contain the patient information. Options: (1) re-export the image from the PACS or imaging software without burned-in demographics enabled; (2) apply pixel-level redaction to mask the annotation region - this requires specialist software and must be validated before egress; (3) if the annotation region is cropped out, confirm the cropped image retains sufficient scientific utility. Document which approach was taken in the submission note.",
  "DCM-003" = "Head imaging files require manual specialist review for facial reconstruction risk before egress can be approved. Structural MRI and CT volumes of the head and face can be surface-rendered to produce a recognisable facial likeness, constituting a biometric identifier. Automated de-facing tools (e.g. pydeface, mri_deface, FSL deface) can be applied to remove the facial surface from the volume while preserving the brain tissue of scientific interest. Confirm with your data access agreement whether head imaging egress requires additional governance approval.",
  "DCM-004" = "Truncate or shift study and content dates before egress. Options: (1) replace with year only (YYYY0101) - this removes the day-level precision while retaining the year for longitudinal analysis; (2) apply a consistent date shift (e.g. shift all dates by the same random offset per participant) - ensure the shift is consistent across all related files in the submission; (3) remove dates entirely if they are not required for the analysis. Apply the same approach to all date-containing tags: StudyDate, ContentDate, AcquisitionDate, and SeriesDate.",
  "DCM-005" = "Review whether institution and equipment tags need to be retained for the scientific purpose of this output. If not required: empty InstitutionName (0008,0080), InstitutionAddress (0008,0081), and StationName (0008,1010). If institution identity is scientifically necessary (e.g. for multi-site studies), confirm this is within the scope of your data access agreement and document the justification in your submission note.",
  "DCM-006" = "The file could not be parsed as a standard DICOM file. Possible causes: corrupted file; proprietary or non-standard transfer syntax; DICOMDIR index file (which indexes a directory rather than containing image data). Confirm the file is a valid DICOM image, attempt to re-export from the source PACS system, or convert to a standard transfer syntax. If this is a DICOMDIR, inspect the individual image files it references instead.",

  # ── ACRO/SACRO integration ─────────────────────────────────────────────
  "ACR-001" = paste0(
    "This output failed ACRO's SDC checks at analysis time. The specific cells that need ",
    "suppression are listed in the evidence panel. Options: (1) apply cell suppression using ",
    "acro.suppress(), (2) aggregate the table to eliminate small cells, (3) provide an exception ",
    "justification if the values are not disclosive (e.g. synthetic data, non-individual counts). ",
    "Re-run ACRO and re-finalise before resubmitting."),
  "ACR-002" = paste0(
    "ACRO flagged this as a custom output requiring manual review. Examine the output content ",
    "(plot, image, or non-tabular file) for any identifiable information, small-group patterns, ",
    "or other disclosure risks. The researcher's comments describe what the output contains."),
  "ACR-003" = "No action required. ACRO confirmed this output passed all SDC checks.",
  "ACR-004" = paste0(
    "The ACRO session used different SDC thresholds than AIRAlock's current configuration. ",
    "Check whether the difference is intentional (e.g. different pipeline stages may use ",
    "different thresholds) or an error. If the ACRO threshold is more lenient, outputs that ",
    "passed ACRO may still fail AIRAlock's rules."),
  "ACR-005" = paste0(
    "ACRO references output files not in this submission batch. The researcher may have chosen ",
    "not to submit those outputs, or files were renamed after the ACRO session was finalised. ",
    "Ask the researcher to confirm which outputs they intend to submit."),
  "ACR-006" = paste0(
    "The file's checksum does not match the ACRO session record. The file may have been modified ",
    "after ACRO ran its checks. Re-run ACRO on the current file to get up-to-date SDC results, ",
    "or review the changes manually."),
  "ACR-007" = "No action required. ACRO session metadata file."
)

# Look up remediation text for a rule hit.
# Where the rule detail mentions specific columns/values, we can interpolate.
get_remediation <- function(rule_id, detail="") {
  base <- REMEDIATION[[rule_id]]
  if (is.null(base)) return(NULL)

  # Contextualise where detail gives us a specific column or value
  if (rule_id == "TAB-001" && nzchar(detail)) {
    cols <- regmatches(detail, gregexpr("'[^']+'" , detail, perl=TRUE))[[1]]
    if (length(cols) > 0)
      base <- paste0("Remove or hash column(s) ", paste(cols, collapse=", "),
        " before resubmitting. If a linking key is needed, replace values with a ",
        "synthetic pseudonym that cannot be reverse-mapped to individuals.")
  }
  if (rule_id == "TAB-005" && nzchar(detail)) {
    base <- paste0("Apply small-number suppression: replace counts below the threshold ",
      "with '<5'. Review percentage and rate columns - if the denominator is known, ",
      "suppressed counts can be back-calculated from a percentage. Detail: ", detail)
  }
  if (rule_id %in% c("JSON-001","DOC-007","HTM-007","JSON-005",
                      "XML-005","MD-004","PDF-007","GEN-003",
                      "ACR-003","ACR-007") &&
      grepl("^No action", base)) return(NULL)   # Suppress green-rule remediation

  base
}