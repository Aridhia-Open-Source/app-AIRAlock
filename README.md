# AIRAlock

Statistical disclosure control for egress review in Trusted Research Environments, combining a deterministic rule engine with an in-workspace AI advisory layer.

AIRAlock inspects research output files before they leave an Aridhia Digital Research Environment (DRE) workspace. A deterministic rule engine classifies each file against more than 100 disclosure-control rules. An AI advisory layer, powered by the Aridhia AIRA service running inside the workspace, narrates the rule findings and supplies interpretive context the heuristics cannot capture. The reviewer reads both signals and records the decision. Every decision is written to an append-only audit log and preserved in a governance PDF.

The governing principle is simple and binding: **the rule engine classifies, AIRA narrates, the reviewer decides.** AIRA never produces a competing verdict, never overrides a classification, and never writes audit entries.

You can view a video of how to use AIRA here: https://scribehow.com/embed-preview/Conducting_a_Data_Airlock_Review_and_Approval__jHS7L0YoSKGhqMsc8irNqA?as=video&size=flexible&voice=shimmer&scaleMode=contain

---

## Status

| Property | Value |
|---|---|
| Archetype | File-based, air-gapped, AIRA-augmented |
| Platform | Aridhia DRE workspace (Azure-hosted) |
| Runtime | R 4.0 or later, Shiny |
| Rule catalogue | 106 rules across 18 categories |
| Network access | None, except the single sanctioned AIRA endpoint |
| Distribution | Aridhia community app |

---

## Contents

- [Overview](#overview)
- [Key concepts](#key-concepts)
- [How it works](#how-it-works)
- [The rule engine](#the-rule-engine)
- [The AIRA advisory layer](#the-aira-advisory-layer)
- [Operating modes](#operating-modes)
- [File type coverage](#file-type-coverage)
- [Requirements](#requirements)
- [Installation and deployment](#installation-and-deployment)
- [Configuration](#configuration)
- [Audit and governance](#audit-and-governance)
- [Reviewer workflow](#reviewer-workflow)
- [Architecture](#architecture)
- [Development](#development)
- [Testing](#testing)
- [Known limitations](#known-limitations)
- [Security model](#security-model)
- [Reference documents](#reference-documents)
- [Licence](#licence)

---

## Overview

When a researcher in a Trusted Research Environment (TRE) is ready to take results out, egress review is the point at which every upstream protection (project agreements, controlled ingest, isolated compute) either holds or fails. If the review is weak or inconsistent, identifiable data leaks out, and the rationale behind a release cannot be defended later when it is questioned.

Egress review has to satisfy three requirements at once. It must be **defensible** if anyone asks why a file was released six months on. It must be **consistent** across reviewers and across time. It must be **fast enough** that review does not become a bottleneck for the research. Reviewers face high and bursty volume, subtle signals (a file can be technically clean yet re-identifying when combined with auxiliary data), and the constant risk of pattern-matching a past decision onto a file that does not quite fit the pattern.

AIRAlock implements a rules-with-AI-advisory model rather than either of the two common alternatives. Pure manual review scales badly and produces an audit trail that is only whatever the reviewer typed in a comment box. Pure AI review is non-deterministic, exposes no enumerable policy a governance team can stand behind, and leaves accountability unclear when something goes wrong. AIRAlock keeps the gating deterministic and version-controlled, and uses AI only as an interpretive second opinion the reviewer reads alongside the rule output.

---

## Key concepts

**Trusted Research Environment (TRE).** A controlled compute environment for analysis of sensitive data. The Aridhia DRE is a specific TRE platform.

**Statistical disclosure control (SDC).** The body of methods for preventing re-identification of individuals from released data. AIRAlock's rules are derived from established SDC guidance.

**Egress.** The movement of files from inside the TRE to outside. AIRAlock checks files at egress time, after they exist as artefacts in the workspace.

**Quasi-identifier.** A column that does not directly identify an individual but, combined with others, can. K-anonymity and linkage checks reason over quasi-identifiers.

**The trust boundary.** AIRA runs inside the workspace, so prompts can contain real values from the file under review without crossing any governance boundary. This is what makes the advisory layer genuinely data-aware rather than restricted to metadata. Sampling sent to AIRA remains bounded for audit reproducibility.

**The governance contract.** AIRA narrates rule findings. The engine classifies. The reviewer decides. AIRA never produces a competing verdict, never overrides classifications, and never writes audit entries. This binding is specified in the product requirements at section 14.1.

---

## How it works

Each file passes through a fixed pipeline:

1. **Detection.** The file type is determined from extension, magic bytes, and (for ambiguous text) content heuristics. Detection handles files whose extension lies, such as binary data with a `.csv` extension.
2. **Inspection.** The relevant inspector runs the rules applicable to that file type and returns a list of structured hits. Inspectors never crash on pathological input; on any failure they return a sentinel `UNCERTAIN` hit and the batch continues.
3. **Classification.** The engine derives a single file-level outcome from the hits and computes an informational numeric risk score.
4. **AIRA advisory.** For non-GREEN files, the AIRA layer reviews the engine output plus a bounded structural sample of the file and returns a structured narrative assessment.
5. **Decision.** In reviewer mode, the reviewer records approve, reject, or approve-with-override, each with a mandatory rationale.
6. **Record.** The decision is written to the append-only audit log and is preserved, alongside rule evidence and the AIRA assessment, in the governance PDF.

Batch-level checks run across the whole selection, including linkage risk where multiple files share quasi-identifier columns.

---

## The rule engine

The engine is deterministic and version-controlled. The same file inspected twice with the same configuration produces identical hits. The only exception is cross-file linkage analysis, which is batch-dependent by nature.

### Outcomes

| Outcome | Meaning | Reviewer guidance |
|---|---|---|
| GREEN | No risk detected; the rule passed affirmatively | Safe by this rule; check others |
| AMBER | Caution; something warrants review | Reviewer judgement required |
| RED | Likely disclosure risk; the rule fired with high confidence | Should not be approved without strong rationale |
| UNCERTAIN | The inspector could not complete (parse error, missing optional dependency) | Treat conservatively, not as GREEN |

A file's overall classification is derived from its hits by precedence: any RED yields RED; otherwise any UNCERTAIN yields UNCERTAIN; otherwise any AMBER yields AMBER; otherwise GREEN. The numeric score is informational only; the classification is the binding output.

### Rule categories

The catalogue contains 106 rules across 18 categories. The authoritative list lives in code (`02_rules.R`); the table below is a summary.

| Prefix | Domain | Count |
|---|---|---|
| TAB | Tabular (CSV, TSV, XLSX): identifiers, small counts, k-anonymity, free text, suppression | 24 |
| GEN | Genomic: VCF, PLINK, BAM, GWAS summaries, pedigree | 15 |
| DCM | DICOM: patient identifiers, burned-in text, private tags | 7 |
| PDF | PDF: extracted-text PII, forms, metadata | 7 |
| DOC | Office: DOCX, PPTX, XLSX-as-document text and metadata | 7 |
| HTM | HTML and web pages: embedded Plotly, iframes, scripts | 7 |
| NII | NIfTI: header PII, defacing status | 5 |
| ARC | Archives: ZIP, TAR, 7Z manifests | 5 |
| JSON | JSON: credentials, nested identifiers | 5 |
| XML | XML: FHIR/HL7, identifier elements | 5 |
| SCR | Scripts (R, Python, SQL): hardcoded IDs, sensitive filters, credentials | 4 |
| MD | Markdown: embedded IDs, front matter | 4 |
| IMG | Images: EXIF GPS and device metadata | 4 |
| STAT | Statistical: SAS, Stata, SPSS | 2 |
| SER | Serialised: RDS, pickle | 2 |
| DAT | Databases: SQLite, DuckDB | 1 |
| COL | Columnar: Parquet, Arrow | 1 |
| BIN | Unknown binary | 1 |

### Representative rules

A few rules illustrate the kinds of checks performed:

- **TAB-005, unmasked small counts.** Fires when a count-shaped column (named `n`, `count`, `freq`) contains values below a configurable threshold (default 5).
- **TAB-015, k-anonymity below threshold.** Groups rows by quasi-identifier columns and flags equivalence classes smaller than a configurable k. RED if k is below half the threshold, AMBER if below the threshold.
- **TAB-018, direct identifiers in cells.** Matches NHS numbers using mod-11 check-digit validation rather than a bare ten-digit regex, so accession numbers and study identifiers that merely share the shape do not fire. Also matches CHI numbers, NI numbers, and email addresses.
- **TAB-023, suppression back-calculation.** When a table has both totals and suppressed cells, checks whether a suppressed value can be recovered by subtraction. A single suppressed cell next to a total is recoverable and is therefore RED, even though the cell itself shows nothing.
- **GEN-001, per-sample genotypes.** Detects VCF files carrying per-sample genotype columns, which is individual-level data.
- **JSON-001, credentials in JSON.** Detects keys such as `api_key`, `password`, or `secret` with non-empty values.

Every rule that fires produces structured evidence: the columns it flagged, the rows it sampled, the values it could back-calculate. That evidence is visible to the reviewer in the UI and preserved in the governance PDF. At no point is the reviewer asked to take the engine's word for it.

### Linkability gating

Identifier-named columns and high-uniqueness columns are signals, not verdicts. Before firing RED on an identifier shape, the engine runs a linkability classifier that returns one of `synthetic_sequential`, `synthetic_uuid`, `synthetic_hash`, or `unrecognised`. Only the three positive synthetic classes downgrade RED to AMBER; everything else preserves the existing severity. The design is conservative by direction: a real identifier wrongly classified as synthetic would be unacceptable, whereas a synthetic identifier kept at RED is tolerable.

A specific guard protects against NHS-shaped data: `synthetic_sequential` requires a maximum value of seven digits or fewer. Sequential ten-digit numbers, which appear in sorted cohort extracts and look NHS-shaped, would otherwise classify as synthetic and downgrade real identifiers. The length guard prevents this, and the reason is recorded in the evidence string so it is visible in the audit log. Linkability gating is applied in the identifier-name rule (TAB-001) and the high-uniqueness rule (TAB-003).

### Quasi-identifier context

Uniqueness alone is a poor signal for individual-level data, because measurement columns, free-text columns, and transactional logs all have high uniqueness without per-participant identification risk. For the high-cardinality rule (TAB-003), after the linkability check passes, the engine checks whether quasi-identifier columns exist elsewhere in the file and whether the row count is in per-participant scale before firing RED. If either condition fails, the outcome is AMBER with the reason recorded. The outcome priority is: synthetic linkability gives AMBER, then quasi-identifier context gives RED, then absence of context gives a final AMBER fallback, each path producing distinct evidence prose.

### Batch linkage

Beyond per-file rules, the engine checks for linkage risk across a batch. When multiple files share quasi-identifier columns, the batch's overall classification can be raised even where individual files are GREEN. The default trigger is at least two files sharing at least three categories of quasi-identifier.

---

## The AIRA advisory layer

AIRA is the in-workspace LLM service that powers the advisory layer. All AIRA traffic flows through a single self-contained module (`27_aira.R`), the only place that constructs a client, calls the model, or reads the workspace API key. The Shiny layer calls a stable public API; everything else in the module is private.

### Governance role

AIRA narrates rule findings and surfaces context the heuristics cannot capture. It never overrides the engine. If TAB-018 fired on a file, the file stays RED until a reviewer says otherwise. A concrete case shows the value: GWAS summary statistics often contain an `rs_id` column with thousands of unique values, which trips the engine's high-cardinality rule. AIRA sees the column names, recognises summary statistics, and reports low concern with a one-line explanation. The reviewer sees both signals side by side and decides whether the conservative flag warrants holding the file.

### Public API

The public surface is stable and additive:

- `aira_is_enabled(cfg)`, the single gate for whether AIRA runs
- `aira_status_summary(cfg)`, diagnostics
- `aira_review_disclosure(result, cfg)` and `aira_review_disclosure_async(...)`
- `aira_summarise_file(result, cfg)` and `aira_summarise_file_async(...)`
- `aira_summarise_batch(res, cfg, aira_reviews)` and `aira_summarise_batch_async(...)`
- `aira_thinking_disabled()`, introspection
- `set_aira_client_override(fn)` and `get_aira_client_override()`, a test seam

### Response shape

Every public function returns the same six-field object on every path (success, disabled, timeout, network failure, malformed output, async rejection):

```r
list(
  status         = "ok",   # one of: ok, disabled, timeout, unavailable, malformed
  text           = "",
  prompt_version = "",
  duration_ms    = 0L,
  reason         = "",
  timing_ms      = NULL
)
```

Consumers switch on all five statuses; none assumes `"ok"`. The UI never blocks on AIRA, and an AIRA failure never blocks the audit pathway. Async variants translate promise rejection into `status = "unavailable"`.

For the disclosure review, the `text` field carries a structured assessment (the current shape, `disclosure_review_v6`):

```
{
  dataset_recognition: { recognised, name, confidence, evidence },
  structure_summary:   <1 to 2 sentences>,
  anomalies:           [ { column, observation } ],
  engine_alignment:    { agrees: yes | no | cannot_assess, rationale },
  reviewer_focus:      <1 sentence>
}
```

### Prompt registry

Prompts are frozen and versions are immortal. Each prompt has a version (equal to its registry key), a system message, a user builder, model parameters, and a response validator keyed by shape. Once shipped, a prompt is never edited and never removed, because audit entries reference versions by name and replay requires the exact text. Any change, including a typo fix, becomes a new version, and an active-prompt map points at the live one. Superseded versions remain in the registry for rollback and audit replay. The current active prompts are `disclosure_review_v6`, `batch_summary_v3`, and `summary_v1`.

### Thinking mode

Reasoning models emit thinking blocks before output. For prompts with strict JSON schemas, thinking adds no measurable quality but dominates decode time. The effect is large: one breast-cancer file moved from a 300-second timeout to a 91-second success once thinking was disabled. Thinking is therefore disabled by default, which passes `chat_template_kwargs = list(enable_thinking = FALSE)` to the client. If a client version rejects that parameter, construction falls back without it and AIRA continues; the outcome is recorded once per client at construction time.

### Bounded input

The prompt receives the rule engine output as its primary input, plus a bounded structural sample of the file: capped column names, column profiles for engine-flagged or otherwise interesting columns, and head-and-tail sample rows for tabular files, or a content excerpt for non-tabular types. The sampling caps are deterministic constants so that audit replay is reproducible. Because AIRA is an offline workspace LLM inside the trust boundary, raw cell values within those bounds are permitted.

---

## Operating modes

The application runs in one of two modes, sharing the same codebase but exposing different actions. Mode is read at startup from `app_mode.txt` in the application directory. If the file is absent or invalid, the application defaults to reviewer, the safer mode. The current mode is shown in the UI at all times so a user is never unsure whether a decision is binding.

| Action | Researcher | Reviewer |
|---|---|---|
| Browse the workspace filesystem | Yes | Yes |
| Add files to an assessment batch | Yes | Yes |
| Run the assessment | Yes | Yes |
| View classifications, hits, evidence, remediation | Yes | Yes |
| Edit configuration | Session only | Session and saved |
| Approve or reject files for egress | No | Yes |
| Provide rationale notes | No | Required on every decision |
| Generate the governance PDF | Yes | Yes |
| Modify the audit log | No | No (append-only for everyone) |
| View the audit log | Own activity | Full |

---

## File type coverage

The detector classifies each file into one category, which maps to one inspector:

`tabular`, `script`, `genomic`, `archive`, `image`, `document`, `office`, `webpage`, `json`, `xml`, `markdown`, `serialised`, `database`, `columnar`, `dicom`, `nifti`, `statistical`, `binary`.

Every inspector takes a filesystem path and a configuration object, returns a possibly empty list of hits, and never throws. Inspectors survive empty files, truncated files, binary content with a text extension, invalid UTF-8, embedded null bytes, mismatched row lengths, duplicate column names, very wide tables, very long single values, all-NA columns, unreadable files, and broken symlinks. Text-format inspectors read through a shared safe-read helper that detects binary content and replaces invalid UTF-8 rather than letting downstream regular expressions throw.

---

## Requirements

### Runtime

- R 4.0 or later
- A Shiny-equivalent web framework on the workspace
- A modern browser reachable from inside the workspace
- Read access to the workspace files directory (default `/home/workspace/files`)
- Read and write access to the output directory (default `/home/workspace/files/airlockcheck`)

### Required packages

The application will not start without all of these:

| Package | Purpose |
|---|---|
| shiny | Web framework |
| bslib | Bootstrap theming |
| DT | Interactive tables |
| dplyr | Data manipulation |
| stringr | String handling |
| readr | CSV and TSV parsing |
| readxl | XLSX parsing |
| base64enc | File fingerprinting |
| ellmer | AIRA client (for AIRA-augmented deployment) |

### Optional packages

Specific inspectors degrade gracefully when these are absent:

| Package | Without it |
|---|---|
| pdftools | PDF text extraction; PDF rules return UNCERTAIN |
| oro.dicom | DICOM tag inspection; DICOM files return UNCERTAIN |
| oro.nifti | NIfTI header reading; pixel-data rules disabled |
| haven | SAS, Stata, and SPSS reading; statistical files return UNCERTAIN |

The workspace is network-isolated, so packages cannot be installed at runtime. Every dependency is guarded with `requireNamespace()`; the application never calls `install.packages()`. A standalone dependency check (`dependencies.R`) reports the status of each required and optional package, in plain text, with a non-zero exit status if any required package is missing.

---

## Installation and deployment

AIRAlock is deployed into a DRE workspace as a flat set of R files alongside `app.R`. There is no build step; the platform handles deployment and identity.

```bash
# Place the application under the persistent workspace store
cd /home/workspace/files/
unzip airlock_checker_<version>.zip
# The result is a single folder with all R files flat alongside app.R
```

To select the operating mode, place `app_mode.txt` in the application directory containing either `researcher` or `reviewer`. With the file absent or invalid, the application starts in reviewer mode.

Before first launch, run the dependency check from a workspace terminal to confirm the required packages are present:

```bash
Rscript dependencies.R
```

Launch the application through the usual workspace mechanism. On first run it creates its output directories under the workspace store if they are not already present.

### AIRA endpoint

For the advisory layer, the workspace must expose the AIRA service and the `WORKSPACE_API_KEY` environment variable. The default base URL is the regional AIRA endpoint (`https://api.uksouth.saas.aridhia.io/api/aira/v1`) with the `workspace-chat` model. If AIRA is unavailable or disabled, the engine, audit, and reporting paths continue to work; only the advisory layer is absent.

---

## Configuration

Configurations are named sets of rule thresholds and vocabulary lists that modify inspector behaviour. They are stored as JSON and read and written without external libraries, using a hand-rolled serialiser, because the air-gapped environment makes that more portable. A configuration can be viewed, modified for the session, applied, saved under a name, exported as JSON, loaded, and reset to defaults. Loading or modifying a configuration is itself an audit event: the log records the diff.

The AIRA layer carries its own configuration block. Its defaults are safe (AIRA off unless explicitly enabled), and `aira_is_enabled(cfg)` is the sole gate, never re-implemented at a call site:

```r
cfg$aira <- list(
  enabled          = TRUE,
  base_url         = AIRA_BASE_URL_DEFAULT,
  model            = "workspace-chat",
  timeout_s_file   = 15L,
  timeout_s_batch  = 45L,
  disable_thinking = TRUE
)
```

Per-file and batch timeouts are separate so that a slow batch summary does not constrain individual reviews.

---

## Audit and governance

### The audit log

The audit log is append-only and is never modified after writing. Each entry records an ISO 8601 UTC timestamp, the mode at the time, the event type, and, where applicable, the file path, the file fingerprint, the decision outcome, the rationale text, the rule IDs involved, and the reviewer identity asserted by the host workspace. The format is machine-readable for downstream analysis. The application refuses to overwrite existing entries and can detect truncation or replacement of the log.

Every AIRA-related event records the prompt version, the call duration, and per-phase timings (client construction, inference, parsing, async overhead), so a review can be reconstructed and, where needed, replayed against the exact prompt text.

### File fingerprinting

When a file is added to a batch, its SHA-256 fingerprint is computed and stored. This supports detection of changes between sessions, deduplication of identical files, and audit integrity, since decisions reference fingerprints rather than paths alone.

### The governance PDF

Every batch can produce a PDF report, generated locally with base R graphics primitives and no external rendering service. It contains the batch metadata and risk scorecard, every file's classification, every rule that fired with its evidence, the AIRA assessment where present, the reviewer's decisions and notes, and the configuration in effect. If the configuration was changed from defaults during the session, the report says so explicitly and lists the changed parameters. The report is the audit artefact: when a question arises about a particular release, it answers the question without anyone reconstructing the application state.

---

## Reviewer workflow

A reviewer browses the workspace filesystem from the configured root, never above it, and adds files to a batch. Running the assessment dispatches each file to its inspector with a progress indicator; a failure on one file is logged and converted to an UNCERTAIN sentinel rather than aborting the batch. For each file the reviewer sees the classification, the score, the fired rules with detail and evidence, the remediation guidance, and the AIRA assessment for non-GREEN files. A holistic batch summary becomes available once every non-GREEN file has a successful AIRA review; until then the control stays disabled with an explanatory note.

In reviewer mode, each result offers approve (for GREEN and AMBER), reject (for any file), or approve-with-override (for RED), each requiring a rationale. Bulk actions accept all GREEN and AMBER, or reject all RED, with a single shared rationale. Decisions are immediate; to change one, the reviewer records a new decision with its own rationale. Each decision updates the session state, writes to the audit log, and re-renders the affected result. After remediation, a researcher can re-run the assessment without restarting the session, and changed files are detectable by fingerprint.

---

## Architecture

AIRAlock is the file-based, air-gapped archetype augmented with AIRA. All source files sit flat alongside `app.R`, numerically prefixed, and are sourced by explicit `source(..., local = TRUE)` calls rather than a loop (a loop over `source()` creates a sub-scope that hides `ui` and `server` from the Shiny launcher).

```
<app_dir>/
  app.R                      entry point, sources modules, launches Shiny
  01_constants.R             paths, brand colours, mode, output directory
  02_rules.R                 rule definitions
  03_vocabularies.R          phenotype lists, quasi-identifier categories, defaults
  04_remediation.R           remediation guidance
  05_helpers.R               shared utilities and safe readers
  06_file_detection.R        file type detection and dispatch
  07_inspect_tabular.R       CSV, TSV, XLSX
  08_inspect_script.R        R, Python, SQL
  09_inspect_genomic.R       VCF, PLINK, SAM/BAM, GWAS
  10_inspect_archive.R       ZIP, TAR, 7Z
  11_inspect_image.R         PNG, JPEG metadata
  12_inspect_document.R      PDF, DOCX, PPTX, binary
  13_inspect_text.R          JSON, XML, Markdown, HTML
  14_inspect_medical.R       DICOM, NIfTI
  15_inspect_statistical.R   SAS, Stata, SPSS
  16_inspect_data.R          SQLite, DuckDB, columnar, serialised
  17_engine.R                classification, batch scoring, linkage
  18_preview.R               file previews
  19_preview_ui.R            preview rendering
  20_audit.R                 append-only audit log, fingerprinting
  21_config.R                JSON config load and save
  22_pdf_report.R            base R grid governance PDF
  23_evidence_ui.R           evidence tables and offender views
  24_css.R                   application styles
  25_ui.R                    UI definition
  26_server.R                server logic
  27_aira.R                  canonical AIRA module (sole AIRA path)
  dependencies.R             standalone dependency check
```

Runtime output lives under a single workspace root:

```
/home/workspace/files/airlockcheck/
  audit/          append-only audit log
  config/         saved configurations
  reports/        generated governance PDFs
  downloads/      sanitised exports (reviewer mode)
  logs/           diagnostic JSONL
  cache/          optional cached results by fingerprint
  fingerprints.csv
```

Brand colours are defined once in `01_constants.R` (`ARIDHIA_BLUE` `#0066A1`, `ARIDHIA_NAVY` `#003366`, accept green `#2E7D32`, reject red `#C62828`, amber `#E65100`) and propagate to the styles, the plots, and the PDF report.

---

## Development

The codebase follows a set of conventions, each of which corresponds to a real production failure. The most important:

- **Parse-check every R file before delivery.** `Rscript -e "parse('file.R')"`. Brace-counting is not a substitute.
- **Version before modifying.** Copy a file before editing it; never edit the only copy.
- **Never claim to have tested what was not tested.** An honest "I could not test this" beats implied testing.
- **No `install.packages()`, no external HTTP, no `system()`.** The only sanctioned exceptions are the `pdftotext` fallback for PDF extraction and the AIRA HTTP endpoint.
- **Writes confined to `/home/workspace/files/`.**
- **`vapply` over `sapply`** for any result used as a subscript or condition, because `sapply` returns a list on empty or ragged input.
- **All reactive values declared before the observers that reference them.**
- **Helper functions at module scope, never inside the server function.**
- **Never define the same output ID twice;** Shiny silently uses the last definition.
- **Audit every widget-type change.** Changing an input from single-value to multi-value alters its return type and every downstream reference.

AIRA work follows additional rules. The canonical module is the only place that builds a client or reads the API key. Prompts are frozen and versions immortal; a change is a new version, never an edit. Every public function returns the canonical six-field shape on every path. Async wrappers capture the environment by value and translate rejection into an `unavailable` status. Tests exercise all five statuses through the client override seam, not just the success path.

---

## Testing

An optional test harness lives in `tests/`, self-contained, run with `cd tests && Rscript run_tests.R`. It sources the inspector modules, reads declarative test cases, generates fixtures deterministically, runs each case, and writes an HTML and a CSV report.

A test case declares its rule, its fixture, the inspector, the configuration, and the expected firing and outcome. Known inspector bugs are documented with an XFAIL marker and reason rather than breaking the build; if such a test unexpectedly passes it becomes an XPASS, a signal to remove the marker. Tests that need an optional package declare it and skip cleanly when it is absent. The suite covers functional firing on positive fixtures, boundary behaviour at threshold edges, engine dispatch and classification, and inspector robustness against the pathological-input fixture set.

---

## Known limitations

- **High-cardinality false positives.** Unique values such as GWAS `rs` identifiers and sequential integers can trip the high-cardinality rule. The linkability gate and quasi-identifier context reduce this, but context-aware detection of summary-statistics files remains an engine improvement.
- **Rule coverage.** Unit tests cover a subset of the 106 rules. Expanding coverage toward the archive, NIfTI, PDF, and DICOM families is ongoing.
- **Stderr noise.** ReadStat (via `haven`) and `tar` (via `system2`) emit harmless messages to workspace stderr. Cosmetic; suppression is on the backlog.
- **Module size.** The tabular inspector and the server module are large and are candidates for decomposition into sub-functions and observer files.
- **In-UI diagnostics.** Log viewing currently requires terminal access; an in-application diagnostics tab is designed but not yet implemented.

---

## Security model

The application sits inside a TRE with assumed-trustworthy users. The threat model is mistakes by trusted users, not malicious users: a researcher accidentally requesting egress of disclosive data, a reviewer making a snap decision without full information, a decision later questioned with the rationale unrecoverable, a false negative from an engine bug, or a new file type the inspectors do not cover.

The mitigations are conservative defaults and comprehensive rule coverage, mandatory rationale notes, an append-only audit log, an explicit UI distinction between modes, file fingerprinting so post-decision changes are detectable, and robust handling of unknown types (default to UNCERTAIN, not GREEN). User authentication, encryption at rest, and network security are delegated to the host workspace and are out of scope for the application. The application composes no SQL, runs no untrusted code, and makes no shell composition, so injection is not part of its attack surface.

---

## Reference documents

| Document | Contents |
|---|---|
| `dre_shiny_platform_reference.md` | Platform constraints, helpers, design system, canonical patterns |
| `airlock_checker_reference.md` | Module map, rule catalogue, directory layout |
| `airlock_checker_PRS.md` | Product requirements; section 14.1 binds AIRA's role |
| `Airlock_Checker_Manual_v3.md` | Reviewer-facing manual |

Aridhia Knowledge Base: https://knowledgebase.aridhia.io/

---

## Licence

Copyright Aridhia Informatics. Licensing terms are defined in the `LICENSE` file in this repository. Add the appropriate licence before publishing.
