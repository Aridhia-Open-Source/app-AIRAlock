# Application CSS
# Auto-split from app.R - do not edit the monolithic file

# ============================================================
# CSS
# ============================================================
app_css <- paste0("
/* ============================================================ */
/* DESIGN TOKENS                                                */
/* Single source of truth for type, spacing, colour, borders.  */
/* Added 2026-06-16. Adoption is staged: this block defines the */
/* tokens; existing rules are migrated to reference them in     */
/* later stages. Brand and RAG colours are unchanged, only      */
/* named. The grey/blue-grey proliferation is consolidated to   */
/* one border, two surfaces, and a two-tier muted text set.     */
/* ============================================================ */
:root {
  /* ── Type scale (6 steps) ──────────────────────────────── */
  /* Anchored on existing usage clusters so adoption shifts    */
  /* each size to its nearest step with minimal movement.      */
  --fs-caption:     0.68rem;  /* labels, hints, metadata, pills, uppercase section labels */
  --fs-body:        0.75rem;  /* default body, table cells, finding text */
  --fs-emphasis:    0.82rem;  /* filenames, emphasised values, card titles */
  --fs-heading:     0.90rem;  /* section and card headers */
  --fs-display:     1.30rem;  /* large figures shown on screen */
  --fs-display-lg:  1.80rem;  /* reserved, report/score contexts */

  /* ── Spacing scale (4 steps) ───────────────────────────── */
  --sp-1: 0.25rem;  /* tight: pill padding, inline gaps */
  --sp-2: 0.5rem;   /* default: card-internal padding, row gaps */
  --sp-3: 0.75rem;  /* section separation */
  --sp-4: 1rem;     /* panel separation */

  /* ── Brand (unchanged) ─────────────────────────────────── */
  --brand-blue: ",ARIDHIA_BLUE,";   /* #0066A1 */
  --brand-navy: ",ARIDHIA_DARK,";   /* #003366 */

  /* ── RAG semantic colours (unchanged) ──────────────────── */
  --rag-red:      #C62828;  --rag-red-bg:    #FFEBEE;
  --rag-amber:    #E65100;  --rag-amber-bg:  #FFF3E0;
  --rag-green:    #2E7D32;  --rag-green-bg:  #E8F5E9;
  --rag-purple:   #7B1FA2;  /* UNCERTAIN */

  /* ── Surfaces, borders, text (consolidated) ────────────── */
  --surface:       #FFFFFF;  /* card / panel backgrounds */
  --surface-sunk:  #F0F4F8;  /* inset areas: path display, evidence, sunk panels */
  --fill-pale:     #E2EAF0;  /* pale-blue fill for pills/tags (distinct from border) */
  --page-bg:       #EFF3F7;  /* page background */
  --border:        #D0DCE8;  /* the one border colour (absorbs the blue-grey family) */
  --border-strong: ",ARIDHIA_BLUE,";  /* emphasis borders = brand blue */
  --text:          #1A1A1A;  /* primary text */
  --text-muted:    #555555;  /* secondary text */
  --text-hint:     #777777;  /* tertiary / hint text, one step lighter than muted */
  --radius:        8px;      /* standard corner radius */
  --radius-pill:   10px;     /* pills and rounded tags */
}

/* ── Reset & base ── */
body { background:#EFF3F7; font-family:'Segoe UI',Arial,sans-serif; font-size:14px; }
* { box-sizing:border-box; }

/* ── Viewport lock - prevents iframe outer scroll / whitespace ── */
/* No outer scroll - panels constrained by max-height internally */

/* ── Header ── */
/* dte-header removed */

/* ── Cards ── */
.card { border-radius:8px; border:1px solid var(--border); box-shadow:0 1px 6px rgba(0,0,0,0.06); }
.card-header {
  font-weight:700; background:#fff; font-size:var(--fs-emphasis);
  border-bottom:2px solid ",ARIDHIA_BLUE,"; padding:0.5rem 0.8rem;
}
.card-body { padding:0.7rem !important; }

/* ── Section headings ── */
.sect-hd {
  font-size:var(--fs-caption); text-transform:uppercase; letter-spacing:0.09em;
  color:",ARIDHIA_BLUE,"; font-weight:800; margin-bottom:0.35rem;
  padding-bottom:0.2rem; border-bottom:2px solid ",ARIDHIA_BLUE,";
}

/* ── File list checkboxes ── */
#selected_files .form-check {
  display:flex !important; align-items:center !important;
  padding:0.3rem 0.5rem 0.3rem 0.5rem !important;
  margin-bottom:2px !important;
  background:white; border:1px solid var(--border); border-radius:5px;
  transition:border-color 0.1s, background 0.1s; cursor:pointer;
  gap:0.5rem;
}
#selected_files .form-check:hover { border-color:",ARIDHIA_BLUE,"; background:var(--surface-sunk); }
#selected_files .form-check-input {
  flex-shrink:0; margin:0 !important; position:static !important;
  width:14px; height:14px; cursor:pointer;
}
#selected_files .form-check-label {
  flex:1; margin:0 !important; padding:0 !important;
  cursor:pointer; display:flex; align-items:center; gap:0.4rem;
  line-height:1;
}
.fchoice { display:flex; align-items:center; gap:0.4rem; width:100%; }
.f-badge {
  font-size:var(--fs-caption); font-weight:800; padding:0.1rem 0.32rem;
  border-radius:3px; letter-spacing:0.05em; white-space:nowrap;
  min-width:2.4rem; text-align:center; flex-shrink:0;
}
.t-tabular { background:#E3F2FD; color:#1565C0; }
.t-genomic { background:#E8F5E9; color:#2E7D32; }
.t-script  { background:#FFF3E0; color:#E65100; }
.t-archive { background:#EDE7F6; color:#6A1B9A; }
.t-image   { background:#FCE4EC; color:#C62828; }
.t-dicom   { background:#E0F7FA; color:#00695C; }
.t-binary  { background:#EFEBE9; color:#4E342E; }
.f-name { flex:1; font-size:var(--fs-emphasis); font-weight:500; color:var(--text); min-width:0;
  overflow:hidden; text-overflow:ellipsis; white-space:nowrap; }
.f-size { font-size:var(--fs-caption); color:#AAA; flex-shrink:0; }

/* ── Select All / Clear buttons ── */
.btn-outline-secondary { font-size:var(--fs-body) !important; padding:0.2rem 0.5rem !important; }

/* ── Run button ── */
.btn-run {
  width:100%; padding:0.55rem; font-weight:700; font-size:var(--fs-heading);
  background:linear-gradient(135deg,",ARIDHIA_BLUE,",",ARIDHIA_DARK,");
  color:white; border:none; border-radius:7px; cursor:pointer;
  transition:opacity 0.15s;
}
.btn-run:hover { opacity:0.88; color:white; }

/* ── File card (Wave 1) ──────────────────────────────────────────────────
   Single outer frame per file. Thick left border carries the classification
   colour so the reviewer sees file boundaries at a glance. Replaces the
   former standalone .verdict-wrap + .meta-strip blocks - everything for
   one file now lives inside a single .file-card. */
.file-card {
  background:var(--surface);
  border:1px solid var(--border);
  border-left:4px solid #B0BEC5;  /* overridden by modifier classes below */
  border-radius:6px;
  padding:0.7rem 0.9rem 0.8rem;
  margin-bottom:1.1rem;           /* replaces the old hr() separator */
  box-shadow:0 1px 2px rgba(0,0,0,0.03);
}
.file-card-red   { border-left-color:",RED_C,"; }
.file-card-amber { border-left-color:",AMBER_C,"; }
.file-card-green { border-left-color:",GREEN_C,"; }
.file-card-unc   { border-left-color:#7B1FA2; }

/* Verdict banner inside a file card: coloured classification chip + filename
   + optional preview button. No score/rule-count here - they live in the
   meta line below. */
.fc-banner {
  display:flex; align-items:center; gap:0.65rem;
  margin-bottom:0.3rem;
}
.fc-label {
  font-size:var(--fs-body); font-weight:800; letter-spacing:0.03em;
  color:white;
  padding:0.2rem 0.65rem; border-radius:4px;
  white-space:nowrap; flex-shrink:0;
}
.fc-filename {
  flex:1; min-width:0;
  font-size:var(--fs-heading); font-weight:600; color:",ARIDHIA_DARK,";
  white-space:nowrap; overflow:hidden; text-overflow:ellipsis;
}
.fc-preview-btn {
  font-size:var(--fs-body); padding:0.18rem 0.55rem; border-radius:4px;
  border:1px solid ",ARIDHIA_BLUE,"; color:",ARIDHIA_BLUE,";
  background:transparent; cursor:pointer; font-weight:600;
  flex-shrink:0; transition:background 0.12s, color 0.12s;
}
.fc-preview-btn:hover {
  background:",ARIDHIA_BLUE,"; color:white;
}

/* Per-file exclude (X) button. Subtle by default - dark grey text on
   transparent background - escalates to red on hover so the reviewer
   sees clearly that this is a destructive action when they're about
   to click. Sits next to the preview button at the right end of the
   verdict banner. */
.fc-exclude-btn {
  font-size:var(--fs-body); padding:0.18rem 0.55rem; border-radius:4px;
  border:1px solid #BDBDBD; color:#666;
  background:transparent; cursor:pointer; font-weight:600;
  flex-shrink:0; margin-left:0.4rem;
  transition:background 0.12s, color 0.12s, border-color 0.12s;
}
.fc-exclude-btn:hover {
  background:#C62828; color:white; border-color:#C62828;
}

/* Meta line: single row of muted inline text. Replaces the 4-box meta-strip. */
.fc-meta-line {
  font-size:var(--fs-body); color:#666;
  margin:0 0 0.5rem 0;
  padding:0 0 0.45rem 0;
  border-bottom:1px dashed var(--border);
  line-height:1.4;
}
.fc-meta-item { color:#555; }
.fc-meta-sep  { color:#BBB; margin:0 0.4rem; }

/* ── Rule hit cards ── */
.hit-card {
  display:flex; gap:0.55rem; padding:0.5rem 0.65rem;
  border-radius:6px; margin-bottom:0.35rem; align-items:flex-start;
}
.hr-red  { background:",RED_BG,";   border-left:4px solid ",RED_C,"; }
.hr-amb  { background:",AMBER_BG,"; border-left:4px solid ",AMBER_C,"; }
.hr-grn  { background:",GREEN_BG,"; border-left:4px solid ",GREEN_C,"; }
.hr-unc  { background:#F3E5F5; border-left:4px solid #7B1FA2; }
.hbadge {
  display:inline-block; padding:0.1rem 0.4rem; border-radius:3px;
  font-size:var(--fs-caption); font-weight:800; letter-spacing:0.05em; white-space:nowrap;
}
.hb-red  { background:",RED_C,";   color:white; }
.hb-amb  { background:",AMBER_C,"; color:white; }
.hb-grn  { background:",GREEN_C,"; color:white; }
.hb-unc  { background:#7B1FA2; color:white; }
.h-rid   { font-family:monospace; font-weight:700; font-size:var(--fs-body); color:",ARIDHIA_DARK,"; }
.h-det   { font-size:var(--fs-emphasis); color:#222; line-height:1.4; margin-top:0.15rem; }
.h-ref   { font-size:var(--fs-body); color:#777; margin-top:0.1rem; font-style:italic; }

/* ── Evidence button & panel ── */
.ev-btn {
  font-size:var(--fs-caption); padding:0.12rem 0.5rem; border-radius:4px;
  border:1px solid currentColor; cursor:pointer; margin-top:0.3rem;
  display:inline-flex; align-items:center; gap:0.2rem;
  background:transparent; font-weight:600; transition:opacity 0.12s;
}
.ev-btn:hover { opacity:0.7; }
.ev-btn-red   { color:",RED_C,"; }
.ev-btn-amb   { color:",AMBER_C,"; }
.ev-btn-grn   { color:",GREEN_C,"; }
.ev-panel {
  margin-top:0.4rem; padding:0.45rem 0.55rem;
  background:rgba(255,255,255,0.85); border-radius:5px;
  border:1px solid rgba(0,0,0,0.07);
}

/* ── Batch summary ── */
.batch-wrap { display:flex; gap:0.6rem; flex-wrap:wrap; align-items:center; }

/* ── AIRA disclosure-review banner (per-file, on-demand) ─────────
   Distinct visual identity from rule-engine output. After the
   2026-04-28 redesign, all completed-review banners use a single
   NEUTRAL colour scheme regardless of AI risk level. The previous
   green/amber/red coding was misread by reviewers as a verdict - the
   AI panel is advisory observations, not a verdict, so colour does
   not vary with the AI's assessment. The rule-engine RAG bar is the
   only verdict signal in the per-file card. */

.aira-review-banner {
  border-left: 4px solid #B0BEC5;
  background: var(--surface-sunk);
  padding: 0.55rem 0.8rem;
  margin: 0.5rem 0 0.3rem 0;
  border-radius: 0 4px 4px 0;
  font-size: 0.8rem;
}
/* Single neutral style for ALL completed reviews. Used regardless of
   whether the AI returned LOW/MEDIUM/HIGH/UNCERTAIN/Unstructured. */
.aira-review-neutral { border-left-color: #003366; background: var(--surface-sunk); }
/* Process states keep their existing muted styling. */
.aira-review-loading { border-left-color: #B0BEC5; background: var(--surface-sunk); }
.aira-review-queued  { border-left-color: #CFD8DC; background: var(--surface-sunk); }
.aira-review-err     { border-left-color: #BDBDBD; background: #FAFAFA; }
/* Skipped (parse-only files): even more muted to indicate the AI panel
   is informational only, no review will arrive. */
.aira-review-skipped { border-left-color: #ECEFF1; background: #FAFAFA; }

.aira-review-hd {
  display: flex; justify-content: space-between; align-items: baseline;
  font-size: 0.62rem; font-weight: 800; color: #003366;
  letter-spacing: 0.08em; margin-bottom: 0.3rem;
  text-transform: uppercase;
}
/* Risk-pill is now an observation label, not a verdict pill. Single
   neutral colour scheme: dark navy on light background. The label
   text (Minor observations / Notable observations / Significant
   observations / Unable to assess) carries the meaning without
   priming the reviewer with red/amber/green semantics. */
.aira-review-risk-pill {
  font-size: 0.62rem; font-weight: 700;
  padding: 0.08rem 0.5rem; border-radius: 3px;
  background: var(--fill-pale); color: #003366;
  letter-spacing: 0.04em; text-transform: none;
}

.aira-review-body { color: #222; line-height: 1.45; }
.aira-review-assessment {
  font-size: 0.82rem; font-weight: 600; margin: 0 0 0.35rem 0; color: #222;
}
.aira-review-reasoning {
  font-size: 0.76rem; color: #445; margin: 0 0 0.4rem 0;
}
.aira-review-raw {
  font-family: ui-monospace, Menlo, Consolas, monospace;
  font-size: 0.72rem; color: #333;
  background: white; border: 1px solid var(--border); border-radius: 3px;
  padding: 0.4rem 0.55rem; margin: 0.3rem 0;
  white-space: pre-wrap; word-break: break-word;
  max-height: 260px; overflow-y: auto;
}
.aira-review-foot {
  font-size: 0.66rem; color: #888; font-style: italic;
  margin-top: 0.35rem; padding-top: 0.3rem;
  border-top: 1px dashed var(--border);
}

/* v2 (2026-04-28) sections within the AI banner: concerns list, blind
   spots, reviewer focus. Each has a small uppercase label followed by
   a list (concerns, blind spots) or paragraph (reviewer focus). All
   neutral-coloured: red/amber/green semantics never appear in the AI
   panel. */
.aira-section-label {
  font-size: 0.62rem; font-weight: 700; color: #003366;
  letter-spacing: 0.06em; text-transform: uppercase;
  margin: 0.5rem 0 0.2rem 0;
}
.aira-concerns { margin: 0.3rem 0 0.4rem 0; }
.aira-concerns-list {
  list-style: disc inside; padding-left: 0.4rem; margin: 0;
  font-size: 0.76rem; color: #333;
}
.aira-concern-item {
  padding: 0.08rem 0; line-height: 1.45;
}
.aira-concern-flag {
  font-family: ui-monospace, Menlo, Consolas, monospace;
  font-size: 0.72rem; font-weight: 600;
  background: var(--fill-pale); color: #003366;
  padding: 0.05rem 0.35rem; border-radius: 2px;
}
.aira-concern-expl { color: #445; }

.aira-blind-spots { margin: 0.3rem 0 0.4rem 0; }
.aira-blind-spots-list {
  list-style: disc inside; padding-left: 0.4rem; margin: 0;
  font-size: 0.74rem; color: #666; font-style: italic;
}
.aira-blind-spot-item { padding: 0.05rem 0; line-height: 1.45; }

.aira-reviewer-focus {
  margin: 0.4rem 0;
  padding: 0.45rem 0.65rem;
  background: var(--surface);
  border: 1px solid #D8E1EA;
  border-left: 3px solid #003366;
  border-radius: 0 3px 3px 0;
}
.aira-reviewer-focus-text {
  font-size: 0.78rem; color: #222; margin: 0; line-height: 1.45;
}

/* v6 anomalies list - reuses concerns styling shape but with column-name
   inline tag rather than flag-vocabulary chip. Distinct visual to avoid
   reviewers reading anomalies as a re-statement of engine concerns. */
.aira-anomalies { margin: 0.3rem 0 0.4rem 0; }
.aira-anomalies-list {
  list-style: disc inside; padding-left: 0.4rem; margin: 0;
  font-size: 0.76rem; color: #333;
}
.aira-anomaly-item {
  padding: 0.08rem 0; line-height: 1.45;
}
.aira-anomaly-col {
  font-family: ui-monospace, Menlo, Consolas, monospace;
  font-size: 0.72rem; font-weight: 600;
  background: #FFF4E0; color: #7A4A00;
  padding: 0.05rem 0.35rem; border-radius: 2px;
}
.aira-anomaly-obs { color: #333; }

/* v6 dataset recognition callout - the only field where AIRA can
   contribute information the engine cannot. Visually distinctive but
   restrained to avoid overshadowing the engine's verdict. */
.aira-recognition {
  margin: 0.4rem 0;
  padding: 0.5rem 0.7rem;
  background: #F4F8FB;
  border: 1px solid #C7D9E6;
  border-left: 3px solid #0066A1;
  border-radius: 0 3px 3px 0;
}
.aira-recognition-hd {
  display: flex; align-items: center; gap: 0.4rem;
  font-size: 0.78rem;
}
.aira-recognition-icon {
  color: #0066A1;
  font-size: 0.85rem;
}
.aira-recognition-name {
  font-weight: 600; color: #003366;
}
.aira-recognition-conf {
  font-size: 0.7rem; color: #5A7388;
  font-style: italic;
  margin-left: auto;
}
.aira-recognition-evid {
  font-size: 0.72rem; color: #445;
  margin: 0.3rem 0 0 0;
  line-height: 1.4;
}

/* Expandable column-classification list */
.aira-review-cols {
  margin-top: 0.4rem;
  font-size: 0.72rem;
}
.aira-review-cols summary {
  cursor: pointer;
  color: #0066A1;
  font-weight: 600;
  font-size: 0.7rem;
  padding: 0.15rem 0;
  outline: none;
}
.aira-review-cols summary:hover { color: #003366; }
.col-cat-grid {
  display: grid; grid-template-columns: repeat(auto-fill, minmax(220px, 1fr));
  gap: 0.15rem 0.6rem;
  margin-top: 0.3rem;
  padding: 0.35rem 0;
  max-height: 260px; overflow-y: auto;
}
.col-cat-row {
  display: flex; justify-content: space-between; align-items: center;
  gap: 0.4rem; padding: 0.12rem 0;
  border-bottom: 1px dashed #EEE;
}
.col-cat-name {
  font-family: ui-monospace, Menlo, Consolas, monospace;
  font-size: 0.7rem; color: #333;
  white-space: nowrap; overflow: hidden; text-overflow: ellipsis;
  flex: 1; min-width: 0;
}
.col-cat-tag {
  font-size: 0.6rem; font-weight: 700;
  padding: 0.05rem 0.35rem; border-radius: 2px;
  white-space: nowrap; flex-shrink: 0;
  letter-spacing: 0.03em;
}
.col-cat-direct    { background: #9A2A2A; color: white; }
.col-cat-quasi     { background: #E67E00; color: white; }
.col-cat-sensitive { background: #7B1FA2; color: white; }
.col-cat-nonid     { background: #E8F5E9; color: #1B5E20; }
.col-cat-unknown   { background: #ECEFF1; color: #555; }

/* ── Tab bar badge pills ── */
.nav-badge {
  font-size:var(--fs-caption); font-weight:700;
  padding:0.1rem 0.45rem; border-radius:10px;
  white-space:nowrap; display:inline-block;
}

/* ── AIRA batch summary slot (full-width, above per-file cards) ── */
.aira-batch-slot {
  border-left: 4px solid #0066A1;
  background: var(--surface-sunk);
  padding: 0.6rem 0.9rem;
  margin: 0.3rem 0 0.8rem;
  border-radius: 0 5px 5px 0;
  font-size: 0.88rem;
}
.aira-batch-slot-ok       { border-left-color: #0066A1; background: var(--surface-sunk); }
.aira-batch-slot-loading  { border-left-color: #B0BEC5; background: var(--surface-sunk); }
.aira-batch-slot-err      { border-left-color: #BDBDBD; background: #FAFAFA; }
.aira-batch-slot-hd {
  display: flex; justify-content: space-between; align-items: baseline;
  font-size: 0.66rem; font-weight: 800; color: #003366;
  letter-spacing: 0.08em; margin-bottom: 0.3rem;
  text-transform: uppercase;
}
.aira-batch-slot-tag {
  font-size: 0.62rem; font-weight: 600; color: #666;
  text-transform: none; letter-spacing: 0;
}
.aira-batch-slot-body {
  color: var(--text); line-height: 1.45;
}

/* ── Risk scorecard ── */
.risk-score-bar-track {
  height:10px; border-radius:5px; background:#E0E0E0; margin:0.4rem 0 0.15rem;
}
.risk-score-bar-fill {
  height:100%; border-radius:5px; transition:width 0.5s cubic-bezier(.4,0,.2,1);
}
.risk-sub-box {
  background:#F5F5F5; border-radius:5px; padding:0.45rem 0.55rem;
}
.risk-sub-label {
  font-size:var(--fs-caption); font-weight:700; color:#666; margin-bottom:0.12rem;
  letter-spacing:0.06em; text-transform:uppercase;
}
.risk-sub-value {
  font-size:var(--fs-display); font-weight:800; line-height:1.1;
}
.risk-sub-note { font-size:var(--fs-caption); color:#888; }
.risk-rule-row {
  display:flex; align-items:center; gap:0.35rem;
  font-size:var(--fs-body); margin-bottom:0.15rem;
}
.risk-section-hd {
  font-size:var(--fs-caption); font-weight:700; color:#666;
  letter-spacing:0.06em; text-transform:uppercase; margin-bottom:0.22rem;
}
.risk-footer {
  margin-top:0.6rem; padding-top:0.5rem; border-top:1px solid #E8E8E8;
  font-size:var(--fs-caption); color:#999; line-height:1.5;
}
.bs { padding:0.3rem 0.8rem; border-radius:14px; font-weight:700; font-size:var(--fs-emphasis); border:1px solid; }
.bs-g { background:",GREEN_BG,"; color:",GREEN_C,"; border-color:",GREEN_C,"; }
.bs-a { background:",AMBER_BG,"; color:",AMBER_C,"; border-color:",AMBER_C,"; }
.bs-r { background:",RED_BG,";   color:",RED_C,";   border-color:",RED_C,"; }
.bs-u { background:#F3E5F5; color:#7B1FA2; border-color:#7B1FA2; }

/* ── Rule ref panel ── */
.rule-ref-item {
  padding:0.45rem 0.6rem; border-radius:6px; margin-bottom:0.25rem;
  font-size:var(--fs-body); border-left:3px solid transparent;
}
.rr-active  { box-shadow:0 1px 4px rgba(0,0,0,0.08); }
.rr-dimmed  { opacity:0.45; }
.rr-id      { font-family:monospace; font-weight:800; font-size:var(--fs-body); }
.rr-label   { font-weight:600; color:#222; margin-top:0.1rem; }
.rr-check   { color:#555; margin-top:0.1rem; line-height:1.35; font-size:var(--fs-body); }
.rr-ref     { color:#999; font-size:var(--fs-caption); margin-top:0.1rem; }
.rr-trigger {
  background:",ARIDHIA_BLUE,"; color:white; padding:0.06rem 0.28rem;
  border-radius:3px; font-size:var(--fs-caption); font-weight:800; margin-left:0.3rem;
}

/* ── Preview modal ── */
.prev-toolbar {
  display:flex; align-items:center; gap:0.5rem;
  padding:0.4rem 0.7rem; background:var(--surface-sunk);
  border-bottom:1px solid var(--border); flex-shrink:0;
}
.prev-meta {
  font-size:var(--fs-body); color:#666; flex:1;
}
.prev-body {
  flex:1; overflow:hidden; padding:0; min-height:0;
  display:flex; flex-direction:column;
}
.prev-code {
  margin:0; padding:0.7rem 0.9rem;
  font-family:'Consolas','Monaco','Courier New',monospace;
  font-size:var(--fs-emphasis); line-height:1.55;
  white-space:pre; background:#1E1E1E; color:#D4D4D4;
  min-height:100%; flex:1; overflow:auto;
}
.prev-img {
  max-width:100%; max-height:72vh;
  display:block; margin:auto; padding:0.5rem;
}
.prev-vcf-header {
  font-family:monospace; font-size:var(--fs-body); color:#555;
  padding:0.4rem 0.8rem; background:#F8FBFE;
  border-bottom:2px solid var(--border);
  flex-shrink:0; overflow:hidden;
}
.prev-vcf-header div { padding:0.05rem 0; white-space:nowrap;
  overflow:hidden; text-overflow:ellipsis; }
.prev-vcf-table { flex:1; overflow:auto; min-height:0; }
.prev-vcf-key { color:#0066A1; font-weight:700; }
.ln {
  display:inline-block; min-width:2.8rem; text-align:right;
  color:#555; padding-right:0.8rem; user-select:none;
  border-right:1px solid #333; margin-right:0.6rem;
}
.modal-xl { max-width:92vw !important; }
.modal-content { display:flex; flex-direction:column; }
.modal-body { flex:1; overflow:hidden; display:flex; flex-direction:column; padding:0 !important; }
#previewModal .modal-dialog { margin:20px auto; }
#previewModal .modal-content { height:calc(100vh - 60px); max-height:calc(100vh - 60px); }

/* ── Rule Configuration panel ── */
.cfg-group {
  margin-bottom:1rem;
}
.cfg-lbl {
  font-size:var(--fs-body); font-weight:800; text-transform:uppercase;
  letter-spacing:0.06em; color:#003366; display:block; margin-bottom:0.2rem;
}
.cfg-hint {
  display:block; font-size:var(--fs-body); color:#888; margin-bottom:0.4rem; line-height:1.35;
}
.cfg-ta {
  font-family:'Consolas','Monaco',monospace; font-size:var(--fs-body) !important;
  border:1px solid var(--border); border-radius:5px; resize:vertical;
  padding:0.4rem 0.5rem !important; line-height:1.4;
}
.cfg-ta:focus { border-color:#0066A1; outline:none; box-shadow:0 0 0 2px rgba(0,102,161,0.15); }
.cfg-num {
  font-size:var(--fs-emphasis) !important; padding:0.25rem 0.4rem !important;
  border:1px solid var(--border); border-radius:5px; text-align:center;
}
.cfg-applied {
  font-size:var(--fs-body); font-weight:700; color:#2E7D32;
  background:#E8F5E9; border:1px solid #2E7D32; border-radius:4px;
  padding:0.15rem 0.6rem; margin-left:0.6rem; white-space:nowrap;
}

.ph { text-align:center; padding:2rem 1rem; color:var(--text-muted); }
.ph-icon { font-size:var(--fs-display-lg); margin-bottom:0.4rem; color:var(--text-hint); }

/* ── Scrollable panels ── */
.scroll-panel { overflow-y:auto; max-height:calc(100vh - 105px); }
/* Results panel: the card body is itself the scroll container so the
   scrollbar runs to the card's bottom edge and the corner stays clean. */
.results-card-body {
  padding:0.5rem 0.6rem !important;
  overflow-y:auto;
  max-height:calc(100vh - 105px);
  border-bottom-left-radius:8px;
  border-bottom-right-radius:8px;
}

/* ── Collapsible file panel layout ──────────────────────────────────
   Explicit flex row replacing layout_columns. The file panel collapses
   to zero width (quick slide) when .files-collapsed is on the row; the
   always-visible rail toggles it; the results panel flexes to fill
   whatever width remains. Workflow: assemble batch with panel open,
   panel auto-collapses on Run Assessment (JS), reviewer reopens via the
   rail to remove files and re-run. */
.mpr-flex {
  display:flex;
  align-items:flex-start;   /* each panel its own height, no equal-height stretch */
  gap:0;
}
.file-panel {
  flex:0 0 360px;           /* fixed width when open */
  min-width:0;
  overflow:hidden;
  transition:flex-basis 0.18s ease, opacity 0.18s ease, margin 0.18s ease;
  opacity:1;
}
.files-collapsed .file-panel {
  flex-basis:0;
  opacity:0;
  margin:0;
  pointer-events:none;      /* not interactive while hidden */
}
/* Toggle rail: thin always-visible strip between the panel and results. */
.file-rail {
  flex:0 0 26px;
  align-self:stretch;
  min-height:120px;
  max-height:calc(100vh - 105px);
  margin:0 0.4rem;
  background:var(--surface-sunk);
  border:1px solid var(--border);
  border-radius:6px;
  cursor:pointer;
  display:flex; flex-direction:column; align-items:center;
  padding:0.5rem 0;
  gap:0.5rem;
  transition:background 0.12s;
  user-select:none;
}
.file-rail:hover { background:var(--fill-pale); }
.file-rail-chevron {
  font-size:var(--fs-body); color:var(--brand-blue);
  transition:transform 0.18s ease;
}
/* Chevron points left (collapse) when open, right (expand) when collapsed */
.files-collapsed .file-rail-chevron { transform:rotate(180deg); }
.file-rail-label {
  writing-mode:vertical-rl; text-orientation:mixed;
  font-size:var(--fs-caption); font-weight:700; color:var(--brand-navy);
  letter-spacing:0.04em; white-space:nowrap;
}
.results-panel {
  flex:1 1 auto;
  min-width:0;               /* allow shrink so long content wraps, not overflows */
}

/* ── Help tab ── */
.help-section details {
  border:1px solid var(--border); border-radius:6px;
  margin-bottom:0.55rem; overflow:hidden;
}
.help-section details summary {
  list-style:none; cursor:pointer;
  padding:0.55rem 0.8rem;
  background:#F0F5FA;
  border-bottom:1px solid var(--border);
  font-weight:700; font-size:var(--fs-emphasis); color:#003366;
  display:flex; align-items:center; gap:0.5rem;
  user-select:none;
}
.help-section details summary::-webkit-details-marker { display:none; }
.help-section details summary::before {
  content:'\25B6 '; font-size:var(--fs-caption); color:#0066A1;
  transition:transform 0.15s; flex-shrink:0;
}
.help-section details[open] summary::before { transform:rotate(90deg); }
.help-section details[open] summary {
  background:#E8EEF5; border-bottom-color:var(--border);
}
.help-section .help-body {
  padding:0.7rem 0.9rem; font-size:var(--fs-emphasis); line-height:1.6; color:#222;
}
.help-section .help-body p  { margin:0 0 0.5rem; }
.help-section .help-body ul { margin:0.2rem 0 0.6rem 1.2rem; padding:0; }
.help-section .help-body li { margin-bottom:0.2rem; }
.help-section .help-body h4 {
  font-size:var(--fs-emphasis); font-weight:700; color:#003366;
  margin:0.6rem 0 0.25rem; border-bottom:1px solid #E0E8F0;
  padding-bottom:0.15rem;
}
.help-section .help-body code {
  font-size:var(--fs-emphasis); background:var(--surface-sunk);
  padding:0.05rem 0.3rem; border-radius:3px; color:#003366;
}
.help-kv { display:grid; grid-template-columns:max-content 1fr;
           gap:0.15rem 0.8rem; margin:0.35rem 0; }
.help-kv dt { font-weight:700; color:#003366; font-size:var(--fs-emphasis); white-space:nowrap; }
.help-kv dd { margin:0; font-size:var(--fs-emphasis); color:#333; }
.help-tl { display:inline-block; font-weight:800; font-size:var(--fs-body);
           padding:0.08rem 0.45rem; border-radius:10px; color:white;
           vertical-align:middle; }
.help-rule-eg { background:#F8FAFB; border:1px solid #DDE5EE;
                border-left:4px solid #0066A1; border-radius:4px;
                padding:0.4rem 0.6rem; margin:0.3rem 0;
                font-size:var(--fs-emphasis); }
.help-rule-eg code { color:#C62828; background:transparent; }


/* ── Batch panel ── */
.batch-file-row {
  display:flex; align-items:center; gap:0.4rem;
  padding:0.22rem 0.4rem; margin-bottom:2px;
  background:white; border:1px solid var(--border); border-radius:4px;
}
.batch-file-row:hover { border-color:var(--border); }
.batch-file-info { flex:1; min-width:0; }
.batch-file-name { font-size:var(--fs-body); font-weight:500; color:var(--text);
  overflow:hidden; text-overflow:ellipsis; white-space:nowrap; }
.batch-file-path { font-size:var(--fs-caption); color:#AAA;
  overflow:hidden; text-overflow:ellipsis; white-space:nowrap; }
.batch-remove-btn {
  flex-shrink:0; font-size:var(--fs-body); padding:0.05rem 0.3rem;
  border-radius:3px; border:1px solid #DDD; background:#FFF;
  color:#C62828; cursor:pointer; line-height:1; font-weight:700;
  transition:background 0.1s;
}
.batch-remove-btn:hover { background:#FFEBEE; border-color:#C62828; }
.batch-empty { font-size:var(--fs-body); color:#AAA; font-style:italic;
  padding:0.3rem 0; }
/* File list row when already in batch */
.frow-in-batch { border-color:#2E7D32 !important; background:#F1FBF3 !important; }
.frow-in-batch .f-name { color:#1B5E20 !important; }
.in-batch-badge {
  font-size:var(--fs-caption); font-weight:800; color:#2E7D32; background:#C8E6C9;
  border-radius:3px; padding:0.05rem 0.25rem; flex-shrink:0;
  letter-spacing:0.03em;
}

/* ── Airlock folder modal ── */
.airlock-path-preview {
  font-family:monospace; font-size:var(--fs-emphasis); color:#003366;
  background:#EEF4FB; border:1px solid var(--border); border-radius:4px;
  padding:0.35rem 0.6rem; margin-top:0.4rem; word-break:break-all;
  min-height:1.6rem;
}
.airlock-file-list {
  font-size:var(--fs-body); color:#555; background:#F5F5F5;
  border:1px solid #DDD; border-radius:4px;
  padding:0.4rem 0.6rem; max-height:120px; overflow-y:auto;
  margin-top:0.35rem;
}
.airlock-file-list div { padding:0.1rem 0; border-bottom:1px solid #EEE; }
.airlock-file-list div:last-child { border-bottom:none; }



/* ── Folder browser modal ── */
.fb-folder-row {
  display:flex; align-items:center; gap:0.5rem;
  padding:0.35rem 0.55rem; border-radius:5px; cursor:pointer;
  border:1px solid transparent; margin-bottom:2px;
  transition:background 0.1s, border-color 0.1s;
  font-size:var(--fs-emphasis);
}
.fb-folder-row:hover  { background:#E8F0F8; border-color:var(--border); }
.fb-folder-row.fb-sel { background:#E3F2FD; border-color:#0066A1; font-weight:700; }
.fb-folder-icon { font-size:var(--fs-display); flex-shrink:0; }
.fb-folder-name { flex:1; min-width:0; overflow:hidden; text-overflow:ellipsis; white-space:nowrap; color:var(--text); }
.fb-empty { font-size:var(--fs-emphasis); color:#999; padding:0.6rem 0.4rem; font-style:italic; }


/* ── Rule Summary ── */
.rs-wrap { margin-bottom:0.8rem; }
.rem-block {
  margin-top:0.5rem; border-radius:5px;
  background:#EDF7EE; border:1px solid #A5D6A7;
  padding:0.4rem 0.6rem;
}
.rem-hd {
  font-size:var(--fs-body); font-weight:700; color:#1B5E20;
  display:flex; align-items:center; gap:0.3rem;
  margin-bottom:0.2rem;
}
.rem-icon { font-size:var(--fs-emphasis); }
.rem-body {
  font-size:var(--fs-body); color:#2E4D2E; line-height:1.5;
}
.hr-red  .rem-block { background:#FFF8F0; border-color:#FFCC80; }
.hr-red  .rem-hd    { color:#BF360C; }
.hr-red  .rem-body  { color:#4A1A00; }
.hr-amb  .rem-block { background:#FFFDE7; border-color:#FFE082; }
.hr-amb  .rem-hd    { color:#E65100; }
.hr-amb  .rem-body  { color:#4A2C00; }

.rs-block { margin-bottom:0.25rem; border-radius:6px; overflow:hidden;
  border:1px solid var(--border); }
.rs-row {
  display:flex; align-items:center; gap:0.5rem; padding:0.4rem 0.6rem;
  background:white; cursor:pointer; user-select:none;
  transition:background 0.12s;
}
.rs-row:hover { background:#F0F6FC; }
.rs-rid { font-family:monospace; font-weight:700; font-size:var(--fs-emphasis);
  color:#003366; flex-shrink:0; }
.rs-label { font-size:var(--fs-emphasis); color:#333; flex:1; min-width:0;
  white-space:nowrap; overflow:hidden; text-overflow:ellipsis; }
.rs-count { font-size:var(--fs-body); font-weight:700; padding:0.1rem 0.45rem;
  border-radius:10px; background:#E8EEF5; color:#003366; flex-shrink:0;
  white-space:nowrap; }

.rs-fname { font-size:var(--fs-body); font-weight:700; color:#0066A1;
  white-space:nowrap; flex-shrink:0; }
.rs-fdetail { font-size:var(--fs-body); color:#555; line-height:1.35; }
/* Folder selector handled by modal browser */

/* ── Review strip ── */
.review-strip {
  margin-top:0.6rem; padding:0.55rem 0.7rem;
  background:var(--surface-sunk); border-radius:6px;
  border:1px solid var(--border);
  display:flex; align-items:center; gap:0.6rem; flex-wrap:wrap;
}
.review-strip label { font-size:var(--fs-body); font-weight:700;
  color:#003366; margin:0; white-space:nowrap; }
.review-strip select, .review-strip input[type=text] {
  font-size:var(--fs-body) !important; padding:0.2rem 0.4rem !important;
  border-radius:4px; border:1px solid #C0CDD8;
}
.review-strip select { min-width:160px; }
.review-strip input[type=text] { flex:1; min-width:120px; }
.rev-logged {
  font-size:var(--fs-body); font-weight:700; color:#2E7D32;
  background:#E8F5E9; border:1px solid #2E7D32;
  border-radius:4px; padding:0.15rem 0.5rem; white-space:nowrap;
}
.rev-submit {
  font-size:var(--fs-body) !important; padding:0.2rem 0.7rem !important;
  font-weight:700 !important; white-space:nowrap;
}

/* ── Preview button on verdict strip ── */
.btn-verdict-preview {
  flex-shrink:0; font-size:var(--fs-body); padding:0.2rem 0.6rem;
  border-radius:4px; border:none;
  background:rgba(255,255,255,0.92); color:#003366; cursor:pointer;
  font-weight:700; white-space:nowrap; transition:background 0.12s;
}
.btn-verdict-preview:hover { background:white; }

/* ── Review Complete ── */
.accept-all-bar {
  display:flex; align-items:center; gap:0.75rem;
  padding:0.5rem 0.9rem; margin-bottom:0.4rem;
  background:#F0F7EE; border:1px solid #A5D6A7; border-radius:6px;
}
.btn-accept-all {
  background:#2E7D32 !important; color:#fff !important;
  border:none !important; border-radius:5px !important;
  font-size:var(--fs-emphasis) !important; font-weight:700 !important;
  padding:0.3rem 0.9rem !important; white-space:nowrap;
  cursor:pointer;
}
.btn-accept-all:hover { background:#1B5E20 !important; }
.accept-all-note { font-size:var(--fs-body); color:#555; font-style:italic; }

/* ── Reject all RED ── */
.reject-all-bar {
  display:flex; align-items:center; gap:0.75rem;
  padding:0.5rem 0.9rem; margin-bottom:0.4rem;
  background:#FDECEA; border:1px solid #EF9A9A; border-radius:6px;
}
.btn-reject-all {
  background:#C62828 !important; color:#fff !important;
  border:none !important; border-radius:5px !important;
  font-size:var(--fs-emphasis) !important; font-weight:700 !important;
  padding:0.3rem 0.9rem !important; white-space:nowrap;
  cursor:pointer;
}
.btn-reject-all:hover { background:#8E0000 !important; }
.reject-all-note { font-size:var(--fs-body); color:#555; font-style:italic; }

.review-complete-bar {
  display:flex; align-items:center; gap:1rem;
  padding:0.8rem 1.1rem; border-radius:8px; margin-top:0.8rem;
  background:linear-gradient(135deg,#003366 0%,#0066A1 100%);
  box-shadow:0 3px 10px rgba(0,102,161,0.25);
}
.btn-review-complete {
  padding:0.5rem 1.4rem; font-size:var(--fs-heading); font-weight:800;
  background:white; color:#003366; border:none; border-radius:6px;
  cursor:pointer; white-space:nowrap; letter-spacing:0.02em;
  transition:opacity 0.15s; flex-shrink:0;
}
.btn-review-complete:hover { opacity:0.88; }
.review-complete-msg {
  color:rgba(255,255,255,0.9); font-size:var(--fs-emphasis); line-height:1.4;
}
.review-complete-msg strong { color:white; }

/* ── Review Complete ── */
.readiness-strip {
  display:flex; align-items:center; gap:0.6rem;
  padding:0.55rem 0.85rem; border-radius:6px; margin-top:0.6rem;
  font-size:var(--fs-emphasis); font-weight:600; border:1px solid;
  transition:background 0.25s, border-color 0.25s;
}
.readiness-strip.rs-idle {
  background:#F5F5F5; border-color:#DDD; color:#888;
}
.readiness-strip.rs-pending {
  background:#FFF3E0; border-color:#E65100; color:#E65100;
}
.readiness-strip.rs-ready {
  background:#E8F5E9; border-color:#2E7D32; color:#2E7D32;
  animation:rs-pulse 0.6s ease-out;
}
@keyframes rs-pulse {
  0%   { box-shadow:0 0 0 0 rgba(46,125,50,0.45); }
  70%  { box-shadow:0 0 0 8px rgba(46,125,50,0); }
  100% { box-shadow:0 0 0 0 rgba(46,125,50,0); }
}
.readiness-dot {
  width:8px; height:8px; border-radius:50%; flex-shrink:0;
}
.rs-idle   .readiness-dot { background:#BBB; }
.rs-pending .readiness-dot { background:#E65100; }
.rs-ready  .readiness-dot { background:#2E7D32; animation:dot-pulse 1.2s infinite; }
@keyframes dot-pulse {
  0%,100% { opacity:1; }
  50%     { opacity:0.35; }
}

/* ── Collapsed scorecard strip ── */
.score-strip {
  display:flex; align-items:center; gap:0.7rem;
  padding:0.55rem 0.85rem; border-radius:6px; margin-bottom:0.5rem;
  background:var(--surface-sunk); border:1px solid var(--border); cursor:pointer;
  user-select:none; transition:background 0.15s;
}
.score-strip:hover { background:#E3EDF7; }
.score-strip-num {
  font-size:var(--fs-display-lg); font-weight:900; line-height:1; flex-shrink:0;
}
.score-strip-info { flex:1; min-width:0; }
.score-strip-label {
  font-size:var(--fs-body); font-weight:700; text-transform:uppercase;
  letter-spacing:0.05em;
}
.score-strip-detail {
  font-size:var(--fs-body); color:#555; margin-top:0.08rem;
  white-space:nowrap; overflow:hidden; text-overflow:ellipsis;
}
.score-strip-chevron {
  font-size:var(--fs-emphasis); color:#888; flex-shrink:0; transition:transform 0.2s;
}
.score-strip-chevron.open { transform:rotate(180deg); }
.score-expanded {
  border:1px solid var(--border); border-radius:6px;
  padding:0.7rem 0.85rem; margin-bottom:0.5rem;
  background:#FAFCFF;
}

/* ── Learning dashboard ── */
.ld-stat {
  display:flex; flex-direction:column; align-items:center;
  background:white; border-radius:7px; border:1px solid var(--border);
  padding:0.7rem 1rem; min-width:90px; text-align:center;
}
.ld-stat-val {
  font-size:var(--fs-display-lg); font-weight:800; color:",ARIDHIA_DARK,"; line-height:1;
}
.ld-stat-lbl {
  font-size:var(--fs-caption); text-transform:uppercase; letter-spacing:0.07em;
  color:#888; margin-top:0.25rem;
}


/* ── Consolidated batch header ─────────────────────────────────────────── */
.batch-header {
  background:var(--surface);
  border:1px solid var(--border);
  border-radius:8px;
  padding:0.9rem 1.1rem;
  margin-bottom:1rem;
  box-shadow:0 1px 3px rgba(0,0,0,0.04);
}
.bh-zone { margin:0.6rem 0; }
.bh-zone:first-child { margin-top:0; }
.bh-zone:last-child  { margin-bottom:0; }
.bh-zone-hd {
  font-size:var(--fs-caption); font-weight:800; letter-spacing:0.08em;
  color:#003366; text-transform:uppercase;
  margin-bottom:0.35rem;
}

/* Zone 1: scale + re-run */
.bh-zone-scale {
  display:flex; justify-content:space-between; align-items:center;
  padding-bottom:0.5rem; border-bottom:2px solid #0066A1;
}
.bh-scale-label {
  font-size:var(--fs-emphasis); font-weight:800; color:",ARIDHIA_DARK,";
  letter-spacing:0.04em;
}
.btn-bh-rerun {
  font-size:var(--fs-body); font-weight:600;
  background:transparent; color:",ARIDHIA_BLUE,";
  border:1px solid ",ARIDHIA_BLUE,"; border-radius:4px;
  padding:0.25rem 0.65rem; cursor:pointer;
  transition:background 0.12s, color 0.12s;
}
.btn-bh-rerun:hover {
  background:",ARIDHIA_BLUE,"; color:white;
}

/* Zone 2: RAG tiles */
.bh-rag-row {
  display:flex; gap:0.6rem; flex-wrap:wrap;
}
.rag-tile {
  flex:1; min-width:110px;
  border:1px solid; border-radius:6px;
  padding:0.5rem 0.6rem;
  display:flex; flex-direction:column; align-items:flex-start;
}
.rag-tile-n {
  font-size:var(--fs-display); font-weight:800; line-height:1;
}
.rag-tile-label {
  font-size:var(--fs-caption); font-weight:700; letter-spacing:0.08em;
  text-transform:uppercase; margin-top:0.2rem;
}
.rag-red   { background:",RED_BG,";    border-color:",RED_C,";    color:",RED_C,"; }
.rag-amber { background:",AMBER_BG,";  border-color:",AMBER_C,";  color:",AMBER_C,"; }
.rag-green { background:",GREEN_BG,";  border-color:",GREEN_C,";  color:",GREEN_C,"; }
.rag-unc   { background:#F3E5F5;       border-color:#7B1FA2;      color:#7B1FA2; }

/* Zone 3: risk score bar */
.bh-score-row {
  display:flex; align-items:center; gap:0.6rem;
  padding:0.4rem 0;
}
.bh-score-label {
  font-size:var(--fs-caption); font-weight:700; letter-spacing:0.06em;
  text-transform:uppercase; color:#666;
  white-space:nowrap;
}
.bh-score-bar-track {
  flex:1; height:8px; min-width:120px;
  background:#E0E0E0; border-radius:4px; overflow:hidden;
}
.bh-score-bar-fill {
  height:100%; border-radius:4px;
  transition:width 0.5s cubic-bezier(.4,0,.2,1);
}
.bh-score-value {
  font-size:var(--fs-emphasis); font-weight:700; color:#333;
  white-space:nowrap;
}
.bh-score-band {
  font-size:var(--fs-caption); font-weight:700; color:white;
  padding:0.15rem 0.5rem; border-radius:10px;
  white-space:nowrap;
}
.bh-score-info {
  font-size:var(--fs-emphasis); color:",ARIDHIA_BLUE,"; cursor:pointer;
  padding:0 0.25rem; user-select:none;
  transition:opacity 0.12s;
}
.bh-score-info:hover { opacity:0.7; }

/* Zone 4: linkage risk (uses helper-function markup + a scope class) */
.bh-zone-linkage .bh-zone-hd {
  color:#B71C1C;
  border-bottom:1px solid #FFCDD2;
  padding-bottom:0.3rem;
}

/* Zone 5: AIRA batch summary, flattened when rendered inside the header.
   The .in-header modifier overrides the standalone .aira-batch-slot framing. */
.bh-zone-aira.in-header {
  border-left:none; background:transparent;
  padding:0; margin:0.6rem 0;
  border-radius:0;
}
.bh-zone-aira.in-header.aira-batch-slot-ok {
  background:transparent;
}
.bh-zone-aira.in-header.aira-batch-slot-ready {
  background:transparent;
}
.bh-zone-aira.in-header.aira-batch-slot-loading {
  background:transparent;
}
.bh-zone-aira.in-header.aira-batch-slot-err {
  background:transparent;
}
.bh-zone-aira.in-header .aira-batch-slot-hd {
  color:#003366;
  border-bottom:1px solid var(--border);
  padding-bottom:0.25rem; margin-bottom:0.35rem;
}

/* Disabled Generate-AI-Summary button (visible when per-file reviews
   aren't all complete yet). Bootstrap's default disabled styling is
   mostly fine; we just add a not-allowed cursor and a muted look so
   the reason-note beside it is the main visual signal, not the
   button itself. */
.aira-batch-generate[disabled],
.aira-batch-generate:disabled {
  opacity: 0.55;
  cursor: not-allowed;
}
.aira-batch-not-ready-note {
  color: #7B1FA2;
  font-size: 0.76rem;
  font-style: italic;
}

/* ── Zone 5b: AIRA disclosure-review progress indicator ─────────
   Sits below the AIRA batch summary, above the actions zone. Shown
   only while the automatic review queue is active or recently
   completed. Updates reactively as each file's response arrives. */
.bh-review-progress {
  border-left: 4px solid #0066A1;
  background: #F5FAFD;
  padding: 0.5rem 0.7rem;
  border-radius: 0 4px 4px 0;
  margin: 0.6rem 0;
}
.bh-review-progress-done {
  border-left-color: #2E7D32;
  background: #F3F9F3;
}
.bh-review-progress .bh-zone-hd {
  color: #003366;
  font-size: 0.62rem; font-weight: 800;
  letter-spacing: 0.08em; margin-bottom: 0.3rem;
  text-transform: uppercase;
}
.bh-review-progress-done .bh-zone-hd { color: #1B5E20; }
.bh-review-progress-body {
  font-size: 0.78rem;
  color: #333;
  margin-bottom: 0.35rem;
}
.bh-review-progress-status {
  font-weight: 700;
}
.bh-review-progress-failed {
  color: #9A2A2A;
  font-weight: 600;
}
.bh-review-progress-remaining {
  color: #666;
  font-weight: 400;
}
.bh-review-progress-bar-outer {
  height: 6px;
  background: var(--border);
  border-radius: 3px;
  overflow: hidden;
  width: 100%;
}
.bh-review-progress-bar-inner {
  height: 100%;
  background: linear-gradient(90deg, #0066A1 0%, #3A9BD9 100%);
  border-radius: 3px;
  transition: width 0.4s ease-out;
}
.bh-review-progress-done .bh-review-progress-bar-outer { display: none; }

/* Zone 6: actions - reuse existing .btn-accept-all, .btn-reject-all,
   .btn-review-complete button styles from earlier in this stylesheet. */
.bh-actions .bh-action-row {
  display:flex; align-items:center; gap:0.75rem; flex-wrap:wrap;
  margin:0.35rem 0;
}
.bh-action-note {
  font-size:var(--fs-body); color:#666; font-style:italic;
  flex:1; min-width:200px;
}
.bh-review-complete-row {
  padding:0.5rem 0.65rem;
  border-radius:5px;
  border:1px solid transparent;
}
.bh-rc-pending {
  background:#FFF8E1; border-color:#FFE0B2;
}
.bh-rc-ready {
  background:",GREEN_BG,"; border-color:",GREEN_C,";
}
.bh-rc-status {
  font-size:var(--fs-emphasis); color:#333; flex:1;
}
.bh-rc-pending .bh-rc-status { color:#666; }
.bh-rc-ready   .bh-rc-status { color:#1B5E20; font-weight:600; }
.btn-review-complete[disabled] {
  opacity:0.45; cursor:not-allowed;
}
.btn-review-complete[disabled]:hover {
  opacity:0.45;
}

/* Zone exclude: bulk-remove buttons. Visually subtle - sits between
   zone 5b and zone 6, only present when at least one UNCERTAIN or
   INSUFFICIENT file is in the batch. Single row of 1-2 buttons plus
   an explanatory note. Buttons are red-tinted to signal destructive
   action; consistent with .btn-reject-all but slightly less prominent
   (smaller padding, lighter hover) since exclusion is reversible by
   re-adding files and re-running. */
.bh-exclude .bh-exclude-row {
  display:flex; align-items:center; gap:0.6rem; flex-wrap:wrap;
  margin:0.3rem 0;
}
.btn-bulk-exclude {
  background:transparent !important; color:#C62828 !important;
  border:1px solid #C62828 !important; border-radius:5px !important;
  font-size:var(--fs-body) !important; font-weight:700 !important;
  padding:0.25rem 0.75rem !important; white-space:nowrap;
  cursor:pointer; transition:background 0.12s, color 0.12s;
}
.btn-bulk-exclude:hover {
  background:#C62828 !important; color:white !important;
}
.bh-exclude-note {
  font-size:var(--fs-body); color:#666; font-style:italic;
  flex:1; min-width:200px;
}

/* Zone 7: rule summary, collapsible via <details>/<summary>.
   We reuse the existing .rs-wrap / .rs-block / .rs-row styles for the
   inner content - only the collapsible wrapper is new here. */
.bh-rule-summary {
  margin-top:0.4rem;
}
.bh-rule-summary > summary.bh-rule-summary-hd {
  cursor:pointer; user-select:none;
  font-size:var(--fs-body); font-weight:700; letter-spacing:0.06em;
  color:",ARIDHIA_DARK,"; padding:0.35rem 0;
  list-style:none;
  display:flex; align-items:center; gap:0.45rem;
}
.bh-rule-summary > summary.bh-rule-summary-hd::-webkit-details-marker {
  display:none;
}
.bh-rule-summary > summary.bh-rule-summary-hd::before {
  content:'\u25B8';
  font-size:var(--fs-body); color:",ARIDHIA_BLUE,";
  transition:transform 0.15s;
}
.bh-rule-summary[open] > summary.bh-rule-summary-hd::before {
  transform:rotate(90deg);
}
.bh-rule-summary > summary.bh-rule-summary-hd:hover {
  color:",ARIDHIA_BLUE,";
}

/* On narrow viewports, stack zone 1 children and RAG tiles vertically */
@media (max-width: 720px) {
  .bh-zone-scale { flex-direction:column; align-items:flex-start; gap:0.4rem; }
  .bh-rag-row { flex-direction:column; }
  .rag-tile { width:100%; min-width:0; }
  .bh-score-row { flex-wrap:wrap; }
}

/* ── ACRO researcher comments block ── */
/* Surfaces comments and exception justifications captured during the
   researcher's ACRO session. Distinct from AIRA banners: this is the
   researcher's own voice, not the engine's or the AI's. Warm amber accent
   so it reads as 'human note to the reviewer' rather than a verdict. */
.acro-comments-block {
  border:1px solid #E0A33E; border-left:4px solid #E0A33E;
  background:#FFFBF3; border-radius:6px; margin-top:0.7rem;
  padding:0.6rem 0.8rem;
}
.acro-comments-hd {
  display:flex; align-items:center; gap:0.4rem;
  font-size:var(--fs-body); font-weight:800; letter-spacing:0.05em;
  color:#8A5A00; text-transform:uppercase; margin-bottom:0.5rem;
}
.acro-comments-hd-icon { font-size:var(--fs-emphasis); }
.acro-comments-hd-note {
  margin-left:auto; font-weight:500; font-size:var(--fs-caption);
  color:#A0773A; text-transform:none; letter-spacing:0; font-style:italic;
}
.acro-output-panel {
  background:white; border:1px solid #EDD9B5; border-radius:5px;
  padding:0.5rem 0.6rem; margin-bottom:0.45rem;
}
.acro-output-panel:last-of-type { margin-bottom:0; }
.acro-output-hd {
  display:flex; align-items:center; gap:0.5rem; margin-bottom:0.35rem;
  flex-wrap:wrap;
}
.acro-status-pill {
  font-size:var(--fs-caption); font-weight:800; padding:0.1rem 0.45rem;
  border-radius:10px; flex-shrink:0; letter-spacing:0.03em;
}
.acro-output-uid {
  font-size:var(--fs-body); font-family:'Consolas',monospace; color:#003366;
  background:#EEF2F7; padding:0.05rem 0.35rem; border-radius:3px;
}
.acro-output-method {
  font-size:var(--fs-body); color:#777; font-style:italic;
}
.acro-comment-label {
  display:flex; align-items:center; gap:0.3rem;
  font-size:var(--fs-caption); font-weight:700; color:#8A5A00;
  text-transform:uppercase; letter-spacing:0.03em;
  margin:0.35rem 0 0.2rem 0;
}
.acro-comment-icon { font-size:var(--fs-emphasis); }
.acro-comment-text {
  margin:0.15rem 0 0.15rem 0; padding:0.35rem 0.6rem;
  background:#FFF8E8; border-left:3px solid #E0A33E; border-radius:0 4px 4px 0;
  font-size:var(--fs-emphasis); color:#3A2E14; font-style:normal;
  white-space:pre-wrap; word-break:break-word;
}
.acro-exception {
  margin-top:0.4rem; padding-top:0.4rem; border-top:1px dashed #EDD9B5;
}
.acro-exception-text {
  margin:0.15rem 0; padding:0.35rem 0.6rem;
  background:#FDF0F0; border-left:3px solid #C9603C; border-radius:0 4px 4px 0;
  font-size:var(--fs-emphasis); color:#4A2418; white-space:pre-wrap; word-break:break-word;
}
.acro-comment-gap {
  display:flex; align-items:center; gap:0.35rem;
  margin:0.3rem 0; padding:0.35rem 0.6rem;
  background:#FBEFE0; border-radius:4px;
  font-size:var(--fs-emphasis); color:#9A5B00; font-style:italic;
}
.acro-comments-foot {
  margin-top:0.5rem; font-size:var(--fs-caption); color:#A0773A; font-style:italic;
}

/* ── AIRA ACRO-consistency observation (v8) ──
   Rendered at the top of the AI Observations banner, which sits directly
   below the ACRO researcher-comments block, so the reviewer reads the
   researcher's claim and AIRA's claim-vs-file consistency note together.
   Styled in AIRA's navy voice (NOT the researcher's amber) so it is clearly
   the AI speaking, and carries a left-border tint by outcome. It is an
   observation, not a verdict - no RAG semantics, no decision language. */
.aira-acro-consistency {
  margin:0 0 0.5rem 0;
  padding:0.45rem 0.6rem;
  background:var(--surface);
  border:1px solid var(--border);
  border-left:3px solid #607D8B;
  border-radius:0 4px 4px 0;
}
.aira-acro-consistency-hd {
  display:flex; align-items:center; gap:0.4rem;
  font-size:var(--fs-caption); font-weight:800;
  letter-spacing:0.04em; text-transform:uppercase;
  color:var(--brand-navy);
}
.aira-acro-consistency-icon { font-size:var(--fs-emphasis); color:#607D8B; }
.aira-acro-consistency-obs {
  margin:0.25rem 0 0 0; font-size:var(--fs-body); color:#333; line-height:1.45;
}
/* Outcome tints on the left border only - muted, observational, not RAG. */
.aira-acro-consistency.acn-consistent     { border-left-color:#2E7D32; }
.aira-acro-consistency.acn-divergent       { border-left-color:#C62828; }
.aira-acro-consistency.acn-partial         { border-left-color:#E65100; }
.aira-acro-consistency.acn-indeterminate   { border-left-color:#607D8B; }
.aira-acro-consistency.acn-consistent   .aira-acro-consistency-icon { color:#2E7D32; }
.aira-acro-consistency.acn-divergent    .aira-acro-consistency-icon { color:#C62828; }
.aira-acro-consistency.acn-partial      .aira-acro-consistency-icon { color:#E65100; }

/* ── ACRO package container ── */
/* A bounded group: the ACRO session leads (header with title, config,
   checklist, roll-up), member output cards nested inside. Navy frame to
   read as a first-class structural unit distinct from standalone files. */
.acro-package {
  border:2px solid #003366; border-radius:9px; margin:0.6rem 0 1rem 0;
  background:var(--surface-sunk); overflow:hidden;
}
.acro-pkg-hd {
  background:linear-gradient(180deg,#EAF1F8 0%,var(--surface-sunk) 100%);
  border-bottom:1px solid var(--border); padding:0.7rem 0.9rem;
}
.acro-pkg-hd-top {
  display:flex; align-items:center; gap:0.55rem; flex-wrap:wrap;
  margin-bottom:0.5rem;
}
.acro-pkg-badge {
  font-size:var(--fs-caption); font-weight:800; letter-spacing:0.06em;
  background:#003366; color:white; padding:0.18rem 0.5rem; border-radius:4px;
  flex-shrink:0;
}
.acro-pkg-title {
  font-size:var(--fs-heading); font-weight:700; color:#003366;
}
.acro-pkg-version {
  font-size:var(--fs-body); color:#5A7A9A; font-family:'Consolas',monospace;
  background:var(--fill-pale); padding:0.05rem 0.35rem; border-radius:3px;
}
.acro-pkg-rollup { margin-left:auto; display:flex; gap:0.3rem; flex-wrap:wrap; }
.acro-pkg-pill {
  font-size:var(--fs-caption); font-weight:800; padding:0.12rem 0.5rem; border-radius:10px;
}
.acro-pkg-pill-fail   { background:#FFEBEE; color:#B71C1C; }
.acro-pkg-pill-review { background:#FFF3E0; color:#E65100; }
.acro-pkg-pill-pass   { background:#E8F5E9; color:#2E7D32; }
.acro-pkg-pill-total  { background:var(--fill-pale); color:#003366; }
.acro-pkg-cfg {
  display:flex; flex-wrap:wrap; gap:0.4rem 0.8rem; margin-bottom:0.5rem;
  padding:0.4rem 0.55rem; background:white; border:1px solid #DCE6F0;
  border-radius:5px;
}
.acro-pkg-cfg-item { font-size:var(--fs-body); display:flex; gap:0.25rem; align-items:baseline; }
.acro-pkg-cfg-key { color:#777; }
.acro-pkg-cfg-val { font-weight:700; color:#003366; font-family:'Consolas',monospace; }
.acro-pkg-checklist {
  margin-bottom:0.5rem; padding:0.45rem 0.6rem;
  background:white; border:1px solid #DCE6F0; border-radius:5px;
}
.acro-pkg-checklist-hd {
  display:flex; align-items:center; gap:0.35rem;
  font-size:var(--fs-caption); font-weight:800; color:#003366; text-transform:uppercase;
  letter-spacing:0.04em; margin-bottom:0.35rem;
}
.acro-chk-list { list-style:none; margin:0; padding:0; }
.acro-chk-item {
  display:flex; align-items:flex-start; gap:0.4rem;
  font-size:var(--fs-emphasis); padding:0.12rem 0; color:#333;
}
.acro-chk-mark { font-weight:800; flex-shrink:0; width:1rem; text-align:center; }
.acro-chk-yes .acro-chk-mark { color:#2E7D32; }
.acro-chk-no  .acro-chk-mark { color:#C62828; }
.acro-chk-no  .acro-chk-label { color:#8A4A00; }
.acro-pkg-missing {
  display:flex; align-items:flex-start; gap:0.4rem; margin-bottom:0.5rem;
  padding:0.45rem 0.6rem; background:#FFF3E0; border:1px solid #E0A33E;
  border-radius:5px; font-size:var(--fs-emphasis); color:#8A4A00;
}
.acro-pkg-missing-icon { flex-shrink:0; }
.acro-pkg-precedence-note {
  font-size:var(--fs-body); color:#5A7A9A; font-style:italic;
  padding-top:0.3rem; border-top:1px dashed var(--border);
}
.acro-pkg-members { padding:0.6rem 0.7rem 0.3rem 0.7rem; }
.acro-pkg-members .file-card { margin-bottom:0.6rem; }
.acro-pkg-no-members, .acro-pkg-no-members {
  padding:0.7rem; font-size:var(--fs-emphasis); color:#777; font-style:italic;
  text-align:center;
}

")