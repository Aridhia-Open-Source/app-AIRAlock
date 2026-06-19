# UI definition (page_fluid)
# Auto-split from app.R - do not edit the monolithic file

# ============================================================
# UI
# ============================================================
ui <- page_fluid(
  theme = bs_theme(version=5, bg="#EFF3F7", fg="#1A1A1A", primary=ARIDHIA_BLUE),
  tags$head(
    tags$style(HTML(app_css)),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('collapseFilePanel', function(msg) {
        var row = document.getElementById('main_panel_row');
        if (row) row.classList.add('files-collapsed');
      });
      Shiny.addCustomMessageHandler('showModal', function(msg) {
        var el = document.getElementById(msg.id);
        if (el) new bootstrap.Modal(el).show();
      });
      Shiny.addCustomMessageHandler('hideModal', function(msg) {
        var el = document.getElementById(msg.id);
        if (el) { var m = bootstrap.Modal.getInstance(el); if (m) m.hide(); }
      });
      // Scorecard expand/collapse toggle
      $(document).on('click', '.score-strip', function() {
        var exp = $('#score-expanded-panel');
        var chev = $(this).find('.score-strip-chevron');
        if (exp.is(':visible')) {
          exp.slideUp(180);
          chev.removeClass('open');
        } else {
          exp.slideDown(180);
          chev.addClass('open');
        }
      });
      Shiny.addCustomMessageHandler('setAllCheckboxes', function(msg) {
        document.querySelectorAll('.file-cb').forEach(function(cb) {
          cb.checked = msg.checked;
        });
      });
      // Batch remove button - set input value then clear after brief delay
      // (prevents duplicate fire on rapid clicks)
      var _batchRemoveTimer = null;
      $(document).on('click', '.batch-remove-btn', function() {
        var p = $(this).data('path');
        if (!p) return;
        Shiny.setInputValue('remove_batch_path', p, {priority:'event'});
      });
      // Override note validation
      // onchange fires on the wrapper div (tagAppendAttributes target).
      // selectize.js fires a change event on init - guard against this by
      // checking for empty value and treating it as non-override.
      function dteCheckOverrideNote(el, dteClass, noteId, btnId) {
        // querySelector gets the real <select> from inside the wrapper div
        var selEl  = (el && el.querySelector) ? el.querySelector('select') : el;
        if (!selEl) selEl = el;
        var noteEl = document.getElementById(noteId);
        var btnEl  = document.getElementById(btnId);
        if (!noteEl || !btnEl) return;
        var val = selEl ? (selEl.value || '') : '';
        // Empty value = selectize not yet initialised - preserve defaults
        if (val === '') return;
        // A note is required when the response differs from the DTE classification
        // (exception request, withdrawal, or escalation)
        var needsNote = (val !== dteClass);
        if (needsNote) {
          // Context-sensitive placeholder based on DTE class and chosen response
          var ph = 'Justification required';
          if (dteClass === 'RED' && val === 'GREEN')
            ph = 'Justification for airlock exception - explain why this file is safe despite the finding';
          else if (dteClass === 'RED' && val === 'AMBER')
            ph = 'Describe what manual review should focus on';
          else if (val === 'RED')
            ph = 'Note why this file is being withdrawn from the submission (optional)';
          else if (val === 'GREEN')
            ph = 'Confirm why no disclosure risk is present';
          else if (val === 'AMBER')
            ph = 'Describe your concern for the airlock reviewer';
          noteEl.placeholder = ph;
        } else {
          // Restore the R-defined default placeholder for this classification
          if (dteClass === 'RED')
            noteEl.placeholder = 'Note confirming you will remediate before resubmitting (optional)';
          else if (dteClass === 'AMBER')
            noteEl.placeholder = 'Add context to assist the airlock reviewer (optional)';
          else
            noteEl.placeholder = 'Note (optional)';
        }
        noteEl.style.borderColor = (needsNote && noteEl.value.trim() === '')
          ? '#C62828' : '';
        // Note: button is never disabled - server validates and notifies if note missing
      }
      function dteNoteInput(noteEl, selId, btnId, dteClass) {
        var wrapper = document.getElementById(selId);
        var selEl   = (wrapper && wrapper.querySelector) ? (wrapper.querySelector('select') || wrapper) : wrapper;
        if (!selEl) return;
        var val = selEl.value || '';
        if (val === '') return;
        var needsNote = (val !== dteClass);
        // Clear red border as soon as a note is present
        noteEl.style.borderColor = (needsNote && noteEl.value.trim() === '')
          ? '#C62828' : '';
      }


      // Register slider initial value so Shiny sees it from the start
      $(document).on('shiny:connected', function() {
        var sl = document.getElementById('cfg_tab003_cardinality');
        if (sl) Shiny.setInputValue('cfg_tab003_cardinality', parseInt(sl.value), {priority:'event'});
      });

      Shiny.addCustomMessageHandler('resetCfgNumerics', function(msg) {
        var defaults = {
          cfg_tab003_min_rows: 10,
          cfg_count_threshold: 5,
          cfg_size_gb: 5,
          cfg_gen003_min_cols: 4,
          cfg_eid_digits: 7,
          cfg_tab003_cardinality: 85,
          cfg_htm_table_rows: 20,
          cfg_scr003_min_len: 3,
          cfg_robj_large_mb: 100,
          cfg_kanon_max_rows: 10000,
          cfg_kanon_max_qi: 6
        };
        Object.keys(defaults).forEach(function(id) {
          var el = document.getElementById(id);
          if (el) {
            el.value = defaults[id];
            if (id === 'cfg_tab003_cardinality') {
              var lbl = document.getElementById('cfg_card_val');
              if (lbl) lbl.textContent = defaults[id] + '%';
              Shiny.setInputValue('cfg_tab003_cardinality', defaults[id], {priority:'event'});
            }
          }
        });
        var pcb = document.getElementById('cfg_patient_word');
        if (pcb) pcb.checked = true;
      });
    "))
  ),



    # Folder browser modal
  div(
    tags$div(class="modal fade", id="folderBrowserModal", tabindex="-1",
      `data-bs-backdrop`="static",
      tags$div(class="modal-dialog modal-dialog-scrollable",
        style="max-width:520px;",
        tags$div(class="modal-content",
          tags$div(class="modal-header", style="padding:0.55rem 0.9rem;",
            tags$h5(class="modal-title",
              style="font-size:var(--fs-heading); font-weight:700; color:var(--brand-navy);",
              "\U0001F4C2  Select Source Folder"),
            tags$button(type="button", class="btn-close btn-sm",
              `data-bs-dismiss`="modal")
          ),
          tags$div(class="modal-body", style="padding:0.6rem 0.9rem;",
            # Breadcrumb trail
            div(style=paste0(
              "font-size:var(--fs-body); font-family:monospace; color:var(--text-muted); ",
              "background:#F0F4F8; border:1px solid #DDE6EE; border-radius:4px; ",
              "padding:0.3rem 0.6rem; margin-bottom:0.5rem; word-break:break-all;"),
              uiOutput("fb_breadcrumb_ui", inline=TRUE)
            ),
            # Up button
            uiOutput("fb_up_ui"),
            # Folder list
            div(style="margin-top:0.35rem; max-height:340px; overflow-y:auto;",
              uiOutput("fb_folder_list_ui")
            )
          ),
          tags$div(class="modal-footer", style="padding:0.5rem 0.9rem;",
            div(style="flex:1; font-size:var(--fs-body); color:var(--text-muted); font-family:monospace;",
              uiOutput("fb_selected_path_ui", inline=TRUE)
            ),
            tags$button(type="button", class="btn btn-sm btn-outline-secondary",
              `data-bs-dismiss`="modal", "Cancel"),
            actionButton("fb_confirm", "Select this folder",
              class="btn btn-sm btn-primary",
              style="font-weight:700;")
          )
        )
      )
    )
  ),

  # Preview modal
  div(
    # ── Airlock folder creation modal ───────────────────────────────────────
    tags$div(class="modal fade", id="airlockFolderModal", tabindex="-1",
      `data-bs-backdrop`="static",
      tags$div(class="modal-dialog", style="max-width:520px;",
        tags$div(class="modal-content",
          tags$div(class="modal-header", style="padding:0.55rem 0.9rem;",
            tags$h5(class="modal-title",
              style="font-size:var(--fs-heading); font-weight:700; color:var(--brand-navy);",
              "\U0001F4C2  Create Airlock Submission Package"),
            tags$button(type="button", class="btn-close btn-sm",
              `data-bs-dismiss`="modal")),
          tags$div(class="modal-body", style="padding:0.7rem 0.9rem;",
            uiOutput("airlock_modal_body_ui")
          ),
          tags$div(class="modal-footer", style="padding:0.5rem 0.9rem; gap:0.5rem;",
            tags$button(type="button",
              class="btn btn-sm btn-outline-secondary",
              `data-bs-dismiss`="modal", "Cancel"),
            actionButton("airlock_report_only",
              "Report only",
              class="btn btn-sm btn-outline-primary",
              style="font-weight:600;"),
            actionButton("airlock_confirm",
              "\u2705  Create folder & generate report",
              class="btn btn-sm btn-primary",
              style="font-weight:700;"))))),

    tags$div(class="modal fade", id="previewModal", tabindex="-1",
      tags$div(class="modal-dialog modal-xl",
        tags$div(class="modal-content",
          tags$div(class="modal-header", style="padding:0.5rem 0.9rem;",
            tags$h5(class="modal-title", style="font-size:var(--fs-heading); font-weight:700;",
              "File Preview"),
            tags$button(type="button", class="btn-close btn-sm",
              `data-bs-dismiss`="modal")
          ),
          tags$div(class="modal-body",
            uiOutput("preview_content")
          )
        )
      )
    )
  ),

  div(class="main-panel-row", id="main_panel_row", style="padding:0.6rem 0.8rem 1rem;",
    # Explicit flex row (replaces layout_columns/bslib-grid, which forced
    # equal-height columns). Three parts: collapsible file panel, a thin
    # always-visible toggle rail, and the results panel which flexes to
    # fill remaining width. Collapsing is driven by the .files-collapsed
    # class on this row (toggled by JS); a quick slide animation is in CSS.
    div(class="mpr-flex",
      # ── Collapsible file panel ──
      div(class="file-panel", id="file_panel",
        card(
          card_header(style="padding:0.5rem 0.7rem;",
            uiOutput("assess_btn_ui")
          ),
          card_body(style="padding:0.7rem; overflow-y:auto; max-height:calc(100vh - 105px);",
            div(style="display:flex; justify-content:space-between; align-items:center; margin-bottom:0.5rem;",
              div(style="font-size:var(--fs-emphasis); font-weight:700; color:var(--brand-navy);", "File Browser"),
              actionButton("refresh", "Refresh", class="btn btn-sm btn-outline-secondary",
                           style="font-size:var(--fs-body); padding:0.15rem 0.5rem;")
            ),
            div(class="sect-hd", "SOURCE FOLDER"),
            div(style="margin-bottom:0.5rem;",
              div(style=paste0(
                  "font-size:var(--fs-body); font-family:monospace; color:var(--text-muted); ",
                  "background:#F0F4F8; border:1px solid #DDE6EE; border-radius:4px; ",
                  "padding:0.3rem 0.55rem; margin-bottom:0.4rem; word-break:break-all; ",
                  "min-height:1.6rem;"),
                textOutput("breadcrumb_display", inline=TRUE)
              ),
              actionButton("open_folder_browser", "\U0001F4C2  Browse folders\u2026",
                class="btn btn-sm btn-outline-primary",
                style="width:100%; font-size:var(--fs-emphasis); font-weight:600; padding:0.3rem 0.5rem;")
            ),
            div(class="sect-hd", "BROWSE FOLDER"),
            div(style="display:flex;gap:0.3rem;margin-bottom:0.4rem;",
              actionButton("sel_all_btn","Select All",class="btn btn-sm btn-outline-secondary",
                style="flex:1;font-size:var(--fs-body);padding:0.15rem 0.4rem;"),
              actionButton("sel_none_btn","Clear",class="btn btn-sm btn-outline-secondary",
                style="flex:1;font-size:var(--fs-body);padding:0.15rem 0.4rem;"),
              actionButton("add_to_batch_btn","+ Add to batch",class="btn btn-sm btn-primary",
                style="flex:1;font-size:var(--fs-body);padding:0.15rem 0.4rem;font-weight:700;")
            ),
            uiOutput("file_list"),
            hr(style="margin:0.5rem 0 0.4rem;"),
            div(style="display:flex; justify-content:space-between; align-items:center; margin-bottom:0.3rem;",
              uiOutput("batch_sect_hd_ui", inline=TRUE),
              actionButton("clear_batch_btn","Clear",class="btn btn-sm btn-outline-secondary",
                style="font-size:var(--fs-caption);padding:0.1rem 0.45rem;")
            ),
            uiOutput("batch_panel_ui")
          )
        )
      ),

      # ── Toggle rail (always visible) ──
      # Click toggles .files-collapsed on the row. Shows a chevron and the
      # batch count (filled by batch_rail_count_ui) so the reviewer always
      # knows how many files are in the batch and can reopen the panel in
      # one click to remove files and re-run.
      div(class="file-rail", id="file_rail",
        onclick="document.getElementById('main_panel_row').classList.toggle('files-collapsed');",
        title="Show or hide the file browser",
        div(class="file-rail-chevron", HTML("&#9664;")),
        div(class="file-rail-label", uiOutput("batch_rail_count_ui", inline=TRUE))
      ),

      # ── Results panel (flexes to fill) ──
      div(class="results-panel",
        card(
          div(class="card-body results-card-body",
            uiOutput("batch_header_ui"),
            uiOutput("results_ui")
          )
        )
      )
    ),

    # Batch summary
    uiOutput("batch_ui"),
    # Review complete action - now rendered as zone 6 of batch_header_ui
    # inside the Egress Assessment tab, so no standalone output here.
    # ── Rule Configuration (reviewer mode only) ────────────────
    # Sits just below the results panel. Because the panels already fill
    # nearly the full viewport height (results-card-body capped at
    # ~100vh), a normal margin places this just below the fold; the
    # reviewer scrolls the outer page to reach it. Collapsed by default.
    if (APP_MODE == "reviewer") div(style="margin-top:1rem;",
      card(
        card_header(
          div(style="display:flex; justify-content:space-between; align-items:center;",
            div(style="display:flex; align-items:center; gap:0.5rem;",
              tags$span(style="cursor:pointer; user-select:none;",
                onclick="var b=document.getElementById('cfg_body'); b.style.display=(b.style.display=='none'?'block':'none'); this.querySelector('.cfg-chevron').textContent=(b.style.display=='none'?'▶':'▼');",
                tags$span(class="cfg-chevron", style="font-size:var(--fs-body); margin-right:0.2rem;", "▶"),
                "Assessment Rule Settings"
              ),
              tags$span(style=paste0("font-size:var(--fs-caption); background:var(--rag-amber-bg); color:var(--rag-amber); ",
                "border-radius:10px; padding:0.1rem 0.5rem; font-weight:700;"), "EDITABLE")
            ),
            actionButton("cfg_reset", "Reset to defaults",
              class="btn btn-sm btn-outline-secondary",
              style="font-size:var(--fs-body); padding:0.15rem 0.5rem;")
          )
        ),
        card_body(
          div(id="cfg_body", style="display:none;",
            div(style="display:grid; grid-template-columns:1fr 1fr; gap:0.7rem 1rem; padding:0.5rem 0; align-items:start;",
              div(style="grid-column:1/-1; margin:0.7rem 0 0.15rem; padding-bottom:0.25rem; border-bottom:2px solid var(--brand-navy); color:var(--brand-navy); font-size:var(--fs-heading); font-weight:800;", "Thresholds and limits"),
                    div(class="cfg-group",
                      tags$label(class="cfg-lbl", "TAB-005 - Small count threshold"),
                      tags$small(class="cfg-hint", "Counts strictly less than this value in count columns will be flagged."),
                      div(style="display:flex; gap:0.6rem; align-items:center;",
                        tags$input(type="number", id="cfg_count_threshold",
                          class="form-control cfg-num", value=5, min=2, max=20, style="width:90px;"),
                        tags$span(style="font-size:var(--fs-emphasis); color:var(--text-muted);", "minimum permitted count")
                      )
                    ),
                    div(class="cfg-group",
                      tags$label(class="cfg-lbl", "TAB-003 - Per-participant row detection"),
                      tags$small(class="cfg-hint", "Minimum rows before cardinality check applies."),
                      div(style="display:flex; gap:0.6rem; align-items:center; margin-bottom:0.6rem;",
                        tags$input(type="number", id="cfg_tab003_min_rows",
                          class="form-control cfg-num", value=10, min=2, max=500, style="width:90px;"),
                        tags$span(style="font-size:var(--fs-emphasis); color:var(--text-muted);", "minimum rows")
                      ),
                      tags$small(class="cfg-hint", "Uniqueness ratio above which a column is flagged as per-participant."),
                      div(style="display:flex; gap:0.6rem; align-items:center;",
                        tags$input(type="range", id="cfg_tab003_cardinality",
                          min=50, max=99, value=85, step=1,
                          style="flex:1;",
                          oninput=paste0(
                            "document.getElementById('cfg_card_val').textContent=this.value+'%';",
                            "Shiny.setInputValue('cfg_tab003_cardinality',parseInt(this.value),{priority:'event'});"
                          )),
                        tags$span(id="cfg_card_val", style="font-size:var(--fs-emphasis); font-weight:700; color:var(--brand-navy); min-width:40px;", "85%")
                      )
                    ),
                    div(class="cfg-group",
                      tags$label(class="cfg-lbl", "TAB-008 - File size threshold (GB)"),
                      tags$small(class="cfg-hint", "Files exceeding this size trigger an AMBER warning."),
                      div(style="display:flex; gap:0.6rem; align-items:center;",
                        tags$input(type="number", id="cfg_size_gb",
                          class="form-control cfg-num", value=5, min=1, max=500, step=0.5, style="width:90px;"),
                        tags$span(style="font-size:var(--fs-emphasis); color:var(--text-muted);", "GB")
                      )
                    ),
                    div(class="cfg-group",
                      tags$label(class="cfg-lbl", "GEN-003 - Minimum GWAS columns to confirm summary format"),
                      tags$small(class="cfg-hint", "Number of standard GWAS column names (SNP, CHR, BP, BETA, SE, P, OR...) that must be present."),
                      div(style="display:flex; gap:0.6rem; align-items:center;",
                        tags$input(type="number", id="cfg_gen003_min_cols",
                          class="form-control cfg-num", value=4, min=2, max=10, style="width:90px;"),
                        tags$span(style="font-size:var(--fs-emphasis); color:var(--text-muted);", "columns required")
                      )
                    ),
                    div(class="cfg-group",
                      tags$label(class="cfg-lbl", "IMG-002 - Identifier digit length"),
                      tags$small(class="cfg-hint", "SVG numeric sequences of exactly this length are flagged as potential participant identifier values."),
                      div(style="display:flex; gap:0.6rem; align-items:center;",
                        tags$input(type="number", id="cfg_eid_digits",
                          class="form-control cfg-num", value=7, min=4, max=12, style="width:90px;"),
                        tags$span(style="font-size:var(--fs-emphasis); color:var(--text-muted);", "digits")
                      )
                    ),
                    div(class="cfg-group",
                      tags$label(class="cfg-lbl",
                        span(class="bs bs-r", style="font-size:var(--fs-caption); margin-right:0.3rem;", "SCR-003"),
                        " Sensitive Phenotype Minimum Term Length"),
                      tags$small(class="cfg-hint",
                        "Phenotype terms shorter than this are skipped by SCR-003. ",
                        "Increase to 4+ to avoid very short terms (e.g. 'hiv') matching ",
                        "substrings in encoded data. Default: 3 (all terms matched)."),
                      div(style="display:flex; gap:0.6rem; align-items:center; margin-top:0.4rem;",
                        tags$input(type="number", id="cfg_scr003_min_len",
                          class="form-control cfg-num", value=3, min=2, max=10,
                          style="width:80px;"),
                        tags$span(style="font-size:var(--fs-emphasis); color:var(--text-muted);", "minimum characters")
                      )
                    ),
                    div(class="cfg-group",
                      tags$label(class="cfg-lbl",
                        span(class="bs bs-a", style="font-size:var(--fs-caption); margin-right:0.3rem;", "HTM-006, MD-003"),
                        " Large Table Row Threshold"),
                      tags$small(class="cfg-hint",
                        "HTML and Markdown tables with more rows than this threshold are flagged AMBER. ",
                        "Increase if your outputs routinely include large summary tables. Default: 20."),
                      div(style="display:flex; gap:0.6rem; align-items:center; margin-top:0.4rem;",
                        tags$input(type="number", id="cfg_htm_table_rows",
                          class="form-control cfg-num", value=20, min=5, max=500,
                          style="width:80px;"),
                        tags$span(style="font-size:var(--fs-emphasis); color:var(--text-muted);", "rows")
                      )
                    ),
              div(style="grid-column:1/-1; margin:0.7rem 0 0.15rem; padding-bottom:0.25rem; border-bottom:2px solid var(--brand-navy); color:var(--brand-navy); font-size:var(--fs-heading); font-weight:800;", "K-anonymity estimation"),
                    div(class="cfg-group",
                      tags$label(class="cfg-lbl", "TAB-015/016 \u2014 K-Anonymity Estimation"),
                      tags$small(class="cfg-hint",
                        "Automatically computes k across recognised quasi-identifier columns. ",
                        "Disable for very large files or datasets where column names do not follow ",
                        "standard conventions."),
                      div(style="display:flex; align-items:center; gap:0.5rem; margin-bottom:0.5rem;",
                        tags$input(type="checkbox", id="cfg_kanon_enabled",
                          checked=NA, style="width:16px; height:16px;"),
                        tags$span(style="font-size:var(--fs-emphasis);", "Enable k-anonymity estimation")
                      ),
                      tags$small(class="cfg-hint", "Maximum rows for automated computation:"),
                      div(style="display:flex; gap:0.6rem; align-items:center; margin-bottom:0.4rem;",
                        tags$input(type="number", id="cfg_kanon_max_rows",
                          class="form-control cfg-num", value=10000, min=100, max=100000,
                          step=1000, style="width:100px;"),
                        tags$span(style="font-size:var(--fs-emphasis); color:var(--text-muted);", "rows")
                      ),
                      tags$small(class="cfg-hint", "Maximum quasi-identifier columns to include:"),
                      div(style="display:flex; gap:0.6rem; align-items:center;",
                        tags$input(type="number", id="cfg_kanon_max_qi",
                          class="form-control cfg-num", value=6, min=2, max=10,
                          style="width:80px;"),
                        tags$span(style="font-size:var(--fs-emphasis); color:var(--text-muted);", "columns")
                      )
                    ),
              div(style="grid-column:1/-1; margin:0.7rem 0 0.15rem; padding-bottom:0.25rem; border-bottom:2px solid var(--brand-navy); color:var(--brand-navy); font-size:var(--fs-heading); font-weight:800;", "Identifier and quasi-identifier vocabularies"),
                    div(class="cfg-group",
                      tags$label(class="cfg-lbl", "TAB-001 - Identifier column patterns"),
                      tags$small(class="cfg-hint", "One regex per line. Anchored with ^ for exact column name match."),
                      tags$textarea(id="cfg_id_patterns", class="form-control cfg-ta",
                        rows="6", placeholder="^participant_id$\n^subject_id$",
                        paste(participant_id_patterns, collapse="\n"))
                    ),
                    div(class="cfg-group",
                      tags$label(class="cfg-lbl", "TAB-014 - Derived temporal/spatial identifier patterns"),
                      tags$small(class="cfg-hint",
                        "Column names matching these patterns are flagged as derived quasi-identifiers. ",
                        "One regex per line."),
                      tags$textarea(id="cfg_derived_id_patterns", class="form-control cfg-ta",
                        rows="6", placeholder="^time_since\n^age_at_",
                        paste(derived_id_patterns, collapse="\n"))
                    ),
                    div(class="cfg-group",
                      tags$label(class="cfg-lbl", "TAB-007 - Restricted field terms"),
                      tags$small(class="cfg-hint", "Substring match against column names. One term per line."),
                      tags$textarea(id="cfg_restricted_fields", class="form-control cfg-ta",
                        rows="5", placeholder="systolic_bp\nmri",
                        paste(restricted_fields, collapse="\n"))
                    ),
              div(style="grid-column:1/-1; margin:0.7rem 0 0.15rem; padding-bottom:0.25rem; border-bottom:2px solid var(--brand-navy); color:var(--brand-navy); font-size:var(--fs-heading); font-weight:800;", "Sensitive-content vocabularies"),
                    div(class="cfg-group",
                      tags$label(class="cfg-lbl", "TAB-004 / SCR-003 / GEN-002 - Sensitive phenotype vocabulary"),
                      tags$small(class="cfg-hint", "Applied across tabular column names, script coding contexts, and genomic metadata. One term per line."),
                      tags$textarea(id="cfg_sensitive_phenotypes", class="form-control cfg-ta",
                        rows="14", placeholder="hiv\ndepression",
                        paste(sensitive_phenotypes, collapse="\n"))
                    ),
                    div(class="cfg-group",
                      tags$label(class="cfg-lbl", "TAB-012 - Free text column name patterns"),
                      tags$small(class="cfg-hint",
                        "Column names matching these patterns are flagged as likely free text. ",
                        "One regex per line. Suffix patterns like .*_notes$ match any column ending in _notes."),
                      tags$textarea(id="cfg_free_text_patterns", class="form-control cfg-ta",
                        rows="6", placeholder="^notes$\n.*_comments$",
                        paste(free_text_patterns, collapse="\n"))
                    ),
                    div(class="cfg-group",
                      tags$label(class="cfg-lbl", "GEN-003 - GWAS column vocabulary"),
                      tags$small(class="cfg-hint", "Recognised GWAS summary statistic column names. One per line."),
                      tags$textarea(id="cfg_gwas_cols", class="form-control cfg-ta",
                        rows="9", placeholder="snp\nchr\nbp",
                        paste(gwas_cols, collapse="\n"))
                    ),
                    div(class="cfg-group",
                      tags$label(class="cfg-lbl", "IMG-004 - Restricted plot type keywords"),
                      tags$small(class="cfg-hint", "Pipe-separated regex applied to filename. Matches trigger an AMBER review flag."),
                      tags$textarea(id="cfg_img004_keywords", class="form-control cfg-ta",
                        rows="5", placeholder="circos|igv|oncoplot",
                        "circos|igv|oncoplot|tmb|mutational")
                    ),
              div(style="grid-column:1/-1; margin:0.7rem 0 0.15rem; padding-bottom:0.25rem; border-bottom:2px solid var(--brand-navy); color:var(--brand-navy); font-size:var(--fs-heading); font-weight:800;", "Free-text named entity recognition"),
                    div(class="cfg-group", style="grid-column:1/-1;",
                      tags$label(class="cfg-lbl",
                        span(class="bs bs-r",
                          style="font-size:var(--fs-caption); margin-right:0.3rem;", "TAB-024"),
                        " Free-Text Named Entity Recognition"),
                      tags$small(class="cfg-hint",
                        "Scans free-text columns for person names, geographic places, institution names, and quasi-identifier composites. All vocabulary lists are configurable for your geography and language. Defaults are UK English examples."),
                      div(style="display:flex; align-items:center; gap:0.5rem; margin:0.4rem 0 0.5rem;",
                        tags$input(type="checkbox", id="cfg_ner_enabled",
                          checked=NA, style="width:16px; height:16px;"),
                        tags$span(style="font-size:var(--fs-emphasis);", "Enable NER scan (TAB-024)")
                      ),
                      tags$small(class="cfg-hint",
                        tags$strong("Person name title prefixes"),
                        " - honorifics before a name (Mr, Dr, Mme, Herr...). One per line:"),
                      tags$textarea(id="cfg_ner_titles",
                        class="form-control cfg-ta", rows="3",
                        style="font-size:var(--fs-emphasis); font-family:monospace;",
                        paste(DEFAULT_CFG$ner_person_titles, collapse="\n")),
                      tags$small(class="cfg-hint",
                        style="margin-top:0.35rem;",
                        tags$strong("Geographic place names"),
                        " - cities, counties, regions for your geography. One per line:"),
                      tags$textarea(id="cfg_ner_places",
                        class="form-control cfg-ta", rows="5",
                        style="font-size:var(--fs-emphasis); font-family:monospace;",
                        paste(DEFAULT_CFG$ner_geo_places, collapse="\n")),
                      tags$small(class="cfg-hint",
                        style="margin-top:0.35rem;",
                        tags$strong("Institution keywords"),
                        " - terms identifying healthcare/research sites. Remove NHS/Royal if not UK. One per line:"),
                      tags$textarea(id="cfg_ner_insts",
                        class="form-control cfg-ta", rows="3",
                        style="font-size:var(--fs-emphasis); font-family:monospace;",
                        paste(DEFAULT_CFG$ner_inst_patterns, collapse="\n")),
                      tags$small(class="cfg-hint",
                        style="margin-top:0.35rem;",
                        tags$strong("Occupation keywords"),
                        " - used for quasi-identifier composite detection (age + sex + occupation = RED). One per line:"),
                      tags$textarea(id="cfg_ner_occ",
                        class="form-control cfg-ta", rows="4",
                        style="font-size:var(--fs-emphasis); font-family:monospace;",
                        paste(DEFAULT_CFG$ner_occ_patterns, collapse="\n")),
                      tags$small(class="cfg-hint",
                        style="margin-top:0.35rem;",
                        tags$strong("Name exclusions"),
                        " - two-capitalised-word phrases to EXCLUDE from person name detection (e.g. compass directions, months). One per line:"),
                      tags$textarea(id="cfg_ner_exclusions",
                        class="form-control cfg-ta", rows="3",
                        style="font-size:var(--fs-emphasis); font-family:monospace;",
                        paste(DEFAULT_CFG$ner_name_exclusions, collapse="\n"))
                    ),
              div(style="grid-column:1/-1; margin:0.7rem 0 0.15rem; padding-bottom:0.25rem; border-bottom:2px solid var(--brand-navy); color:var(--brand-navy); font-size:var(--fs-heading); font-weight:800;", "Suppression and back-calculation"),
            div(class="cfg-group", style="grid-column:1/-1;",
              tags$label(class="cfg-lbl",
                span(class="bs bs-r",
                  style="font-size:var(--fs-caption); margin-right:0.3rem;", "TAB-023"),
                " Suppression Markers & Total Labels"),
              tags$small(class="cfg-hint",
                "Cell values treated as suppression markers, checked for back-calculation risk. One per line."),
              tags$textarea(id="cfg_supp_markers",
                class="form-control cfg-ta", rows="4",
                style="font-size:var(--fs-emphasis); font-family:monospace;",
                paste(DEFAULT_CFG$suppression_markers, collapse="\n")),
              tags$small(class="cfg-hint",
                style="margin-top:0.35rem;",
                "Total row label patterns (one per line):"),
              tags$textarea(id="cfg_total_patterns",
                class="form-control cfg-ta", rows="3",
                style="font-size:var(--fs-emphasis); font-family:monospace;",
                paste(DEFAULT_CFG$total_row_patterns, collapse="\n"))
            ),
              div(style="grid-column:1/-1; margin:0.7rem 0 0.15rem; padding-bottom:0.25rem; border-bottom:2px solid var(--brand-navy); color:var(--brand-navy); font-size:var(--fs-heading); font-weight:800;", "File-type specifics"),
                    div(class="cfg-group",
                      tags$label(class="cfg-lbl", "IMG-002 - Patient word check"),
                      tags$small(class="cfg-hint", "Flag SVG files where the word 'patient' appears in the text layer."),
                      div(style="display:flex; align-items:center; gap:0.5rem; margin-top:0.4rem;",
                        tags$input(type="checkbox", id="cfg_patient_word",
                          checked=NA, style="width:16px; height:16px;"),
                        tags$span(style="font-size:var(--fs-emphasis);", "Enable patient word detection")
                      )
                    ),
                    div(class="cfg-group",
                      tags$label(class="cfg-lbl", "IMG-002 - Additional SVG trigger words"),
                      tags$small(class="cfg-hint", "Extra words to search for in SVG text layers. Whole-word match, case-insensitive. One per line."),
                      tags$textarea(id="cfg_extra_words", class="form-control cfg-ta",
                        rows="5", placeholder="subject\nparticipant\ndonor", "")
                    ),
                    div(class="cfg-group",
                      tags$label(class="cfg-lbl",
                        span(class="bs bs-a", style="font-size:var(--fs-caption); margin-right:0.3rem;", "SCR-006"),
                        " Rendered Notebook Outputs"),
                      tags$small(class="cfg-hint",
                        "Flag Jupyter notebooks (.ipynb) that contain non-empty cell outputs. ",
                        "When enabled, the PII scan option also checks output text for participant identifiers and sensitive terms."),
                      div(style="display:flex; flex-direction:column; gap:0.4rem; margin-top:0.5rem;",
                        div(style="display:flex; align-items:center; gap:0.5rem;",
                          tags$input(type="checkbox", id="cfg_scr006_flag_outputs",
                            checked=NA, style="width:16px; height:16px;"),
                          tags$span(style="font-size:var(--fs-emphasis);", "Flag notebooks with non-empty outputs (AMBER)")
                        ),
                        div(style="display:flex; align-items:center; gap:0.5rem;",
                          tags$input(type="checkbox", id="cfg_scr006_pii_scan",
                            checked=NA, style="width:16px; height:16px;"),
                          tags$span(style="font-size:var(--fs-emphasis);", "Scan output content for PII patterns")
                        )
                      )
                    ),
                    div(class="cfg-group",
                      tags$label(class="cfg-lbl",
                        span(class="bs bs-a", style="font-size:var(--fs-caption); margin-right:0.3rem;", "COL-001"),
                        " Columnar Format Outcome"),
                      tags$small(class="cfg-hint",
                        "Default outcome for Parquet and Feather files. AMBER (default) flags for manual review. ",
                        "Set to RED in environments where columnar formats should never be egressed without explicit approval."),
                      div(style="display:flex; align-items:center; gap:0.5rem; margin-top:0.5rem;",
                        tags$input(type="checkbox", id="cfg_columnar_red",
                          style="width:16px; height:16px;"),
                        tags$span(style="font-size:var(--fs-emphasis);", "Treat Parquet / Feather as RED (default: AMBER)")
                      )
                    ),
                    div(class="cfg-group",
                      tags$label(class="cfg-lbl",
                        span(class="bs bs-a", style="font-size:var(--fs-caption); margin-right:0.3rem;", "SER-001/002"),
                        " Serialised R Object Rules"),
                      tags$small(class="cfg-hint",
                        "SER-002 fires RED when an .rds or .RData file exceeds this size - ",
                        "large serialised objects are likely to contain full data structures. ",
                        "Below the threshold, SER-001 fires AMBER."),
                      div(style="display:flex; gap:0.6rem; align-items:center; margin-top:0.4rem; margin-bottom:0.8rem;",
                        tags$input(type="number", id="cfg_robj_large_mb",
                          class="form-control cfg-num", value=100, min=10, max=10000,
                          style="width:100px;"),
                        tags$span(style="font-size:var(--fs-emphasis); color:var(--text-muted);", "MB threshold for RED")
                      ),
                      tags$label(class="cfg-lbl", style="margin-top:0.4rem;",
                        "Model/result name hints"),
                      tags$small(class="cfg-hint",
                        "Filenames containing these terms are labelled as likely model objects ",
                        "(lower re-identification risk). One term per line."),
                      tags$textarea(id="cfg_robj_model_names", class="form-control cfg-ta",
                        rows="7",
                        placeholder="model\nfit\nlm\nglm",
                        paste(c("model","fit","lm","glm","cox","surv",
                                "rf","xgb","result","summary","output"),
                              collapse="\n"))
                    ),
              div(style="grid-column:1/-1; margin:0.7rem 0 0.15rem; padding-bottom:0.25rem; border-bottom:2px solid var(--brand-navy); color:var(--brand-navy); font-size:var(--fs-heading); font-weight:800;", "Diagnostics"),
                    div(class="cfg-group", style="grid-column:1/-1;",
                      tags$label(class="cfg-lbl",
                        span(class="bs bs-r",
                          style="font-size:var(--fs-caption); margin-right:0.3rem;", "AIRA"),
                        " AI Review Diagnostics"),
                      tags$small(class="cfg-hint",
                        "For model testing only. When enabled, each AI disclosure review writes the exact prompt and raw response to an aira_capture/ folder under the debug logs, for diagnosing prompt or context-window problems with a given model. Off by default. Capture files persist full prompt content, which can include sample data, so leave this off for routine use."),
                      div(style="display:flex; align-items:center; gap:0.5rem; margin:0.4rem 0 0.5rem;",
                        tags$input(type="checkbox", id="cfg_aira_capture_prompts",
                          style="width:16px; height:16px;"),
                        tags$span(style="font-size:var(--fs-emphasis);",
                          "Capture AI prompts and responses to disk")
                      )
                    )
            ),
            # Apply button
            div(style="display:flex; justify-content:flex-end; margin-top:0.8rem; padding-top:0.6rem; border-top:1px solid var(--border);",
              div(style="font-size:var(--fs-body); color:var(--text-hint); margin-right:auto; align-self:center;",
                "\u24d8 Changes take effect on the next Run Assessment."),
              actionButton("cfg_apply", "Apply configuration",
                class="btn btn-sm btn-primary",
                style="font-weight:700; font-size:var(--fs-emphasis);")
            ),


          )
        )
      )
    ), # end Rule Configuration (reviewer only)

    # Full rule table - always visible
    div(style="margin-top:1rem;",
      card(
        card_header("Full Rule Set"),
        card_body(DT::dataTableOutput("rule_table"))
      )
    ),

    # Submission History / Audit Log (reviewer mode only)
    if (APP_MODE == "reviewer") div(style="margin-top:1rem;",
      card(
        card_header(
          div(style="display:flex; justify-content:space-between; align-items:center;",
            div(style="display:flex; align-items:center; gap:0.5rem;",
              "Audit Log",
              tags$span(style=paste0(
                "font-size:var(--fs-caption); background:#E3F2FD; color:#1565C0; ",
                "border-radius:10px; padding:0.1rem 0.5rem; font-weight:700;"),
                "PROTOTYPE")
            ),
            actionButton("refresh_log", "Refresh", class="btn btn-sm btn-outline-secondary",
              style="font-size:var(--fs-body); padding:0.15rem 0.5rem;")
          )
        ),
        card_body(
          navset_tab(
            nav_panel("Overview",        uiOutput("ld_overview_ui")),
            nav_panel("Rule Performance", DT::dataTableOutput("ld_rules_dt")),
            nav_panel("Decision Log",    DT::dataTableOutput("ld_log_dt"))
          )
        )
      )
    ) # end Audit Log (reviewer only)
  )
)