# Evidence rendering (HTML tables and script views)
# Auto-split from app.R

# ============================================================
# EVIDENCE RENDERER
# ============================================================
# Both renderers harden against degenerate evidence shapes from
# malformed-input inspectors:
#   - ev itself NULL
#   - ev fields missing or NULL ($data, $type, $lines, $caption, $src)
#   - ev$data not a data frame
#   - ev$data with zero rows or columns
#   - per-row $lineno / $text / $flag NULL
# Comparisons against possibly-NULL fields use identical()/isTRUE() to
# avoid "argument is of length zero" errors that would propagate into
# the renderUI and disconnect the Shiny session.

# Build an HTML table from evidence, highlighting flagged columns
render_evidence_html <- function(ev, bar_col) {
  if (is.null(ev)) return(NULL)
  df <- ev$data
  if (is.null(df) || !is.data.frame(df)) return(NULL)
  if (nrow(df) == 0L || ncol(df) == 0L) return(NULL)

  flag_cols <- ev$flag_cols %||% character(0)
  caption   <- ev$caption  %||% ""

  # Truncate long values for display. Per-column tryCatch protects
  # against exotic column types that resist as.character() coercion.
  df_disp <- tryCatch(
    as.data.frame(lapply(df, function(col) {
      v <- tryCatch(as.character(col),
                    error = function(e) rep("?", length(col)))
      if (is.null(v) || length(v) == 0L) v <- rep("?", nrow(df))
      substr(v, 1, 40)
    }), stringsAsFactors=FALSE),
    error = function(e) NULL
  )
  if (is.null(df_disp) || nrow(df_disp) == 0L || ncol(df_disp) == 0L) return(NULL)

  header_cells <- paste(sapply(names(df_disp), function(cn) {
    is_flag <- isTRUE(cn %in% flag_cols)
    sprintf('<th style="padding:4px 8px; white-space:nowrap; font-size:11px;
      background:%s; color:%s; border:1px solid %s;">%s</th>',
      if(is_flag) bar_col else "#003366",
      "white",
      if(is_flag) bar_col else "#003366",
      htmltools::htmlEscape(safe_utf8(cn)))
  }), collapse="")

  data_rows <- paste(sapply(seq_len(nrow(df_disp)), function(ri) {
    cells <- paste(sapply(seq_len(ncol(df_disp)), function(ci) {
      cn     <- names(df_disp)[ci]
      val    <- df_disp[ri, ci]
      is_flag <- isTRUE(cn %in% flag_cols)
      sprintf('<td style="padding:3px 8px; font-size:11px; border:1px solid #E0E0E0;
        background:%s; color:%s; font-weight:%s;">%s</td>',
        if(is_flag) "#FFF3CD" else if(ri%%2==0) "#FAFAFA" else "white",
        if(is_flag) "#6B3D00" else "#222",
        if(is_flag) "700" else "400",
        htmltools::htmlEscape(safe_utf8(as.character(val %||% ""))))
    }), collapse="")
    sprintf('<tr>%s</tr>', cells)
  }), collapse="")

  html <- sprintf(
    '<div style="margin-top:8px;">
       <div style="font-size:11px; color:#555; margin-bottom:4px; font-style:italic;">%s</div>
       <div style="overflow-x:auto; border-radius:5px; border:1px solid #DDD;">
         <table style="border-collapse:collapse; width:100%%; min-width:300px;">
           <thead><tr>%s</tr></thead>
           <tbody>%s</tbody>
         </table>
       </div>
     </div>',
    htmltools::htmlEscape(safe_utf8(caption)),
    header_cells,
    data_rows
  )
  HTML(html)
}

# For scripts: show offending lines with line numbers
render_script_evidence <- function(ev, bar_col) {
  if (is.null(ev)) return(NULL)
  # identical() is NULL-safe and length-safe; ev$type != "lines" throws
  # "argument is of length zero" if ev$type is NULL.
  if (!identical(ev$type, "lines")) return(NULL)
  lines <- ev$lines
  if (is.null(lines) || !is.list(lines) || length(lines) == 0L) return(NULL)

  rows <- paste(sapply(seq_along(lines), function(i) {
    l <- lines[[i]]
    if (!is.list(l)) return("")
    # Defensive field access: each l$X may be NULL on degenerate input.
    lineno <- suppressWarnings(as.integer(l$lineno %||% NA_integer_))
    if (is.na(lineno)) lineno <- 0L
    text <- as.character(l$text %||% "")
    flag <- isTRUE(l$flag)
    sprintf(
      '<tr>
         <td style="padding:2px 8px; font-family:monospace; font-size:11px;
           color:#999; background:#F5F5F5; border:1px solid #E0E0E0;
           white-space:nowrap; user-select:none;">%d</td>
         <td style="padding:2px 10px; font-family:monospace; font-size:11px;
           background:%s; color:%s; border:1px solid #E0E0E0; white-space:pre;">%s</td>
       </tr>',
      lineno,
      if(flag) "#FFF3CD" else if(i%%2==0) "#FAFAFA" else "white",
      if(flag) "#6B3D00" else "#333",
      htmltools::htmlEscape(safe_utf8(substr(text, 1, 120)))
    )
  }), collapse="")

  HTML(sprintf(
    '<div style="margin-top:8px;">
       <div style="font-size:11px; color:#555; margin-bottom:4px; font-style:italic;">%s</div>
       <div style="overflow-x:auto; border-radius:5px; border:1px solid #DDD;">
         <table style="border-collapse:collapse; width:100%%;">
           <thead>
             <tr>
               <th style="padding:3px 8px; background:#003366; color:white;
                 font-size:11px; width:40px;">Line</th>
               <th style="padding:3px 8px; background:#003366; color:white;
                 font-size:11px; text-align:left;">Content</th>
             </tr>
           </thead>
           <tbody>%s</tbody>
         </table>
       </div>
     </div>',
    htmltools::htmlEscape(safe_utf8(ev$caption %||% "")),
    rows
  ))
}