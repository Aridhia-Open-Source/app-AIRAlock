# Preview UI rendering (render_preview_ui)
# Auto-split from app.R

render_preview_ui <- function(prev, filepath) {
  fname <- basename(filepath)

  # Toolbar
  toolbar <- div(class="prev-toolbar",
    tags$strong(style="font-size:0.82rem; color:#003366;", fname),
    div(class="prev-meta", prev$meta %||% "")
  )

  body <- switch(prev$type,

    # Tabular table
    tabular = div(class="prev-body",
      DT::renderDataTable(
        DT::datatable(prev$data, rownames=TRUE, class="compact",
          options=list(pageLength=20, scrollX=TRUE, scrollY="60vh",
                       dom="ftipr", autoWidth=FALSE)),
        server=TRUE)
    ),

    # VCF: header + variant table
    vcf = div(class="prev-body",
      if (length(prev$meta_lines)>0)
        div(class="prev-vcf-header",
          tagList(lapply(head(prev$meta_lines,30), function(l) {
            l2 <- sub("^##","",l)
            parts <- regmatches(l2, regexpr("^[^=]+",l2))
            key <- if(length(parts)>0) parts[1] else ""
            tags$div(tags$span(class="prev-vcf-key",paste0("##",key)),
              if (nchar(l2)>nchar(key)) paste0("=",substring(l2,nchar(key)+2)) else "")
          }))
        ),
      if (!is.null(prev$data))
        div(class="prev-vcf-table",
          DT::renderDataTable(
            DT::datatable(prev$data, rownames=FALSE, class="compact",
              options=list(pageLength=20, scrollX=TRUE, dom="ftipr")),
            server=TRUE)
        )
      else div(style="padding:1rem; color:#888; font-size:0.82rem;",
             "No variant data rows found.")
    ),

    # NIfTI: header metadata panel
    # NIfTI with rendered pixel slice
    nifti_image = div(class="prev-body",
      style="display:flex; flex-direction:row; overflow:hidden;",
      # Rendered axial slice
      div(style=paste0(
        "flex:1; background:#111; display:flex; align-items:center; ",
        "justify-content:center; min-width:0; overflow:hidden;"),
        tags$img(src=prev$src,
          style="max-width:100%; max-height:100%; object-fit:contain;",
          alt="NIfTI axial mid-slice")
      ),
      # Metadata panel
      div(style=paste0(
        "width:220px; flex-shrink:0; background:#1A2A3A; color:#B8CCE0; ",
        "font-family:'Consolas','Monaco',monospace; font-size:0.72rem; ",
        "overflow-y:auto; padding:0.7rem 0.8rem;"),
        div(style="color:#7BAFD4; font-weight:700; font-size:0.68rem;
                   text-transform:uppercase; letter-spacing:0.06em;
                   margin-bottom:0.5rem;", "NIfTI Header"),
        if (!is.null(prev$meta_rows))
          tagList(lapply(prev$meta_rows, function(r) {
            div(style="margin-bottom:0.35rem;",
              div(style="color:#7BAFD4; font-size:0.65rem;", r[1]),
              div(style="color:#D4E4F4; word-break:break-all;", r[2])
            )
          })),
        div(style="margin-top:0.8rem; padding-top:0.6rem;
                   border-top:1px solid #2A3A4A; color:#5A7A9A;
                   font-size:0.65rem; font-style:italic;",
          "Axial mid-slice. Pixel data loaded for preview only.")
      )
    ),

    # NIfTI header-only (pixel render failed / oro.nifti unavailable)
    nifti = {
      div(class="prev-body",
        if (is.null(prev$meta_rows))
          div(style="padding:1rem; color:#C62828; font-size:0.82rem;",
            "\u26a0 NIfTI header could not be parsed.")
        else {
          div(style="padding:0.8rem 1rem; overflow-y:auto;",
            tags$table(style=paste0(
              "width:100%; border-collapse:collapse; font-size:0.82rem; ",
              "font-family:'Consolas','Monaco',monospace;"),
              tagList(lapply(prev$meta_rows, function(r) {
                tags$tr(
                  tags$td(style=paste0(
                    "padding:0.3rem 0.7rem 0.3rem 0; font-weight:700; ",
                    "color:#0066A1; white-space:nowrap; width:130px; ",
                    "vertical-align:top; border-bottom:1px solid #EEE;"), r[1]),
                  tags$td(style=paste0(
                    "padding:0.3rem 0; color:#1A1A1A; ",
                    "border-bottom:1px solid #EEE; word-break:break-all;"), r[2])
                )
              })),
              tags$tr(tags$td(colspan="2",
                style="padding-top:0.7rem; font-size:0.75rem; color:#888; font-style:italic;",
                "\u2139 Pixel slice could not be rendered for this file."))
            )
          )
        }
      )
    },
    # Statistical (Stata/SAS/SPSS): variable labels + data table
    statistical = {
      div(class="prev-body",
        if (is.null(prev$data))
          div(style="padding:1rem; color:#888; font-size:0.82rem;", prev$error %||% "No data.")
        else {
          tagList(
            if (!is.null(prev$var_labels) && nrow(prev$var_labels) > 0)
              div(class="prev-vcf-header",
                tags$strong(style="font-size:0.7rem; color:#003366;", "VARIABLE LABELS"),
                tags$table(style="font-size:0.7rem; width:100%; border-collapse:collapse;",
                  tagList(lapply(seq_len(min(nrow(prev$var_labels), 20)), function(i) {
                    tags$tr(
                      tags$td(style="color:#0066A1; font-weight:700; padding:0 0.5rem 0 0; white-space:nowrap;",
                        prev$var_labels$col[i]),
                      tags$td(style="color:#444;", prev$var_labels$label[i])
                    )
                  }))
                )
              ),
            div(class="prev-vcf-table",
              DT::renderDataTable(
                DT::datatable(sanitise_for_dt(prev$data), rownames=FALSE,
                  class="compact", filter="none",
                  options=list(pageLength=20, scrollX=TRUE, dom="ftipr")),
                server=TRUE)
            )
          )
        }
      )
    },

    # Code viewer with line numbers
    script = {
      lines <- prev$lines %||% character(0)
      numbered <- paste(sapply(seq_along(lines), function(i)
        paste0('<span class="ln">',i,'</span>',
               htmltools::htmlEscape(lines[i]))
      ), collapse="\n")
      div(class="prev-body",
        div(class="prev-code", HTML(numbered))
      )
    },

    # SVG inline render
    svg = div(class="prev-body",
      div(style="padding:1rem; text-align:center; overflow:auto; max-height:75vh;",
        HTML(prev$src))
    ),

    # Raster image
    image = div(class="prev-body",
      div(style="text-align:center; padding:0.5rem; overflow:auto; max-height:75vh;",
        if (!is.null(prev$src))
          tags$img(class="prev-img", src=prev$src)
        else
          div(style="color:#AAA; padding:2rem;", "Image could not be loaded")
      )
    ),

    # DICOM image + tag summary
    dicom_image = div(class="prev-body",
      div(style="display:flex; gap:0; height:100%; min-height:0;",
        # Image panel
        div(style="flex:1; text-align:center; background:black; display:flex;
                   align-items:center; justify-content:center; overflow:hidden;",
          if (!is.null(prev$src))
            tags$img(src=prev$src,
              style="max-width:100%; max-height:70vh; display:block; margin:auto;")
          else
            div(style="color:#888; padding:2rem; font-size:0.82rem;",
              "Pixel data could not be rendered")
        ),
        # Tag summary panel
        div(style=paste0(
          "width:220px; flex-shrink:0; background:#1A1A2E; color:#A0C4E8; ",
          "font-family:monospace; font-size:0.72rem; line-height:1.7; ",
          "padding:0.8rem; overflow-y:auto; border-left:1px solid #333;"),
          tags$div(style="font-weight:700; color:#4FC3F7; margin-bottom:0.5rem;
                         font-size:0.75rem; letter-spacing:0.06em;",
            "TAG SUMMARY"),
          tagList(lapply(prev$tag_lines %||% character(0), function(l) {
            parts <- strsplit(l, ":", fixed=TRUE)[[1]]
            tagList(
              tags$span(style="color:#607D8B;", trimws(parts[1])), " : ",
              tags$span(style="color:#E0E0E0;",
                trimws(paste(parts[-1], collapse=":"))),
              tags$br()
            )
          }))
        )
      )
    ),

    # Error from build_preview — show actual error message
    error = div(class="prev-body",
      div(style="padding:1rem; color:#C62828; font-size:0.82rem;",
        tags$strong("\u26a0 Preview error: "),
        tags$span(prev$msg %||% "Unknown error in build_preview"))),

    # Unsupported
    div(class="prev-body",
      div(class="ph", div(class="ph-icon","\u26A0\uFE0F"),
        tags$h6("Preview not available"),
        tags$p(style="font-size:0.8rem; color:#AAA;", prev$meta %||% ""))
    )
  )

  tagList(toolbar, body)
}

