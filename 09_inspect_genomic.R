# Genomic file inspector (VCF, PLINK, SAM)
# Auto-split from app.R — do not edit the monolithic file

inspect_genomic <- function(filepath, cfg=list()) {
  sens_ph  <- cfg$sensitive_phenotypes %||% sensitive_phenotypes
  g_cols   <- cfg$gwas_cols            %||% gwas_cols
  gwas_min <- cfg$gen003_min_cols      %||% 4L
  maf_thr  <- cfg$gen004_maf_threshold %||% 0.01

  hits   <- list()
  ext_lc <- tolower(tools::file_ext(filepath))
  name   <- tolower(basename(filepath))

  # ── PLINK .bed — binary genotype matrix (magic byte check) ────────────────
  if (ext_lc == "bed") {
    magic <- tryCatch(readBin(filepath, "raw", n=3L), error=function(e) raw(0))
    if (length(magic) == 3L &&
        magic[1] == as.raw(0x6c) &&
        magic[2] == as.raw(0x1b) &&
        magic[3] == as.raw(0x01)) {
      return(list(list(rule="GEN-010", outcome="RED",
        detail=paste0("PLINK BED magic bytes confirmed (0x6c 0x1b 0x01). ",
          "Contains individual-level genotype matrix for all variants in the paired .bim file. ",
          "Cannot be egressed without specialist review and explicit approval."),
        evidence=list(type="lines",
          lines=list(
            list(lineno=1, text=paste0("Magic bytes (hex): ",
              paste(toupper(format(magic, scientific=FALSE)), collapse=" ")), flag=TRUE),
            list(lineno=2, text="Signature: PLINK1 BED binary genotype matrix", flag=FALSE),
            list(lineno=3, text=paste0("File size: ", round(file.info(filepath)$size/1024/1024, 2), " MB"), flag=FALSE)
          ),
          caption="Binary file identification — PLINK BED magic byte confirmation"))))
    } else {
      return(list(list(rule="GEN-010", outcome="AMBER",
        detail=paste0("File has .bed extension but PLINK BED magic bytes not confirmed. ",
          "May be a UCSC BED coordinate file (lower risk) or a corrupt PLINK BED. ",
          "Verify file type before approving."),
        evidence=list(type="lines",
          lines=list(
            list(lineno=1, text=paste0("Observed bytes (hex): ",
              if (length(magic)>0) paste(toupper(format(magic, scientific=FALSE)), collapse=" ") else "(unreadable)"),
              flag=TRUE),
            list(lineno=2, text="Expected PLINK BED: 6C 1B 01", flag=FALSE)
          ),
          caption="Magic byte comparison — PLINK BED expected vs observed"))))
    }
  }

  # ── PLINK2 .pgen — binary genotype file (magic byte check) ───────────────
  if (ext_lc == "pgen") {
    magic <- tryCatch(readBin(filepath, "raw", n=2L), error=function(e) raw(0))
    if (length(magic) == 2L &&
        magic[1] == as.raw(0x6c) &&
        magic[2] == as.raw(0x1b)) {
      return(list(list(rule="GEN-011", outcome="RED",
        detail="PLINK2 PGEN magic bytes confirmed (0x6c 0x1b). Individual-level binary genotype file.",
        evidence=list(type="lines",
          lines=list(
            list(lineno=1, text=paste0("Magic bytes (hex): ",
              paste(toupper(format(magic, scientific=FALSE)), collapse=" ")), flag=TRUE),
            list(lineno=2, text="Signature: PLINK2 PGEN binary genotype matrix", flag=FALSE),
            list(lineno=3, text=paste0("File size: ", round(file.info(filepath)$size/1024/1024, 2), " MB"), flag=FALSE)
          ),
          caption="Binary file identification — PLINK2 PGEN magic byte confirmation"))))
    }
    return(list(list(rule="GEN-011", outcome="RED",
      detail="File has .pgen extension — assumed to be PLINK2 binary genotype data. Cannot be egressed.")))
  }

  # ── PLINK2 .psam — individual phenotype / covariate file ─────────────────
  if (ext_lc == "psam") {
    tryCatch({
      lines <- readLines(filepath, n=50L, warn=FALSE)
      hdr_line <- lines[grepl("^#IID|^#FID|^IID|^FID", lines, ignore.case=TRUE)][1]
      n_extra <- 0L
      extra_cols <- character(0)
      if (!is.na(hdr_line)) {
        cols <- strsplit(trimws(hdr_line), "\t")[[1]]
        standard <- c("#FID","FID","#IID","IID","PAT","MAT","SEX","PHENO1","PHENOTYPE")
        extra_cols <- cols[!toupper(cols) %in% toupper(standard)]
        n_extra <- length(extra_cols)
      }
      n_indiv <- sum(!grepl("^#", lines))
      return(list(list(rule="GEN-012", outcome="RED",
        detail=paste0("PLINK2 .psam file with approximately ", n_indiv,
          " individual(s) in the header sample. ",
          if (n_extra > 0)
            paste0(n_extra, " additional covariate column(s) beyond standard fields: ",
              paste(head(extra_cols, 4), collapse=", "),
              if (n_extra > 4) paste0(" (+", n_extra-4, " more)") else ".")
          else "Standard fields only (FID/IID/SEX/PHENO1)."),
        evidence=list(type="lines",
          lines=lapply(seq_len(min(6L, length(lines))), function(i)
            list(lineno=i, text=lines[[i]],
                 flag=grepl("^#IID|^#FID|^IID|^FID", lines[[i]], ignore.case=TRUE))),
          creates="First 6 lines of .psam file — header row flagged"))))
    }, error=function(e)
      list(list(rule="GEN-012", outcome="RED",
        detail="PLINK2 .psam file — individual-level phenotype data. Could not parse header.")))
  }

  # ── PLINK .fam — pedigree and per-individual phenotype ────────────────────
  if (ext_lc == "fam") {
    tryCatch({
      lines <- readLines(filepath, n=200L, warn=FALSE)
      lines <- lines[nzchar(trimws(lines)) & !grepl("^#", lines)]
      n_indiv <- length(lines)
      has_ped  <- FALSE   # any non-zero paternal/maternal IDs
      has_pheno <- FALSE  # any non-missing phenotype
      sex_counts <- c(`1`=0L, `2`=0L, `0`=0L)
      pheno_vals <- character(0)

      for (ln in head(lines, 100L)) {
        parts <- strsplit(trimws(ln), "\\s+")[[1]]
        if (length(parts) < 6L) next
        if (parts[3] != "0" || parts[4] != "0") has_ped <- TRUE
        if (!parts[6] %in% c("0","-9","NA")) {
          has_pheno <- TRUE
          pheno_vals <- union(pheno_vals, parts[6])
        }
        sx <- parts[5]
        if (sx %in% names(sex_counts)) sex_counts[sx] <- sex_counts[sx] + 1L
      }

      detail <- paste0(
        "PLINK .fam file: ", n_indiv, " individual(s). ",
        if (has_ped) "Pedigree structure present (non-zero paternal/maternal IDs). " else "",
        if (has_pheno) paste0("Phenotype values: ", paste(head(pheno_vals,5), collapse=", "),
          if (length(pheno_vals)>5) paste0(" (+",length(pheno_vals)-5," more)") else "",". ")
        else "No phenotype data (all 0/-9). ",
        sprintf("Sex distribution: %d male / %d female / %d unknown.",
          sex_counts["1"], sex_counts["2"], sex_counts["0"])
      )
      return(list(list(rule="GEN-008", outcome="RED", detail=detail,
        evidence=list(type="lines",
          lines=lapply(seq_len(min(6L, length(lines))), function(i)
            list(lineno=i, text=lines[i], flag=TRUE)),
          caption=paste0("First ", min(6L, length(lines)), " rows of .fam file ",
            "(FID IID PAT MAT SEX PHENO)"))
      )))
    }, error=function(e)
      list(list(rule="GEN-008", outcome="RED",
        detail="PLINK .fam file — individual-level pedigree/phenotype data. Could not parse.")))
  }

  # ── PLINK .bim — variant map ───────────────────────────────────────────────
  if (ext_lc == "bim") {
    tryCatch({
      lines <- readLines(filepath, n=500L, warn=FALSE)
      lines <- lines[nzchar(trimws(lines)) & !grepl("^#", lines)]
      n_var <- length(lines)
      # Check for rsIDs and any unusual chromosome annotations
      rs_count <- sum(grepl("^rs[0-9]+$", sapply(lines, function(l)
        strsplit(trimws(l),"\\s+")[[1]][2]), ignore.case=TRUE))
      chrs <- unique(sapply(lines, function(l) strsplit(trimws(l),"\\s+")[[1]][1]))
      chrs <- head(chrs, 10)
      return(list(list(rule="GEN-009", outcome="AMBER",
        detail=paste0("PLINK .bim variant map: ", n_var, " variant(s) in sample. ",
          rs_count, " rsID(s) detected. Chromosomes: ", paste(chrs, collapse=", "), ".",
          " Review whether the variant panel is considered sensitive under your DAA."),
        evidence=list(type="lines",
          lines=lapply(seq_len(min(5L, length(lines))), function(i)
            list(lineno=i, text=lines[i], flag=FALSE)),
          caption="First 5 rows of .bim file (CHR SNP_ID cM BP ALT REF)")
      )))
    }, error=function(e)
      list(list(rule="GEN-009", outcome="AMBER",
        detail="PLINK .bim variant map — could not parse file.")))
  }

  # ── SAM alignment file ────────────────────────────────────────────────────
  if (ext_lc == "sam") {
    tryCatch({
      lines  <- readLines(filepath, n=60L, warn=FALSE)
      hd_lines  <- lines[grepl("^@HD", lines)]
      sq_lines  <- lines[grepl("^@SQ", lines)]
      rg_lines  <- lines[grepl("^@RG", lines)]
      # Extract sample IDs from @RG SM: tags
      sm_tags <- regmatches(rg_lines, regexpr("SM:[^\\t]+", rg_lines))
      sm_vals <- gsub("^SM:", "", sm_tags)
      detail <- paste0("SAM alignment file. ",
        length(sq_lines), " reference sequence(s). ",
        length(rg_lines), " read group(s).",
        if (length(sm_vals) > 0)
          paste0(" Sample IDs in @RG SM tags: ", paste(head(sm_vals,3), collapse=", "),
            if (length(sm_vals) > 3) paste0(" (+", length(sm_vals)-3, " more)") else ".")
        else ""
      )
      ev_lines <- lapply(seq_along(rg_lines), function(i)
        list(lineno=i, text=rg_lines[i], flag=TRUE))
      return(list(list(rule="GEN-006", outcome="RED",
        detail=detail,
        evidence=if (length(ev_lines)>0)
          list(type="lines", lines=ev_lines,
               caption="@RG (read group) header lines — may contain individual sample IDs")
        else NULL
      )))
    }, error=function(e)
      list(list(rule="GEN-006", outcome="RED",
        detail=paste0("SAM alignment file — individual-level sequencing reads. Parse error: ",
          conditionMessage(e)))))
  }

  # ── VCF / BCF / text genomic ──────────────────────────────────────────────
  tryCatch({
    lines  <- readLines(filepath, n=200L, warn=FALSE)
    lc     <- tolower(paste(lines, collapse=" "))
    meta   <- lines[grepl("^##", lines)]
    noncom <- lines[!grepl("^##", lines)]

    # GEN-006: Pedigree or individual sample metadata in VCF header
    ped_lines <- meta[grepl("^##PEDIGREE|^##SAMPLE", meta, ignore.case=TRUE)]
    if (length(ped_lines) > 0) {
      hits <- append(hits, list(list(rule="GEN-006", outcome="RED",
        detail=paste0(length(ped_lines),
          " ##PEDIGREE/##SAMPLE meta-information line(s) found. ",
          "These encode family relationships and/or individual identifiers."),
        evidence=list(type="lines",
          lines=lapply(seq_along(ped_lines), function(i)
            list(lineno=i, text=ped_lines[i], flag=TRUE)),
          caption="##PEDIGREE / ##SAMPLE meta-information lines")
      )))
    }

    # GEN-001: VCF per-sample genotypes
    hdr <- noncom[grepl("^#CHROM", noncom)]
    if (length(hdr) > 0) {
      vcf_cols  <- strsplit(hdr[1], "\t")[[1]]
      fixed_vcf <- c("#CHROM","POS","ID","REF","ALT","QUAL","FILTER","INFO","FORMAT")
      samp_cols <- vcf_cols[!vcf_cols %in% fixed_vcf]
      data_rows <- noncom[!grepl("^#", noncom)]
      has_gt <- length(data_rows) > 0 &&
        length(strsplit(data_rows[1],"\t")[[1]]) >= 9 &&
        grepl("GT", strsplit(data_rows[1],"\t")[[1]][9])

      if (length(samp_cols) > 0 && has_gt) {
        show_lines <- c(hdr, head(data_rows, 6))
        ev_df <- tryCatch({
          rows <- lapply(show_lines, function(l) strsplit(l,"\t")[[1]])
          mx   <- max(sapply(rows, length))
          rows_pad <- lapply(rows, function(r) c(r, rep("", mx-length(r))))
          df <- as.data.frame(do.call(rbind, rows_pad[-1]), stringsAsFactors=FALSE)
          names(df) <- rows[[1]]
          df
        }, error=function(e) NULL)
        hits <- append(hits, list(list(rule="GEN-001", outcome="RED",
          detail=paste0(length(samp_cols), " per-sample genotype column(s): ",
            paste(head(samp_cols,3), collapse=", "),
            if (length(samp_cols)>3) paste0(" (+",length(samp_cols)-3," more)") else ""),
          evidence=if (!is.null(ev_df))
            list(type="table", data=head(ev_df,6), flag_cols=samp_cols,
                 caption=paste0("VCF with ",length(samp_cols)," per-sample column(s) — first 6 rows"))
          else NULL
        )))

        # GEN-007: DP/GQ in FORMAT of per-sample VCF
        fmt_fields <- if (length(data_rows)>0)
          strsplit(strsplit(data_rows[1],"\t")[[1]][9], ":")[[1]]
        else character(0)
        if (any(c("DP","GQ") %in% fmt_fields))
          gen007_lines <- c(
            paste0("FORMAT fields: ", paste(fmt_fields, collapse=":")),
            if (length(data_rows) > 0) head(data_rows, 3L) else character(0)
          )
          hits <- append(hits, list(list(rule="GEN-007", outcome="AMBER",
            detail=paste0("FORMAT field contains: ",
              paste(intersect(c("DP","GQ"), fmt_fields), collapse=", "),
              " — per-sample depth/quality annotations present alongside genotype calls."),
            evidence=list(type="lines",
              lines=lapply(seq_along(gen007_lines), function(i)
                list(lineno=i, text=gen007_lines[[i]],
                     flag=i==1L)),  # highlight FORMAT line
              caption="FORMAT field and first data row(s)"))))

      } else {
        # No per-sample columns — summary VCF
        hits <- append(hits, list(list(rule="GEN-003", outcome="GREEN",
          detail="VCF with no per-sample columns — summary variant list")))
      }

      # GEN-004: Rare variants (only inspect if no per-sample columns — summary VCF)
      if (length(samp_cols) == 0 && length(data_rows) > 0) {
        rare_count <- 0L
        rare_ex    <- character(0)
        for (dr in head(data_rows, 100L)) {
          parts <- strsplit(dr, "\t")[[1]]
          if (length(parts) < 8) next
          info <- parts[8]
          # Extract AF= or MAF= from INFO
          af_m <- regmatches(info, regexpr("(?i)\\b(?:AF|MAF)=([0-9.eE+\\-]+)", info, perl=TRUE))
          if (length(af_m) == 0) next
          af_v <- suppressWarnings(as.numeric(sub("(?i)\\b(?:AF|MAF)=","", af_m)))
          if (!is.na(af_v) && af_v > 0 && af_v < maf_thr) {
            rare_count <- rare_count + 1L
            if (length(rare_ex) < 3)
              rare_ex <- c(rare_ex, paste0(parts[1],":",parts[2]," AF=",af_v))
          }
        }
        if (rare_count > 0) {
          rare_rows <- Filter(function(dr) {
            parts <- strsplit(dr, "\t")[[1]]
            if (length(parts) < 8) return(FALSE)
            af_m <- regmatches(parts[8], regexpr("(?i)\\b(?:AF|MAF)=([0-9.eE+\\-]+)", parts[8], perl=TRUE))
            if (!length(af_m)) return(FALSE)
            af_v <- suppressWarnings(as.numeric(sub("(?i)\\b(?:AF|MAF)=","", af_m)))
            !is.na(af_v) && af_v > 0 && af_v < maf_thr
          }, head(data_rows, 100L))
          ev_004 <- list(type="lines",
            lines=lapply(seq_along(head(rare_rows, 8L)), function(i)
              list(lineno=i, text=head(rare_rows, 8L)[[i]], flag=TRUE)),
            caption=paste0(rare_count, " rare variant row(s) with MAF < ", maf_thr,
              " (first 8 shown)"))
          hits <- append(hits, list(list(rule="GEN-004", outcome="RED",
            detail=paste0(rare_count, " rare variant(s) (MAF < ", maf_thr, ") in first 100 rows. ",
              "Examples: ", paste(rare_ex, collapse="; ")),
            evidence=ev_004)))
        }
      }

      # GEN-005: Sample size embedded in INFO
      if (length(data_rows) > 0) {
        ns_found <- FALSE; an_found <- FALSE; implied_n <- NA_integer_
        for (dr in head(data_rows, 20L)) {
          info <- strsplit(dr, "\t")[[1]][8]
          if (is.na(info)) next
          ns_m <- regmatches(info, regexpr("(?i)\\bNS=([0-9]+)", info, perl=TRUE))
          an_m <- regmatches(info, regexpr("(?i)\\bAN=([0-9]+)", info, perl=TRUE))
          if (length(ns_m)>0) {
            ns_found  <- TRUE
            implied_n <- as.integer(sub("(?i)\\bNS=","", ns_m, perl=TRUE))
            break
          }
          if (length(an_m)>0) {
            an_found  <- TRUE
            implied_n <- as.integer(sub("(?i)\\bAN=","", an_m, perl=TRUE)) %/% 2L
            break
          }
        }
        if (ns_found || an_found) {
          # Find the actual row that contained NS= or AN=
          tag <- if (ns_found) "NS=" else "AN="
          ns_rows <- Filter(function(dr) grepl(tag, dr, fixed=TRUE), head(data_rows, 20L))
          ev_005 <- list(type="lines",
            lines=lapply(seq_along(head(ns_rows, 4L)), function(i)
              list(lineno=i, text=head(ns_rows, 4L)[[i]], flag=TRUE)),
            caption=paste0(tag, " annotation in INFO field (implied N = ",
              if (!is.na(implied_n)) implied_n else "unknown", ")"))
          hits <- append(hits, list(list(rule="GEN-005", outcome="AMBER",
            detail=paste0(
              if (ns_found) "NS= (number of samples) " else "AN= (allele number) ",
              "annotation found in INFO field. Implied N = ",
              if (!is.na(implied_n)) implied_n else "unknown",
              ". Verify against approved cohort size."),
            evidence=ev_005)))
        }
      }
    }

    # GEN-003: GWAS summary statistics (non-VCF text files)
    if (length(hdr) == 0) {
      n_gcols <- sum(sapply(g_cols, function(gc)
        grepl(paste0("\\b",gc,"\\b"), lc)))
      if (n_gcols >= gwas_min)
        hits <- append(hits, list(list(rule="GEN-003", outcome="GREEN",
          detail=paste0("GWAS summary statistics format detected (",
            n_gcols, " standard column names matched)"))))
    }

    # GEN-002: Sensitive phenotype in genomic file
    for (sp in sens_ph) {
      match_lines <- which(grepl(sp, tolower(lines), fixed=TRUE))
      if (length(match_lines) > 0) {
        hits <- append(hits, list(list(rule="GEN-002", outcome="RED",
          detail=paste0("Sensitive phenotype '", sp, "' in genomic file"),
          evidence=list(type="lines",
            lines=lapply(head(match_lines,5), function(ln)
              list(lineno=ln, text=lines[ln], flag=TRUE)),
            caption=paste0("Lines referencing '", sp, "'"))
        )))
        break
      }
    }

  }, error=function(e) {
    hits <<- append(hits, list(list(rule="PARSE", outcome="UNCERTAIN",
      detail=paste0("Genomic parse error: ", conditionMessage(e)))))
  })
  hits
}
