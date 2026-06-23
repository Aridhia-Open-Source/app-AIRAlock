# ============================================================
# AIRAlock - Shiny App Container
# Aridhia Informatics
# ============================================================
# Exposes Shiny on port 8080 via shiny::runApp()
# No Shiny Server - simpler, smaller, TRE-friendly
# Runs as UID 1000:1000 (TRE standard)
# ============================================================

FROM rocker/r-ver:4.5.1

LABEL maintainer="Aridhia Informatics"
LABEL description="AIRAlock - AI-assisted disclosure risk assessment for DRE egress"
LABEL version="v1"

# ── System dependencies ──────────────────────────────────────
# Grouped by purpose. All installed in a single RUN to minimise layers.
RUN apt-get update && apt-get install -y --no-install-recommends \
    # ── tesseract OCR (for raster image inspection: PNG, JPG, TIFF, BMP) ──
    libtesseract-dev \
    libleptonica-dev \
    tesseract-ocr \
    tesseract-ocr-eng \
    # ── pdftools (PDF text extraction) ────────────────────────────────────
    libpoppler-cpp-dev \
    # ── readxl / xml2 / curl (core R package deps) ────────────────────────
    libxml2-dev \
    libssl-dev \
    libcurl4-openssl-dev \
    # ── haven (Stata/SAS/SPSS reading) ────────────────────────────────────
    # haven is pure R but links zlib at compile time
    zlib1g-dev \
    # ── oro.dicom / oro.nifti (DICOM and NIfTI medical imaging) ───────────
    # Pure R packages - no additional system deps beyond base build tools
    # ── arrow (Parquet / Feather / Arrow IPC) ─────────────────────────────
    # Downloads pre-built binaries via pak - no system deps required
    # ── General utilities ─────────────────────────────────────────────────
    curl \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# ── R packages ───────────────────────────────────────────────
# Installed in two steps:
#   Step 1: pak bootstrapper (pure R, no system deps)
#   Step 2: all app packages via pak (handles dependency resolution)
#
# Package notes:
#   tesseract   - OCR for PNG/JPG/TIFF/BMP; requires libtesseract-dev above
#   pdftools    - PDF text extraction; requires libpoppler-cpp-dev above
#   oro.dicom   - DICOM tag inspection; pure R
#   oro.nifti   - NIfTI header/pixel reading; pure R, pulled as oro.dicom dep
#   haven       - Stata/SAS/SPSS reading; pure R
#   ellmer      - AIRA LLM client (OpenAI-compatible)
#   future      - Async execution for non-blocking AIRA calls
#   promises    - Promise-based reactive integration for AIRA
#   R.utils     - Per-call timeout enforcement for AIRA
RUN Rscript -e "\
    install.packages('pak', repos='https://r-lib.github.io/p/pak/stable/'); \
    pak::pkg_install(c( \
        'shiny', \
        'bslib', \
        'DT', \
        'dplyr', \
        'stringr', \
        'readr', \
        'readxl', \
        'base64enc', \
        'htmltools', \
        'tesseract', \
        'pdftools', \
        'arrow', \
        'oro.dicom', \
        'oro.nifti', \
        'haven', \
        'ellmer', \
        'future', \
        'promises', \
        'R.utils' \
    ), ask=FALSE) \
"

# ── App user (TRE standard: UID/GID 1000) ───────────────────
RUN groupadd -g 1000 shiny && useradd -m -u 1000 -g 1000 shiny

# ── Directory structure ───────────────────────────────────────
# /home/workspace/files              - workspace root (mount point for DRE volume)
# /home/workspace/files/airlockcheck - audit log, config, report output (needs write)
# /home/workspace/files/AIRAlock     - AIRA-specific logs and state
# /app                               - app source files (read-only at runtime)
#
# The app browses /home/workspace/files by default and writes audit/config/reports
# to /home/workspace/files/airlockcheck. AIRA debug logs go to
# /home/workspace/files/AIRAlock/debuglogs. Both paths must be writable by UID 1000.
# In the DRE, /home/workspace/files is typically a CIFS-backed workspace mount.
RUN mkdir -p \
        /home/workspace/files/airlockcheck \
        /home/workspace/files/AIRAlock/debuglogs \
        /app \
    && chown -R 1000:1000 \
        /home/workspace \
        /app

# ── Copy app source files ─────────────────────────────────────
# app.R              - main application entry point
# 01-28_*.R          - module source files (constants, rules, inspectors,
#                      engine, UI, server, AIRA integration)
# dependencies.R     - package installer script (for workspace Terminal use, not runtime)
COPY --chown=1000:1000 *.R                 /app/

# ── Runtime ──────────────────────────────────────────────────
USER 1000:1000
WORKDIR /app
EXPOSE 8080

CMD ["Rscript", "-e", "shiny::runApp('/app', host='0.0.0.0', port=8080, launch.browser=FALSE)"]
