# ==============================================================================
# One-time reproducibility setup: write .Rprofile options, install vendored
# packages, and restore the renv environment. Run once before any other script.
# Run:     Rscript --vanilla code/cleaning_voi/0.0.PRELIMINARIES.r
# ==============================================================================

# ==============================================================================
# 0. Environment setup
# ==============================================================================

rprofile_lines <- c(
  "options(download.file.method = \"libcurl\")",
  "options(renv.bootstrap.quiet = TRUE)",
  "options(renv.verbose = FALSE)",
  "Sys.setenv(DOWNLOAD_STATIC_LIBV8 = 1)",
  "Sys.setenv(DOWNLOAD_STATIC_LIBV8 = 1)"
)

# Append only the .Rprofile lines not already present.
rprofile_lines_existing <- if (file.exists(".Rprofile")) readLines(".Rprofile", warn = FALSE) else character()
new <- setdiff(rprofile_lines, rprofile_lines_existing)

if (length(new)) {
  write(new, ".Rprofile", sep = "\n", append = TRUE)
} else {
}

# ==============================================================================
# 1. Initialize renv and install vendored packages
# ==============================================================================


install.packages("./local_packages/renv_1.1.4.tar.gz", repos = NULL, type = "source")
renv::settings$ignored.packages(c("arrow", "SafeGraphR", "starpolishr", "startR", "stormwindmodel", "hurricaneexposuredata", "urbnmapr", "weathermetrics"))
renv::restore()

# Install packages with error checking
cat("Installing weathermetrics...\n")
install.packages('./local_packages/weathermetrics_1.2.2.tar.gz', repos = NULL, type = 'source')
if (!requireNamespace("weathermetrics", quietly = TRUE)) {
  stop("Failed to install weathermetrics")
}

cat("Installing stormwindmodel...\n")
install.packages("./local_packages/stormwindmodel.tar.gz", repos = NULL, type = "source")
if (!requireNamespace("stormwindmodel", quietly = TRUE)) {
  stop("Failed to install stormwindmodel")
}

cat("Installing hurricaneexposuredata...\n")
renv::install('./local_packages/hurricaneexposuredata-master.zip')
if (!requireNamespace("hurricaneexposuredata", quietly = TRUE)) {
  stop("Failed to install hurricaneexposuredata")
}

cat("Installing urbnmapr...\n")
renv::install('./local_packages/urbnmapr-master.zip')
if (!requireNamespace("urbnmapr", quietly = TRUE)) {
  stop("Failed to install urbnmapr")
}

renv::snapshot()

# ==============================================================================
# 2. API keys (optional; only needed to re-download Census data or install
#    GitHub-hosted packages). Set in ~/.Renviron, e.g.:
#      CENSUS_API_KEY=...
#      GITHUB_PAT=...
#      RENV_CONFIG_SANDBOX_ENABLED=FALSE
# ==============================================================================
