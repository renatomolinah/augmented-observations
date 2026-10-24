# ==============================================================================
# One-time reproducibility setup: write .Rprofile options, install vendored
# packages, and restore the renv environment. Run once before any other script.
# Run:     Rscript --no-save --no-restore --no-environ --no-site-file code/cleaning_voi/0.0.PRELIMINARIES.r
#          (not --vanilla: the project .Rprofile must load so renv installs into the project library)
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


# renv itself is locked at 1.1.4 and bootstraps from renv/activate.R
renv::settings$ignored.packages(c("arrow", "SafeGraphR", "starpolishr", "startR", "stormwindmodel", "hurricaneexposuredata", "urbnmapr", "weathermetrics"))
renv::restore()

# Packages kept out of renv.lock, each pinned to an exact source:
# weathermetrics from CRAN, stormwindmodel from the staged tarball in Box,
# and the two GitHub-only packages at fixed commits.
vendored <- list(
  weathermetrics        = "weathermetrics@1.2.2",
  stormwindmodel        = "data/code/run_stormwindmodel/stormwindmodel_0.1.4.tar.gz",
  hurricaneexposuredata = "geanders/hurricaneexposuredata@b42fe54788ba8ade5e6aab614c75eea41d51a80c",
  urbnmapr              = "UrbanInstitute/urbnmapr@ef9f4488d6bc916a07746aafe5755e9fb9da19eb"
)
for (pkg in names(vendored)) {
  cat("Installing", pkg, "...\n")
  renv::install(vendored[[pkg]], prompt = FALSE)
  if (!requireNamespace(pkg, quietly = TRUE)) stop("Failed to install ", pkg)
}

renv::snapshot()

# ==============================================================================
# 2. API keys (optional; only needed to re-download Census data or install
#    GitHub-hosted packages). Set in ~/.Renviron, e.g.:
#      CENSUS_API_KEY=...
#      GITHUB_PAT=...
#      RENV_CONFIG_SANDBOX_ENABLED=FALSE
# ==============================================================================
