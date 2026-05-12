#=============================
# Package Reproducibility Setup
#=============================

# This script ensures reproducible package versions for the replication package
# Run this before executing any other scripts

#=============================
# 0. Environment setup
#=============================

rprofile_lines <- c(
  "options(download.file.method = \"libcurl\")",
  "options(renv.bootstrap.quiet = TRUE)",
  "options(renv.verbose = FALSE)",
  "Sys.setenv(DOWNLOAD_STATIC_LIBV8 = 1)",
  "Sys.setenv(DOWNLOAD_STATIC_LIBV8 = 1)"
)

# Read any existing content (silently ignore if the file doesn't exist yet)
rprofile_lines_existing <- if (file.exists(".Rprofile")) readLines(".Rprofile", warn = FALSE) else character()

# Add only the lines that aren't already present
new <- setdiff(rprofile_lines, rprofile_lines_existing)

if (length(new)) {
  write(new, ".Rprofile", sep = "\n", append = TRUE)
} else {
}
# ---------------------------------------


#=============================
# 1. Initialize renv for reproducible package environment
#=============================


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

#=============================
# 2. API Keys Setup (if user wants to download data from source from census, GitHub pat may be required for GitHub package installation)
#=============================

# Example Census API key entry
# census_api_key = "YOUR_API_KEY_HERE"

# Example GitHub personal access token entry
# github_pat = "GITHUB_PAT_HERE"

# Example commands to write keys to .Renviron
# write(sprintf("\nCENSUS_API_KEY=%s", census_api_key), file = "~/.Renviron", append = TRUE)
# write(sprintf("\nGITHUB_PAT=%s", github_pat), file = "~/.Renviron", append = TRUE)
# write(sprintf("\nRENV_CONFIG_SANDBOX_ENABLED = FALSE"), file = "~/.Renviron", append = TRUE)
