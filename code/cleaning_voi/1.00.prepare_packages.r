# ==============================================================================
# Load packages and set global options for the VOI cleaning scripts.
# Run:     sourced by code/cleaning_voi/run_all.r
# ==============================================================================

# ==============================================================================
# 1. Environment setup
# ==============================================================================

suppressPackageStartupMessages(library(tidyverse))
options(dplyr.summarise.inform = FALSE)
options(conleyreg.verbose = FALSE)
options(verbose = FALSE)

# ==============================================================================
# 2. Load required packages
# ==============================================================================

library(stormwindmodel)
library(weathermetrics)
library(hurricaneexposuredata)
library(urbnmapr)
library(data.table)
library(tidyverse)
library(tigris)
library(maps)
library(vroom)
library(janitor)
library(sf)
library(foreign)
library(naniar)
library(readxl)
library(lubridate)
library(here)
library(parallel)
library(tidycensus)
library(housingData)

source("code/cleaning_voi/functions.r")

# ==============================================================================
# 3. Global settings and variables
# ==============================================================================

sf_use_s2(FALSE)

"%notin%" = Negate("%in%")

# Puerto Rico build: AO_INCLUDE_PR=1 adds the Puerto Rico landfalls to the panel and
# writes to data/clean_pr so the continental data/clean is never overwritten.
# AO_CLEAN_DIR overrides the output directory (e.g. to check a continental rebuild).
include_pr <- Sys.getenv("AO_INCLUDE_PR", "0") == "1"
pr_storms <- if (include_pr) c("maria", "fiona") else character(0)
clean_dir <- Sys.getenv("AO_CLEAN_DIR", if (include_pr) "data/clean_pr" else "data/clean")
dir.create(clean_dir, showWarnings = FALSE, recursive = TRUE)
# AO_PR_MOCK=1 also reads the NHC-file mock-ups in pr_mock/ for storms with no official file in pr/
pr_files <- function(dir) {
  f <- list.files(file.path(dir, "pr"), pattern = "csv$", full.names = TRUE)
  if (Sys.getenv("AO_PR_MOCK", "0") == "1") {
    m <- list.files(file.path(dir, "pr_mock"), pattern = "csv$", full.names = TRUE)
    f <- c(f, m[basename(m) %notin% basename(f)])
  }
  f
}

# Track-error cutoffs (km) for robustness checks
cutoffs <- c(400, 500, 600, 700, 800)
reference_cutoff <- 400
