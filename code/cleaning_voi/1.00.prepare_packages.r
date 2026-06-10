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

# Track-error cutoffs (km) for robustness checks
cutoffs <- c(400, 500, 600, 700, 800)
reference_cutoff <- 400
