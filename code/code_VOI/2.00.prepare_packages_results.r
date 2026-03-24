#=============================
# Package Setup for Results Generation (2.0.MAKE_RESULTS)
#=============================

# This script sets up packages and environment for results generation scripts
# Run this before executing any 2.x scripts

#=============================
# 1. Environment Setup
#=============================

# Suppress verbose output
suppressPackageStartupMessages(library(tidyverse))
options(dplyr.summarise.inform = FALSE)
options(conleyreg.verbose = FALSE)
options(conleyreg.ncores = 2)
options(verbose = FALSE)
options("tidylog.display" = NULL)

#=============================
# 2. Load Required Packages
#=============================

library(stormwindmodel)
library(weathermetrics)
library(hurricaneexposuredata)
library(urbnmapr)
library(kableExtra)
library(data.table)
library(tidyverse)
library(tidylog)
library(fixest)
library(broom)
library(scales)
library(moments)
library(lubridate)
library(covidcast)
library(here)
library(cowplot)
library(patchwork)
library(conleyreg)
library(janitor)
library(modelsummary)
library(sf)
library(maps)
library(naniar)
library(ggrepel)
library(housingData)
library(tidycensus)
library(scoringRules)
library(ggpattern)

# Load custom functions for results
source("code/functions_results.r")

# Useful shortcut operator
"%ni%" = Negate("%in%")

#=============================
# 3. Analysis Parameters and Settings
#=============================

# Conley standard error cutoff distance
dist = 400

# Track distance cutoffs for robustness
cutoffs = seq(400, 800, 100)

# State FIPS codes for coastal states robustness check
coastal_states = c(48, 22, 28, 01, 13, 12, 45, 37, 51, 24, 34, 10, 42, 36, 09, 44, 33, 23, 25)

# Wind intensity bins for interactions
wind_obs_bins = c(18, 33, 43, 50, 58, 70)

# Plot styling: colors and shapes for fixed effects specifications
fe_colors <- c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33','#a65628','#f781bf')
fe_shapes <- c(16, 17, 15, 18, 8, 12, 10, 14)

#=============================
# 4. Load Core Datasets
#=============================

df = load_df()
df_track = load_df_track()
his_error = fread("data/raw/noaa/ATL_TI_errors_new.csv")
track_forecast = readRDS("data/clean/track_forecast.rds")
data_weather_map = readRDS("data/clean/weather_error_sf.rds")
