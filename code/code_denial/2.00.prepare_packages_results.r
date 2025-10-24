#=============================
# 0. load packages and data for results
#=============================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  kableExtra, data.table, tidyverse, tidylog, fixest, broom, scales, moments, lubridate, covidcast,
  patchwork, conleyreg, janitor, modelsummary, sf, maps, naniar, ggrepel, modelsummary, housingData, tidycensus,
  sysfonts, cowplot
)

pacman::p_load_gh("geanders/hurricaneexposuredata")
pacman::p_load_gh("UrbanInstitute/urbnmapr")

options("tidylog.display" = NULL)
source("code_denial/functions_plot.r")
source("code_denial/functions_table.r")
"%ni%" = Negate("%in%")

# conley default cutoff distance
dist = 400

# state fips for coastal states robustness check
coastal_states = c(48, 22, 28, 01, 13, 12, 45, 37, 51, 24, 34, 10, 42, 36, 09, 44, 33, 23, 25)

