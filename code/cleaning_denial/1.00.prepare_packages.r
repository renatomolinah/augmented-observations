# ==============================================================================
# Load packages and set global options for the denial cleaning scripts.
# Run:     sourced by code/cleaning_denial/run_all.r
# ==============================================================================

# Disable renv sandbox, then reload the environment
write(sprintf("\nRENV_CONFIG_SANDBOX_ENABLED = FALSE"), file = "~/.Renviron", append = TRUE)
readRenviron("~/.Renviron")

library(renv)

if (!require("pacman")) install.packages("pacman")
pacman::p_load_gh("UrbanInstitute/urbnmapr")
pacman::p_load(
  sf, tidyverse, data.table, maps, housingData, naniar
)

sf_use_s2(FALSE)
"%notin%" = Negate("%in%")