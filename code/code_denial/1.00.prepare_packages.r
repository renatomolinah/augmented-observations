#=============================
# 1. install packages, etc
#=============================

# Create a new line in the .Renviron file and write the API key to it
# write(sprintf("\nCENSUS_API_KEY=%s", census_api_key), file = "~/.Renviron", append = TRUE)
# write(sprintf("\nGITHUB_PAT=%s", github_pat), file = "~/.Renviron", append = TRUE)
write(sprintf("\nRENV_CONFIG_SANDBOX_ENABLED = FALSE"), file = "~/.Renviron", append = TRUE)
    
# Reload the .Renviron file to update the environment variables
readRenviron("~/.Renviron")

# initialize renv for packages
# install.packages("renv")
library(renv)

# these two lines initialize and save the renv lockfile
# renv::init()
# renv::activate()
# renv::snapshot()

# this restores it on your machine
#renv::restore()

# load all packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load_gh("UrbanInstitute/urbnmapr")
pacman::p_load(
  sf, tidyverse, data.table, maps, housingData, naniar
)

# fix for sf
sf_use_s2(FALSE)

# shortcut
"%notin%" = Negate("%in%")