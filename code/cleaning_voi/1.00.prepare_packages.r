#=============================
# Package Setup for Data Cleaning (1.0.CLEAN_DATA)
#=============================

# This script sets up packages and environment for data cleaning scripts
# Run this before executing any 1.x scripts

#=============================
# 1. Environment Setup
#=============================

# Suppress verbose output
suppressPackageStartupMessages(library(tidyverse))
options(dplyr.summarise.inform = FALSE)
options(conleyreg.verbose = FALSE)
options(verbose = FALSE)

#=============================
# 2. Load Required Packages  
#=============================

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

# Load custom functions
source("code/cleaning_voi/functions.r")

#=============================
# 3. Global Settings and Variables
#=============================

# Fix for sf package
sf_use_s2(FALSE)

# Useful shortcut operator
"%notin%" = Negate("%in%")

# Define cutoffs for robustness checks
cutoffs <- c(400, 500, 600, 700, 800)
reference_cutoff <- 400
