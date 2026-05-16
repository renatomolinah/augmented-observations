################################################################################
# Stormwindmodel county-grid runner
# ------------------------------------------------------------------------------
# Imported upstream script for generating county-level wind swaths from
# pre-built stormwindmodel inputs. Preserved for provenance and lightly adapted
# to follow the repo's comment style.
################################################################################

library(stormwindmodel)    # Storm winds model
library(furrr)             # Functional programming with future resolution (parallel)
#library(tidyverse, lib.loc = "/usr/lib64/R/library")         # Data wrangling
library(tidyverse)         # Data wrangling
#library(lubridate, lib.loc = "/usr/lib64/R/library")         # Date-time wrangling
library(lubridate)         # Date-time wrangling
library(here)              # To manage filepaths

source(here("code", "external_models", "stormwindmodel", "run_model", "hurricane_processing_parallel.R"))

# Parse a positive integer environment override.
get_env_int <- function(var_name, default) {
  raw_val <- trimws(Sys.getenv(var_name, unset = ""))
  if (raw_val == "") {
    return(default)
  }

  parsed <- suppressWarnings(as.integer(raw_val))
  if (is.na(parsed) || parsed < 1L) {
    warning(sprintf("Ignoring invalid %s=%s; using %s.", var_name, raw_val, default))
    return(default)
  }

  parsed
}

get_env_flag <- function(var_name, default = 0L) {
  raw_val <- tolower(trimws(Sys.getenv(var_name, unset = "")))
  if (raw_val == "") {
    return(as.integer(default))
  }

  if (raw_val %in% c("1", "true", "yes", "y")) {
    return(1L)
  }

  if (raw_val %in% c("0", "false", "no", "n")) {
    return(0L)
  }

  warning(sprintf("Ignoring invalid %s=%s; using %s.", var_name, raw_val, default))
  as.integer(default)
}

load_storm_metadata <- function(storm_name) {
  manifest_path <- trimws(
    Sys.getenv(
      "AO_STORM_MANIFEST",
      unset = here("code", "external_models", "config", "storm_manifest.csv")
    )
  )
  if (!file.exists(manifest_path)) {
    stop(sprintf("Storm manifest not found: %s", manifest_path))
  }

  manifest <- readr::read_csv(manifest_path, show_col_types = FALSE, col_types = readr::cols(.default = "c"))
  storm_row <- manifest %>% filter(stormname == storm_name)

  if (nrow(storm_row) != 1) {
    stop(sprintf("Storm '%s' was not found in %s.", storm_name, manifest_path))
  }

  fcst <- unlist(storm_row[1, c("forecast_24h", "forecast_48h", "forecast_72h")], use.names = FALSE)
  fcst <- fcst[!is.na(fcst) & fcst != ""]

  list(
    stormid = storm_row$stormid[[1]],
    year = storm_row$year[[1]],
    landfall = storm_row$landfall[[1]],
    fcst = fcst
  )
}

  #=============================
  # 1. Configure the run
  #=============================
  stormname <- trimws(Sys.getenv("AO_STORMNAME", unset = "milton"))
  type <- trimws(Sys.getenv("AO_STORMWINDMODEL_TYPE", unset = "bdeck"))   #bdeck,adeck,mc
  model <- trimws(Sys.getenv("AO_STORMWINDMODEL_MODEL", unset = "OFCL"))   #OFCL,OCD5 (if bdeck, "BEST" will be used automatically)
  if (type == "bdeck") { model <- "BEST" }

  denial <- get_env_flag("AO_STORMWINDMODEL_DENIAL", 0L)
  exper <- trimws(Sys.getenv("AO_DENIAL_EXPERIMENT", unset = "NO"))    #ALL,NO
  if (denial == 1) { 
    model <- "HWRF"
    if (type == "bdeck") { 
      exper <- "BEST"
    }
  }

  storm_metadata <- load_storm_metadata(stormname)
  stormid <- storm_metadata$stormid
  year <- storm_metadata$year
  landfall <- storm_metadata$landfall
  fcst <- storm_metadata$fcst


  default_input_dir <- if (denial == 1) {
    here("data", "raw_denial", stormname, "wind", "inputs")
  } else {
    here("data", "raw", "stormwindmodel_inputs", stormname)
  }
  input_dir <- trimws(Sys.getenv("AO_STORMWINDMODEL_INPUT_DIR", unset = default_input_dir))
  if (input_dir == "") {
    input_dir <- default_input_dir
  }

  default_output_dir <- if (denial == 1) {
    here("data", "raw_denial", stormname, "wind")
  } else {
    here("data", "raw", "wind_error", "all_forecasts")
  }
  output_dir <- trimws(Sys.getenv("AO_STORMWINDMODEL_OUTPUT_DIR", unset = default_output_dir))
  if (output_dir == "") {
    output_dir <- default_output_dir
  }

  puertorico_county_points_file <- trimws(
    Sys.getenv(
      "AO_PUERTO_RICO_COUNTY_POINTS",
      unset = here("code", "external_models", "stormwindmodel", "run_model", "us_county_stats_puertorico.csv")
    )
  )

  if (!file.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    print(sprintf("Created output directory: %s", output_dir))
  }


  #=============================
  # 2. Run serial cases
  #=============================
  if (type != "mc") {
    # Select matching input files.
#print(paste(stormid,year,"*",type,"_",model,".csv",sep=""))
print(paste(stormid,year,"*",type,"_",exper,".csv",sep=""))
    infiles <- list.files(path=input_dir,pattern=paste(stormid,year,".*",type,"_",model,".csv",sep=""))
    if (denial == 1) { infiles <- list.files(path=input_dir,pattern=paste(stormid,year,".*",type,"_",exper,".csv",sep=""))}
    print(infiles)

    # Load built-in county_points
    data("county_points")
    if (stormname == "maria") {
      print("Using Puerto Rico county data")
      county_points <- read.csv(puertorico_county_points_file) %>%
        mutate(glandsea = TRUE)
    }    
 
    counter <- 1
    for (i in infiles) {
      # Read-in the selected file(s)
      print(i)
      if (denial == 0) {
        track <- read.csv(file.path(input_dir, i),                               # Point to the data within the repo data folder
                          header = F,                                             # The raw data does not have column names
                          col.names = c("date", "latitude", "longitude", "wind"))
      }
      if (denial == 1) {      
        track <- read.csv(file.path(input_dir, i),                               # Point to the data within the repo data folder
                          header = F,                                             # The raw data does not have column names
                          col.names = c("date", "latitude", "longitude", "wind"))
      }   

      # Run the model
      print("Running wind model")
      start_time <- Sys.time()
      wind_grid <- get_grid_winds(hurr_track = track, grid_df = county_points)
      end_time <- Sys.time()
      print(end_time - start_time)
      print("Complete")
  
      if (stormname == "maria") {
        wind_grid$glandsea <- NULL  #remove the landsea column so it won't be written to the CSV files
      } 

      # Save the county-grid output.
#if using bdeck input files that are already truncated, don't need to do this
      #if (type == "bdeck") { label <- track[1,1] }
      #if (type == "adeck") { label <- fcst[counter] }
      label <- fcst[counter]
      outfile <- paste(stormid,year,"_",label,"_countygrid_",type,"_",model,".csv",sep="")
      if (denial == 1) { outfile <- paste(stormid,year,"_",label,"_countygrid_",type,"_",exper,".csv",sep="") }
      #write.csv(wind_grid, here("outputs",stormname,outfile),row.names = F)
      write.csv(wind_grid, file.path(output_dir, outfile), row.names = F)
      print(outfile)
   
#if using bdeck input files that are already truncated, don't need to do this
#      ####################
#      #if using BDECK, a full-track swath was just created, but also make swaths for each forecast time
#      if (type == "bdeck") {
#        for (f in fcst) {
#          # Run the model
#          print("Running wind model")
#          start_time <- Sys.time()
#          #extract just the portion of the best-track times for this forecast window
#          track_index_begin <- which(track$date == f,arr.ind=TRUE)
#          track_time_begin <- as_datetime(toString(track[track_index_begin,1]),format="%Y%m%d%H%M")
#          track_time_end <- track_time_begin + hours(120)
#          track_index_end <- which(track$date == strftime(track_time_end,format="%Y%m%d%H%M",tz="GMT"))
#          #if there aren't 5 days remaining, go to the last entry in the Bdeck
#          if (length(track_index_end) == 0) { track_index_end=length(track$date) }
#          track_part <- track[track_index_begin:track_index_end,]
#          wind_grid <- get_grid_winds(hurr_track = track_part, grid_df = county_points)
#          end_time <- Sys.time()
#          print(end_time - start_time)
#          print("Complete")
#           
#          # Save the data
#          label <- f
#          type <- "bdeckpart"
#          outfile <- paste(stormid,year,"_",type,"_",label,"_countygrid.csv",sep="")
#          #write.csv(wind_grid, here("outputs",stormname,outfile),row.names = F)
#          write.csv(wind_grid, paste(output_dir,"/",outfile,sep=""),row.names = F)
#          print(outfile)
#        }
#      }
      ###################
      counter = counter+1
    }
    #stop()
    quit()
  }


  #=============================
  # 3. Run Monte Carlo cases
  #=============================
  if (type == "mc") {
  # Create one filepath per Monte Carlo track to process in parallel.

    # Collect candidate input files.
    file_paths <- list.files(path = input_dir, 
#                             pattern = paste(stormid,year,"_",type,"[0-9][0-9][0-9][0-9]_",fcst[3],"*",sep=""),
                             pattern = paste(stormid,year,"_",type,"[0-9][0-9][0-9][0-9]_*",sep=""),
                             full.names = T)
    if (denial  == 1) {
      file_paths <- list.files(path = input_dir, 
#                               pattern = paste(stormid,year,"_",type,"[0-9][0-9][0-9][0-9]_",fcst[3],"*",sep=""),
                               pattern = paste(stormid,year,"_",type,"[0-9][0-9][0-9][0-9]_*",sep=""),
                               #pattern = paste(stormid,year,"_",type,"0001_*",sep=""),
                               full.names = T)
    }
    print(file_paths)

    # Configure the future plan.
    n_cores <- get_env_int("AO_STORMWINDMODEL_CORES", 35L)
    #n_cores <- availableCores() - 2 # Alternative specification
    plan(multisession, workers = n_cores, gc = T)

    # Write outputs directly to disk.
    future_walk(file_paths, grid_winds_wrapper,.progress = T, .options=furrr_options(seed=NULL))

    # Combine outputs in memory instead of writing files.
    #all_wind_data <- future_map_dfr(file_paths, grid_winds_wrapper, return = T, .progress = T, .options=furrr_options(seed=NULL))

    # Explicitly close multisession workers by switching plans.
    plan(sequential)
  
    quit()
  }
