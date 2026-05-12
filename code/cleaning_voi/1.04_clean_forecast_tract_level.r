cat("================================================================================\n")
cat("RUNNING: 1.04_clean_forecast_tract_level.r\n")
cat("PURPOSE: Clean tract-level population data and create tract_population_constant_2005.rds\n")
cat("================================================================================\n")

#=============================
# 1. Process tract-level population data (SEER)
#=============================
# Read and process the data using correct column widths from SEER dictionary
tract_pop <- vroom::vroom_fwf(
  file = "data/raw/seer/us.2006_2023.tract.level.pops.txt",
  col_positions = vroom::fwf_widths(
    c(4, 2, 2, 3, 6, 1, 1, 2, 9),  # Correct widths from SEER dictionary
    col_names = c("year", "state", "state_fips", "county_fips", "tract", 
                  "race", "sex", "age", "population")
  ),
  col_types = "icccciicd"  # integer, character x4, integer x3, double
) %>%
  dplyr::as_tibble() %>%
  filter(year == 2006) %>%  # Use 2006 data as proxy for 2005
  mutate(
    state_fips = str_pad(state_fips, width = 2, pad = "0"),
    county_fips = str_pad(county_fips, width = 3, pad = "0"),
    tract = str_pad(tract, width = 6, pad = "0"),
    tract_fips = paste0(state_fips, county_fips, tract),
    county_fips_full = paste0(state_fips, county_fips)
  ) %>%
  group_by(tract_fips, state, state_fips, county_fips, county_fips_full) %>%
  summarise(population = sum(population), .groups = "drop") %>%
  mutate(population = population / 10000)

# Create a sequence of years from 2005 to 2022
years <- 2005:2022

# Create a complete dataset with the same 2005 population for all years
tract_pop_complete <- tract_pop %>%
  crossing(year = years) %>%
  select(tract_fips, year, state, state_fips, county_fips, county_fips_full, population) %>%
  arrange(tract_fips, year)

# Save as RDS
saveRDS(tract_pop_complete, "data/clean/tract_population_constant_2005.rds") 

#=============================
# 2. Process tract grid data
#=============================

tracts_rds_path <- "data/raw/census/tracts.rds"
census_key <- trimws(Sys.getenv("CENSUS_API_KEY", unset = ""))
has_census_key <- nzchar(census_key)

census_override <- tolower(trimws(Sys.getenv("HF_CENSUS_OVERRIDE", unset = "")))
if (census_override == "") {
  census_override <- "auto"
}
if (!census_override %in% c("auto", "process", "skip")) {
  warning("Invalid HF_CENSUS_OVERRIDE value. Using 'auto'. Valid values: auto, process, skip.")
  census_override <- "auto"
}

# Precedence: explicit skip, then key-based processing, otherwise skip.
census_effective_mode <- if (census_override == "skip") {
  "skip"
} else if (has_census_key) {
  "process"
} else {
  "skip"
}

print(
  sprintf(
    "Census tract mode: HF_CENSUS_OVERRIDE=%s, CENSUS_API_KEY=%s, effective_mode=%s.",
    census_override,
    ifelse(has_census_key, "present", "missing"),
    census_effective_mode
  )
)

if (census_effective_mode == "process") {
  states <- tigris::states(cb = FALSE, year = 2010)$STATEFP
  states <- states[states != "72"]

  target_crs <- 4269
  n_cores_tract <- get_safe_mclapply_cores(target_gb_per_core = 2.5, hard_max_cores = 16L)

  if (.Platform$OS.type == "windows" || n_cores_tract == 1L) {
    tract_list <- lapply(
      states,
      function(st) {
        tigris::tracts(
          state = st,
          year = 2010,
          cb = FALSE,
          class = "sf"
        ) |>
          st_transform(target_crs)
      }
    )
  } else {
    tract_list <- parallel::mclapply(
      states,
      function(st) {
        tigris::tracts(
          state = st,
          year = 2010,
          cb = FALSE,
          class = "sf"
        ) |>
          st_transform(target_crs)
      },
      mc.cores = n_cores_tract
    )
  }

  tracts_2010 <- do.call(rbind, tract_list)
  saveRDS(tracts_2010, tracts_rds_path)
  print(sprintf("Rebuilt and saved Census tracts to %s", tracts_rds_path))
}

if (!file.exists(tracts_rds_path)) {
  stop(
    "Missing data/raw/census/tracts.rds. Either provide this file or rerun with CENSUS_API_KEY set and HF_CENSUS_OVERRIDE=process."
  )
}

tracts_east = readRDS(tracts_rds_path)

tracts_east = tracts_east |>
    st_transform(4326)

# Extract centroid for each census tract
tract_grid <- tracts_east %>%
  st_centroid() %>%
  mutate(
    gridid = GEOID10,
    county_fips = substr(GEOID10, 1, 5)  # First 5 digits of GEOID10 is county FIPS
  ) %>%
  select(gridid, county_fips, glat = INTPTLAT10, glon = INTPTLON10) %>%
  st_drop_geometry() %>%
  mutate(
    glat = as.numeric(glat), 
    glon = as.numeric(glon),
    glandsea = 1  # Assume all tracts are on land
  )

# Load and filter population data for 2005 only
tract_population <- readRDS("data/clean/tract_population_constant_2005.rds") %>%
  filter(year == 2005) %>%
  select(tract_fips, population)

# Save processed tract data
saveRDS(tract_grid, "data/clean/popw/tract_grid.rds")
saveRDS(tract_population, "data/clean/popw/tract_population_2005.rds")

#=============================
# 3. Calculate wind exposure by tract
#=============================

# Verify tract FIPS codes match
grid_tracts <- unique(tract_grid$gridid)
pop_tracts <- unique(tract_population$tract_fips)

# Base directory for storms
unzip("data/raw/stormwindmodel_inputs.zip", exdir = "data/raw/")
storms_dir <- "data/raw/stormwindmodel_inputs"

# Get list of storms
storm_names <- list.files(storms_dir)
print(paste("\nProcessing storms:", paste(storm_names, collapse = ", ")))

# Parse a positive integer from an environment variable.
get_env_int <- function(var_name) {
  raw_val <- trimws(Sys.getenv(var_name, unset = ""))
  if (raw_val == "") {
    return(NA_integer_)
  }
  parsed <- suppressWarnings(as.integer(raw_val))
  if (is.na(parsed) || parsed < 1L) {
    return(NA_integer_)
  }
  parsed
}

# Parallel settings can be tuned for memory-constrained environments.
# Use automatic memory-based core detection with manual override support
num_cores <- get_safe_mclapply_cores(target_gb_per_core = 2.5, hard_max_cores = 16L)

# Windows compatibility: mclapply doesn't work on Windows
if (.Platform$OS.type == "windows" && num_cores > 1L) {
  warning("Windows detected: forcing single-core execution for compatibility with mclapply.")
  num_cores <- 1L
}

max_files_per_batch <- get_env_int("HF_MAX_FILES_PER_BATCH")
if (is.na(max_files_per_batch)) {
  max_files_per_batch <- num_cores
}

print(
  sprintf(
    "Parallel settings: HF_N_CORES=%s, HF_MAX_FILES_PER_BATCH=%s, using up to %d worker(s).",
    ifelse(Sys.getenv("HF_N_CORES", unset = "") == "", "auto (memory-based)", Sys.getenv("HF_N_CORES")),
    ifelse(Sys.getenv("HF_MAX_FILES_PER_BATCH", unset = "") == "", "default", Sys.getenv("HF_MAX_FILES_PER_BATCH")),
    num_cores
  )
)

# Function to process a single track file
process_track <- function(track_file, is_mc = FALSE) {
  # Read the track
  track_data <- tibble(read.csv(track_file, header = FALSE, 
                                col.names = c("date", "latitude", "longitude", "wind"),
                                colClasses = c("character", "numeric", "numeric", "numeric")
  ))
  
  # Take every other row for MC tracks
  if (is_mc) {
    track_data <- track_data[seq(1, nrow(track_data), by = 2),]
  }
  
  # Calculate wind exposure
  wind_exposure <- get_grid_winds(
    hurr_track = track_data,
    grid_df = tract_grid
  )
  
  # Process results
  tract_wind_pop <- wind_exposure %>%
    left_join(tract_population %>% 
                select(tract_fips, population), 
              by = c("gridid" = "tract_fips")) %>%
    mutate(
      county_fips = substr(gridid, 1, 5),
      population = coalesce(population, 0),
      wind_pop_product = vmax_sust * coalesce(population, 0)
    )
  
  results <- tract_wind_pop %>%
    group_by(county_fips) %>%
    summarize(
      wind_popw = sum(wind_pop_product, na.rm = TRUE) / sum(coalesce(population, 0), na.rm = TRUE),
      wind_tract = mean(vmax_sust, na.rm = TRUE),
      total_population = sum(coalesce(population, 0), na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(
      county = as.numeric(county_fips),
      hurricane = storm_name
    )
  
  return(results)
}

# Wrapper to run in parallel (or serial) with bounded workers.
run_track_batch <- function(files, is_mc) {
  if (length(files) == 0) {
    return(list())
  }
  workers <- max(1L, min(num_cores, max_files_per_batch, length(files)))
  if (.Platform$OS.type == "windows" || workers == 1L) {
    return(lapply(files, process_track, is_mc = is_mc))
  }
  parallel::mclapply(files, process_track, is_mc = is_mc, mc.cores = workers)
}

# Process each storm
for (storm_name in storm_names) {
  print(sprintf("\nProcessing storm: %s", storm_name))
  
  # Get all track files for the storm
  storm_files <- list.files(
    path = file.path(storms_dir, tolower(storm_name)),
    pattern = "csv$",
    full.names = TRUE,
    recursive = FALSE
  )
  
  if (length(storm_files) == 0) {
    warning(sprintf("No files found for storm %s", storm_name))
    next
  }
  
  print(paste("Found", length(storm_files), "files"))
  
  # Separate Best and MC files
  best_files <- storm_files[grepl("BEST", storm_files)]
  mc_files <- storm_files[grepl("mc", storm_files)]
  
  # Process Best track files
  if (length(best_files) > 0) {
    print("\nProcessing Best track files...")
    best_results_list <- run_track_batch(best_files, is_mc = FALSE)
  } else {
    best_results_list <- list()
  }
  
  # Process all MC track files
  if (length(mc_files) > 0) {
    print("\nProcessing MC track files...")
    mc_results_list <- run_track_batch(mc_files, is_mc = TRUE)
  } else {
    mc_results_list <- list()
  }
  
  # Combine results for this storm
  storm_results <- bind_rows(
    bind_rows(best_results_list) %>% 
      rename(wind_obs_popw = wind_popw, wind_obs_tract = wind_tract),
    bind_rows(mc_results_list) %>% 
      rename(wind_fc_popw = wind_popw, wind_fc_tract = wind_tract)
  ) %>%
  group_by(county, hurricane) %>%
  summarize(
    wind_obs_popw = mean(wind_obs_popw, na.rm = TRUE),
    wind_obs_tract = mean(wind_obs_tract, na.rm = TRUE),
    wind_fc_popw = mean(wind_fc_popw, na.rm = TRUE),
    wind_fc_tract = mean(wind_fc_tract, na.rm = TRUE),
    total_population = first(total_population)
  )
  
  # Save results for this storm as RDS
  output_file <- file.path("data/clean/popw", paste0(storm_name, "_wind_exposure.rds"))
  saveRDS(storm_results, output_file)
  print(sprintf("\nSaved results for %s to: %s", storm_name, output_file))
}

# Parallel processing complete
print("\n--- All storms processed ---\n")
