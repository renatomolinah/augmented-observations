# ==============================================================================
# Clean hurricane track shapefiles: build observed and forecast tracks, compute
# county-level track-position errors across cutoffs, and county left/right side.
# Inputs:  data/raw/track_raw/ (or track_raw.zip): trackline_{bdeck,adeck} shapefiles
# Outputs: data/clean/{track_actual.rds, track_observed_points.rds, track_forecast.rds,
#          track_errors_all_cutoffs.rds, county_direction.csv}
# Run:     sourced by code/cleaning_voi/run_all.r
# ==============================================================================
cat("================================================================================\n")
cat("RUNNING: 1.02.clean_forecast_track.r\n")
cat("PURPOSE: Clean hurricane track data - creates track_forecast.rds, track_actual.rds, county_direction.csv\n")
cat("================================================================================\n")

# ==============================================================================
# 1. Read actual track files
# ==============================================================================

if (file.exists("data/raw/track_raw.zip")) {
  unzip("data/raw/track_raw.zip", exdir = "data/raw/")
} else if (!dir.exists("data/raw/track_raw")) {
  stop("Missing track raw input: expected data/raw/track_raw/ or data/raw/track_raw.zip")
}

file_names <- list.files(
  path = "data/raw/track_raw/",
  pattern = "trackline_bdeck.*\\.shp$",
  full.names = TRUE,
  all.files = TRUE,
  recursive = TRUE
)

# Tag each file with its source type up front, so the type stays aligned
# even if a shapefile carries more than one feature.
parse_type <- function(path) {
  bn <- basename(path)
  substr(bn, nchar(bn) - 7, nchar(bn) - 4)
}
track_list = Map(
  function(file) {
    dt <- st_read(file)
    names(dt) <- c("Track", "Vertices", "geometry")
    dt$type <- parse_type(file)
    dt
  },
  file_names
)

# ==============================================================================
# 2. Combine actual track segments
# ==============================================================================

track = do.call(rbind, track_list) %>%
  mutate(hurricane = gsub(" .*$", "", Track),
         date = substr(parse_number(Track), 1, 8),
         date = as_date(date),
         storm_id = paste0(hurricane, "-", str_sub(date,1,4)),
         storm_id = str_to_title(storm_id)) |> 
  group_by(hurricane, type) |> 
  mutate(max_date = max(date),
         forecast_time = as.numeric((-1*(date - max_date)+1))) |> 
  select(hurricane, type, date, storm_id, forecast_time) |> 
  st_set_crs(4326) |> 
  ungroup()

# ==============================================================================
# 3. Save actual track
# ==============================================================================

saveRDS(track, file.path(clean_dir, "track_actual.rds"))

# ==============================================================================
# 3b. Observed track positions at the forecast verification times
# ==============================================================================

# The b-deck trackpoints carry the best-track position at each verification hour
# of the cycle (0-120 h from initialization), so pooling cycles recovers the
# observed track over the full window the analysis looks at.
point_files <- list.files(
  path = "data/raw/track_raw/",
  pattern = "trackpoints_bdeck_BEST.*\\.shp$",
  full.names = TRUE,
  recursive = TRUE
)

track_points <- do.call(rbind, lapply(point_files, function(file) {
  dt <- st_read(file, quiet = TRUE)
  names(dt)[1:5] <- c("Track", "forecast_hour", "lat", "lon", "wind_kt")
  dt
})) |>
  mutate(hurricane = gsub(" .*$", "", Track),
         init  = ymd_hm(str_extract(Track, "[0-9]{12}")),
         valid = init + hours(forecast_hour)) |>
  select(hurricane, init, forecast_hour, valid, lat, lon, wind_kt) |>
  st_set_crs(4326)

# cycles overlap; the best track is identical where they do, so keep one row per time
track_points <- track_points |>
  arrange(hurricane, valid, init) |>
  distinct(hurricane, valid, .keep_all = TRUE)

saveRDS(track_points, file.path(clean_dir, "track_observed_points.rds"))

# ==============================================================================
# 4. Read forecast track files
# ==============================================================================

file_names <- list.files(
  path = "data/raw/track_raw/",
  pattern = "trackline_adeck_OFCL.*\\.shp$",
  full.names = TRUE,
  all.files = TRUE,
  recursive = TRUE
)

# Same per-file tagging pattern as the bdeck pass above.
track_list = Map(
  function(file) {
    dt <- st_read(file)
    names(dt) <- c("Track", "Vertices", "geometry")
    dt$type <- parse_type(file)
    dt
  },
  file_names
)

# ==============================================================================
# 5. Combine forecast track segments
# ==============================================================================

track_forecast = do.call(rbind, track_list) %>%
  mutate(hurricane = gsub(" .*$", "", Track),
         date = substr(parse_number(Track), 1, 8),
         date = as_date(date),
         storm_id = paste0(hurricane, "-", str_sub(date,1,4)),
         storm_id = str_to_title(storm_id)) |> 
  group_by(hurricane, type) |> 
  mutate(max_date = max(date),
         forecast_time = as.numeric((-1*(date - max_date)+1))) |> 
  select(hurricane, type, date, storm_id, forecast_time) |> 
  st_set_crs(4326) |> 
  ungroup()

# ==============================================================================
# 6. Save forecast track
# ==============================================================================

saveRDS(track_forecast, file.path(clean_dir, "track_forecast.rds"))

# ==============================================================================
# 7. Compute track errors
# ==============================================================================

county_centroids <- load_county_centroids()
hurricanes <- readRDS(file.path(clean_dir, "track_forecast.rds")) |>
  as_tibble() %>%
  mutate(year = year(date)) |>
  select(hurricane, year) %>%
  distinct() |>
  arrange(year, hurricane)

# Initialize list to store results
all_distances <- list()

# Process each hurricane
for (i in 1:nrow(hurricanes)) {
  hurricane_name <- tolower(hurricanes$hurricane[i])
  print(sprintf("Processing %s...", hurricane_name))
  
  # Get lead times for this hurricane
  hurricane_path <- here("data/raw/track_raw", hurricane_name, "track")
  if (!dir.exists(hurricane_path)) {
    print(sprintf("No track data found for %s", hurricane_name))
    next
  }
  
  lead_times <- list.dirs(hurricane_path, full.names = FALSE, recursive = FALSE)
  lead_times <- lead_times[grepl("^[0-9]+$", lead_times)]
  
  # Process each lead time
  for (lead_time in lead_times) {
    tryCatch({
      distances <- calculate_distances(hurricane_name, lead_time, county_centroids)
      all_distances[[paste(hurricane_name, lead_time, sep = "_")]] <- distances
    }, error = function(e) {
      print(sprintf("Error processing %s lead time %s: %s", hurricane_name, lead_time, e$message))
    })
  }
}

filtered_distance <- function(all_distances, cutoff, reference_cutoff = 400){
  
  # Combine all results
  final_distances <- bind_rows(all_distances) %>% 
    filter(obs_distance <= cutoff, pred_distance <= cutoff)
  
  mean_errors <- final_distances %>%
    group_by(hurricane, county) %>%
    summarise(
      track_obs = mean(obs_distance),
      track_pred = mean(pred_distance),
      level_error_track = mean(pred_distance - obs_distance)
    ) %>%
    ungroup() %>% 
    filter(track_obs <= cutoff, track_pred <= cutoff) %>%
    mutate(
      track_obs = pmin(track_obs, reference_cutoff),
      track_pred = pmin(track_pred, reference_cutoff),
      level_error_track = pmin(pmax(level_error_track, -reference_cutoff), reference_cutoff)
    ) %>%
    mutate(
      # Bin forecast track
      forecast_track_cut = cut(track_pred, 
                               breaks = seq(0, reference_cutoff, reference_cutoff/10),  
                               include.lowest = TRUE),
      
      # Bin track position error
      track_error_bin = cut(level_error_track, 
                            breaks = seq(-reference_cutoff, reference_cutoff, reference_cutoff/10*2), 
                            include.lowest = TRUE),
      # Bin realized track distance
      track_cut = cut(track_obs, 
                      breaks = seq(0, reference_cutoff, reference_cutoff/10), 
                      include.lowest = TRUE)
    )
  
  return(mean_errors)
}

# Apply filtered_distance to all cutoffs and combine

all_cutoff_results <- list()

for (cutoff in cutoffs) {
  print(sprintf("Processing cutoff %d km...", cutoff))
  
  result <- filtered_distance(all_distances, cutoff = cutoff, reference_cutoff = 400)
  
  all_cutoff_results[[as.character(cutoff)]] <- result
}

# Combine all results
combined_results <- bind_rows(all_cutoff_results, .id = "cutoff") %>%
  mutate(cutoff = as.numeric(cutoff))

# Save as single file
saveRDS(combined_results, file.path(clean_dir, "track_errors_all_cutoffs.rds"))

# ==============================================================================
# 8. County direction (left/right) relative to track
# ==============================================================================

track_actual <- readRDS(file.path(clean_dir, "track_actual.rds")) |>
  filter(forecast_time == 1 & type == "BEST")

track_forecast <- readRDS(file.path(clean_dir, "track_forecast.rds")) |>
  filter(forecast_time == 1 & type == "OFCL")

county_sf <- get_urbn_map("counties", sf = TRUE) |>
  st_transform(4326) |>
  select(county_fips) |>
  rename(county_id = county_fips) |>
  st_make_valid()

# Ensure consistent CRS
county_sf <- st_transform(county_sf, st_crs(track_actual))

# Get county centroids
county_centroids <- st_centroid(county_sf |> st_make_valid())

# Function to process track data
process_track <- function(track_data, track_type) {
  results_list <- list()
  
  # Loop over each track line
  for (i in 1:nrow(track_data)) {
    track_line <- track_data[i,]
    
    # Determine position for each county
    county_positions <- county_centroids |>
      mutate(
        # Find the latitude of the county
        county_lat = st_coordinates(geometry)[,2],
        # Find the closest latitude on the track
        track_lat = sapply(county_lat, function(lat) {
          track_coords <- st_coordinates(track_line)
          closest_lat <- track_coords[which.min(abs(track_coords[,2] - lat)), 2]
          return(closest_lat)
        }),
        # Find the corresponding longitude on the track
        track_lon = sapply(track_lat, function(lat) {
          track_coords <- st_coordinates(track_line)
          track_point <- track_coords[which.min(abs(track_coords[,2] - lat)), ]
          return(track_point[1])
        }),
        # Determine if the county is left or right of the track
        left_of_track = st_coordinates(geometry)[,1] < track_lon,
        hurricane = track_line$hurricane,
        track_type = track_type
      ) |>
      st_drop_geometry() |>
      select(county_id, hurricane, left_of_track, track_type)
    
    # Store the result
    results_list[[i]] <- county_positions
  }
  
  # Combine all results into a single dataframe
  return(bind_rows(results_list))
}

# Process both actual and forecast tracks
county_hurricane_actual <- process_track(track_actual, "actual")
county_hurricane_forecast <- process_track(track_forecast, "forecast")

# Combine results and pivot wider
county_hurricane_df <- bind_rows(county_hurricane_actual, county_hurricane_forecast) |>
  pivot_wider(
    names_from = track_type,
    values_from = left_of_track,
    names_prefix = "left_of_track_"
  ) |>
  rename(left_of_track = left_of_track_actual)

# Save results
fwrite(county_hurricane_df, file.path(clean_dir, "county_direction.csv"))
