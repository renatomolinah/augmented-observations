# ==============================================================================
cat("================================================================================\n")
cat("RUNNING: 1.02.clean_forecast_track.r\n")
cat("PURPOSE: Clean hurricane track data - creates track_forecast.rds, track_actual.rds, county_direction.csv\n")
cat("================================================================================\n")

#=============================
# 1. Read actual track files
#=============================

unzip("data/raw/track_raw.zip", exdir = "data/raw/")

file_names <- list.files(
  path = "data/raw/track_raw/",
  pattern = "trackline_bdeck.*\\.shp$",
  full.names = TRUE,
  all.files = TRUE,
  recursive = TRUE
)

file_names_var <- list.files(
  path = "data/raw/track_raw/",
  pattern = "trackline_bdeck.*\\.shp$",
  full.names = FALSE,
  all.files = TRUE,
  recursive = TRUE
)

forecast_type = lapply(
  file_names_var,
  function(x) substr(x, nchar(x)-7, nchar(x)-4)
) |> 
  c() |> 
  unlist()

track_list = lapply(file_names, st_read)

for (i in seq_along(track_list)) {
  names(track_list[[i]]) = c("Track", "Vertices", "geometry")
}

#=============================
# 2. Combine with data for plotting
#=============================

track = do.call(rbind, track_list) %>%
  mutate(type = forecast_type,
         hurricane = gsub(" .*$", "", Track),
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

#=============================
# 3. Save actual track
#=============================

saveRDS(track, "data/clean/track_actual.rds")


#=============================
# 4. Read forecast track files
#=============================

file_names <- list.files(
  path = "data/raw/track_raw/",
  pattern = "trackline_adeck_OFCL.*\\.shp$",
  full.names = TRUE,
  all.files = TRUE,
  recursive = TRUE
)

file_names_var <- list.files(
  path = "data/raw/track_raw/",
  pattern = "trackline_adeck_OFCL.*\\.shp$",
  full.names = FALSE,
  all.files = TRUE,
  recursive = TRUE
)

forecast_type = lapply(
  file_names_var,
  function(x) substr(x, nchar(x)-7, nchar(x)-4)
) |> 
  c() |> 
  unlist()

track_list = lapply(file_names, st_read)

for (i in seq_along(track_list)) {
  names(track_list[[i]]) = c("Track", "Vertices", "geometry")
}

#=============================
# 5. Combine with data for plotting
#=============================

track_forecast = do.call(rbind, track_list) %>%
  mutate(type = forecast_type,
         hurricane = gsub(" .*$", "", Track),
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

#=============================
# 6. Save forecast track
#=============================

saveRDS(track_forecast, "data/clean/track_forecast.rds")


#=============================
# 7. Compute track errors
#=============================

county_centroids <- load_county_centroids()
hurricanes <- readRDS("data/clean/track_forecast.rds") |>
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
saveRDS(combined_results, "data/clean/track_errors_all_cutoffs.rds")

#=============================
# 8. Get county direction relative to track
#=============================

# Load track data
track_actual <- readRDS("data/clean/track_actual.rds") |>
  filter(forecast_time == 1 & type == "BEST")

track_forecast <- readRDS("data/clean/track_forecast.rds") |>
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
fwrite(county_hurricane_df, "data/clean/county_direction.csv")
