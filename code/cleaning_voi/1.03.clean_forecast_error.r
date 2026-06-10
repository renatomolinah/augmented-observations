# ==============================================================================
# Combine wind and precipitation forecast-error CSVs, attach county identifiers,
# average over lead times, and build a county shapefile for maps.
# Inputs:  data/raw/{wind_error,precip_error}/*.csv;
#          data/raw/census/{counties_tigris.rds, va_acs.rds}
# Outputs: data/clean/{forecast_data.csv, weather_error_sf.rds}
# Run:     sourced by code/cleaning_voi/run_all.r
# ==============================================================================
cat("================================================================================\n")
cat("RUNNING: 1.03.clean_forecast_error.r\n")
cat("PURPOSE: Clean weather forecast errors - creates forecast_data.csv, weather_error_sf.rds\n")
cat("================================================================================\n")

# ==============================================================================
# 1. Load wind and rain forecasts
# ==============================================================================
# CSVs are produced upstream by code/external_models/{stormwindmodel,prain}.

# Read and tag wind files
wind_file_names <- list.files(
  path = "data/raw/wind_error",
  recursive = FALSE,
  full.names = TRUE,
  pattern = "*csv"
)

wind_list <- lapply(wind_file_names, function(file) {
  hurricane_name <- gsub("_wind_errors\\.csv$", "", basename(file))
  fread(file)
})

wind <- rbindlist(wind_list) |>
  rename(hurricane = name) 

# Read and tag rain files
rain_file_names <- list.files(
  path = "data/raw/precip_error",
  recursive = FALSE,
  full.names = TRUE,
  pattern = "*csv"
)

rain_list <- lapply(rain_file_names, function(file) {
  hurricane_name <- gsub("_precip_errors?\\.csv$", "", basename(file))
  fread(file)[, `:=`(
    hurricane = hurricane_name,
    pred_date = as.IDate(format(pred_date, "%Y-%m-%d"))
  )]
})

rain <- rbindlist(rain_list) |>
  select(-V1)

# ==============================================================================
# 2. Combine forecast files
# ==============================================================================

weather = left_join(
  wind,
  rain,  # rain covers some counties not in wind
  by = c("pred_date" = "pred_date", "lead_days" = "lead_days", "lead_hours" = "lead_hrs", "gridid" = "gridid", "hurricane" = "hurricane")
) |>
  mutate(
    glon = ifelse(is.na(glon.x), glon.y, glon.x),
    glat = ifelse(is.na(glat.x), glat.y, glat.x)
  ) |>
  select(-glon.x, -glon.y, -glat.x, -glat.y) 

# ==============================================================================
# 3. Create county variables
# ==============================================================================

counties_map = readRDS("data/raw/census/counties_tigris.rds")
st_crs(counties_map) = st_crs(counties_map)

# ==============================================================================
# 4. Merge and average over lead times
# ==============================================================================

weather <- weather |> 
  mutate(
    county = as.integer(gridid),
    date = as.Date(pred_date)
  ) |> 
  left_join(
    counties_map |> 
      mutate(county = as.integer(GEOID)) |> 
      select(county, county_name = NAME),
    by = "county"
  ) |> 
  st_as_sf(coords = c("glon", "glat"), crs = 4326, remove = FALSE)

weather = weather |>
  group_by(hurricane) |>
  # Hurricane date (day after last forecast)
  mutate(date = max(date) + 1) |>
  ungroup() |>
  as_tibble() |>
  group_by(hurricane, date, gridid, glon, glat, county, county_name, geometry) |>
  # n_lead = how many lead-time rows contributed to each group's mean;
  # small values signal a per-group mean driven by very few observations.
  mutate(n_lead = n()) |>
  summarise_all(
    mean, na.rm = TRUE
  ) |>
  ungroup()

# Save weather data
fwrite(weather, "data/clean/forecast_data.csv")

# ==============================================================================
# 5. Build county shapefile for maps
# ==============================================================================

data_counties = st_as_sf(maps::map("county", plot = FALSE, fill = TRUE)) |>
  mutate(state = sub("\\,.*", "", ID)) |>
  inner_join(maps::county.fips |> rename(ID = polyname) |> mutate(ID = gsub(":.*", "", ID)), # remove redundant characters after :
    by = "ID"
  ) |>
  rename(gridid = fips) |>
  mutate(
    ID = gsub("[^0-9A-Za-z///' ]", " ", ID, ignore.case = TRUE),
    ID = gsub("[[:blank:]]", "", ID)
  ) |>
  group_by(gridid) |>
  summarise(geom = st_union(geom), ID = first(ID), state = first(state)) |>
  ungroup()

# va_acs.rds is a saved tidycensus::get_acs() pull for Virginia counties
# (B01003_001, ACS5, 2009, with geometry); used to supply VA county polygons.
data_va = readRDS("data/raw/census/va_acs.rds") |>
  select(-moe) |>
  spread(variable, estimate) |>
  rename(
    gridid = GEOID,
    geom = geometry,
    ID = NAME
  ) |>
  select(ID, gridid, geom) |>
  mutate(
    ID = gsub(",.*", "", ID),
    ID = str_replace_all(ID, c("County" = "")),
    ID = gsub("[[:blank:]]", "", ID),
    ID = paste0("virginia", tolower(ID))
  ) |>
  st_transform(4326) |>
  mutate(state = "virginia")

data_wva = data_counties |>
  st_transform(4326) |>
  filter(grepl("westvirginia", ID)) |>
  mutate(state = "west virginia")

data_counties_final = data_counties |>
  st_transform(4326) |>
  filter(!grepl("virginia", ID)) |> 
  rbind(data_va, data_wva) |>
  mutate(gridid = as.integer(gridid))

weather_error_sf = left_join(weather, data_counties_final |> select(gridid, geom, state), by = "gridid")

# Save county shape files
saveRDS(weather_error_sf, "data/clean/weather_error_sf.rds")
