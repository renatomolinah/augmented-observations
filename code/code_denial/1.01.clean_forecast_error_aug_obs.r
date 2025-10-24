print("Cleaning forecasts.")

#=============================
# 1. load wind and rain forecasts
#=============================

wind_file_names_all = list.files(
  path = "data/raw_denial/wind_error_ALL",
  recursive = FALSE,
  full.names = TRUE,
  pattern = "*csv"
)
wind_list_all = lapply(wind_file_names_all, fread)

wind_file_names_no = list.files(
  path = "data/raw_denial/wind_error_NO",
  recursive = FALSE,
  full.names = TRUE,
  pattern = "*csv"
)
wind_list_no = lapply(wind_file_names_no, fread)


rain_file_names_all = list.files(
  path = "data/raw_denial/precip_error_ALL",
  recursive = FALSE,
  full.names = TRUE,
  pattern = "*csv"
)
rain_list_all = lapply(rain_file_names_all, fread)

rain_file_names_no = list.files(
  path = "data/raw_denial/precip_error_NO",
  recursive = FALSE,
  full.names = TRUE,
  pattern = "*csv"
)
rain_list_no = lapply(rain_file_names_no, fread)



#=============================
# 2. combine forecast files
#=============================

# 2. Combine hurricane files
hurricanes_list = c(
  "dorian", 
  "florence", 
  "hanna", 
  "harvey", 
  "irma", 
  "isaias", 
  "laura", 
  "maria", 
  "michael", 
  "sally"
)

## 2.1 Wind data

wind_all = lapply(seq_along(wind_list_all), function(j) {
  wind_list_all[[j]] = wind_list_all[[j]] |>
    mutate(hurricane = hurricanes_list[j])
}) |>
  rbindlist() |>
  mutate(augmented_obs = 1)

wind_no = lapply(seq_along(wind_list_no), function(j) {
  wind_list_no[[j]] = wind_list_no[[j]] |>
    mutate(hurricane = hurricanes_list[j])
}) |>
  rbindlist() |>
  mutate(augmented_obs = 0)

wind = rbind(wind_all, wind_no)

# Calculate predicted means and sds are missing from wind data. Can recover predicted
# means based on obs and dif_mean, but not sds.
wind = wind |>
  mutate(
    vmax_sust_pred_mean = vmax_sust_obs + vmax_sust_dif_mean,
    vmax_gust_pred_mean = vmax_gust_obs + vmax_gust_dif_mean,
    sust_dur_pred_mean  = sust_dur_obs  + sust_dur_dif_mean,
    gust_dur_pred_mean  = gust_dur_obs  + gust_dur_dif_mean
  )

# Ensure no errors matching names and hurricanes
any(wind$name != wind$hurricane)


## 2.2 Rain data

rain_all = lapply(seq_along(rain_list_all), function(j) {
  rain_list_all[[j]] = rain_list_all[[j]] |>
    mutate(
      hurricane = hurricanes_list[j],
      pred_date = as.IDate(format(pred_date, "%Y-%m-%d"))
    )
}) |>
  rbindlist() |>
  mutate(augmented_obs = 1)

rain_no = lapply(seq_along(rain_list_no), function(j) {
  rain_list_no[[j]] = rain_list_no[[j]] |>
    mutate(
      hurricane = hurricanes_list[j],
      pred_date = as.IDate(format(pred_date, "%Y-%m-%d"))
    )
}) |>
  rbindlist() |>
  mutate(augmented_obs = 0)

rain = rbind(rain_all, rain_no)

# Ensure no errors matching names and hurricanes
any(rain$name != rain$hurricane)


## 2.3 Wind + rain merge

weather = left_join(
  wind, 
  rain,  # FYI: rain has data for extra counties
  by = c("name", "pred_date", "lead_days", "lead_hrs", "gridid", "hurricane", "augmented_obs")
) |>
  mutate(
    glon = ifelse(is.na(glon.x), glon.y, glon.x),
    glat = ifelse(is.na(glat.x), glat.y, glat.x)
  ) |>
  select(-glon.x, -glon.y, -glat.x, -glat.y
  ) |>
  rename(lead_hours = lead_hrs) 
  

#=============================
# 3. create county variables
#=============================

# counties_map = tigris::counties(cb = TRUE) |>
#   st_set_crs(4326)
# saveRDS(counties_map, "data/raw/census/counties_tigris.rds")

counties_map = readRDS("data/raw/census/counties_tigris.rds")
st_crs(counties_map) = st_crs(counties_map)

#=============================
# 4. merge
#=============================

weather = weather |>
  st_as_sf(
    coords = c("glon", "glat"),
    remove = FALSE
  ) |>
  st_set_crs(4326) |>
  st_join(counties_map |> select(county_fips = GEOID, county_name = NAME), join = st_nearest_feature) |> 
  as_tibble() |>
  rename(county = county_fips, county_name = county_name, date = pred_date) |>
  mutate(county = as.integer(county))

# Save forecast data before aggregating over lead times for later comparisons
fwrite(weather, "data/clean_denial/forecast_denial_data_with_lead_times.csv")


weather = weather |>
  group_by(name) |>
  # hurricane date (day after last forecast)
  mutate(date = max(date) + 1) |>
  ungroup() |>
  # select(-starts_with("gust"), -starts_with("sust")) |>
  as_tibble() |>
  group_by(name, date, gridid, hurricane, augmented_obs, glon, glat, county, county_name, geometry) |>
  summarise_all(
    mean, na.rm = TRUE
  ) |>
  ungroup()

# Reorder columns alphabetically after the ID columns (first 12)
weather = weather |>
  select(1:12, sort(names(weather)[13:ncol(weather)]))

# save weather data
fwrite(weather, "data/clean_denial/forecast_denial_data.csv")
