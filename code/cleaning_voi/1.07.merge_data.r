# ==============================================================================
# Merge all cleaned sources into the county-by-storm analysis panel: forecast
# errors, SHELDUS damages, FEMA spending, demographics, GDP, and track errors.
# Builds cost outcomes, error/forecast bins, and normalized-error datasets.
# Inputs:  data/clean/{forecast_data.csv, data_sheldus.csv, fema_spending.csv,
#          income_gdp.csv, coastline_counties.csv, county_direction.csv,
#          county_demographics.csv, track_errors_all_cutoffs.rds}
# Outputs: data/clean/{final_dataset.rds, ncei_compare.rds,
#          pooled_track_errors_all_cutoffs.rds,
#          normalized_dataset.rds, normalized_dataset_global.rds}
# Run:     sourced by code/cleaning_voi/run_all.r
# ==============================================================================
cat("================================================================================\n")
cat("RUNNING: 1.07.merge_data.r\n")
cat("PURPOSE: Merge all data sources - creates final_dataset.rds/csv\n")
cat("================================================================================\n")

# ==============================================================================
# 1. Load county/state data
# ==============================================================================

counties_map = get_urbn_map("counties", sf = TRUE) |>
  st_set_crs(4326)
st_crs(counties_map) = st_crs(counties_map)

counties_sf <- get_urbn_map("counties", sf = FALSE) |> 
  distinct(county_name, state_name, state_fips, fips_class, county_fips)

county_lat_lon = housingData::geoCounty |> 
  select(county = fips, lon, lat)
# geoCounty has no Puerto Rico; use the wind-model municipio centroids
if (include_pr) {
  county_lat_lon = bind_rows(
    county_lat_lon,
    fread("data/code/run_stormwindmodel/us_county_stats_puertorico.csv") |>
      transmute(county = str_pad(gridid, 5, "left", "0"), lon = glon, lat = glat))
}
  
# ==============================================================================
# 2. Load county demographics
# ==============================================================================

county_demos = fread(file.path(clean_dir, "county_demographics.csv")) |>
  as_tibble() |>
  mutate(county = str_pad(county, 5, 'left', '0'))

# ==============================================================================
# 3. Load cleaned data
# ==============================================================================

weather = fread(file.path(clean_dir, "forecast_data.csv")) |>
  mutate(date = as.Date(date)) |>
  mutate(county_fips = str_pad(county, 5, 'left', '0'))

fema_spending = fread(file.path(clean_dir, "fema_spending.csv")) |>
  mutate(county_fips = str_pad(fips, 5, 'left', '0')) |>
  select(-fips)

data_sheldus = fread(file.path(clean_dir, "data_sheldus.csv")) |>
  mutate(county_fips = str_pad(county_fips, 5, 'left', '0')) |> 
  group_by(hurricane, county_fips, state_fips) |>
  summarise(
    across(
      crop_damage:deaths_pc,
      ~ sum(.x, na.rm = TRUE)
    )
  ) |> 
  ungroup()

income_gdp = fread(file.path(clean_dir, "income_gdp.csv")) |>
  mutate(
    county_fips = str_pad(as.character(county_fips), 5, 'left', '0'),
    year = as.numeric(year),
    income_pc = as.numeric(gsub("[^0-9.]", "", income_pc)),
    gdp = as.numeric(gsub("[^0-9.]", "", gdp))
  )


coastline_counties = fread(file.path(clean_dir, "coastline_counties.csv")) |>
  as_tibble()

county_direction = fread(file.path(clean_dir, "county_direction.csv")) |>
  as_tibble() |>
  mutate(county_id = str_pad(county_id, 5, 'left', '0'))

# ==============================================================================
# 4. Prepare forecast data
# ==============================================================================

# Counties with forecast records
data_weather = weather |>
  select(-county, -geometry) |>
  left_join(counties_map |> select(-county_name), by = "county_fips") |>
  mutate(
    state_name = str_to_lower(state_name),
    county_name = str_to_lower(county_name),
    year = year(date),
    month = month(date),
    # urbnmapr has no Puerto Rico, so its municipios get their state code here
    state_fips = ifelse(is.na(state_fips) & substr(county_fips, 1, 2) == "72", "72", state_fips)
  ) |>
  as_tibble() |>
  select(
    hurricane, date, year, month, lead_hours, state_fips, state_name, county_fips, county_name, geometry,
    starts_with(c("vmax", "prec")), sust_dur_obs, sust_dur_pred_mean, sust_dur_dif_mean, sust_dur_pred_sd, -prec_dif_sd, -vmax_gust_dif_sd, -vmax_sust_dif_sd
    )

# zero-impact rows for never-forecast counties cover continental storms only
data_date_hurr = weather |>
  distinct(date, lead_hours, hurricane) |>
  filter(hurricane %notin% pr_storms)

# Add counties without forecast records and assign zero forecast impacts
data_unaffected_counties = anti_join(
  counties_map |> select(-fips_class),
  data_weather |> distinct(county_fips),
  by = "county_fips"
) |>
  expand_grid(data_date_hurr) |>
  mutate(
    state_name = str_to_lower(state_name),
    county_name = str_to_lower(county_name),
    year = year(date),
    month = month(date)
  ) |>
  as_tibble() |>
  mutate(
    vmax_sust_obs = 0, vmax_gust_obs = 0, sust_dur_obs = 0,
    vmax_gust_dif_mean = 0, vmax_sust_dif_mean = 0, sust_dur_dif_mean = 0, 
    vmax_gust_pred_mean = 0, vmax_gust_pred_sd = 0, vmax_sust_pred_mean = 0, vmax_sust_pred_sd = 0, sust_dur_pred_mean = 0, sust_dur_pred_sd = 0,
    prec_dif_mean = 0, prec_pred_mean = 0, prec_pred_sd = 0, prec_obs = 0
  ) |>
  select(
    hurricane, date, year, month, lead_hours, state_fips, state_name, county_fips, county_name, geometry,
    starts_with(c("vmax", "prec", "sust_dur"))
  )

data_weather_final = rbind(data_weather, data_unaffected_counties) |>
  arrange(date, county_fips)

# ==============================================================================
# 5. Merge datasets and build outcomes
# ==============================================================================

# Catch SHELDUS hurricane names that have no forecast counterpart.
# Without this, such rows survive the full_join with NA weather columns,
# get zeroed by replace_na, and produce county-storm rows with damages
# but zero wind/precip — silently distorting beta.
# A Puerto Rico storm whose forecast files are not in yet is skipped, not an error
pr_pending = setdiff(intersect(unique(data_sheldus$hurricane), pr_storms), unique(data_weather_final$hurricane))
if (length(pr_pending) > 0) {
  cat("[1.07] Puerto Rico storms without forecast files, skipped:", paste(pr_pending, collapse = ", "), "\n")
  data_sheldus = data_sheldus |> filter(hurricane %notin% pr_pending)
}
sheldus_orphans = setdiff(unique(data_sheldus$hurricane), unique(data_weather_final$hurricane))
if (length(sheldus_orphans) > 0) {
  stop("SHELDUS storms with no forecast match: ", paste(sheldus_orphans, collapse = ", "))
}

# Join forecast and SHELDUS data
data_weather_dmg_en = full_join(
  data_weather_final,
  data_sheldus |>
    select(-state_fips) |>
    mutate(county_fips = str_pad(county_fips, 5, 'left', '0')),
  by = c("hurricane", "county_fips")
) |>
  replace_na(list(
    property_damage = 0,
    property_damage_2019 = 0,
    property_damage_pc_2019 = 0,
    crop_damage = 0,
    crop_damage_2019 = 0,
    crop_damage_pc_2019 = 0,
    injuries = 0,
    deaths = 0,
    deaths_pc = 0,
    vmax_sust_obs = 0, vmax_sust_pred_mean = 0, vmax_sust_dif_mean = 0,
    vmax_gust_obs = 0, vmax_gust_pred_mean = 0, vmax_gust_dif_mean = 0,
    sust_dur_pred_mean = 0, sust_dur_obs = 0, sust_dur_dif_mean = 0, 
    prec_dif_mean = 0, prec_pred_mean = 0, prec_obs = 0
  )) |>
  mutate(
    wind_obs = vmax_sust_obs,
    wind_fc = vmax_sust_pred_mean,
    wind_error = vmax_sust_dif_mean,
    gust_obs = vmax_gust_obs,
    gust_fc = vmax_gust_pred_mean,
    gust_error = vmax_gust_dif_mean,
    dur_obs = sust_dur_obs,
    dur_fc = sust_dur_pred_mean,
    dur_error = sust_dur_dif_mean,
    precip_obs = prec_obs,
    precip_fc = prec_pred_mean,
    precip_error = prec_dif_mean,
    total_damage = property_damage_2019 + crop_damage_2019,
    total_damage_pc = property_damage_pc_2019 + crop_damage_pc_2019,
    # EPA value of a statistical life: $7.4M (2006$) inflated to 2019$ via
    # CPI-U (7.4 * 255.657/201.6 = 9.38). Inflation-only, no income adjustment.
    value_deaths = 9.39 * 10^6 * deaths,
    value_deaths_pc = 9.39 * 10^6 * deaths_pc,
    dmg_prop_death = property_damage_2019 + value_deaths + crop_damage_2019,
    dmg_prop_death_pc = property_damage_pc_2019 + value_deaths_pc + crop_damage_pc_2019
  ) |>
  filter(!is.na(hurricane)) |>
  # Puerto Rico rain was island-wide, so a missing (-999) gauge there is missing, not zero
  mutate(precip_missing = hurricane %in% pr_storms & (is.na(prec_obs) | prec_obs == -999)) |>
  # In this source, -999 indicates missing values and is treated as zero
  mutate(
    precip_obs = ifelse(precip_obs == -999, 0, precip_obs),
    precip_fc = ifelse(precip_fc == -999, 0, precip_fc),
    precip_error = ifelse(precip_error == -999, 0, precip_error),
    prec_pred_sd = ifelse(prec_pred_sd == -999, 0, prec_pred_sd),
    precip_obs = ifelse(precip_missing, NA, precip_obs)
  ) |>
  select(-geometry) |>
  select(
    hurricane, date, year, month,
    state = state_fips, county = county_fips,
    wind_obs, wind_fc, wind_fc_sd = vmax_sust_pred_sd, wind_error,
    precip_obs, precip_fc, precip_fc_sd = prec_pred_sd, precip_error,
    gust_obs, gust_fc, gust_fc_sd = vmax_gust_pred_sd, gust_error,
    dur_obs, dur_fc, dur_fc_sd = sust_dur_pred_sd, dur_error,
    damages = dmg_prop_death, damage_pc = dmg_prop_death_pc, deaths, 
    property_damage = property_damage_2019, 
    crop_damage = crop_damage_2019, mortality_damage = value_deaths
  )

saveRDS(data_weather_dmg_en, file.path(clean_dir, "ncei_compare.rds"))

# Merge in FEMA data
final_dataset = left_join(
    data_weather_dmg_en, 
    fema_spending, 
    by = c("hurricane", "county" = "county_fips")
  ) |> # 64 rows only in fema data: county fips starts with 72
  replace_na(list(ex_post_spending = 0, ex_ante_spending = 0))

# Drop any rows whose year exceeds BEA demographic/GDP coverage, since the
# inner_join on income_gdp would silently drop them anyway. Computed from the
# data so adding 2025+ BEA vintages auto-extends without a code edit.
bea_max_year = max(income_gdp$year, na.rm = TRUE)
final_dataset = final_dataset |> filter(year <= bea_max_year)

# Merge in county demographics
final_dataset = final_dataset |> 
  mutate(date = as.character(date)) |>
  distinct() |>
  inner_join(county_demos, by = "county")

# Merge in county GDP/income
final_dataset = bind_rows(
  final_dataset |>
    filter(hurricane %notin% pr_storms) |>
    # Rows dropped here are primarily Virginia independent cities and some Alaska counties without GDP
    inner_join(income_gdp, by = c("county" = "county_fips", "year" = "year")),
  # Puerto Rico
  final_dataset |>
    filter(hurricane %in% pr_storms) |>
    left_join(income_gdp, by = c("county" = "county_fips", "year" = "year")))

# Merge in coastal-county indicators
final_dataset = final_dataset |>
  # Rows dropped here are primarily Virginia independent cities and some Alaska counties without GDP
  left_join(coastline_counties, by = c("county" = "county_fips")) |>
  replace_na(list(coastal = FALSE)) |>
  left_join(county_direction, by = c("county" = "county_id", "hurricane" = "hurricane"))

# Visibility: how many county-storm rows have no county_direction record
# and therefore default to left_of_track = TRUE? Large counts bias any
# asymmetry analysis.
n_lot_na = sum(is.na(final_dataset$left_of_track))
cat(sprintf("[1.07] left_of_track defaulted to TRUE for %d / %d rows (%.1f%%)\n",
            n_lot_na, nrow(final_dataset), 100 * n_lot_na / nrow(final_dataset)))

final_dataset = final_dataset |>
  replace_na(list(left_of_track = TRUE, left_of_track_forecast = TRUE))

final_dataset = final_dataset |> 
  arrange(county, year) |> 
  group_by(county) |> 
  mutate(last_error = lag((wind_fc - wind_obs))) |>
  as_tibble() |> 
  mutate(log_damage = log(damages + 1),
        log_wind = log(wind_obs + 1),
        log_precip = log(precip_obs + 1),
        log_wind_fc = log(wind_fc + 1),
        log_precip_fc = log(precip_fc + 1),
        log_gdp = log(gdp),
        log_mortality = log(mortality_damage + 1),
        log_property = log(property_damage + 1),
        log_crop = log(crop_damage + 1),
        damage_per_gdp = damages / gdp / 1e3,
        ex_post_spending_per_gdp = ex_post_spending / gdp / 1e3,
        asinh_ex_post_spending = asinh(ex_post_spending),
        log_ex_post_spending = log(ex_post_spending + 1),
        ex_post_spending_pc = (ex_post_spending / population),
        ex_ante_spending_per_gdp = ex_ante_spending / gdp / 1e3,
        log_ex_ante_spending = log(ex_ante_spending + 1),
        ex_ante_spending_pc = (ex_ante_spending / population),
        asinh_damage = asinh(damages),
        asinh_ex_ante_spending = asinh(ex_ante_spending),
        total_cost = (damages + ex_post_spending) / 1e6,
        log_total_cost = log(damages + ex_post_spending + 1),
        asinh_total_cost = asinh(damages + ex_post_spending),
        total_cost_pc = (damage_pc + (ex_post_spending / population)),
        total_cost_per_gdp = (damages + ex_post_spending) / gdp / 1e3,
        asinh_wind = asinh(wind_obs),
        asinh_precip = asinh(precip_obs),
        wind_error = -1 * wind_error,
        precip_error = -1 * precip_error,
        gust_error = -1 * gust_error,
        dur_error = -1 * dur_error,
        pdd = ifelse(is.na(pdd), FALSE, TRUE)
        ) |> 
  left_join(county_lat_lon, by = "county") |>
  drop_na(lat, lon, precip_obs) |>
  ungroup() |>
  ## Winsorize upper and lower tails for errors, forecasts, and realizations
  ## due to sparse support in extreme bins
  # The underlying error variable is not inherently bounded
  mutate(
    var = ifelse(wind_error < -10, -10, wind_error),
    var = ifelse(var > 10, 10, var)
  ) |>
  # Wind error bins
  mutate(wind_error_bin = cut(var, seq(-10, 10, 2), include.lowest = TRUE)) |>
  ungroup() |>
  mutate(
    var = ifelse(precip_error < -200, -200, precip_error),
    var = ifelse(var > 200, 200, var)
  ) |>
  # Precipitation error bins
  mutate(precip_error_bin = cut(var, seq(-200, 200, 40), include.lowest = TRUE)) |>
  ungroup() |>
  mutate(
    date = as.numeric(as_date(date)),
    county = as.numeric(county)
    ) |>
  # The underlying forecast variable is not inherently bounded
  mutate(
    var = ifelse(wind_fc > 40, 40, wind_fc)
  ) |>
  # Wind forecast bins
  mutate(forecast_cut = cut(var, seq(0, 40, 5), include.lowest = TRUE)) |>
  # The underlying forecast-SD variable is not inherently bounded
  mutate(
    var = ifelse(wind_fc_sd > 4, 4, wind_fc_sd)
  ) |>
  # Wind forecast-SD bins
  mutate(forecast_sd_cut = cut(var, seq(0, 4, 0.5), include.lowest = TRUE)) |>
  # The underlying precipitation-forecast variable is not inherently bounded
  mutate(
    var = ifelse(precip_fc > 200, 200, precip_fc)
  ) |>
  # Precipitation forecast bins
  mutate(forecast_precip_cut = cut(var, seq(0, 200, 20), include.lowest = TRUE)) |> 
  # The underlying precipitation-forecast-SD variable is not inherently bounded
  mutate(
    var = ifelse(precip_fc_sd > 8, 8, precip_fc_sd)
  ) |>
  # Precipitation forecast-SD bins
  mutate(forecast_precip_sd_cut = cut(var, seq(0, 40, 5), include.lowest = TRUE)) |>
  # Wind observation bins
  group_by(wind_obs > 0) |>
  mutate(wind_cut = ntile(wind_obs, 20)) |>
  ungroup() |>
  mutate(wind_cut = ifelse(wind_obs == 0, 0, wind_cut)) |>
  # Precipitation observation bins
  group_by(precip_obs > 0) |>
  mutate(precip_cut = ntile(precip_obs, 20)) |>
  ungroup() |>
  mutate(precip_cut = ifelse(precip_obs == 0, 0, precip_cut)) |>
  mutate(dur_cut = ntile(dur_obs, 20)) |>
  group_by(wind_cut, hurricane) |>
  mutate(
    mean_wind_error = mean(wind_error, na.rm = TRUE),
    mean_precip_error = mean(precip_error, na.rm = TRUE)
  ) |>
  ungroup() |>
  mutate(
    level_error = wind_error,
    level_error_precip = precip_error,
    squared_error = (level_error - mean_wind_error)^2,
    squared_error_nodemean = (wind_error)^2,
    squared_error_precip = (precip_error - mean_precip_error)^2,
    squared_error_precip_nodemean = (precip_error)^2
  ) |> 
  mutate(
    wind_inter = wind_error*log_wind,
    precip_inter = precip_error*log_precip,
    squared_error_inter = squared_error*log_wind,
    squared_error_precip_inter = squared_error*log_precip,
    level_error_inter = level_error*log_wind,
    level_error_precip_inter = level_error_precip*log_precip
  ) |> 
  mutate(
    category = case_when(
      wind_obs <= 32 ~ "Tropical Storm", 
      TRUE ~ "Hurricane")
      ) |> 
  mutate(
    category_error = case_when(
      wind_obs <= 32 & wind_fc <= 32 ~ "TS_Forecast_TS_Actual",
      wind_obs <= 32 & wind_fc > 32 ~ "Hurricane_Forecast_TS_Actual",
      wind_obs > 32 & wind_fc <= 32 ~ "TS_Forecast_Hurricane_Actual",
      wind_obs > 32 & wind_fc > 32 ~ "Hurricane_Forecast_Hurricane_Actual"
      )
  ) 

final_dataset = final_dataset |>
  arrange(county, year, month) |>
  group_by(year, month, hurricane) |>
  mutate(hurricane_id = cur_group_id()) |>
  ungroup()

saveRDS(final_dataset, file.path(clean_dir, "final_dataset.rds"))

# ==============================================================================
# 6. Create track-error dataset
# ==============================================================================

df_track_errors <- readRDS(file.path(clean_dir, "track_errors_all_cutoffs.rds")) |>
  inner_join(final_dataset, by = c("county", "hurricane")) |>
  mutate(
    squared_error_track = level_error_track^2
  ) |>
  arrange(county, year, month) |>
  group_by(year, month, hurricane) |>
  mutate(hurricane_id = cur_group_id()) |>
  ungroup()

saveRDS(df_track_errors, file.path(clean_dir, "pooled_track_errors_all_cutoffs.rds"))

# ==============================================================================
# 7. Create normalized datasets
# ==============================================================================

df_normalized <- final_dataset |>
  group_by(hurricane) |>
  mutate(
    # Calculate hurricane-specific error statistics
    mean_error = mean(level_error),
    sd_error = sd(level_error),
    
    # Convert to percentiles using level_error instead of wind_error
    r = rank(level_error, ties.method = "first"),
    n = n(),
    p = (r - 0.5) / n,
    
    # Convert to z-scores and rescale
    z = qnorm(p),
    level_error_norm = mean_error + sd_error * z
  ) |>
  ungroup() |>
  mutate(
    squared_error_norm = level_error_norm^2
  )

# Save the normalized dataset for future use
saveRDS(df_normalized, file.path(clean_dir, "normalized_dataset.rds"))

df_norm_global <- final_dataset |>
  mutate(
    r = rank(level_error, ties.method = "first"),
    n = n(),
    p = (r - 0.5) / n,
    z = qnorm(p),
    level_error_norm = mean(level_error, na.rm = TRUE) + sd(level_error, na.rm = TRUE) * z,
    squared_error_norm = level_error_norm^2
  )

saveRDS(df_norm_global, file.path(clean_dir, "normalized_dataset_global.rds"))

# ==============================================================================
# 8. Cleanup intermediate files not used by 2.* scripts
# ==============================================================================

