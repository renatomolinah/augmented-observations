cat("================================================================================\n")
cat("RUNNING: 1.02.merge_data.r\n")
cat("PURPOSE: Merge denial forecasts with county geography and VOI panel\n")
cat("================================================================================\n")

#=============================
# 1. load county/state data
#=============================

counties_map = get_urbn_map("counties", sf = TRUE) |>
  st_set_crs(4326)

counties_sf <- get_urbn_map("counties", sf = FALSE) |> 
  distinct(county_name, state_name, state_fips, fips_class, county_fips)

county_lat_lon = geoCounty |> 
  select(county = fips, lon, lat)
  
#=============================
# 2. pull tidycensus
#=============================

county_demos = fread("data/clean/county_demographics.csv") |>
  as_tibble() |>
  mutate(county = str_pad(county, 5, 'left', '0'))

#=============================
# 3. load cleaned data
#=============================
x = fread("data/clean/forecast_data.csv")
weather = fread("data/clean_denial/forecast_denial_data.csv") |>
  mutate(date = as.Date(date)) |>
  mutate(county_fips = str_pad(county, 5, 'left', '0'))

fema_spending = fread("data/clean/fema_spending.csv") |>
  mutate(county_fips = str_pad(fips, 5, 'left', '0')) |>
  select(-fips)

data_sheldus = fread("data/clean/data_sheldus.csv") |>
  mutate(county_fips = str_pad(county_fips, 5, 'left', '0')) |> 
  group_by(year, month, county_fips, state_fips) |> 
  summarise(
    across(
      crop_damage:deaths_pc, 
      ~ sum(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  ) |> 
  ungroup()

income_gdp = fread("data/clean/income_gdp.csv") |>
  mutate(
    county_fips = str_pad(county_fips, 5, 'left', '0'),
    across(year:gdp, ~ suppressWarnings(as.numeric(.x))))

#=============================
# 4. prep forecast data
#=============================

# these are counties with forecasts
data_weather = weather |>
  select(-county, -geometry) |>
  left_join(counties_map |> select(-county_name), by = "county_fips") |>
  mutate(
    state_name = str_to_lower(state_name),
    county_name = str_to_lower(county_name),
    year = year(date),
    month = month(date)
  ) |>
  as_tibble() |>
  select(
    hurricane, date, year, month, lead_hours, state_fips, state_name, county_fips, county_name, geometry, augmented_obs,
    starts_with(c("gust_dur", "sust_dur", "prec", "vmax"))
  )

data_date_hurr = weather |>
  distinct(date, lead_hours, hurricane)

# join in counties forecast to have 0 impact that are not in our forecast dataset
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
    gust_dur_dif_mean = 0, gust_dur_dif_sd = 0, gust_dur_obs = 0, gust_dur_pred_mean = 0, gust_dur_pred_sd = 0,
    sust_dur_dif_mean = 0, sust_dur_dif_sd = 0, sust_dur_obs = 0, sust_dur_pred_mean = 0, sust_dur_pred_sd = 0,
    prec_dif_mean = 0, prec_dif_sd = 0, prec_obs = 0, prec_pred_mean = 0, prec_pred_sd = 0, 
    vmax_gust_dif_mean = 0, vmax_gust_dif_sd = 0, vmax_gust_obs = 0, vmax_gust_pred_mean = 0, vmax_gust_pred_sd = 0,
    vmax_sust_dif_mean = 0, vmax_sust_dif_sd = 0, vmax_sust_obs = 0, vmax_sust_pred_mean = 0, vmax_sust_pred_sd = 0,
    augmented_obs = NA # Indicate with NA that these are imputations
  ) |>
  select(
    hurricane, date, year, month, lead_hours, state_fips, state_name, county_fips, county_name, geometry, augmented_obs,
    starts_with(c("gust_dur", "sust_dur", "prec", "vmax"))
  )

data_weather_final = rbind(data_weather, data_unaffected_counties) |>
  arrange(date, county_fips)

# #=============================
# # 5. merge datasets
# #=============================

# join forecast and sheldus data
data_weather_dmg_en = full_join(
  data_weather_final,
  data_sheldus |>
    select(-state_fips) |>
    mutate(county_fips = str_pad(county_fips, 5, 'left', '0')),
  by = c("year", "month", "county_fips")
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
    deaths_pc = 0
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
    value_deaths = 9.39 * 10^6 * deaths,
    value_deaths_pc = 9.39 * 10^6 * deaths_pc,
    dmg_prop_death = property_damage_2019 + value_deaths + crop_damage_2019,
    dmg_prop_death_pc = property_damage_pc_2019 + value_deaths_pc + crop_damage_pc_2019
  ) |>
  filter(!is.na(hurricane)) |>
  replace_na(replace = list(precip_obs = -999, precip_fc = -999, 
                                 precip_error = -999, prec_pred_sd = -999)
                                 ) |>
  select(-geometry) |>
  select(
    hurricane, date, year, month, augmented_obs,
    state = state_fips, county = county_fips,
    wind_obs, wind_fc, wind_error, wind_fc_sd = vmax_sust_pred_sd,
    precip_obs, precip_fc, precip_fc_sd = prec_pred_sd, precip_error,
    gust_obs, gust_fc, gust_error, gust_fc_sd = vmax_gust_pred_sd,
    dur_obs, dur_fc, dur_error, dur_fc_sd = sust_dur_pred_sd,
    damages = dmg_prop_death, damage_pc = dmg_prop_death_pc, deaths, 
    property_damage = property_damage_2019, 
    crop_damage = crop_damage_2019, mortality_damage = value_deaths
  )

saveRDS(data_weather_dmg_en, "data/clean_denial/ncei_compare_denial.rds")

# merge in FEMA
final_dataset = left_join(
    data_weather_dmg_en, 
    fema_spending, 
    by = c("hurricane", "county" = "county_fips")
  ) |> # 64 rows only in fema data: county fips starts with 72
  replace_na(list(ex_post_spending = 0, ex_ante_spending = 0))

final_dataset = final_dataset |> filter(year <= 2022)

# merge in bad forecasts
final_dataset = final_dataset |> 
  mutate(date = as.character(date)) |>
  distinct() |>
  inner_join(county_demos)

# merge in county gdp/income
final_dataset = final_dataset |>
  # drops are all virginia cities or alaska counties without GDP
  inner_join(income_gdp, by = c("county" = "county_fips", "year" = "year"))

# Add calculated variables
final_dataset = final_dataset |> 
  arrange(augmented_obs, county, year) |> 
  group_by(augmented_obs, county) |> 
  mutate(last_error = lag((wind_fc - wind_obs))) |>
  as_tibble() |> 
  mutate(log_damage = log(damages + 1),
        log_wind = log(wind_obs + 1),
        log_precip = ifelse(precip_obs < 0, NA, log(precip_obs + 1)),
        log_wind_fc = log(wind_fc + 1),
        log_precip_fc = ifelse(precip_fc < 0, NA, log(precip_fc + 1)),
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
  left_join(county_lat_lon) |>
  drop_na(lat, lon, precip_obs) |>
  ungroup()


## winsorize upper and lower bins for errors, forecast, realizations
## because of sparse data in those bins
# so the error variable isn't actually bounded itself
final_dataset = final_dataset |> 
  mutate(
    var = ifelse(wind_error < -10, -10, wind_error),
    var = ifelse(wind_error > 10, 10, wind_error)
  ) |>
  # wind error bins
  mutate(wind_error_bin = cut(var, seq(-10, 10, 2), include.lowest = TRUE)) |>
  ungroup() |>
  mutate(
    var = ifelse(precip_error < -200, -200, precip_error),
    var = ifelse(precip_error > 200, 200, precip_error)
  ) |>
  # precip error bins
  mutate(precip_error_bin = cut(var, seq(-200, 200, 40), include.lowest = TRUE)) |>
  ungroup() |>
  mutate(
    date = as.numeric(as_date(date)),
    county = as.numeric(county)
    ) |>
  # so the error variable isn't actually bounded itself
  mutate(
    var = ifelse(wind_fc > 40, 40, wind_fc)
  ) |>
  # wind forecast bins
  mutate(forecast_cut = cut(var, seq(0, 40, 5), include.lowest = TRUE)) |>
  # so the error variable isn't actually bounded itself
  mutate(
    var = ifelse(precip_fc > 200, 200, precip_fc)
  ) |>
  # precip forecast bins
  mutate(forecast_precip_cut = cut(var, seq(0, 200, 20), include.lowest = TRUE))
  
  
  # so the obs variable isn't actually bounded itself
  # wind observation bins
  # NOTE FROM DIEGO: Quantiles are being defined separately for augmented vs non-augmented obs. Might need to redo this and enforce quantiles based on all observations only. REVISIT THIS.
  final_dataset = final_dataset |> 
  group_by(augmented_obs, wind_obs > 0) |>
  mutate(wind_cut = ntile(wind_obs, 20)) |>
  ungroup() |>
  mutate(wind_cut = ifelse(wind_obs == 0, 0, wind_cut)) |>
  # precip observation bins
  mutate(precip_cut = ntile(precip_obs, 20)) |>
  mutate(dur_cut = ntile(dur_obs, 20)) |>
  group_by(augmented_obs, wind_cut, hurricane) |>
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
      )

final_dataset = final_dataset |>
  arrange(county, year, month) |>
  group_by(year, month, hurricane) |>
  mutate(hurricane_id = cur_group_id()) |>
  ungroup()

saveRDS(final_dataset, "data/clean_denial/final_dataset_denial.rds")
fwrite(final_dataset, "data/clean_denial/final_dataset_denial.csv")
