# ==============================================================================
# TEXT NUMBERS FOR PAPER
# ==============================================================================
# OUTLINE:
# 1. ABSTRACT - 23% Cost Reduction Analysis
# 2. ABSTRACT - $2 Billion Per Hurricane Cost Reduction  
# 3. INTRODUCTION - Data Coverage Analysis (68% damages, 29% deaths)
# 4. INTRODUCTION - Protective Expenditures Results
# 5. INTRODUCTION - Ex Post Results
# 6. INTRODUCTION - Value of Improving the Forecast (VOI) Analysis
# 7. INTRODUCTION - Historical VOI Summary
# 8. BACKGROUND - Historical Forecast Improvement Analysis
# 9. DATA - Summary of Damages
# 10. METHODS AND RESULTS - Protective Expenditures
# 11. METHODS AND RESULTS - Ex Post Damage Details
# 12. METHODS AND RESULTS - VOI Details
# 13. METHODS AND RESULTS - Historical HFIP Value Summary
# 14. APPENDIX B2 - Non-Constant Variance Analysis
# 15. APPENDIX C3 - Track Error Analysis
# 16. APPENDIX C3 - CPRS
# ==============================================================================
gc()

# ==============================================================================
# 1. ABSTRACT - 23% Cost Reduction Analysis
# ==============================================================================

# Calculate the value of improving the forecast (VOI) - marginal effect of reducing forecast error
# This estimates how much a 1 standard deviation reduction in forecast error saves
mean_voi = fixest::feols(
      total_cost ~ squared_error:category + squared_error_precip + level_error + level_error_precip | 
              county + state^hurricane + wind_cut + precip_cut,
      df,
      cluster = c("county", "state^hurricane")
    ) |> 
      tidy() |> 
      filter(str_detect(term, "cane")) |>  # Filter for hurricane category effects
      pull(estimate)

# Process historical forecast error data to analyze improvements over time
his_error_wind = his_error |>
  mutate(
    year = as.integer(str_sub(`Date/Time`, 7, 10)),  # Extract year from date
    month = as.integer(str_sub(`Date/Time`, 4, 5))   # Extract month from date
  ) |>
  select(`Date/Time`, year, month, STMID, WS, `024hI01`, `048hI01`, `072hI01`) |>
  replace_with_na(list(`024hI01` = -9999, `048hI01` = -9999, `072hI01` = -9999)) |>  # Replace missing values
  mutate(
    `024hI01` = abs(`024hI01`),  # Take absolute value of forecast errors
    `048hI01` = abs(`048hI01`),
    `072hI01` = abs(`072hI01`)
  ) |>
  rename(
    avg_error_24 = `024hI01`,  # 24-hour forecast error
    avg_error_48 = `048hI01`,  # 48-hour forecast error
    avg_error_72 = `072hI01`   # 72-hour forecast error
  ) |>
  pivot_longer(
    cols = contains("avg"),
    names_to = "lead_day",
    values_to = "forecast_error"
  ) |>
  ungroup() |>
  mutate(forecast_error = forecast_error / 1.94384) |>  # Convert from knots to m/s (1.94 knots per m/s)
  group_by(year) |> 
  summarise(forecast_error = mean(forecast_error, na.rm = T)) |> 
  mutate(early = year < 2007)  # Mark pre-HFIP period (before 2007)

# Calculate the rate of forecast improvement before and after HFIP (Hurricane Forecast Improvement Program)
slopes = feols(log(forecast_error) ~ year:early | early, his_error_wind) |> 
  tidy() |>
  pull(estimate)
 
# Compute hurricane-specific avoided costs from HFIP improvements
hurricane_values = df |>
  drop_na(wind_fc, wind_fc_sd) |>  # Remove observations with missing forecast data
  filter(year >= 2007) |>  # Focus on post-HFIP period
  # Calculate actual vs counterfactual forecast uncertainty
  mutate(sd_actual = wind_fc_sd) |>
  # Counterfactual: what SD would have been without HFIP improvements
  mutate(sd_counterfactual = wind_fc_sd  * (1 + max(slopes))^((year-2007)) / (1 + min(slopes))^((year-2007))) |>
  mutate(
    sd_reduction = sd_counterfactual - sd_actual,  # Reduction in forecast uncertainty
    sd_reduction_sq = sd_counterfactual^2 - sd_actual^2  # Squared reduction for VOI calculation
    ) |>
  # Calculate value of forecast improvements using VOI coefficient
  mutate(sd_value_const = 
  case_when(
    category == "Tropical Storm" ~ (sd_counterfactual^2 - sd_actual^2) * 0,  # No value for tropical storms
    category == "Hurricane" ~ (sd_counterfactual^2 - sd_actual^2) * mean_voi,  # Apply VOI for hurricanes
    TRUE ~ 0
    )
    ) |>
  group_by(hurricane, year, month) |>
  summarise(
    sd_reduction = mean(sd_reduction),
    hurricane_value_const = sum(sd_value_const),  # Total value per hurricane
    total_cost = sum(total_cost)  # Total actual costs
  ) |>
  ungroup() |>
  mutate(
    agg_value_const = sum(hurricane_value_const),  # Total value across all hurricanes
    agg_cost = sum(total_cost)  # Total costs across all hurricanes
  ) |>
  mutate(
    damage_reduction_storm = hurricane_value_const / (total_cost + hurricane_value_const),  # Reduction per storm
    damage_reduction_const = agg_value_const / (agg_cost + agg_value_const)  # Overall reduction
    ) |>
  arrange(year, month, hurricane) |>
  mutate(ym = year * 100 + month) |>
  mutate(hurricane = paste0(year, " ", str_to_title(hurricane)))

# Calculate and display the overall cost reduction percentage
percent_reduction = hurricane_values |> 
    slice(1) |> 
    pull(damage_reduction_const) * 100
percent_reduction = round(percent_reduction)

# HFIP has led to a 23% reduction in total hurricane costs
cat("Total cost reduction since HFIP (2007) is", 
    format(percent_reduction, big.mark = ",", scientific = FALSE), 
    "%\n")

# ==============================================================================
# 2. ABSTRACT - $2 Billion Per Hurricane Cost Reduction
# ==============================================================================

# Recalculate VOI coefficient
mean_voi = fixest::feols(
      total_cost ~ squared_error:category + squared_error_precip + level_error + level_error_precip | 
              county + state^hurricane + wind_cut + precip_cut,
      df,
      cluster = c("county", "state^hurricane")
    ) |> 
      tidy() |> 
      filter(str_detect(term, "cane")) |>
      pull(estimate)

# Process historical error data (same as above)
his_error_wind = his_error |>
  mutate(
    year = as.integer(str_sub(`Date/Time`, 7, 10)),
    month = as.integer(str_sub(`Date/Time`, 4, 5))
  ) |>
  select(`Date/Time`, year, month, STMID, WS, `024hI01`, `048hI01`, `072hI01`) |>
  replace_with_na(list(`024hI01` = -9999, `048hI01` = -9999, `072hI01` = -9999)) |>
  mutate(
    `024hI01` = abs(`024hI01`),
    `048hI01` = abs(`048hI01`),
    `072hI01` = abs(`072hI01`)
  ) |>
  rename(
    avg_error_24 = `024hI01`,
    avg_error_48 = `048hI01`,
    avg_error_72 = `072hI01`
  ) |>
  pivot_longer(
    cols = contains("avg"),
    names_to = "lead_day",
    values_to = "forecast_error"
  ) |>
  ungroup() |>
  mutate(forecast_error = forecast_error / 1.94384) |>  # Convert knots to m/s
  group_by(year) |> 
  summarise(forecast_error = mean(forecast_error, na.rm = T)) |> 
  mutate(early = year < 2007)

# Calculate forecast improvement rates
slopes = feols(log(forecast_error) ~ year:early + early, his_error_wind) |> 
  tidy() |>
  slice(3, 4) |>
  pull(estimate)
 
# Calculate hurricane-specific values (same methodology as above)
hurricane_values = df |>
  drop_na(wind_fc, wind_fc_sd) |>
  filter(year >= 2007) |>
  mutate(sd_actual = wind_fc_sd) |>
  mutate(sd_counterfactual = wind_fc_sd  * (1 + max(slopes))^((year-2007)) / (1 + min(slopes))^((year-2007))) |>
  mutate(
    sd_reduction = sd_counterfactual - sd_actual,
    sd_reduction_sq = sd_counterfactual^2 - sd_actual^2
    ) |>
  mutate(sd_value_const = 
  case_when(
    category == "Tropical Storm" ~ (sd_counterfactual^2 - sd_actual^2) * 0,
    category == "Hurricane" ~ (sd_counterfactual^2 - sd_actual^2) * mean_voi,
    TRUE ~ 0
    )
    ) |>
  group_by(hurricane, year, month) |>
  summarise(
    sd_reduction = mean(sd_reduction),
    hurricane_value_const = sum(sd_value_const),
    total_cost = sum(total_cost)
  ) |>
  ungroup() |>
  mutate(
    agg_value_const = sum(hurricane_value_const),
    agg_cost = sum(total_cost)
  ) |>
  mutate(
    damage_reduction_storm = hurricane_value_const / (total_cost+hurricane_value_const),
    damage_reduction_const = agg_value_const / (agg_cost+agg_value_const)
    ) |>
  arrange(year, month, hurricane) |>
  mutate(ym = year * 100 + month) |>
  mutate(hurricane = paste0(year, " ", str_to_title(hurricane)))

# Calculate average dollar reduction per hurricane
dollar_reduction = hurricane_values |> 
    pull(hurricane_value_const) |>
    mean() / 1000  # Convert to billions
dollar_reduction = round(dollar_reduction)

# HFIP saves approximately $2 billion per hurricane on average
cat("Average damage reduction since HFIP (2007) is $", 
    format(dollar_reduction, big.mark = ",", scientific = FALSE), 
    " billion per hurricane.\n")

# ==============================================================================
# 3. INTRODUCTION - Data Coverage Analysis (68% damages, 29% deaths)
# ==============================================================================

# Load comprehensive damage data to assess coverage of our sample
df_ours = fread("data/raw/sheldus/all_events.csv") |>
  summarise(
    damage = sum(`PropertyDmg(ADJ 2019)`)/1e9,  # Total property damage in billions
    deaths = sum(Fatalities)  # Total fatalities
    )

# Load per-hurricane damage files as the numerator
df_all_hur = rbindlist(lapply(Sys.glob("data/raw/sheldus/damages_*.csv"), fread)) |>
  summarise(
    damage = sum(`PropertyDmg(ADJ 2019)`)/1e9,
    deaths = sum(Fatalities)
  )

# Calculate what percentage of total damages and deaths our sample represents
share_prop_damage = round(df_all_hur$damage / df_ours$damage * 100, 1)
share_fatalities = round(df_all_hur$deaths / df_ours$deaths * 100, 1)
 
# Our sample covers 68.3% of total property damages and 29.0% of fatalities
cat("Our data represents ", 
     format(share_prop_damage, big.mark = ",", scientific = FALSE), 
     " % of total property damages.\n")

cat("Our data represents ", 
    format(share_fatalities, big.mark = ",", scientific = FALSE), 
    " % of total fatalities.\n")

# ==============================================================================
# 4. INTRODUCTION - Protective Expenditures Results
# ==============================================================================

# Analyze how hurricane forecasts affect pre-storm protective spending
effect_by_lead_day = feols(
  ex_ante_spending/1e6 ~  # Dependent variable: protective expenditures in millions
    i(forecast_cut) + i(forecast_precip_cut) |  # Forecast wind and precipitation bins
    county + state^hurricane,  # Fixed effects
  data = load_df()
)

# Same analysis for GDP share
effect_by_lead_day_gdp = feols(
  ex_ante_spending_per_gdp ~
    i(forecast_cut) + i(forecast_precip_cut) |
    county + state^hurricane,
  data = load_df()
)

# Same analysis for per capita spending
effect_by_lead_day_pc = feols(
  ex_ante_spending_pc ~
    i(forecast_cut) + i(forecast_precip_cut) |
    county + state^hurricane,
  data = load_df()
)

# Extract coefficient for 30-35 m/s wind forecast (hurricane strength)
exante_coef_name = grep("forecast_cut::\\(30,35\\]", names(coef(effect_by_lead_day)), value = TRUE)

coef_exante <- coef(effect_by_lead_day)[exante_coef_name]
coef_exante_gdp <- coef(effect_by_lead_day_gdp)[exante_coef_name]
coef_exante_pc <- coef(effect_by_lead_day_pc)[exante_coef_name]

# Hurricane forecasts trigger $36 million in protective spending, 0.8% of GDP, $320 per capita
cat("The forecast of hurricane winds (30–35 m/s) increases emergency protective expenditures by $",
    format(round(coef_exante, 0), big.mark = ",", scientific = FALSE), "million,
    ", format(round(coef_exante_gdp, 3) * 100, big.mark = ",", scientific = FALSE), "% of county GDP share, or $",
    format(round(coef_exante_pc, 0), big.mark = ",", scientific = FALSE), "per capita.\n"
    )

# ==============================================================================
# 5. INTRODUCTION - Ex Post Results
# ==============================================================================

# Analyze how forecast errors affect post-storm damages
reg_dam = feols(
  damages/1e6 ~ i(wind_error_bin, ref = "(-2,0]")  # Wind forecast error bins
  |
    county + state^hurricane + wind_cut + precip_cut + precip_error_bin,  # Fixed effects
  data = load_df()
)

# Same analysis for per capita damages
reg_dam_pc = feols(
  damage_pc ~ i(wind_error_bin, ref = "(-2,0]")
  |
    county + state^hurricane + wind_cut + precip_cut + precip_error_bin,
  data = load_df()
)

# Same analysis for GDP share of damages
reg_dam_share = feols(
  damage_per_gdp ~ i(wind_error_bin, ref = "(-2,0]")
  |
    county + state^hurricane + wind_cut + precip_cut + precip_error_bin,
  data = load_df()
)

# Analyze how forecast errors affect recovery spending
reg_spend = feols(
  ex_post_spending/1e6 ~ i(wind_error_bin, ref = "(-2,0]")
  |
    county + state^hurricane + wind_cut + precip_cut + precip_error_bin,
  data = load_df()
)

# Same analysis for GDP share of recovery spending
reg_spend_gdp = feols(
  ex_post_spending_per_gdp ~ i(wind_error_bin, ref = "(-2,0]")
  |
    county + state^hurricane + wind_cut + precip_cut + precip_error_bin,
  data = load_df()
)

# Same analysis for per capita recovery spending
reg_spend_pc = feols(
  ex_post_spending_pc ~ i(wind_error_bin, ref = "(-2,0]")
  |
    county + state^hurricane + wind_cut + precip_cut + precip_error_bin,
  data = load_df()
)

# Extract coefficient for 8-10 m/s forecast underestimate
expost_coef_name <- grep("wind_error_bin::\\(8,10\\]", names(coef(reg_dam)), value = TRUE)

coef_expost <- coef(reg_dam)[expost_coef_name]
coef_expost_spend <- coef(reg_spend)[expost_coef_name]
coef_expost_gdp <- coef(reg_dam_share)[expost_coef_name]
coef_expost_pc <- coef(reg_dam_pc)[expost_coef_name]

# Forecast underestimates cause $177 million in excess damages
cat("A forecast underestimate of 8-10 m/s results in $",
    round(coef_expost, 0), "million USD in excess damages.\n")

# Forecast underestimates also trigger $23 million in recovery spending
cat("A forecast underestimate of 8-10 m/s results in $",
    format(round(coef_expost_spend, 0), big.mark = ",", scientific = FALSE), "million in recovery expenditures,
    ", format(round(coef_expost_gdp, 2) * 100, big.mark = ",", scientific = FALSE), "excess damage as % of county GDP, or $",
    format(round(coef_expost_pc, 0), big.mark = ",", scientific = FALSE), "per capita.\n")

# ==============================================================================
# 6. INTRODUCTION - Value of Improving the Forecast (VOI) Analysis
# ==============================================================================

# Calculate the value of improving forecast accuracy
reg_voi = feols(
  total_cost ~ squared_error:category + squared_error_precip + level_error + level_error_precip | 
    county + state^hurricane + wind_cut + precip_cut,  # Fixed effects
  data = load_df()
)

# Same analysis for GDP share
reg_voi_gdp = feols(
  total_cost_per_gdp*100 ~ squared_error:category + squared_error_precip + level_error + level_error_precip | 
    county + state^hurricane + wind_cut + precip_cut,
  data = load_df()
)

# Same analysis for per capita costs
reg_voi_pc = feols(
  total_cost_pc ~ squared_error:category + squared_error_precip + level_error + level_error_precip | 
    county + state^hurricane + wind_cut + precip_cut,
  data = load_df()
)

# Extract VOI coefficient for hurricanes
voi_coef_name <- grep("squared_error:categoryHurricane", names(coef(reg_voi)), value = TRUE)

coef_voi <- coef(reg_voi)[voi_coef_name]
coef_voi_gdp <- coef(reg_voi_gdp)[voi_coef_name]
coef_voi_pc <- coef(reg_voi_pc)[voi_coef_name]

# Each unit reduction in forecast error saves $5.16 million in total costs
cat("The marginal reduction in forecast's squared wind speed error reduces county 
    total protective expenditures, damages, and recovery expenditures by $",
    format(round(coef_voi, 1), big.mark = ",", scientific = FALSE), "million,
    ",format(round(coef_voi_gdp, 2), big.mark = ",", scientific = FALSE), "% of county GDP, or $",
    format(round(coef_voi_pc, 0), big.mark = ",", scientific = FALSE), "per capita, per hurricane.\n")

# ==============================================================================
# 7. INTRODUCTION - Historical VOI Summary
# ==============================================================================

# Calculate average forecast standard deviation
mean_sd = mean(df$wind_fc_sd)

# Calculate total value of forecast improvements
sample_value = round(2 * as.numeric(coef_voi) * mean_sd, 0)

# Historical value of forecast improvements is $14.6 million per county per hurricane
cat("The total value of improving the hurricane wind speed forecast is $", 
    format(sample_value, big.mark = ",", scientific = FALSE), 
    "million when evaluated across all years\n")

cat("Total cost reduction since HFIP (2007)  is", 
    format(percent_reduction, big.mark = ",", scientific = FALSE), 
    "%\n")

cat("Average damage reduction since HFIP (2007) is $", 
    format(dollar_reduction, big.mark = ",", scientific = FALSE), 
    "billion per hurricane.\n")

# ==============================================================================
# 8. BACKGROUND - Historical Forecast Improvement Analysis
# ==============================================================================

# Re-process historical error data for background section
his_error_wind = his_error |>
  mutate(
    year = as.integer(str_sub(`Date/Time`, 7, 10)),
    month = as.integer(str_sub(`Date/Time`, 4, 5))
  ) |>
  select(`Date/Time`, year, month, STMID, WS, `024hI01`, `048hI01`, `072hI01`) |>
  replace_with_na(list(`024hI01` = -9999, `048hI01` = -9999, `072hI01` = -9999)) |>
  mutate(
    `024hI01` = abs(`024hI01`),
    `048hI01` = abs(`048hI01`),
    `072hI01` = abs(`072hI01`)
  ) |>
  rename(
    avg_error_24 = `024hI01`,
    avg_error_48 = `048hI01`,
    avg_error_72 = `072hI01`
  ) |>
  pivot_longer(
    cols = contains("avg"),
    names_to = "lead_day",
    values_to = "forecast_error"
  ) |>
  ungroup() |>
  mutate(forecast_error = forecast_error / 1.94384) |>  # Convert knots to m/s
  group_by(year) |> 
  summarise(forecast_error = mean(forecast_error, na.rm = T)) |> 
  mutate(post_hfip = year >= 2007)  # Mark post-HFIP period

# Calculate absolute and percentage improvements
slopes_ms = feols(forecast_error ~ year:post_hfip + post_hfip, his_error_wind) |> 
  tidy() |>
  slice(3, 4) |>
  pull(estimate)

slopes_pctg = feols(log(forecast_error) ~ year:post_hfip + post_hfip, his_error_wind) |> 
  tidy() |>
  slice(3, 4) |>
  pull(estimate)

# HFIP dramatically accelerated forecast improvement rates: 0.03 m/s (0.4%) before vs 0.21 m/s (3.4%) after
cat("Average progress in wind error reduction before HFIP (2007) was", 
    format(round(-slopes_ms[1],2), big.mark = ",", scientific = FALSE), 
    "m/s per year, or",
    format(round(-slopes_pctg[1]*100,1), big.mark = ",", scientific = FALSE),"%")

cat("Average progress in wind error reduction after HFIP (2007) was", 
    format(round(-slopes_ms[2],2), big.mark = ",", scientific = FALSE), 
    "m/s per year, or",
    format(round(-slopes_pctg[2]*100,1), big.mark = ",", scientific = FALSE),"%")

# ==============================================================================
# 9. DATA - Summary of Damages 
# ==============================================================================


total_damages = df |> 
  # Optional filter: keep affected counties only
  group_by(hurricane, county) |> 
  mutate(
    wind_obs = max(wind_obs),
    precip_obs = max(precip_obs)
  ) |> 
  ungroup() |> 
  arrange(hurricane, year, county) |> 
  distinct(
    year, hurricane, state, county,
    wind_obs, wind_fc, wind_error,
    precip_obs, precip_fc, precip_error,
    damages, deaths, ex_post_spending, ex_ante_spending
  ) |> 
  group_by(hurricane, year) |> 
  summarise(
    max_wind_speed = sprintf("%.2f", mean(wind_obs)),
    max_wind_error = sprintf("%.2f", mean(abs(wind_error))),
    max_prec = sprintf("%.2f", mean(precip_obs)),
    max_prec_error = sprintf("%.2f", mean(abs(precip_error))),
    sum_damage = sprintf("%.2f", sum(damages) / 10^9),
    ex_ante_spending = sprintf("%.2f", sum(ex_ante_spending) / 10^9),
    ex_post_spending = sprintf("%.2f", sum(ex_post_spending) / 10^9)
  ) |> 
  arrange(year) |> 
  mutate(hurricane = str_to_title(hurricane)) |> 
  rename(
    Hurricane = hurricane,
    Year = year,
    `Wind Speed` = max_wind_speed,
    `Wind Speed Error` = max_wind_error,
    `Precipitation` = max_prec,
    `Precipitation Error` = max_prec_error,
    `Total Damage` = sum_damage,
    `Protective Exp.` = ex_ante_spending,
    `Recovery Exp.` = ex_post_spending
  ) |> ungroup() |>  mutate(`Total Damage` = as.numeric(`Total Damage`), `Protective Exp.` = as.numeric(`Protective Exp.`), `Recovery Exp.` = as.numeric(`Recovery Exp.`)) |> summarise(damage = sum(`Total Damage`), protective = sum(`Protective Exp.`), recovery = sum(`Recovery Exp.`))

# Total damages are $300 billion

cat("Total damages  are $",
    format(round(total_damages$damage, 0), big.mark = ",", scientific = FALSE), "billion.\n")

# ==============================================================================
# 10. METHODS AND RESULTS - Protective Expenditures
# ==============================================================================

# Calculate total protective expenditures and expenditures to low-wind counties
protective_allocation = df |>
  filter(!is.na(ex_ante_spending) & ex_ante_spending > 0) |>  # Only counties with protective spending
  mutate(
    low_wind = wind_obs < 20,  # Counties with realized wind speeds below 20 m/s
    high_wind = wind_obs >= 20  # Counties with realized wind speeds 20 m/s or above
  ) |>
  summarise(
    total_protective_spending = sum(ex_ante_spending, na.rm = TRUE),  # Total protective spending
    low_wind_spending = sum(ex_ante_spending * low_wind, na.rm = TRUE),  # Expenditures in low-wind counties
    high_wind_spending = sum(ex_ante_spending * high_wind, na.rm = TRUE),  # Expenditures in high-wind counties
    low_wind_share = low_wind_spending / total_protective_spending * 100,  # Percentage to low-wind counties
    high_wind_share = high_wind_spending / total_protective_spending * 100  # Percentage to high-wind counties
  )

# Extract the key statistic
low_wind_percentage = round(protective_allocation$low_wind_share, 1)

# About 17.6% of protective expenditures go to counties with realized wind speeds below 20 m/s
cat("About ", 
    format(low_wind_percentage, big.mark = ",", scientific = FALSE), 
    "% of protective expenditures go to counties with realized wind speeds below 20 m/s.\n")

# Report detailed protective expenditures effects for different wind speed ranges
cat("The forecast of hurricane winds (30–35 m/s) increases emergency protective expenditures by $",
    format(round(coef_exante, 1), big.mark = ",", scientific = FALSE), "million.\n")

# Calculate effect for tropical storm winds (20-25 m/s)
ts_exante_coef_name = grep("forecast_cut::\\(20,25\\]", names(coef(effect_by_lead_day)), value = TRUE)
ts_coef_exante <- coef(effect_by_lead_day)[ts_exante_coef_name]

# Tropical storm forecasts trigger $4.3 million in protective spending
cat("The forecast of tropical storm winds (20–25 m/s) increases emergency protective expenditures by $",
    format(round(ts_coef_exante, 1), big.mark = ",", scientific = FALSE), "million.\n")

# Calculate effect for 25-30 m/s winds
twentyfive_exante_coef_name = grep("forecast_cut::\\(25,30\\]", 
                               names(coef(effect_by_lead_day)), value = TRUE)
twentyfive_coef_exante_gdp <- coef(effect_by_lead_day_gdp)[twentyfive_exante_coef_name]
twentyfive_coef_exante_pc <- coef(effect_by_lead_day_pc)[twentyfive_exante_coef_name]

# 25-30 m/s forecasts trigger 0.1% of GDP or $62.7 per capita
cat("The forecast of 25–30 m/s winds increases emergency protective expenditures by ",
    format(round(twentyfive_coef_exante_gdp , 3) * 100, big.mark = ",", scientific = FALSE),
    "% of county GDP share or $", round(twentyfive_coef_exante_pc , 1),
    "per capita.\n")

# 30-35 m/s forecasts trigger 0.8% of GDP or $319.8 per capita
cat("The forecast of 30–35 m/s winds increases emergency protective expenditures by ",
    format(round(coef_exante_gdp , 3) * 100, big.mark = ",", scientific = FALSE),
    "% of county GDP share or $", round(coef_exante_pc , 1),
    "per capita.\n")

# ==============================================================================
# 11. METHODS AND RESULTS - Ex Post Damage Details
# ==============================================================================

# Analyze damage effects for different forecast error magnitudes
six_expost_coef_name <- grep("wind_error_bin::\\(4,6\\]", names(coef(reg_dam)), value = TRUE)
six_coef_expost <- coef(reg_dam)[six_expost_coef_name]

# 4-6 m/s underestimates cause $32.8 million in damages vs $177.4 million for 10 m/s underestimates
cat("Relative to a perfect forecast, underestimates by 4-6 m/s increase county damages by $",
    round(six_coef_expost, 1), "million, 
    while underestimates of 10 m/s increase them by $",
    round(coef_expost, 1),
    "million.\n")

# Calculate frequency of large forecast errors
share_over_10 <- mean(df$wind_error >= 8, na.rm = TRUE)
# Only 1.7% of forecast errors exceed 8 m/s
cat("Share of wind forecast errors over 8 m/s:", round(100 * share_over_10, 1), "%\n")

# Calculate GDP and per capita effects for damages
coef_dam_gdp <- coef(reg_dam_share)[expost_coef_name]
coef_dam_pc <- coef(reg_dam_pc)[expost_coef_name]

# 8-10 m/s underestimates cause 9.88% of GDP or $3,872.1 per capita in damages
cat("Relative to a perfect forecast, underestimates by 8-10 m/s increase county damages by $",
    round(coef_dam_gdp*100, 2), "% share of county GDP, 
    or $",
    round(coef_dam_pc, 1),
    "per capita.\n")

# Analyze recovery expenditures effects
six_expost_coef_name <- grep("wind_error_bin::\\(4,6\\]", names(coef(reg_dam)), value = TRUE)
six_coef_expost_spend <- coef(reg_spend)[six_expost_coef_name]

# 4-6 m/s underestimates cause $9.3 million in recovery expenditures vs $22.8 million for 10 m/s
cat("Relative to a perfect forecast, underestimates by 6 m/s increase county damages by $",
    format(round(six_coef_expost_spend, 1), big.mark = ",", scientific = FALSE), "million, 
    while underestimates of 10 m/s increase them by $",
    format(round(coef_expost_spend, 1), big.mark = ",", scientific = FALSE),
    "million.\n")

# Calculate GDP and per capita effects for recovery spending
coef_expost_spend_gdp <- coef(reg_spend_gdp)[expost_coef_name]
coef_expost_spend_pc <- coef(reg_spend_pc)[expost_coef_name]

# 8-10 m/s underestimates cause 0.54% of GDP or $280.3 per capita in recovery spending
cat("Relative to a perfect forecast, underestimates by 8-10 m/s increase county damages by $",
    round(coef_expost_spend_gdp*100, 2), "% share of county GDP, 
    or $",
    round(coef_expost_spend_pc, 1),
    "per capita.\n")

# ==============================================================================
# 12. METHODS AND RESULTS - VOI Details
# ==============================================================================

# Report forecast uncertainty statistics
# Sample wind speed forecast standard deviation is 1.41 m/s
cat("The sample wind speed forecast standard deviation is", 
    format(round(mean_sd,2), big.mark = ",", scientific = FALSE), 
    "m/s\n.")

# Total value of improving the forecast is $14.6 million per county per hurricane
cat("The total value of improving the hurricane wind speed forecast is $", 
    format(sample_value, big.mark = ",", scientific = FALSE), 
    "million per county per hurricane when evaluated across all years\n")

# Calculate yearly improvement in forecast accuracy
slope_sd <- feols(wind_fc_sd ~ year, df) |> 
  tidy() |>
  slice(2) |>
  pull(estimate)

# Yearly reduction in forecast standard deviation is 0.046 m/s (3.3% of panel sd)
cat("The yearly reduction in wind speed forecast standard deviation is", 
    format(-round(slope_sd, 3), big.mark = ",", scientific = FALSE), 
    "m/s.\n")

cat("The yearly reduction in wind speed forecast standard deviation is", 
    format(-round(slope_sd/mean_sd * 100, 1), big.mark = ",", scientific = FALSE), 
    "% of the panel sd.\n")

# Calculate yearly VOI per county
yearly_voi = - as.numeric(2 * slope_sd * coef_voi) * 1e6

# Yearly VOI is $477,982 per county
cat("The yearly reduction in wind speed forecast standard deviation is $", 
    format(round(yearly_voi), big.mark = ",", scientific = FALSE), 
    "per county.\n")

# Calculate yearly VOI in GDP and per capita terms
yearly_voi_gdp = - as.numeric(2 * slope_sd * coef_voi_gdp) 
yearly_voi_pc = - as.numeric(2 * slope_sd * coef_voi_pc)

# Yearly VOI is 0.03% of GDP or $10.56 per capita
cat("The yearly reduction in wind speed forecast standard deviation is ", 
    format(round(yearly_voi_gdp, 2), big.mark = ",", scientific = FALSE), 
    "% of county GDP or $",
    format(round(yearly_voi_pc, 2), big.mark = ",", scientific = FALSE),
    "per capita.\n")

# ==============================================================================
# 13. METHODS AND RESULTS - Historical HFIP Value Summary
# ==============================================================================

slope_sd_log <- feols(log(wind_fc_sd) ~ year, df) |> 
  tidy() |>
  slice(2) |>
  pull(estimate)

# Summarize historical improvements and value
cat("Average progress in wind error reduction since the first hurricane was", 
    format(round(-slope_sd_log * 100,1), big.mark = ",", scientific = FALSE),
    "%.")

cat("Average progress in wind error reduction before HFIP (2007) was", 
    format(round(-slopes_ms[1],2), big.mark = ",", scientific = FALSE), 
    "m/s per year, or",
    format(round(-slopes_pctg[1]*100,1), big.mark = ",", scientific = FALSE),"%")

cat("Average progress in wind error reduction after HFIP (2007) was", 
    format(round(-slopes_ms[2],2), big.mark = ",", scientific = FALSE), 
    "m/s per year, or",
    format(round(-slopes_pctg[2]*100,1), big.mark = ",", scientific = FALSE),"%")

cat("Total cost reduction since HFIP (2007)  is", 
    format(percent_reduction, big.mark = ",", scientific = FALSE), 
    "%\n")

cat("Average damage reduction since HFIP (2007) is $", 
    format(dollar_reduction, big.mark = ",", scientific = FALSE), 
    "billion per hurricane.\n")

# ==============================================================================
# 14. APPENDIX B2 - Non-Constant Variance Analysis
# ==============================================================================

coef_voi = fixest::feols(
  total_cost ~ squared_error:category + squared_error_precip + level_error + level_error_precip | 
    county + state^hurricane + wind_cut + precip_cut,
  load_df(),
  cluster = c("county", "state^hurricane")
) |> 
  tidy() |> 
  filter(str_detect(term, "cane")) |>
  pull(estimate)

linear_value_aggregate <- load_df() |> 
  filter(category == "Hurricane", wind_fc > 0) |> 
  mutate(
    marginal_voi = 2 * wind_fc * wind_fc_sd * mean_voi 
  ) |>
  group_by(year) |>
  summarize(total_value = sum(marginal_voi)) |>
  ungroup() |>
  summarize(mean_value = mean(total_value))

constant_value_aggregate <- load_df() |> 
  filter(category == "Hurricane", wind_fc > 0) |> 
  mutate(
    marginal_voi = 2 * wind_fc_sd * mean_voi 
  ) |>
  group_by(year) |>
  summarize(total_value = sum(marginal_voi)) |>
  ungroup() |>
  summarize(mean_value = mean(total_value))

# Compute annualized value of reducing sigma under constant-variance model
cat("The average yearly value of a 1-unit reduction in sigma_slope is $", round(linear_value_aggregate$mean_value/1e3, 2), "billion")

cat("The average value of a 1-unit reduction in sigma (constant-variance model) is $", 
    round(constant_value_aggregate$mean_value/1e3, 2), "billion.\n")

# Adjust for how sigma_slope translates into sigma depending on forecast intensity
df_hur <- load_df() |> 
  filter(category == "Hurricane", wind_fc > 0) 

mean_speed = mean(df_hur$wind_fc, na.rm = TRUE)
median_speed = median(df_hur$wind_fc, na.rm = TRUE)
mean_sd = mean(df_hur$wind_fc_sd, na.rm = TRUE)

# \Delta \sigma = \Delta \bar{\sigma} + \Delta \sigma_{slope} \tilde{x}
# \Delta \sigma = 0 + \Delta \sigma_{slope} \tilde{x}
# -1 = \Delta \sigma_{slope} \tilde{x}
# \sigma_{slope} = -1 / \tilde{x}
delta_slope_mean = 1 / mean_speed
delta_slope_median = 1 / median_speed
delta_slope_marginal = 1

linear_value_mean = delta_slope_mean * linear_value_aggregate$mean_value
linear_value_median = delta_slope_median * linear_value_aggregate$mean_value
linear_value_marginal = delta_slope_marginal * linear_value_aggregate$mean_value

cat("The average value of a 1-unit reduction in sigma at the mean forecast intensity via reducing sigma_slope is $", round(linear_value_mean/1e3, 2), "billion")
cat("The average value of a 1-unit reduction in sigma at the median forecast intensity via reducing sigma_slope is $", round(linear_value_median/1e3, 2), "billion")
cat("The average value of a 1-unit reduction in sigma at 1 m/s forecast intensity via reducing sigma_slope is $", round(linear_value_marginal/1e3, 2), "billion")

# ==============================================================================
# 15. APPENDIX C3 - Track Error Analysis
# ==============================================================================

df_track <- load_df_track()

# Calculate VOI including track errors
track_coef_voi = feols(
  total_cost_pc ~ squared_error + squared_error_precip + squared_error_track:category + level_error + level_error_precip + level_error_track | 
    county + state^hurricane + wind_cut + precip_cut + track_cut,  # Include track fixed effects
  data = load_df_track()
) |> 
  tidy() |> 
  filter(str_detect(term, "cane")) |>
  pull(estimate)

# Calculate track error statistics
mean_track_sd = round(sd(df_track$level_error_track),1)
sample_track_voi = round(2 * as.numeric(track_coef_voi) * mean_track_sd, 2)

cat("The restricted sample wind forecast standard deviation is",
    mean_track_sd, "km.\n")

cat("The restricted sample value of improving the wind forecast is $",
    sample_track_voi, "million per person per county per hurricane.\n")

# Calculate wind VOI conditional on track errors
coef_voi_track_sample = feols(
  total_cost ~ squared_error:category + squared_error_precip + squared_error_track + level_error + level_error_precip + level_error_track | 
    county + state^hurricane + wind_cut + precip_cut + track_cut,
  data = load_df_track()
) |> 
  tidy() |> 
  filter(str_detect(term, "cane")) |>
  pull(estimate)

mean_sd_track_sample = round(mean(df_track$wind_fc_sd),1)
wind_voi_track_sample = round(2 * as.numeric(coef_voi) * mean_sd_track_sample, 2)

cat("The restricted sample wind forecast standard deviation is",
    mean_sd_track_sample, "m/s.\n")

cat("The restricted sample value of improving the wind forecast is $",
    wind_voi_track_sample, "million per county per hurricane.\n")

# ==============================================================================
# 16. APPENDIX C3 - CPRS
# ==============================================================================

# CRPS for wind speed errors
y = df$wind_error
mu_hat    = mean(y, na.rm = TRUE)
sigma_hat = sd(y, na.rm = TRUE)

# Calculate CRPS for normal distribution assumption
crps_vals = crps_norm(
  y,
  location = mu_hat,
  scale    = sigma_hat
)

# Oracle CRPS function for empirical distribution
oracle_crps = function(y) {
  n = length(y)
  if (n < 2L) stop("Need at least two observations.")
  
  x_sorted = sort(y)                     # O(n log n)
  weights  = 2 * seq_len(n) - n - 1      # (2i - n - 1) for i = 1..n
  sum(x_sorted * weights) / (n * (n - 1))
}

# Calculate average CRPS values
mean_crps = mean(crps_vals, na.rm = TRUE)
crps_oracle = oracle_crps(df$wind_error[!is.na(df$wind_error)])
sd_y = sd(df$wind_error, na.rm = TRUE)

# Calculate the difference and compare to standard deviation
crps_difference = mean_crps - crps_oracle
sd_ratio = crps_difference / sd_y

# Report results
cat("The average CRPS when comparing against the normal distribution is", 
    format(round(mean_crps, 2), big.mark = ",", scientific = FALSE), 
    "m/s.\n")

cat("The average CRPS of the actual empirical error distribution is", 
    format(round(crps_oracle, 2), big.mark = ",", scientific = FALSE), 
    "m/s.\n")

cat("The difference between the two is", 
    format(round(crps_difference, 2), big.mark = ",", scientific = FALSE), 
    "m/s, which is", 
    format(round(sd_ratio, 2), big.mark = ",", scientific = FALSE),
    "standard deviations.\n")

cat("The standard deviation of wind errors is", 
    format(round(sd_y, 2), big.mark = ",", scientific = FALSE), 
    "m/s.\n")
