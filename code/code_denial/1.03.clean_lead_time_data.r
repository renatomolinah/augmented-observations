print("Cleaning lead time data with cleaned column names.")

#=============================
# 1. Load packages and data
#=============================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, tidyverse, here, lubridate)

# Load the disaggregated forecast data (with lead times)
forecast_lead_times <- fread("data/clean_denial/forecast_denial_data_with_lead_times.csv") |>
  mutate(date = as.Date(date))

# Check what columns we have
print("Columns in original data:")
print(names(forecast_lead_times))

#=============================
# 2. Apply cleaned column names
#=============================

# Rename columns to match the cleaned names used in final_dataset_denial
# Note: In final_dataset_denial, errors are negated (wind_error = -1 * vmax_sust_dif_mean)
# Set negate_errors = TRUE to match final_dataset convention (obs - pred)
negate_errors <- TRUE  # Set to FALSE to keep errors as (pred - obs)

forecast_lead_times_clean <- forecast_lead_times |>
  mutate(
    # Extract year and month from date
    year = year(date),
    month = month(date),
    # Rename wind variables
    wind_obs = vmax_sust_obs,
    wind_fc = vmax_sust_pred_mean,
    wind_error = ifelse(negate_errors, -1 * vmax_sust_dif_mean, vmax_sust_dif_mean),
    wind_fc_sd = vmax_sust_pred_sd,
    # Rename gust variables
    gust_obs = vmax_gust_obs,
    gust_fc = vmax_gust_pred_mean,
    gust_error = ifelse(negate_errors, -1 * vmax_gust_dif_mean, vmax_gust_dif_mean),
    gust_fc_sd = vmax_gust_pred_sd,
    # Rename duration variables
    dur_obs = sust_dur_obs,
    dur_fc = sust_dur_pred_mean,
    dur_error = ifelse(negate_errors, -1 * sust_dur_dif_mean, sust_dur_dif_mean),
    dur_fc_sd = sust_dur_pred_sd,
    # Rename precipitation variables
    precip_obs = prec_obs,
    precip_fc = prec_pred_mean,
    precip_error = ifelse(negate_errors, -1 * prec_dif_mean, prec_dif_mean),
    precip_fc_sd = prec_pred_sd
  ) |>
  # Handle missing precipitation values (set to -999 to match final_dataset convention)
  replace_na(replace = list(
    precip_obs = -999, 
    precip_fc = -999, 
    precip_error = -999, 
    precip_fc_sd = -999
  ))

# Select and reorder columns - keep all useful columns
cols_to_keep <- c(
  # ID columns
  "hurricane", "date", "year", "month", "augmented_obs",
  "lead_days", "lead_hours",
  "county", "county_name",
  # Wind variables
  "wind_obs", "wind_fc", "wind_error", "wind_fc_sd",
  # Gust variables (if they exist)
  "gust_obs", "gust_fc", "gust_error", "gust_fc_sd",
  # Duration variables
  "dur_obs", "dur_fc", "dur_error", "dur_fc_sd",
  # Precipitation variables
  "precip_obs", "precip_fc", "precip_error", "precip_fc_sd",
  # Keep original columns that might be useful
  "gridid", "glon", "glat", "name"
)

# Only select columns that actually exist
cols_to_keep <- cols_to_keep[cols_to_keep %in% names(forecast_lead_times_clean)]

forecast_lead_times_clean <- forecast_lead_times_clean |>
  select(all_of(cols_to_keep))

#=============================
# 3. Save cleaned lead time data
#=============================

fwrite(forecast_lead_times_clean, "data/clean_denial/forecast_denial_data_with_lead_times_clean.csv")
saveRDS(forecast_lead_times_clean, "data/clean_denial/forecast_denial_data_with_lead_times_clean.rds")

print("Done! Saved cleaned lead time data to:")
print("  - data/clean_denial/forecast_denial_data_with_lead_times_clean.csv")
print("  - data/clean_denial/forecast_denial_data_with_lead_times_clean.rds")
print(paste("  Total rows:", nrow(forecast_lead_times_clean)))
print(paste("  Columns:", ncol(forecast_lead_times_clean)))

