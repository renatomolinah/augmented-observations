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

# Check if data is already cleaned (has wind_obs) or needs cleaning (has vmax_sust_obs)
needs_cleaning <- "vmax_sust_obs" %in% names(forecast_lead_times)
already_cleaned <- "wind_obs" %in% names(forecast_lead_times)

if (needs_cleaning && !already_cleaned) {
  print("Data needs cleaning - renaming columns from vmax_* format")
} else if (already_cleaned) {
  print("Data already has cleaned column names - skipping renaming step")
} else {
  print("WARNING: Neither old nor new column names found!")
}

#=============================
# 2. Apply cleaned column names
#=============================

# Rename columns to match the cleaned names used in final_dataset_denial
# Note: In final_dataset_denial, errors are negated (wind_error = -1 * vmax_sust_dif_mean)
# Set negate_errors = TRUE to match final_dataset convention (obs - pred)
negate_errors <- TRUE  # Set to FALSE to keep errors as (pred - obs)

if (needs_cleaning && !already_cleaned) {
  # Data needs renaming - apply transformations
  forecast_lead_times_clean <- forecast_lead_times |>
    mutate(
      # Extract year and month from date
      year = year(date),
      month = month(date),
      # Extract state FIPS from county FIPS (first 2 digits of 5-digit county code)
      # Convert county to character, pad to 5 digits, then extract first 2 digits
      state = str_pad(as.character(county), 5, 'left', '0') |> str_sub(1, 2),
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
    )
} else {
  # Data already cleaned - just add state and ensure year/month exist
  forecast_lead_times_clean <- forecast_lead_times
  
  # Add year/month if missing
  if (!"year" %in% names(forecast_lead_times_clean)) {
    forecast_lead_times_clean <- forecast_lead_times_clean |>
      mutate(year = year(date))
  }
  if (!"month" %in% names(forecast_lead_times_clean)) {
    forecast_lead_times_clean <- forecast_lead_times_clean |>
      mutate(month = month(date))
  }
  
  # Add state if missing
  if (!"state" %in% names(forecast_lead_times_clean)) {
    forecast_lead_times_clean <- forecast_lead_times_clean |>
      mutate(state = str_pad(as.character(county), 5, 'left', '0') |> str_sub(1, 2))
  }
}

forecast_lead_times_clean <- forecast_lead_times_clean |>
  # Handle missing precipitation values (set to -999 to match final_dataset convention)
  replace_na(replace = list(
    precip_obs = -999, 
    precip_fc = -999, 
    precip_error = -999, 
    precip_fc_sd = -999
  )) |>
  # Calculate ex post error (last_error) - must be done AFTER renaming
  # Group by augmented_obs, county, lead_days to match errors by lead time
  arrange(augmented_obs, county, lead_days, date) |>
  group_by(augmented_obs, county, lead_days) |>
  mutate(
    # Ex post error: lagged forecast error from previous period for the SAME lead time
    # Note: Using wind_fc - wind_obs (pred - obs) to match final_dataset convention
    last_error = lag(wind_fc - wind_obs)
  ) |>
  ungroup()

# Select and reorder columns - keep all useful columns
cols_to_keep <- c(
  # ID columns
  "hurricane", "date", "year", "month", "augmented_obs",
  "lead_days", "lead_hours",
  "state", "county", "county_name",
  # Wind variables
  "wind_obs", "wind_fc", "wind_error", "wind_fc_sd", "last_error",
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

# Save cleaned version
fwrite(forecast_lead_times_clean, "data/clean_denial/forecast_denial_data_with_lead_times_clean.csv")
saveRDS(forecast_lead_times_clean, "data/clean_denial/forecast_denial_data_with_lead_times_clean.rds")

# Also overwrite the original file with consistent column names
fwrite(forecast_lead_times_clean, "data/clean_denial/forecast_denial_data_with_lead_times.csv")
saveRDS(forecast_lead_times_clean, "data/clean_denial/forecast_denial_data_with_lead_times.rds")

print("Done! Saved cleaned lead time data to:")
print("  - data/clean_denial/forecast_denial_data_with_lead_times_clean.csv")
print("  - data/clean_denial/forecast_denial_data_with_lead_times_clean.rds")
print("  - data/clean_denial/forecast_denial_data_with_lead_times.csv (overwritten with consistent names)")
print("  - data/clean_denial/forecast_denial_data_with_lead_times.rds (overwritten with consistent names)")
print(paste("  Total rows:", nrow(forecast_lead_times_clean)))
print(paste("  Columns:", ncol(forecast_lead_times_clean)))
print("\nColumn names now match final_dataset_denial:")
print("  - state (extracted from county FIPS: first 2 digits)")
print("  - wind_obs, wind_fc, wind_error, wind_fc_sd, last_error")
print("  - gust_obs, gust_fc, gust_error, gust_fc_sd")
print("  - dur_obs, dur_fc, dur_error, dur_fc_sd")
print("  - precip_obs, precip_fc, precip_error, precip_fc_sd")
print("\nNote: Errors are calculated as obs - pred (negated from raw pred - obs)")
print("\nEx post error (last_error):")
print("  - Calculated separately for each lead_days (1, 2, 3 days)")
print("  - Grouped by: augmented_obs, county, lead_days")
print("  - Lagged forecast error from previous period for the SAME lead time")
print("  - Formula: lag(wind_fc - wind_obs) within each lead_days group")

