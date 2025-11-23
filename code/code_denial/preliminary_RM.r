if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, tidyverse, here, fixest)


# Check differences by lead time (using disaggregated data)

forecast_data_lead_times <- fread(here("data/clean_denial/forecast_denial_data_with_lead_times.csv"))


forecast_data_lead_times %>%
  filter(!is.na(augmented_obs), lead_days == 3, vmax_sust_obs > 33) %>%
  feols(
    vmax_sust_pred_sd ~ augmented_obs | county,
    data = .
  )


forecast_data_lead_times %>%
  filter(!is.na(augmented_obs), lead_days == 3, vmax_sust_obs > 33) %>%
  feols(
    sust_dur_dif_mean ~ augmented_obs | county,
    data = .
  )
