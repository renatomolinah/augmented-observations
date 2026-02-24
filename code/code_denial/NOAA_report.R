if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, tidyverse, here, fixest, patchwork)

# 1. Load data
forecast_data_lead_times <- fread(here("data/clean_denial/forecast_denial_data_with_lead_times.csv")) %>% 
  filter(name != "maria")

# 2. Define Parameters and Estimation Sample
beta_hurr <- 5.49 
reg_data <- forecast_data_lead_times %>%
  filter(!is.na(augmented_obs), lead_days == 3, wind_obs > 33)

# 3. Estimate Stage 2: Physical Impact (delta)
mod_precision <- feols(wind_fc_sd ~ augmented_obs | county + name, data = reg_data)
delta <- abs(coef(mod_precision)["augmented_obs"]) # Approximately 0.26

# 4. Estimate Stage 3: Baseline Uncertainty and Variance Reduction
# We calculate baseline SD (sigma) for the 'With Aug Obs' group
baseline_sd <- reg_data %>%
  filter(augmented_obs == 1) %>%
  summarise(mean_sd = mean(wind_fc_sd, na.rm = TRUE)) %>%
  pull(mean_sd) # Approximately 8.98

# Variance reduction: sigma_no^2 - sigma_all^2 = (sigma_all + delta)^2 - sigma_all^2
delta_sigma_sq <- (baseline_sd + delta)^2 - (baseline_sd)^2

# 5. Monetary Valuation
value_per_county <- beta_hurr * delta_sigma_sq
sample_benefits <- reg_data %>%
  group_by(name) %>%
  summarise(
    affected_counties = n(),
    total_benefit_M = n() * value_per_county
  ) %>%
  arrange(desc(total_benefit_M))

# 6. Results Summary
cat("\n--- Valuation Summary  for in sample (2017-2020) ---")
cat("\nObservations (HF impacts at 3-day lead):", nrow(reg_data))
cat("\nReduction in SD (delta):", round(delta, 3), "m/s")
cat("\nBaseline SD (sigma):", round(baseline_sd, 2), "m/s")
cat("\nReduction in Variance (delta sigma^2):", round(delta_sigma_sq, 2), "(m/s)^2")
cat("\nValue per Affected County ($M):", round(value_per_county, 2))
cat("\nTotal Retrospective Value ($B):", round(sum(sample_benefits$total_benefit_M)/1000, 2))
