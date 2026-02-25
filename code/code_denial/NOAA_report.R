if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, tidyverse, here, fixest, patchwork)

# 1. Load data
forecast_data_lead_times <- fread(here("data/clean_denial/forecast_denial_data_with_lead_times.csv")) %>% 
  filter(name != "maria")

# 7. Create augmented_panel.png
# Top row: Wind speed SD by lead days, faceted by intensity thresholds
plot_top <- forecast_data_lead_times %>%
  mutate(
    intensity_panel =
      case_when(
        wind_obs > 33 ~ "> 33 m/s",
        wind_obs > 17.5 ~ "> 17.5 m/s",
        wind_obs > 0 ~ "> 0 m/s"
      )
  ) %>%
  filter(!is.na(augmented_obs)) %>%
  ggplot(aes(x = wind_fc_sd, color = factor(lead_days))) +
  geom_density(linewidth = 1.1, alpha = 0.6) +
  scale_color_brewer(palette = "Dark2", name = "Lead days") +
  labs(
    x = "Wind speed SD (m/s)",
    y = "Density"
  ) +
  facet_wrap(~ intensity_panel, nrow = 1) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    axis.title = element_text(size = 13)
  )

# Bottom row: Wind speed SD by augmented obs, faceted by lead days (for wind_obs > 33)
plot_bottom <- forecast_data_lead_times %>%
  filter(!is.na(augmented_obs), wind_obs > 33) %>%
  mutate(
    augmented_obs_label = ifelse(augmented_obs == 1, "With Aug Obs", "Without Aug Obs")
  ) %>%
  ggplot(aes(x = wind_fc_sd, color = augmented_obs_label)) +
  geom_density(linewidth = 1.1, alpha = 0.6) +
  scale_color_manual(values = c("With Aug Obs" = "#1B9E77", "Without Aug Obs" = "#D95F02"), name = "") +
  labs(
    x = "Wind speed SD (m/s)",
    y = "Density"
  ) +
  facet_wrap(~ factor(lead_days), nrow = 1, labeller = labeller(.cols = function(x) paste("Lead time:", x, "days"))) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    axis.title = element_text(size = 13)
  )

# Combine into panel
augmented_panel <- plot_top / plot_bottom

# Save
ggsave(
  plot = augmented_panel,
  filename = here("output", "augmented_panel.png"),
  width = 12,
  height = 8,
  dpi = 300
)

cat("\naugmented_panel.png created successfully!\n")





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

