if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, tidyverse, here, fixest, patchwork)


# Check differences by lead time (using disaggregated data)

forecast_data_lead_times <- fread(here("data/clean_denial/forecast_denial_data_with_lead_times.csv"))  %>% filter(name != "maria")

# Plot with uncertainty by intensity and then focusing on 33+ m/s

p1 <- forecast_data_lead_times %>%
  mutate(
    intensity_panel = case_when(
      wind_obs > 33   ~ "> 33 m/s",
      wind_obs > 17.5 ~ "> 17.5 m/s",
      wind_obs > 0    ~ "> 0 m/s"
    )
  ) %>%
  filter(!is.na(augmented_obs)) %>%
  ggplot(aes(x = wind_fc_sd, color = factor(lead_days))) +
  geom_density(linewidth = 1.1, alpha = 0.6) +
  scale_color_brewer(palette = "Set1", name = "Lead days") +
  labs(x = "Wind speed SD (m/s)", y = "Density") +
  facet_wrap(~ intensity_panel, nrow = 1) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 13),
    legend.position = "bottom"
  )


p2 <- forecast_data_lead_times %>%
  filter(!is.na(augmented_obs), wind_obs > 33) %>%
  mutate(
    augmented_obs_label = ifelse(augmented_obs == 1, "With Aug Obs", "Without Aug Obs")
  ) %>%
  ggplot(aes(x = wind_fc_sd, color = augmented_obs_label)) +
  geom_density(linewidth = 1.1, alpha = 0.6) +
  scale_color_manual(
    values = c(
      "With Aug Obs" = "#984EA3",     # purple
      "Without Aug Obs" = "#FF7F00"   # orange
    ),
    name = ""
  ) +
  labs(x = "Wind speed SD (m/s)", y = "Density") +
  facet_wrap(
    ~ factor(lead_days),
    nrow = 1,
    labeller = labeller(.cols = function(x) paste("Lead time:", x, "days (> 33 m/s)"))
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 13),
    legend.position = "bottom"
  )

(p1 / p2) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")


ggsave(
  filename = here("output", "augmented_panel.png"),
  width = 12,
  height = 8,
  dpi = 300
)

# Regression 

forecast_data_lead_times %>%
  filter(!is.na(augmented_obs), lead_days == 3, wind_obs > 33) %>%
  feols(
    wind_fc_sd ~ augmented_obs |  county + name,
    data = .
  ) 

# Calculate baseline uncertainty for Hurricane Force storms at 3-day lead time

# 1. Define the cost parameter from Stage 1 (Molina & Rudik, 2024)
beta_hurr <- 5.49 

# 2. Estimate the physical impact (Stage 2)
# We filter for 3-day lead time and hurricane-force observations (>33 m/s)
reg_data <- forecast_data_lead_times %>%
  filter(!is.na(augmented_obs), lead_days == 3, wind_obs > 33)

mod_precision <- feols(wind_fc_sd ~ augmented_obs | county + name, data = reg_data)
delta <- abs(coef(mod_precision)["augmented_obs"]) # The reduction in SD

# 3. Calculate baseline uncertainty (sigma_all)
baseline_sd <- reg_data %>%
  filter(augmented_obs == 1) %>%
  summarise(mean_sd = mean(wind_fc_sd, na.rm = TRUE)) %>%
  pull(mean_sd)

# 4. Calculate the reduction in variance (delta_sigma_sq)
# Formula: sigma_no^2 - sigma_all^2 = (sigma_all + delta)^2 - sigma_all^2
delta_sigma_sq <- (baseline_sd + delta)^2 - (baseline_sd)^2

# 5. Calculate value per county ($ Millions)
value_per_county <- beta_hurr * delta_sigma_sq

# 6. Aggregate benefits by storm
sample_benefits <- reg_data %>%
  group_by(name) %>% # Using 'name' as used in the FE regression
  summarise(
    affected_counties = n(),
    total_benefit_M = n() * value_per_county
  ) %>%
  arrange(desc(total_benefit_M))

# 7. Print Results
print(sample_benefits)
cat("\n--- Summary ---")
cat("\nValue per County ($M):", round(value_per_county, 2))
cat("\nTotal Sample Value ($M):", round(sum(sample_benefits$total_benefit_M), 2))
cat("\nTotal Sample Value ($B):", round(sum(sample_benefits$total_benefit_M)/1000, 2))

