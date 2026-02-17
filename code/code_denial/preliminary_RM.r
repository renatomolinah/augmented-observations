if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, tidyverse, here, fixest)


# Check differences by lead time (using disaggregated data)

forecast_data_lead_times <- fread(here("data/clean_denial/forecast_denial_data_with_lead_times.csv"))  %>% filter(name != "maria", name != "laura")

forecast_data_lead_times %>%
  filter(!is.na(augmented_obs), lead_days == 3, wind_obs > 33) %>%
  feols(
    wind_fc_sd ~ augmented_obs |  county + name,
    data = .
  ) 

forecast_data_lead_times %>%
  filter(!is.na(augmented_obs), lead_days == 3, wind_obs > 33) %>%
  mutate(wind_error = wind_obs - wind_fc) %>%
  feols(
    wind_error ~ augmented_obs |  county + name,
    data = .
  ) 



forecast_data_lead_times %>%
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
    y = "Density",
    title = "Distribution of forecast uncertainty by lead time"
  ) +
  facet_wrap(~ intensity_panel, nrow = 1) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    axis.title = element_text(size = 13)
  )

ggsave(
  filename = here("output", "forecast_uncertainty_panels.png"),
  width = 12,
  height = 4,
  dpi = 300
)

forecast_data_lead_times %>%
  mutate(
    intensity_panel =
      case_when(
        wind_obs > 33 ~ "> 33 m/s",
        wind_obs > 17.5 ~ "> 17.5 m/s",
        wind_obs > 0 ~ "> 0 m/s"
      )
  ) %>%
  filter(!is.na(augmented_obs)) %>%
  ggplot(aes(x = wind_error, color = factor(lead_days))) +
  geom_density(linewidth = 1.1, alpha = 0.6) +
  scale_color_brewer(palette = "Dark2", name = "Lead days") +
  labs(
    x = "Wind error (m/s)",
    y = "Density",
    title = "Distribution of forecast error by lead time"
  ) +
  facet_wrap(~ intensity_panel, nrow = 1) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    axis.title = element_text(size = 13)
  )

ggsave(
  filename = here("output", "forecast_error_panels.png"),
  width = 12,
  height = 4,
  dpi = 300
)

# Plot 3: Forecast uncertainty by lead time, comparing with/without augmented obs
forecast_data_lead_times %>%
  filter(!is.na(augmented_obs),wind_obs > 33) %>%
  mutate(
    augmented_obs_label = ifelse(augmented_obs == 1, "With Aug Obs", "Without Aug Obs")
  ) %>%
  ggplot(aes(x = wind_fc_sd, color = augmented_obs_label)) +
  geom_density(linewidth = 1.1, alpha = 0.6) +
  scale_color_manual(values = c("With Aug Obs" = "#1B9E77", "Without Aug Obs" = "#D95F02"), name = "") +
  labs(
    x = "Wind speed SD (m/s)",
    y = "Density"#,
    #title = "Distribution of forecast uncertainty by lead time and augmented observations"
  ) +
  facet_wrap(~ factor(lead_days), nrow = 1, labeller = labeller(.cols = function(x) paste("Lead time:", x, "days"))) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    axis.title = element_text(size = 13)
  )

ggsave(
  filename = here("output", "forecast_uncertainty_by_lead_aug_obs.png"),
  width = 12,
  height = 4,
  dpi = 300
)

# Plot 4: Forecast error by lead time, comparing with/without augmented obs
forecast_data_lead_times %>%
  filter(!is.na(augmented_obs), wind_obs > 33) %>%
  mutate(
    wind_error = wind_obs - wind_fc,
    augmented_obs_label = ifelse(augmented_obs == 1, "With Aug Obs", "Without Aug Obs")
  ) %>%
  ggplot(aes(x = wind_error, color = augmented_obs_label)) +
  geom_density(linewidth = 1.1, alpha = 0.6) +
  scale_color_manual(values = c("With Aug Obs" = "#1B9E77", "Without Aug Obs" = "#D95F02"), name = "") +
  labs(
    x = "Wind error (m/s)",
    y = "Density"#,
    #title = "Distribution of forecast error by lead time and augmented observations"
  ) +
  facet_wrap(~ factor(lead_days), nrow = 1, labeller = labeller(.cols = function(x) paste("Lead time:", x, "days"))) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    axis.title = element_text(size = 13)
  )

ggsave(
  filename = here("output", "forecast_error_by_lead_aug_obs.png"),
  width = 12,
  height = 4,
  dpi = 300
)

