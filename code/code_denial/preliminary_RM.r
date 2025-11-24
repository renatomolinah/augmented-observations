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



forecast_data_lead_times %>%
  mutate(
    intensity_panel =
      case_when(
        vmax_sust_obs > 33 ~ "> 33 m/s",
        vmax_sust_obs > 17.5 ~ "> 17.5 m/s",
        vmax_sust_obs > 0 ~ "> 0 m/s"
      )
  ) %>%
  filter(!is.na(augmented_obs)) %>%
  ggplot(aes(x = vmax_sust_pred_sd, color = factor(lead_days))) +
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
    legend.position = "top",
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

