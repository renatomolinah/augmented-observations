# ==============================================================================
# APPENDIX MISCELLANEOUS FIGURES AND TABLES
# ==============================================================================
# OUTLINE:
# 1. Track Forecast Correlation Plots
# 2. Track Error Distribution
# 3. Intensity Distribution
# 4. Error Distribution
# 5. Forecast Distributions
# 6. Summary Forecast Statistics Plot
# 7. Wind Features
# 8. Wind SD vs Intensity Forecast
# ==============================================================================
gc()

# ==============================================================================
# 1. Track Error Distribution
# ==============================================================================

# Calculate track error statistics
track_error_dist = df_track$level_error_track
track_mean = round(mean(track_error_dist, na.rm = TRUE), digits = 2)
track_sd = round(sd(track_error_dist, na.rm = TRUE), digits = 2)
track_skew = round(skewness(track_error_dist, na.rm = TRUE), digits = 2)

# Create enhanced track error distribution plot
p_hist <- ggplot(df_track, aes(x = level_error_track, y = after_stat(density))) +
  geom_histogram(fill = "#377eb8", bins = 100) +
  annotate("text", x = 100, y = .004, family = "Lato", 
           label = paste0("Mean: ", track_mean), size = 6, hjust = 0) +
  annotate("text", x = 100, y = .0035, family = "Lato", 
           label = paste0("Std Dev: ", track_sd), size = 6, hjust = 0) +
  annotate("text", x = 100, y = .003, family = "Lato", 
           label = paste0("Skewness: ", track_skew), size = 6, hjust = 0) +
  main_theme +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  labs(
    title = "A. Distribution of Track Errors",
    x = "Track Error (km)",
    y = "Density"
  )


# Calculate residuals
wind_unconditional = feols(wind_error ~ 1, data = df_track)$residuals
wind_resids = feols(wind_error ~ level_error_track, data = df_track)$residuals
wind_fitted = feols(wind_error ~ level_error_track, data = df_track)$fitted.values
wind_track_r2 = round(feols(wind_error ~ level_error_track, data = df_track)$sq.cor, digits = 2)

# Create histogram plot
p_wind_residuals <- ggplot() +
  geom_histogram_pattern(aes(x = wind_unconditional, y = after_stat(density)), 
                 fill = "#ff7f00", alpha = 0.75, bins = 100, 
                 pattern = 'stripe', pattern_angle = 45, pattern_density = 0.1,
                 pattern_spacing = 0.091, pattern_colour = "black") +
  geom_histogram_pattern(aes(x = wind_resids, y = after_stat(density)), 
                 fill = "#377eb8", alpha = 0.75, bins = 100, 
                 pattern = 'circle', pattern_angle = 45, pattern_density = 0.2,
                 pattern_spacing = 0.02, pattern_colour = "black") +
  geom_histogram(aes(x = wind_fitted, y = after_stat(density)), 
                 fill = "#e41a1c", alpha = 0.75, bins = 100) +
  geom_curve(aes(x = 10, y = .18, xend = 4, yend = 0.15),
             arrow = arrow(length = unit(0.2, "cm")), curvature = 0) +
  geom_curve(aes(x = -8, y = 0.17, xend = -2, yend = 0.16),
             arrow = arrow(length = unit(0.2, "cm")), curvature = 0) +
  geom_curve(aes(x = -10, y = 0.14, xend = -3, yend = 0.14),
             arrow = arrow(length = unit(0.2, "cm")), curvature = 0) +
  # Text labels
  annotate("text", x = 4, y = .2, family = "Lato", 
           label = "Wind Predicted by Track", size = 4, hjust = 0) +
  annotate("text", x = -10, y = .18, family = "Lato", 
           label = "Wind | Track", size = 4, hjust = 0) +
  annotate("text", x = -17, y = .13, family = "Lato", 
           label = "Wind\nUnconditional", 
           size = 4, hjust = 0) +
  main_theme +
  labs(
    title = "B. Distribution of Wind Errors",
    x = "Wind Error (m/s)",
    y = "Density"
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks())

# Create scatter plot of wind error vs track error
p_scatter <- ggplot(df_track, aes(x = level_error_track, y = wind_error)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm", se = FALSE, color = "#377eb8") +
  annotate("text", x = -400, y = 30, family = "Lato", 
           label = paste0("R²: ", wind_track_r2), size = 6, hjust = 0) +
  labs(
    title = "C. Track Error vs Wind Error",
    x = "Track Error (km)",
    y = "Wind Error (m/s)"
  ) +
  main_theme

# Create a heatmap of damages by wind and track errors
p_joint <- df_track |>
  group_by(wind_error_bin, track_error_bin) |>
  summarise(
    mean_damages = mean(damages/1e6, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) |>
  arrange(wind_error_bin, track_error_bin) |>
  ggplot(aes(x = track_error_bin, y = wind_error_bin, 
           fill = mean_damages)) +
  geom_tile() +
  scale_fill_viridis_c(
    name = "($ million)",
    trans = "pseudo_log",
    breaks = c(0, 1, 10, 100, 200),
    labels = c(0, 1, 10, 100, 200)
  ) +
  labs(
    title = "D. Damages by Track and Wind Errors",
    x = "Track Error Bin (km)",
    y = "Wind Error Bin (m/s)"
  ) +
  main_theme +
  theme(
    legend.position = "right",
    legend.key.width = unit(1.5, "cm"),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

plot_out = ( p_hist + p_wind_residuals ) / ( p_scatter + p_joint )

ggsave(here("results", "FigureD4.png"), plot_out, width = 2 * 10 * 2 / 3, height = 2 * 8 * 2 / 3)
ggsave(here("results", "FigureD4.pdf"), plot_out, device = cairo_pdf, width = 2 * 10 * 2 / 3, height = 2 * 8 * 2 / 3)


# ==============================================================================
# 2. Intensity Distribution
# ==============================================================================

df = load_df()
df_plot = df[df$wind_obs != 0 & df$wind_fc != 0,]

wind = ggplot(df_plot) +
  geom_density(
    aes(x = wind_obs, y = after_stat(density)),
      color = "#377eb8", fill = "#377eb8", 
      position = 'identity', alpha = 0.15, linetype = 'solid', linewidth = 1
    ) +
  geom_density(
    aes(x = wind_fc, y = after_stat(density)),
      color = "#e41a1c", fill = "#e41a1c", 
      position = 'identity', alpha = 0.15, linetype = 'dashed', linewidth = 1
    ) +
  annotate("text",
           x = 2, y = .37,
           family = "Lato",
           label = "Realization", size = 5, hjust = 0
  ) +
  annotate("text",
           x = 3, y = .12,
           family = "Lato",
           label = "Forecast", size = 5, hjust = 0
  ) +
  main_theme +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  labs(
    title = "A. Wind Speed Distribution",
    y = "Density",
    x = "Wind Speed (m/s)"
  )


df = load_df()
df_plot = df[df$precip_obs != 0 & df$precip_fc != 0,]

precip = ggplot(df_plot) +
  geom_density(
    aes(x = precip_obs, y = after_stat(density)),
      color = "#377eb8", fill = "#377eb8", 
      position = 'identity', alpha = 0.15, linetype = 'solid', linewidth = 1
    ) +
  geom_density(
    aes(x = precip_fc, y = after_stat(density)),
      color = "#e41a1c", fill = "#e41a1c", 
      position = 'identity', alpha = 0.15, linetype = 'dashed', linewidth = 1
    ) +
  main_theme +
  #scale_x_continuous(trans = "log10", labels = label_log()) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  labs(
    title = "B. Precipitation Distribution",
    y = "Density",
    x = "Precipitation (mm)"
  )

(wind + precip)

ggsave("results/FigureD2.png", width = 2 * 10 * 2 / 3, height = 8 * 2 / 3)
ggsave("results/FigureD2.pdf", device = cairo_pdf, width = 2 * 10 * 2 / 3, height = 8 * 2 / 3)


# ==============================================================================
# 3. Error Distribution
# ==============================================================================

forecast_fc_obs = df |>
  mutate(
    error_bins = ntile(wind_fc, 20)
  ) |>
  ungroup() |>
  group_by(error_bins) |>
  summarise(
    wind_obs = mean(wind_obs, na.rm = TRUE),
    wind_fc = mean(wind_fc, na.rm = TRUE)
  ) |>
  ggplot(aes(wind_fc, wind_obs)) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_point(size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = 'dotted') +
  main_theme +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  labs(
    title = "A. Wind Forecast and Wind Speed",
    y = "Realized Wind Speed (m/s)",
    x = "Wind Speed Forecast (m/s)"
  ) +
  scale_x_continuous(breaks = scales::pretty_breaks())

error_dist = df$wind_obs - df$wind_fc
df_plot = df[df$wind_fc != 0 & error_dist != 0,]
error_dist = error_dist[df$wind_fc != 0 & error_dist != 0]

df_plot = df_plot |> 
  mutate(early = year <= 2007)

fc_mean = round(mean(error_dist), digits = 2)
fc_sd = round(sd(error_dist), digits = 2)
fc_skew = round(skewness(error_dist), digits = 2)

error_dist = ggplot(df_plot, aes(x = wind_obs - wind_fc, y = after_stat(density))) +
  geom_histogram(fill = "#377eb8", bins = 100) +
  annotate("text", x = 10, y = .3, family = "Lato", label = paste0("Mean: ", fc_mean), size = 6, hjust = 0) +
  annotate("text", x = 10, y = .25, family = "Lato", label = paste0("Std Dev: ", fc_sd), size = 6, hjust = 0) +
  annotate("text", x = 10, y = .2, family = "Lato", label = paste0("Skewness: ", fc_skew), size = 6, hjust = 0) +
  main_theme +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  labs(
    title = "B. Distribution of Wind Speed Errors",
    y = "Density",
    x = "Wind Speed Underestimate (m/s)"
  )

forecast_fc_obs + error_dist

ggsave("results/FigureD3.png", width = 2 * 10 * 2 / 3, height = 8 * 2 / 3)
ggsave("results/FigureD3.pdf", device = cairo_pdf, width = 2 * 10 * 2 / 3, height = 8 * 2 / 3)

# ==============================================================================
# 4. Forecast Distributions
# ==============================================================================

normalF = function(parvec) {
  # Log of likelihood of a normal distribution
  # parvec[1] - Mean
  # parvec[2] - Standard deviation
  # x - Set of observations (initialize before MLE)
  sum(-0.5 * log(parvec[2]) - 0.5 * (x - parvec[1])^2 / parvec[2])
}

# Precipitation error

x = df |> filter(precip_fc > 0) |> pull(precip_error)
x = x[!is.infinite(x) & !is.na(x) & x != 0]

MLE = optim(c(0, 0), # initial values for mu and sigma
  fn = normalF, # function to maximize
  method = "L-BFGS-B", # this method lets set lower bounds
  lower = 0.00001, # lower limit for parameters
  control = list(fnscale = -1), # maximize the function
  hessian = T # Hessian needed for confidence intervals
)
MLE

# Histogram of precipitation error
p2 = ggplot(tibble(x = x)) +
  geom_histogram(aes(x = x, y = ..density..), fill = "#377eb8", bins = 101) +
  labs(x = "Precipitation Error (mm)", y = "Density",
       title = "A. Precipitation") +
  main_theme

p2
ggsave("results/FigureB2a.png", p2, width = 10*2/3, height = 8*2/3)
ggsave("results/FigureB2a.pdf", p2, device = cairo_pdf, width = 10*2/3, height = 8*2/3)

# Track error

x = load_df_track() |> pull(level_error_track)
x = x[!is.infinite(x) & !is.na(x) & x != 0]

MLE = optim(c(0, 0), # initial values for mu and sigma
            fn = normalF, # function to maximize
            method = "L-BFGS-B", # this method lets set lower bounds
            lower = 0.00001, # lower limit for parameters
            control = list(fnscale = -1), # maximize the function
            hessian = T # Hessian needed for confidence intervals
)
MLE

# Histogram of track distance error
p3 = ggplot(tibble(x = x)) +
  geom_histogram(aes(x = x, y = ..density..), fill = "#377eb8", bins = 101) +
  labs(x = "Track Error (km)", y = "Density",
       title = "B. Track") +
  main_theme

p3
ggsave("results/FigureB2b.png", p3, width = 10*2/3, height = 8*2/3)
ggsave("results/FigureB2b.pdf", p3, device = cairo_pdf, width = 10*2/3, height = 8*2/3)

# ==============================================================================
# 5. Summary Forecast Statistics Plot
# ==============================================================================

forecast_error = load_df() |>
  mutate(
    error_bins = ntile(wind_obs, 20)
  ) |>
  ungroup() |>
  group_by(error_bins) |>
  summarise(
    wind_obs = mean(wind_obs, na.rm = TRUE),
    wind_error = mean(wind_error, na.rm = TRUE)
  ) |>
  ggplot(aes(wind_obs, wind_error)) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_point(size = 3) +
  main_theme +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  labs(
    title = "A. Wind Forecast Error and Wind Speed",
    y = "Wind Forecast Error (m/s)",
    x = "Wind Speed (m/s)"
  ) +
  coord_cartesian(ylim = c(-2, 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks())

precip_error = load_df() |>
  mutate(
    error_bins = ntile(precip_obs, 20)
  ) |>
  ungroup() |>
  group_by(error_bins) |>
  summarise(
    precip_obs = mean(precip_obs, na.rm = TRUE),
    precip_error = mean(precip_error, na.rm = TRUE)
  ) |>
  ggplot(aes(precip_obs, precip_error)) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_point(size = 3) +
  main_theme +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  labs(
    title = "B. Precip Forecast Error and Precip",
    y = "Precip Forecast Error (mm)",
    x = "Precip (mm)"
  ) +
  coord_cartesian(ylim = c(-20, 100)) +
  scale_x_continuous(breaks = scales::pretty_breaks())

forecast_error2 = load_df() |>
  mutate(
    error_bins = ntile(wind_obs, 20)
  ) |>
  ungroup() |>
  group_by(error_bins) |>
  summarise(
    wind_obs = mean(wind_obs, na.rm = TRUE),
    wind_error = mean((wind_error-mean(wind_error))^2, na.rm = TRUE)
  ) |>
  ggplot(aes(wind_obs, wind_error)) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_point(size = 3) +
  main_theme +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  labs(
    title = "C. Squared Demeaned\nWind Error and Wind Speed",
    y = "Squared Demeaned Wind Error",
    x = "Wind Speed (m/s)"
  ) +
  scale_x_continuous(breaks = scales::pretty_breaks())

precip_error2 = load_df() |>
  mutate(
    error_bins = ntile(precip_obs, 20)
  ) |>
  ungroup() |>
  group_by(error_bins) |>
  summarise(
    precip_obs = mean(precip_obs, na.rm = TRUE),
    precip_error = mean((precip_error-mean(precip_error))^2, na.rm = TRUE)
  ) |>
  ggplot(aes(precip_obs, precip_error)) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_point(size = 3) +
  main_theme +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  labs(
    title = "D. Squared Demeaned\nPrecip Error and Precip",
    y = "Squared Demeaned Precip Error",
    x = "Precipitation (mm)"
  ) +
  scale_x_continuous(breaks = scales::pretty_breaks())

forecast_rmse = load_df() |>
  mutate(
    error_bins = ntile(wind_fc_sd, 20)
  ) |>
  ungroup() |>
  group_by(error_bins) |>
  summarise(
    wind_error = (mean(abs(wind_error), na.rm = TRUE)),
    wind_fc_sd = mean(wind_fc_sd, na.rm = TRUE)
  ) |>
  ggplot(aes(wind_fc_sd, wind_error)) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_point(size = 3) +
  main_theme +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  labs(
    title = "E. Wind Forecast Error and Wind Forecast SD",
    x = "Wind Forecast Standard Deviation (m/s)",
    y = "abs(Wind Forecast Error) (m/s)"
  ) +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  coord_cartesian(xlim = c(0, 8), ylim = c(0, 8))

wind_rain = load_df() |>
  mutate(
    error_bins = ntile(wind_obs, 20)
  ) |>
  ungroup() |>
  group_by(error_bins) |>
  summarise(
    wind_obs = mean(wind_obs, na.rm = TRUE),
    precip_obs = mean(precip_obs, na.rm = TRUE)
  ) |>
  ggplot(aes(wind_obs, precip_obs)) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_point(size = 3) +
  main_theme +
  labs(
    title = "F. Precipitation and Wind Speed",
    x = "Wind Speed (m/s)",
    y = "Precipitation (mm)"
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  scale_x_continuous(breaks = scales::pretty_breaks())

(forecast_error + precip_error) / (forecast_error2 + precip_error2) / (forecast_rmse + wind_rain)

ggsave("results/FigureD1.png", width = 2 * 10 * 2 / 3, height = 3 * 8 * 2 / 3)
ggsave("results/FigureD1.pdf", device = cairo_pdf, width = 2 * 10 * 2 / 3, height = 3 * 8 * 2 / 3)


# ==============================================================================
# 6. Wind SD vs Intensity Forecast
# ==============================================================================

# Compute mean and standard deviation of level_error for each level of wind_fc
wind_fc_analysis <- load_df() |>
  filter(wind_fc > 0) |>
  mutate(wind_fc_quantile = ntile(wind_fc, 50)) |>
  group_by(wind_fc_quantile) |>
  summarize(
    wind_fc = mean(wind_fc, na.rm = TRUE),
    sd_level_error = mean(wind_fc_sd, na.rm = TRUE)
  )

ggplot(wind_fc_analysis, aes(x = wind_fc)) +
  geom_line(aes(y = sd_level_error, color = "Forecast SD"), linewidth = 1.2) +
  geom_smooth(
    aes(y = sd_level_error, linetype = "Linear Fit"),
    method = "lm", formula = y ~ 1 + x, se = FALSE,
    color = "black", linewidth = 1
  ) +
  scale_color_manual(
    name = NULL,
    values = c("Forecast SD" = "#e41a1c")
  ) +
  scale_linetype_manual(
    name = NULL,
    values = c("Linear Fit" = "dashed")
  ) +
  guides(
    color = guide_legend(order = 1),
    linetype = guide_legend(order = 2)
  ) +
  labs(
    x = "Wind Speed Forecast (m/s)",
    y = "Forecast SD (m/s)"
  ) +
  main_theme +  
  theme(
    plot.title = element_text(vjust = -3),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.key.width = unit(.75, "cm"),
    legend.key.height = unit(.75, "cm"),
    panel.grid = element_blank()
  )

# Save the figure
ggsave("results/FigureB1.png",
       width = 1 * 10 * 2 / 3, height = 1 * 8 * 2 / 3, dpi = 300)
ggsave("results/FigureB1.pdf", device = cairo_pdf,
       width = 1 * 10 * 2 / 3, height = 1 * 8 * 2 / 3)
