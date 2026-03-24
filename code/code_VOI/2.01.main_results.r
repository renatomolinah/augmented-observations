# ==============================================================================
# MAIN RESULTS - FIGURES AND TABLES
# ==============================================================================
# OUTLINE:
# 1. Figure 1 - Historical Forecast Error Improvements
# 2. Figure 2 - Katrina Case Study Map
# 3. Table 1 - Summary Statistics by Hurricane
# 4. Table D.1 - Standard Deviation of Hurricane Physical Features
# 5. Figure 3 - Wind Features Plot
# 6. Figure 4 - Protective Expenditures Analysis
# 7. Figure 5 - Ex Post Analysis
# 8. Table 2 - Value of Improving the Forecast Analysis
# ==============================================================================

# ==============================================================================
# 1. Figure 1 - Historical Forecast Error Improvements
# ==============================================================================
gc()

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
  mutate(forecast_error = forecast_error / 1.94384) |>  # 1.94 knots per m/s
  group_by(year) |> 
  summarise(forecast_error = mean(forecast_error, na.rm = T)) |> 
  mutate(early = year < 2007)

error_last = his_error_wind |> filter(year == max(year))

slopes = feols(forecast_error ~ year:early + early, his_error_wind) |> 
  tidy() |>
  slice(3, 4) |>
  pull(estimate)

# NHC forecast error time trend plot by lead days
time_series = ggplot(
  his_error_wind,
  aes(
    x = year, y = forecast_error, color = early, shape = early
  )
) +
  geom_point(size = 2) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = 2006.5, color = "black", linetype = "longdash") +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  scale_color_manual(values = c("#377eb8", "#e41a1c")) +
  scale_shape_manual(values = c(15, 16)) +
  coord_cartesian(xlim = c(1990, 2021.5), ylim = c(4, 10)) +
  labs(x = "", 
       y = "abs(Maximum Wind Speed Error) (m/s)") +
  geom_smooth(data = his_error_wind |> filter(early),
              method = lm, linetype = "dotted", se = FALSE, color = '#e41a1c') +
  geom_smooth(data = his_error_wind |> filter(!early),
              method = lm, linetype = "dotted", se = FALSE, color = '#377eb8') +
  annotate("text", x = 1994, y = 9, family = "Lato",
           label = "Pre-HFIP", size = 7, hjust = 0, color = '#e41a1c') +
  annotate("text", x = 2010, y = 4, family = "Lato", 
           label = "Post-HFIP", size = 7, hjust = 0, color = '#377eb8') +
  main_theme

ggsave("results/Figure1.png", width = 1.75 * 6, height = 1.75 * 3)
ggsave("results/Figure1.pdf", device = cairo_pdf, width = 1.75 * 6, height = 1.75 * 3)

# ==============================================================================
# 2. Figure 2 - Katrina Case Study Map
# ==============================================================================

hurricane_name = "katrina"
lead = 3

forecast_track = track_forecast |>
  filter(hurricane == {{ hurricane_name }} & forecast_time == {{lead}} & type != "OCD5") 

obs_track_path <- list.files(
  path = file.path(here("data/raw/track_raw", hurricane_name, "track"),
                   lead*24),
  pattern = "trackline_bdeck_BEST.shp$",
  full.names = TRUE
)

obs_track <- st_read(obs_track_path[1], quiet = TRUE) |>
  st_set_crs(4326)

temp.data = data_weather_map |>
  filter(hurricane == {{ hurricane_name }}) |>
  st_as_sf(crs = 4326) |> 
  st_transform("+proj=longlat +datum=NAD83 +no_defs") |>
  mutate(state_fips = substr(str_pad(county, 5, 'left', '0'), 1, 2))

state_fips = maps::state.fips |>
  distinct(fips, .keep_all = TRUE) |>
  select(abb, polyname, fips) |>
  mutate(polyname = gsub("\\:.*", "", polyname))

state_name_sdf = st_as_sf(maps::map("state", plot = FALSE, fill = TRUE)) |>
  rename(polyname = ID) |>
  full_join(state_fips) |>
  filter(fips %in% as.numeric(unique(temp.data$state_fips)))

nashville = data.frame(lon = c(-86.78), lat = c(36.16))

nashville_sf = nashville |>
  st_as_sf(coords = c('lon', 'lat'), crs = 4326)

nashville_proj = st_transform(nashville_sf, 5070)

nashville_400km = st_buffer(nashville_proj, dist = units::set_units(400000, "m"), nQuadSegs = 5000)
nashville_600km = st_buffer(nashville_proj, dist = units::set_units(600000, "m"), nQuadSegs = 5000)

# Transform back to WGS84 for plotting
nashville_400km = st_transform(nashville_400km, 4326)
nashville_600km = st_transform(nashville_600km, 4326)
nashville_sf = st_transform(nashville_proj, 4326)

wind_mean = ggplot(temp.data) +
  geom_sf(aes(fill = vmax_sust_pred_mean, geometry = geom), size = 0.1) +
  geom_sf(
    data = state_name_sdf,
    col = "#555555",
    size = 0.4,
    fill = "transparent"
  ) +
  geom_sf(data = forecast_track, aes(color = type, linetype = type), alpha = 1, size = 1) +
  geom_sf(data = obs_track, aes(color = "black", linetype = "dashed"), alpha = 1, size = 1) +
  scale_color_manual(values = c("blue", "black"), guide = 'none') +
  scale_linetype_manual(values = c("solid", "dashed"), guide = 'none') +
  scale_fill_gradient(
    name = "(m/s)",
    low = "#ffffef", high = "#e41a1c",
    space = "Lab", na.value = "#ffffef",
    aesthetics = "fill",
    limits = c(0, 25),
    breaks = seq(0, 25, 5),
    oob = scales::squish,
    guide = guide_colourbar(
      ticks.linewidth = 1,
      ticks.colour = "#111111",
      frame.colour = "#111111",
      frame.linewidth = 1
    )
  ) +
  ggtitle(str_to_title(paste0("A. Wind speed forecast"))) +
  theme(plot.title = element_text(vjust = -5)) +
  main_theme_map +
  theme(axis.title = element_blank(), 
        panel.grid = element_blank(),
        axis.text = element_blank(),
        text = element_text(family="Lato"),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        legend.key.width = unit(.75, "cm"),
        legend.key.height = unit(.75, "cm"),
        legend.position = c(0.90, 0.30)) +
  coord_sf(xlim = c(-105, -67), ylim = c(25, 48))

wind_sd = ggplot(temp.data) +
  geom_sf(aes(fill = vmax_sust_pred_sd, geometry = geom), size = 0.1) +
  geom_sf(
    data = state_name_sdf,
    col = "#555555",
    size = 0.4,
    fill = "transparent"
  ) +
  geom_sf(data = forecast_track, aes(color = type, linetype = type), alpha = 1, size = 1) +
  geom_sf(data = obs_track, aes(color = "black", linetype = "dashed"), alpha = 1, size = 1) +
  scale_color_manual(values = c("blue", "black"), guide = 'none') +
  scale_linetype_manual(values = c("solid", "dashed"), guide = 'none') +
  scale_fill_gradient(
    name = "(m/s)",
    low = "#ffffef", high = "#e41a1c",
    space = "Lab", na.value = "#ffffef",
    aesthetics = "fill",
    limits = c(0, 12),
    breaks = seq(0, 12, 4),
    oob = scales::squish,
    guide = guide_colourbar(
      ticks.linewidth = 1,
      ticks.colour = "#111111",
      frame.colour = "#111111",
      frame.linewidth = 1
    )
  ) +
  ggtitle(str_to_title(paste0("B. Wind speed forecast standard deviation"))) +
  theme(plot.title = element_text(vjust = -5)) +
  main_theme_map +
  theme(axis.title = element_blank(), 
        panel.grid = element_blank(),
        axis.text = element_blank(),
        text = element_text(family="Lato"),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        legend.key.width = unit(.75, "cm"),
        legend.key.height = unit(.75, "cm"),
        legend.position = c(0.90, 0.30)) +
  coord_sf(xlim = c(-105, -67), ylim = c(25, 48))

wind_error = ggplot(temp.data) +
  geom_sf(aes(fill = -vmax_sust_dif_mean, geometry = geom), size = 0.1) + 
  geom_sf(data = state_name_sdf,
          col = "#555555",
          size = 0.4,
          fill = "transparent") +
  geom_sf(data = nashville_sf, color = 'blue', size = 3) +
  geom_sf(data = nashville_400km, color = '#000000', fill = 'transparent', linewidth = .5, linetype = "11") +
  geom_sf(data = nashville_600km, color = '#000000', fill = 'transparent', linewidth = .5, linetype = "11") +
  geom_sf(data = forecast_track, aes(color = type, linetype = type), alpha = 1, size = 1) +
  geom_sf(data = obs_track, aes(color = "black", linetype = "dashed"), alpha = 1, size = 1) +
  scale_color_manual(values = c("blue", "black"), guide = 'none') +
  scale_linetype_manual(values = c("solid", "dashed"), guide = 'none') +
  scale_fill_gradient2(name = "(m/s)",
                       low = "#377eb8", mid = "#ffffef", high = "#e41a1c", 
                       midpoint = 0, space = "Lab", na.value = "#ffffef", 
                       aesthetics = "fill",
                       oob = scales::squish,
                       breaks = scales::pretty_breaks(),
                       guide = guide_colourbar(
                         ticks.linewidth = 1,
                         ticks.colour = "#111111",
                         frame.colour = "#111111",
                         frame.linewidth = 1,
                         labels = waiver()
                       )
  ) +
  ggtitle(str_to_title(paste0("C. Wind speed forecast underestimate"))) +
  theme(plot.title = element_text(vjust = -5)) + 
  main_theme_map +
  theme(axis.title = element_blank(), 
        panel.grid = element_blank(),
        axis.text = element_blank(),
        text = element_text(family="Lato"),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        legend.key.width = unit(.75, "cm"),
        legend.key.height = unit(.75, "cm"),
        legend.position = c(0.90, 0.30)) +
  coord_sf(xlim = c(-105, -67), ylim = c(25, 48))

map_out = (wind_mean + wind_sd + wind_error) + plot_layout(widths = c(1, 1, 1))

ggsave("results/Figure2.png", width = 1.75 * 10, height = 1.75 * 3)
ggsave("results/Figure2.pdf", device = cairo_pdf, width = 1.75 * 10, height = 1.75 * 3)

# ==============================================================================
# 3. Table 1/D1 - Summary Statistics by Hurricane
# ==============================================================================

data_summary = df |> 
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
    `Protective Expenditures` = ex_ante_spending,
    `Recovery Expenditures` = ex_post_spending
  )

# Define column units
column_units <- c("", "", "(m/s)", "abs(m/s)", "(mm)", "abs(mm)", "(Billion \\$)", "(Billion \\$)", "(Billion \\$)")

summary_table = kbl(data_summary,
                    format = "latex",
                    caption = "Summary Statistics by Hurricane.",
                    label = "summary_dmg",
                    booktabs = T,
                    escape = FALSE,
                    linesep = c(""),
                    col.names = column_units, # First row will be the units
                    align = c("l", rep("c", ncol(data_summary) - 1))
) |> 
  add_header_above(setNames(rep(1, length(names(data_summary))), names(data_summary)), escape = FALSE) |> # Add column names on top
  footnote(
    general = "The table includes all category 1 and greater hurricanes (maximum wind speeds greater than 33 m/s) that made landfall in the continental US between 2005--2024. Wind speed, precipitation, and their associated errors are averaged across counties to the hurricane level. Damages and expenditures are summed across counties to the hurricane level. Wind speed is the maximum sustained wind speed in m/s, precipitation is the total precipitation in mm. Wind speed and precipitation errors are averages of the absolute values of county-level errors. All damage and expenditures are reported in billions of (2019) US$.",
    threeparttable = T,
    fixed_small_size = T
  )

fileConn = file("results/Table1.tex")
writeLines(summary_table, fileConn)
close(fileConn)

add_adjust_box("results/Table1.tex")

# ==============================================================================
# 4. Table D.1 - Standard Deviation of Hurricane Physical Features
# ==============================================================================

data_summary_sd = df |> 
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
    sd_wind_speed = sprintf("%.2f", sd(wind_obs)),
    sd_wind_error = sprintf("%.2f", sd(abs(wind_error))),
    sd_prec = sprintf("%.2f", sd(precip_obs)),
    sd_prec_error = sprintf("%.2f", sd(abs(precip_error))),
  ) |> 
  arrange(year) |> 
  mutate(hurricane = str_to_title(hurricane)) |> 
  rename(
    Hurricane = hurricane,
    Year = year,
    `Wind Speed` = sd_wind_speed,
    `Wind Speed Error` = sd_wind_error,
    `Precipitation` = sd_prec,
    `Precipitation Error` = sd_prec_error
  )

# Generate the LaTeX Table for Standard Deviations
summary_table_sd = kbl(data_summary_sd,
                       format = "latex",
                       caption = "Standard Deviations of Physical Values by Hurricane.",
                       label = "summary_dmg_sd",
                       booktabs = T,
                       escape = FALSE,
                       linesep = c(""),
                       col.names = column_units[1:ncol(data_summary_sd) ], # First row will be the units
                       align = c("l", rep("c", ncol(data_summary_sd) - 1))
) |> 
  add_header_above(setNames(rep(1, length(names(data_summary_sd))), names(data_summary_sd)), escape = FALSE) |> # Add column names on top
  footnote(
    general = "The table includes all Category 1 and greater hurricanes (maximum wind speeds greater than 33 m/s) that made landfall in the continental US between 2005--2024. Wind speed, precipitation, and their associated errors are averaged across counties to the hurricane level. Wind speed is the maximum sustained wind speed in m/s, precipitation is the total precipitation in mm. Wind speed and precipitation errors are averages of the absolute values of county-level errors.",
    threeparttable = T,
    fixed_small_size = T
  )

fileConn_sd = file("results/TableD1.tex")
writeLines(summary_table_sd, fileConn_sd)
close(fileConn_sd)

add_adjust_box("results/TableD1.tex")

# ==============================================================================
# 5. Figure 3 - Wind Features Plot
# ==============================================================================

# Filter for non-zero observations
df_plot = df[df$wind_obs != 0 & df$wind_fc != 0,]

# Create wind speed distribution plot
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

# Create wind forecast error distribution plot
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

# Combine plots and save
wind + error_dist

ggsave("results/Figure3.png", width = 2 * 10 * 2 / 3, height = 1 * 8 * 2 / 3)
ggsave("results/Figure3.pdf", device = cairo_pdf, width = 2 * 10 * 2 / 3, height = 1 * 8 * 2 / 3)

# ==============================================================================
# 6. Figure 4 - Protective Expenditures Analysis
# ==============================================================================

effect_by_lead_day = conleyreg(
  ex_ante_spending/1e6 ~
    i(forecast_cut) + i(forecast_precip_cut) |
    county + state^hurricane,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

coef_log = tidy(effect_by_lead_day, conf.int = TRUE) |>
  filter(str_detect(term, "forecast_cut")) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .before = 1,
  ) |>
  mutate(term = sort(unique(df$forecast_cut))) |>
  mutate(x = row_number())

log_plot = ggplot(coef_log, aes(x, estimate)) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(aes(x = x, ymin = conf.low, ymax = conf.high), width = 0, color = "#000000") +
  geom_point(size = 3, color = "#000000") +
  scale_x_continuous(labels = coef_log$term, breaks = coef_log$x) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  main_theme +
  labs(
    title = "A. Protective Expenditures (million $)",
    x = "Wind Speed Forecast (m/s)",
    y = ""
  ) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) + 
  coord_cartesian(ylim = c(-5, 50))

effect_by_lead_day_gdp = conleyreg(
  ex_ante_spending_per_gdp ~
    i(forecast_cut) + i(forecast_precip_cut) |
    county + state^hurricane,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

coef_gdp = tidy(effect_by_lead_day_gdp, conf.int = TRUE) |>
  filter(str_detect(term, "forecast_cut")) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .before = 1,
  ) |>
  mutate(term = sort(unique(df$forecast_cut))) |>
  mutate(x = row_number(),
          estimate = estimate,
          conf.low = conf.low,
          conf.high = conf.high)

share_plot = ggplot(coef_gdp, aes(x, estimate*100)) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(aes(x = x, ymin = conf.low*100, ymax = conf.high*100), width = 0, color = "#000000") +
  geom_point(size = 3, color = "#000000") +
  scale_x_continuous(labels = coef_gdp$term, breaks = coef_gdp$x) +
  main_theme +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  labs(
    title = "B. Protective Expenditures/GDP (%)",
    x = "Wind Speed Forecast (m/s)",
    y = ""
  ) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) + 
  coord_cartesian(ylim = c(-0.15, 1.5))

effect_by_lead_day_pc = conleyreg(
  ex_ante_spending_pc ~
    i(forecast_cut) + i(forecast_precip_cut) |
    county + state^hurricane,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

coef_pc = tidy(effect_by_lead_day_pc, conf.int = TRUE) |>
  filter(str_detect(term, "forecast_cut")) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .before = 1,
  ) |>
  mutate(term = sort(unique(df$forecast_cut))) |>
  mutate(x = row_number(),
          estimate = estimate,
          conf.low = conf.low,
          conf.high = conf.high)

pc_plot = ggplot(coef_pc, aes(x, estimate)) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(aes(x = x, ymin = conf.low, ymax = conf.high), width = 0, color = "#000000") +
  geom_point(size = 3, color = "#000000") +
  scale_x_continuous(labels = coef_pc$term, breaks = coef_pc$x) +
  main_theme +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  labs(
    title = "C. Protective Expenditures/Person ($)",
    x = "Wind Speed Forecast (m/s)",
    y = ""
  ) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) + 
  coord_cartesian(ylim = c(-50, 500))

plot_out = log_plot + share_plot + pc_plot

ggsave(plot = plot_out, "results/Figure4.png", width = 3 * 10 * 2 / 3, height = 1 * 8 * 2 / 3)
ggsave(plot = plot_out, "results/Figure4.pdf", device = cairo_pdf, width = 3 * 10 * 2 / 3, height = 1 * 8 * 2 / 3)

# ======================
# 7. Figure 4 - Ex Post Analysis
# ======================

reg_dam = conleyreg(
  damages/1e6 ~ i(wind_error_bin, ref = "(-2,0]")
 |
  county + state^hurricane + wind_cut + precip_cut + precip_error_bin,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

coef_dam = tidy(reg_dam, conf.int = TRUE) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .after = 4
  ) |>
  filter(!str_detect(term, "wind_cut") & !str_detect(term, "precip")) |>
  mutate(term = sort(unique(df$wind_error_bin))) |>
  mutate(x = row_number()) |>
  mutate(type = "Damages")

reg_spend = conleyreg(
  ex_post_spending/1e6 ~ i(wind_error_bin, ref = "(-2,0]")
 |
  county + state^hurricane + wind_cut + precip_cut + precip_error_bin,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

coef_spend = tidy(reg_spend, conf.int = TRUE) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .after = 4
  ) |>
  filter(!str_detect(term, "wind_cut") & !str_detect(term, "precip")) |>
  mutate(term = sort(unique(df$wind_error_bin))) |>
  mutate(x = row_number()) |>
  mutate(type = "Recovery Expenditures")

reg_dam_share = conleyreg(
  damage_per_gdp ~ i(wind_error_bin, ref = "(-2,0]")
 |
  county + state^hurricane + wind_cut + precip_cut + precip_error_bin,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

coef_dam_share = tidy(reg_dam_share, conf.int = TRUE) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .after = 4
  ) |>
  filter(!str_detect(term, "wind_cut") & !str_detect(term, "precip")) |>
  mutate(term = sort(unique(df$wind_error_bin))) |>
  mutate(x = row_number()) |>
  mutate(type = "damage")

reg_spend_share = conleyreg(
  ex_post_spending_per_gdp ~ i(wind_error_bin, ref = "(-2,0]")
 |
  county + state^hurricane + wind_cut + precip_cut + precip_error_bin,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

coef_spend_share = tidy(reg_spend_share, conf.int = TRUE) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .after = 4
  ) |>
  filter(!str_detect(term, "wind_cut") & !str_detect(term, "precip")) |>
  mutate(term = sort(unique(df$wind_error_bin))) |>
  mutate(x = row_number()) |>
  mutate(type = "spending")

reg_dam_pc = conleyreg(
  damage_pc ~ i(wind_error_bin, ref = "(-2,0]")
 |
  county + state^hurricane + wind_cut + precip_cut + precip_error_bin,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

coef_dam_pc = tidy(reg_dam_pc, conf.int = TRUE) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .after = 4
  ) |>
  filter(!str_detect(term, "wind_cut") & !str_detect(term, "precip")) |>
  mutate(term = sort(unique(df$wind_error_bin))) |>
  mutate(x = row_number()) |>
  mutate(type = "damage")

reg_spend_pc = conleyreg(
  ex_post_spending_pc ~ i(wind_error_bin, ref = "(-2,0]")
 |
  county + state^hurricane + wind_cut + precip_cut + precip_error_bin,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

coef_spend_pc = tidy(reg_spend_pc, conf.int = TRUE) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .after = 4
  ) |>
  filter(!str_detect(term, "wind_cut") & !str_detect(term, "precip")) |>
  mutate(term = sort(unique(df$wind_error_bin))) |>
  mutate(x = row_number()) |>
  mutate(type = "spending")

log_plot_dam = ggplot(coef_dam, aes(x = x, y = estimate)) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(aes(x = x, ymin = conf.low, ymax = conf.high), width = 0,
        position = position_dodge(width = 0.25)) +
  geom_point(size = 3,
        position = position_dodge(width = 0.25)) +
  main_theme +
  scale_x_continuous(labels = coef_dam$term, breaks = coef_dam$x) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  labs(
    title = "A. Damages (million $)",
    x = "",
    y = ""
  ) +
  annotate("text",
           x = 2, y = -150,
           family = "Lato",
           label = "Overestimate", size = 6, hjust = 0
  ) +
  annotate("text",
           x = 6.3, y = -150,
           family = "Lato",
           label = "Underestimate", size = 6, hjust = 0
  ) +
  geom_segment(aes(x = 1.85, y = -150, xend = 1.25, yend = -150),
               arrow = arrow(length = unit(0.1, "cm")), color = "black"
  ) +
  geom_segment(aes(x = 9.35, y = -150, xend = 9.95, yend = -150),
               arrow = arrow(length = unit(0.1, "cm")), color = "black"
  ) +
  main_theme +
  theme(
    axis.text.x = element_text(size = 14, angle = 45, vjust = 1, hjust = 1)
  ) + 
  coord_cartesian(ylim = c(-200, 400))

log_plot_spend = ggplot(coef_spend, aes(x = x, y = estimate)) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(aes(x = x, ymin = conf.low, ymax = conf.high), width = 0,
        position = position_dodge(width = 0.25)) +
  geom_point(size = 3,
        position = position_dodge(width = 0.25)) +
  main_theme +
  scale_x_continuous(labels = coef_spend$term, breaks = coef_spend$x) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  labs(
    title = "D. Recovery Expenditures (million $)",
    x = "Wind Speed Underestimate (m/s)",
    y = ""
  ) +
  main_theme +
  theme(
    axis.text.x = element_text(size = 14, angle = 45, vjust = 1, hjust = 1)
  ) + 
  coord_cartesian(ylim = c(-20, 40))

share_plot_dam = ggplot(coef_dam_share, aes(x = x, y = estimate * 100)) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(aes(x = x, ymin = conf.low * 100, ymax = conf.high * 100), width = 0,
        position = position_dodge(width = 0.25)) +
  geom_point(size = 3,
        position = position_dodge(width = 0.25)) +
  main_theme +
  scale_x_continuous(labels = coef_dam_share$term, breaks = coef_dam_share$x) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  labs(
    title = "B. Damages/GDP (%)",
    x = "",
    y = ""
  ) +
  main_theme +
  theme(
    axis.text.x = element_text(size = 14, angle = 45, vjust = 1, hjust = 1)
  ) + 
  coord_cartesian(ylim = c(-20, 40))

share_plot_spend = ggplot(coef_spend_share, aes(x = x, y = estimate*100)) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(aes(x = x, ymin = conf.low*100, ymax = conf.high*100), width = 0,
        position = position_dodge(width = 0.25)) +
  geom_point(size = 3,
        position = position_dodge(width = 0.25)) +
  main_theme +
  scale_x_continuous(labels = coef_spend_share$term, breaks = coef_spend_share$x) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  labs(
    title = "E. Recovery Expenditures/GDP (%)",
    x = "Wind Speed Underestimate (m/s)",
    y = ""
  ) +
  main_theme +
  theme(
    axis.text.x = element_text(size = 14, angle = 45, vjust = 1, hjust = 1)
  ) + 
  coord_cartesian(ylim = c(-1.5, 1.0))

pc_plot_dam = ggplot(coef_dam_pc, aes(x = x, y = estimate )) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(aes(x = x, ymin = conf.low , ymax = conf.high), width = 0,
        position = position_dodge(width = 0.25)) +
  geom_point(size = 3,
        position = position_dodge(width = 0.25)) +
  main_theme +
  scale_x_continuous(labels = coef_dam_pc$term, breaks = coef_dam_pc$x) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  labs(
    title = "C. Damages/Person ($)",
    x = "",
    y = ""
  ) +
  main_theme +
  theme(
    axis.text.x = element_text(size = 14, angle = 45, vjust = 1, hjust = 1)
  ) + 
  coord_cartesian(ylim = c(-5000, 10000))

pc_plot_spend = ggplot(coef_spend_pc, aes(x = x, y = estimate)) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(aes(x = x, ymin = conf.low, ymax = conf.high), width = 0,
        position = position_dodge(width = 0.25)) +
  geom_point(size = 3,
        position = position_dodge(width = 0.25)) +
  main_theme +
  scale_x_continuous(labels = coef_spend_share$term, breaks = coef_spend_share$x) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  labs(
    title = "F. Recovery Expenditures/Person ($)",
    x = "Wind Speed Underestimate (m/s)",
    y = ""
  ) +
  main_theme +
  theme(
    axis.text.x = element_text(size = 14, angle = 45, vjust = 1, hjust = 1)
  ) + 
  coord_cartesian(ylim = c(-500, 1000))

plot_out = (log_plot_dam + share_plot_dam + pc_plot_dam) / (log_plot_spend + share_plot_spend + pc_plot_spend)

ggsave(plot = plot_out, "results/Figure5.png", width = 3 * 10 * 2 / 3, height = 2 * 8 * 2 / 3)
ggsave(plot = plot_out, "results/Figure5.pdf", device = cairo_pdf, width = 3 * 10 * 2 / 3, height = 2 * 8 * 2 / 3)

# ==============================================================================
# 8. Table 2 - Value of Improving the Forecast Analysis
# ==============================================================================

regs = list()

regs[[1]] = conleyreg(
  total_cost ~ squared_error + squared_error_precip | 
              county + state^hurricane,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs[[2]] = conleyreg(
  total_cost ~ squared_error + squared_error_precip | 
              county + state^hurricane + wind_cut + precip_cut,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs[[3]] = conleyreg(
  total_cost ~ squared_error + squared_error_precip + level_error + level_error_precip | 
              county + state^hurricane + wind_cut + precip_cut,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs[[4]] = conleyreg(
  total_cost ~ squared_error  + squared_error_precip + level_error + level_error_precip | 
              county + state^hurricane + county^month + wind_cut + precip_cut,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs[[5]] = conleyreg(
  total_cost ~ squared_error  + squared_error_precip + level_error + level_error_precip | 
              county + state^hurricane + county^year + wind_cut + precip_cut,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs[[6]] = conleyreg(
  total_cost ~ squared_error  + squared_error_precip + level_error + level_error_precip | 
              county + state^hurricane +
              county^year + county^month + wind_cut + precip_cut,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs[[7]] = conleyreg(
  total_cost ~ squared_error:category + squared_error_precip + level_error + level_error_precip | 
              county + state^hurricane + wind_cut + precip_cut,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pc = list()

regs_pc[[1]] = conleyreg(
  total_cost_pc ~ squared_error + squared_error_precip | 
              county + state^hurricane,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pc[[2]] = conleyreg(
  total_cost_pc ~ squared_error + squared_error_precip | 
              county + state^hurricane + wind_cut + precip_cut,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pc[[3]] = conleyreg(
  total_cost_pc ~ squared_error + squared_error_precip + level_error + level_error_precip | 
              county + state^hurricane + wind_cut + precip_cut,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pc[[4]] = conleyreg(
  total_cost_pc ~ squared_error  + squared_error_precip + level_error + level_error_precip | 
              county + state^hurricane + county^month + wind_cut + precip_cut,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pc[[5]] = conleyreg(
  total_cost_pc ~ squared_error  + squared_error_precip + level_error + level_error_precip | 
              county + state^hurricane + county^year + wind_cut + precip_cut,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pc[[6]] = conleyreg(
  total_cost_pc ~ squared_error  + squared_error_precip + level_error + level_error_precip | 
              county + state^hurricane +
              county^year + county^month + wind_cut + precip_cut,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pc[[7]] = conleyreg(
  total_cost_pc ~ squared_error:category + squared_error_precip + level_error + level_error_precip | 
              county + state^hurricane + wind_cut + precip_cut,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pgdp = list()

regs_pgdp[[1]] = conleyreg(
  total_cost_per_gdp*100 ~ squared_error + squared_error_precip | 
              county + state^hurricane,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pgdp[[2]] = conleyreg(
  total_cost_per_gdp*100 ~ squared_error + squared_error_precip | 
              county + state^hurricane + wind_cut + precip_cut,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pgdp[[3]] = conleyreg(
  total_cost_per_gdp*100 ~ squared_error + squared_error_precip + level_error + level_error_precip | 
              county + state^hurricane + wind_cut + precip_cut,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pgdp[[4]] = conleyreg(
  total_cost_per_gdp*100 ~ squared_error  + squared_error_precip + level_error + level_error_precip | 
              county + state^hurricane + county^month + wind_cut + precip_cut,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pgdp[[5]] = conleyreg(
  total_cost_per_gdp*100 ~ squared_error  + squared_error_precip + level_error + level_error_precip | 
              county + state^hurricane + county^year + wind_cut + precip_cut,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pgdp[[6]] = conleyreg(
  total_cost_per_gdp*100 ~ squared_error  + squared_error_precip + level_error + level_error_precip | 
              county + state^hurricane +
              county^year + county^month + wind_cut + precip_cut,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pgdp[[7]] = conleyreg(
  total_cost_per_gdp*100 ~ squared_error:category + squared_error_precip + level_error + level_error_precip | 
              county + state^hurricane + wind_cut + precip_cut,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

obs = feols(
  total_cost ~ squared_error  + squared_error_precip | 
              county + state^hurricane +
              county^year + county^month + wind_cut + precip_cut,
  data = load_df()
)$nobs |>
  format(big.mark = ",")
  
rows = tribble(
  ~term, ~"(1)", ~"(2)", ~"(3)", ~"(4)", ~"(5)", ~"(6)", ~"(7)",
  "", "", "", "", "", "", "", "", 
  "Observations", obs, obs, obs, obs, obs, obs, obs, 
  "", "", "", "", "", "", "", "", 
  "Realized Wind/Precip Bins", "", "\\checkmark", "\\checkmark", "\\checkmark",  "\\checkmark", "\\checkmark", "\\checkmark",
  "Level Wind/Precip Error", "", "", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", 
  "State-Hurricane FE", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", 
  "County FE", "\\checkmark", "\\checkmark", "\\checkmark",  "", "",  "", "\\checkmark",
  "County-Month of Year FE", "", "", "", "\\checkmark", "", "\\checkmark", "",
  "County-Year FE", "", "", "", "",  "\\checkmark", "\\checkmark", ""
)

msummary(list("Damages + Recovery Expenditures (million $)" = regs, "(Damages + Recovery Expenditures) / GDP (%)" = regs_pgdp, "Damages + Recovery Expenditures Per Capita ($/person)" = regs_pc),
  coef_rename = c("$\\beta_2: (e - \\mu)^2$", "$\\text{Hurricane } \\beta_2: (e - \\mu)^2$", "$\\text{Sub-Hurricane } \\beta_2: (e - \\mu)^2$"),
  coef_omit = c(-1, -5, -6),
  gof_omit = "R2|AIC|BIC|Log.|RMSE|FE|Std.Errors",
  #stars =  c('*' = .1, '**' = .05, '***' = .01),
  fmt = "%.2f",
  add_rows = rows,
  title = "The Value of a Wind Speed Forecast Improvement. \\label{tab:voi-table}",
  notes = list("Standard errors are Conley Spatial HAC with a distance radius of 400 km for spatial correlation and arbitrary autocorrelation within counties. 
               $e$ is the county-hurricane-specific forecast error, and $\\\\mu$ is the hurricane-intensity bin-specific mean forecast error. 
               Hurricane wind speeds are those greater than 33 m/s (74 mph), while Sub-Hurricane wind speeds fall below that threshold."),
  threeparttable = TRUE,
  shape = 'rbind',
  escape = FALSE,
  output = "results/Table2.tex"
)

add_adjust_box("results/Table2.tex")
