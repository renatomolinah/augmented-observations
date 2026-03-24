# ==============================================================================
# DOES FEMA RESPOND TO FORECASTS?
# ==============================================================================
# OUTLINE:
# 1. Main Table 
# 2. Conley Robustness (600km)
# 3. PDD Robustness
# 4. Coastal States Analysis
# 5. Inverse Hyperbolic Sine Analysis
# 6. Precipitation Analysis
# 7. No Early Declarations
# 8. Intensity Controls
# 9. Forecast Uncertainty (Standard Deviation)
# 10. Population Weighting
# 11. Track Effects (side track and track controls)
# 12. Wind Effects Conditional on Track
# 12. Wind Effects Conditional on Location Relative to Track
# ==============================================================================
gc()

# ==============================================================================
# 1. MAIN TABLE
# ==============================================================================

# PROTECTIVE EXPENDITURE TABLES (LEVEL)

regs = list()

regs[[1]] = conleyreg(
  ex_ante_spending/1e6
  ~ wind_fc + precip_fc + wind_fc^2 + precip_fc^2 |    
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
  ex_ante_spending/1e6
  ~ wind_fc + precip_fc + wind_fc^2 + precip_fc^2 |
    county + state^hurricane + county^month,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs[[3]] = conleyreg(
  ex_ante_spending/1e6
  ~ wind_fc + precip_fc + wind_fc^2 + precip_fc^2 |
    county^year + state^hurricane,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs[[4]] = conleyreg(
  ex_ante_spending/1e6
  ~ wind_fc + precip_fc + wind_fc^2 + precip_fc^2 |
    county^year + state^hurricane + county^month,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

# PROTECTIVE EXPENDITURE TABLES (GDP SHARE)

regs_gdp = list()

regs_gdp[[1]] = conleyreg(
  ex_ante_spending_per_gdp*100
  ~ wind_fc + precip_fc + wind_fc^2 + precip_fc^2 |
    county + state^hurricane,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_gdp[[2]] = conleyreg(
  ex_ante_spending_per_gdp*100
  ~ wind_fc + precip_fc + wind_fc^2 + precip_fc^2 |
    county + state^hurricane + county^month,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_gdp[[3]] = conleyreg(
  ex_ante_spending_per_gdp*100
  ~ wind_fc + precip_fc + wind_fc^2 + precip_fc^2 |
    county^year + state^hurricane,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_gdp[[4]] = conleyreg(
  ex_ante_spending_per_gdp*100
  ~ wind_fc + precip_fc + wind_fc^2 + precip_fc^2 |
    county^year + state^hurricane + county^month,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

# PROTECTIVE EXPENDITURE TABLES (PER CAPITA)

regs_pc = list()

regs_pc[[1]] = conleyreg(
  ex_ante_spending_pc
  ~ wind_fc + precip_fc + wind_fc^2 + precip_fc^2 |
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
  ex_ante_spending_pc
  ~ wind_fc + precip_fc + wind_fc^2 + precip_fc^2 |
    county + state^hurricane + county^month,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pc[[3]] = conleyreg(
  ex_ante_spending_pc
  ~ wind_fc + precip_fc + wind_fc^2 + precip_fc^2 |
    county^year + state^hurricane,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pc[[4]] = conleyreg(
  ex_ante_spending_pc
  ~ wind_fc + precip_fc + wind_fc^2 + precip_fc^2 |
    county^year + state^hurricane + county^month,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

obs = feols(
  ex_ante_spending ~ wind_fc + precip_fc + wind_fc^2 + precip_fc^2 |
    county^year + state^hurricane,
  data = df
)$nobs |>
  format(big.mark = ",")

rows = tribble(
  ~term, ~"(1)", ~"(2)", ~"(3)", ~"(4)", 
  "", "", "", "", "", 
  "Observations", obs, obs, obs, obs, 
  "", "", "", "", "",
  "State-Hurricane FE", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark",
  "County FE", "\\checkmark", "", "", "",  
  "County-Month of Year FE", "", "\\checkmark", "", "\\checkmark",
  "County-Year FE", "", "", "\\checkmark", "\\checkmark"
)

tab = msummary(list("Protective Expenditures (million $)" = regs, "Protective Expenditures / GDP (%)" = regs_gdp, "Protective Expenditures / Person ($)" = regs_pc
),
coef_map = c(
  "wind_fc" = "Wind Forecast (m/s)",
  "I(wind_fc^2)" = "Wind Forecast$^2$",
  "precip_fc" = "Precip Forecast (mm)",
  "I(precip_fc^2)" = "Precip Forecast$^2$"
),
gof_omit = "R2|AIC|BIC|Log.|RMSE|FE|Std.Errors",
stars =  c('*' = .1, '**' = .05, '***' = .01),
fmt = "%.4f",
add_rows = rows,
title = "The Effect of Forecast Attributes on Before-Landfall FEMA Protective Expenditures. \\label{atab:fema_spending}",
notes = list("Standard errors are Conley Spatial HAC with a distance radius of 400 km for spatial correlation and arbitrary autocorrelation within counties."),
threeparttable = TRUE,
# longtable = TRUE,
shape = 'rbind',
escape = FALSE,
output = "results/TableC1.tex"
) 

add_adjust_box("results/TableC1.tex")

# ==============================================================================
# 2. CONLEY ROBUSTNESS (600KM RADIUS)
# ==============================================================================

# Load in data
df = readRDS("data/clean/final_dataset.rds") |> as_tibble()

effect_by_lead_day = conleyreg(
  ex_ante_spending/1e6 ~
    i(forecast_cut) + i(forecast_precip_cut)  |
    county + state^hurricane,
  data = df,
  dist_cutoff = 600,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

coef = tidy(effect_by_lead_day, conf.int = TRUE) |>
  filter(str_detect(term, "forecast_cut")) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .before = 1,
  ) |>
  mutate(term = sort(unique(df$forecast_cut))) |>
  mutate(x = row_number())

log_plot = ggplot(coef, aes(x, estimate)) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(aes(x = x, ymin = conf.low, ymax = conf.high), width = 0) +
  geom_point(size = 3) +
  scale_x_continuous(labels = coef$term, breaks = coef$x) +
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

# Load in data
df = readRDS("data/clean/final_dataset.rds") |> as_tibble()

effect_by_lead_day = conleyreg(
  ex_ante_spending_per_gdp ~
    i(forecast_cut) + i(forecast_precip_cut)  |
    county + state^hurricane,
  data = df,
  dist_cutoff = 600,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

coef = tidy(effect_by_lead_day, conf.int = TRUE) |>
  filter(str_detect(term, "forecast_cut")) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .before = 1,
  ) |>
  mutate(term = sort(unique(df$forecast_cut))) |>
  mutate(x = row_number(),
          estimate = estimate,
          conf.low = conf.low,
          conf.high = conf.high)

share_plot = ggplot(coef, aes(x, estimate*100)) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(aes(x = x, ymin = conf.low*100, ymax = conf.high*100), width = 0) +
  geom_point(size = 3) +
  scale_x_continuous(labels = coef$term, breaks = coef$x) +
  main_theme +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  labs(
    title = "B. Protective Expenditures/GDP (%)",
    x = "Wind Speed Forecast (m/s)",
    y = ""
  ) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) + 
  coord_cartesian(ylim = c(-0.15, 1.5))

# Load in data
df = readRDS("data/clean/final_dataset.rds") |> as_tibble()

effect_by_lead_day = conleyreg(
  ex_ante_spending_pc ~
    i(forecast_cut) + i(forecast_precip_cut) |
    county + state^hurricane,
  data = df,
  dist_cutoff = 600,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

coef = tidy(effect_by_lead_day, conf.int = TRUE) |>
  filter(str_detect(term, "forecast_cut")) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .before = 1,
  ) |>
  mutate(term = sort(unique(df$forecast_cut))) |>
  mutate(x = row_number(),
          estimate = estimate,
          conf.low = conf.low,
          conf.high = conf.high)

pc_plot = ggplot(coef, aes(x, estimate)) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(aes(x = x, ymin = conf.low, ymax = conf.high), width = 0) +
  geom_point(size = 3) +
  scale_x_continuous(labels = coef$term, breaks = coef$x) +
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

ggsave(plot = plot_out, "results/FigureC1.png", width = 3 * 10 * 2 / 3, height = 1 * 8 * 2 / 3)
ggsave(plot = plot_out, "results/FigureC1.pdf", device = cairo_pdf, width = 3 * 10 * 2 / 3, height = 1 * 8 * 2 / 3)

# ==============================================================================
# 3. PDD ROBUSTNESS
# ==============================================================================

# Load in data
df = load_pdd()

effect_by_lead_day = conleyreg(
  ex_ante_spending/1e6 ~
    i(forecast_cut) + i(forecast_precip_cut)  |
    county + state^hurricane,
  data = df,
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

coef = tidy(effect_by_lead_day, conf.int = TRUE) |>
  filter(str_detect(term, "forecast_cut")) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .before = 1,
  ) |>
  mutate(term = sort(unique(df$forecast_cut))) |>
  mutate(x = row_number())

log_plot = ggplot(coef, aes(x, estimate)) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(aes(x = x, ymin = conf.low, ymax = conf.high), width = 0) +
  geom_point(size = 3) +
  scale_x_continuous(labels = coef$term, breaks = coef$x) +
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

# Load in data
df = load_pdd()

effect_by_lead_day = conleyreg(
  ex_ante_spending_per_gdp ~
    i(forecast_cut) + i(forecast_precip_cut)  |
    county + state^hurricane,
  data = df,
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

coef = tidy(effect_by_lead_day, conf.int = TRUE) |>
  filter(str_detect(term, "forecast_cut")) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .before = 1,
  ) |>
  mutate(term = sort(unique(df$forecast_cut))) |>
  mutate(x = row_number(),
         estimate = estimate,
         conf.low = conf.low,
         conf.high = conf.high)

share_plot = ggplot(coef, aes(x, estimate*100)) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(aes(x = x, ymin = conf.low*100, ymax = conf.high*100), width = 0) +
  geom_point(size = 3) +
  scale_x_continuous(labels = coef$term, breaks = coef$x) +
  main_theme +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  labs(
    title = "B. Protective Expenditures/GDP (%)",
    x = "Wind Speed Forecast (m/s)",
    y = ""
  ) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) + 
  coord_cartesian(ylim = c(-0.15, 1.5))

# Load in data
df = load_pdd()

effect_by_lead_day = conleyreg(
  ex_ante_spending_pc ~
    i(forecast_cut) + i(forecast_precip_cut) |
    county + state^hurricane,
  data = df,
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

coef = tidy(effect_by_lead_day, conf.int = TRUE) |>
  filter(str_detect(term, "forecast_cut")) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .before = 1,
  ) |>
  mutate(term = sort(unique(df$forecast_cut))) |>
  mutate(x = row_number(),
         estimate = estimate,
         conf.low = conf.low,
         conf.high = conf.high)

pc_plot = ggplot(coef, aes(x, estimate)) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(aes(x = x, ymin = conf.low, ymax = conf.high), width = 0) +
  geom_point(size = 3) +
  scale_x_continuous(labels = coef$term, breaks = coef$x) +
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

ggsave(plot = plot_out, "results/FigureC2.png", width = 3 * 10 * 2 / 3, height = 1 * 8 * 2 / 3)
ggsave(plot = plot_out, "results/FigureC2.pdf", device = cairo_pdf, width = 3 * 10 * 2 / 3, height = 1 * 8 * 2 / 3)

# ==============================================================================
# 4. COASTAL STATES ANALYSIS
# ==============================================================================

# TX, LA, MS, AL, GA, FL, SC, NC, VA, MD, NJ, PA, CT, DE, NY, RI, MA, NH, ME
coastal_states = c(48, 22, 28, 01, 13, 12, 45, 37, 51, 24, 34, 10, 42, 36, 09, 44, 33, 23, 25)

df = readRDS("data/clean/final_dataset.rds") |>
  as_tibble() |> 
  filter(state %in% coastal_states)

effect_by_lead_day = conleyreg(
  ex_ante_spending/1e6 ~
    i(forecast_cut) + i(forecast_precip_cut)  |
    county + state^hurricane,
  data = df,
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

coef = tidy(effect_by_lead_day, conf.int = TRUE) |>
  filter(str_detect(term, "forecast_cut")) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .before = 1,
  ) |>
  mutate(term = sort(unique(df$forecast_cut))) |>
  mutate(x = row_number())

log_plot = ggplot(coef, aes(x, estimate)) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(aes(x = x, ymin = conf.low, ymax = conf.high), width = 0) +
  geom_point(size = 3) +
  scale_x_continuous(labels = coef$term, breaks = coef$x) +
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

# Load in data
df = readRDS("data/clean/final_dataset.rds") |>
  as_tibble() |> 
  filter(state %in% coastal_states)

effect_by_lead_day = conleyreg(
  ex_ante_spending_per_gdp ~
    i(forecast_cut) + i(forecast_precip_cut)  |
    county + state^hurricane,
  data = df,
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

coef = tidy(effect_by_lead_day, conf.int = TRUE) |>
  filter(str_detect(term, "forecast_cut")) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .before = 1,
  ) |>
  mutate(term = sort(unique(df$forecast_cut))) |>
  mutate(x = row_number(),
         estimate = estimate,
         conf.low = conf.low,
         conf.high = conf.high)

share_plot = ggplot(coef, aes(x, estimate*100)) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(aes(x = x, ymin = conf.low*100, ymax = conf.high*100), width = 0) +
  geom_point(size = 3) +
  scale_x_continuous(labels = coef$term, breaks = coef$x) +
  main_theme +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  labs(
    title = "B. Protective Expenditures/GDP (%)",
    x = "Wind Speed Forecast (m/s)",
    y = ""
  ) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) + 
  coord_cartesian(ylim = c(-0.15, 1.5))

# Load in data
df = readRDS("data/clean/final_dataset.rds") |>
  as_tibble() |>
  filter(state %in% coastal_states)

effect_by_lead_day = conleyreg(
  ex_ante_spending_pc ~
    i(forecast_cut) + i(forecast_precip_cut) |
    county + state^hurricane,
  data = df,
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

coef = tidy(effect_by_lead_day, conf.int = TRUE) |>
  filter(str_detect(term, "forecast_cut")) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .before = 1,
  ) |>
  mutate(term = sort(unique(df$forecast_cut))) |>
  mutate(x = row_number(),
         estimate = estimate,
         conf.low = conf.low,
         conf.high = conf.high)

pc_plot = ggplot(coef, aes(x, estimate)) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(aes(x = x, ymin = conf.low, ymax = conf.high), width = 0) +
  geom_point(size = 3) +
  scale_x_continuous(labels = coef$term, breaks = coef$x) +
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

ggsave(plot = plot_out, "results/FigureC3.png", width = 3 * 10 * 2 / 3, height = 1 * 8 * 2 / 3)
ggsave(plot = plot_out, "results/FigureC3.pdf", device = cairo_pdf, width = 3 * 10 * 2 / 3, height = 1 * 8 * 2 / 3)

# ==============================================================================
# 5. INVERSE HYPERBOLIC SINE ANALYSIS
# ==============================================================================

# Load in data
df = readRDS("data/clean/final_dataset.rds") |>
  as_tibble()

effect_by_lead_day = conleyreg(
  asinh_ex_ante_spending ~
    i(forecast_cut) + i(forecast_precip_cut) |
    county + state^hurricane,
  data = df,
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

coef = tidy(effect_by_lead_day, conf.int = TRUE) |>
  filter(str_detect(term, "forecast_cut")) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .before = 1,
  ) |>
  mutate(term = sort(unique(df$forecast_cut))) |>
  mutate(x = row_number())

mean_plot = ggplot(coef, aes(x, estimate)) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(aes(x = x, ymin = conf.low, ymax = conf.high), width = 0) +
  geom_point(size = 3) +
  scale_x_continuous(labels = coef$term, breaks = coef$x) +
  main_theme +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  labs(
    title = "A. Wind Speed Forecast\nand Protective Expenditures",
    x = "Wind Speed Forecast (m/s)",
    y = "asinh(Protective Expenditures)"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) + 
  coord_cartesian(ylim = c(-0.5, 15))

# Load in data
df = readRDS("data/clean/final_dataset.rds") |>
  as_tibble()

effect_by_lead_day = conleyreg(
  asinh_ex_ante_spending ~
    i(forecast_cut) + i(forecast_precip_cut)  |
    county + state^hurricane,
  data = df,
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

coef = tidy(effect_by_lead_day, conf.int = TRUE) |>
  filter(str_detect(term, "forecast_precip_cut")) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .before = 1,
  ) |>
  mutate(term = sort(unique(df$forecast_precip_cut))) |>
  mutate(x = row_number())

mean_precip_plot = ggplot(coef, aes(x, estimate)) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(aes(x = x, ymin = conf.low, ymax = conf.high), width = 0) +
  geom_point(size = 3) +
  scale_x_continuous(labels = coef$term, breaks = coef$x) +
  main_theme +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  labs(
    title = "B. Precipitation Forecast\nand Protective Expenditures",
    x = "Precipitation Forecast (mm)",
    y = "asinh(Protective Expenditures)"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) + 
  coord_cartesian(ylim = c(-0.5, 15))

mean_plot + mean_precip_plot 

ggsave("results/FigureC4.png", width = 2 * 10 * 2 / 3, height = 1 * 8 * 2 / 3)
ggsave("results/FigureC4.pdf", device = cairo_pdf, width = 2 * 10 * 2 / 3, height = 1 * 8 * 2 / 3)

# ==============================================================================
# 6. PRECIPITATION ANALYSIS
# ==============================================================================

# Load in data
df = readRDS("data/clean/final_dataset.rds") |>
  as_tibble()

effect_by_lead_day = conleyreg(
  ex_ante_spending/1e6 ~
    i(forecast_precip_cut) + i(forecast_cut) |
    county + state^hurricane,
  data = df,
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

coef = tidy(effect_by_lead_day, conf.int = TRUE) |>
  filter(str_detect(term, "forecast_precip_cut")) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .before = 1,
  ) |>
  mutate(term = sort(unique(df$forecast_precip_cut))) |>
  mutate(x = row_number())

log_plot = ggplot(coef, aes(x, estimate)) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(aes(x = x, ymin = conf.low, ymax = conf.high), width = 0) +
  geom_point(size = 3) +
  scale_x_continuous(labels = coef$term, breaks = coef$x) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  main_theme +
  labs(
    title = "A. Protective Expenditures (million $)",
    x = "Precipitation Forecast (mm)",
    y = ""
  ) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )

# Load in data
df = readRDS("data/clean/final_dataset.rds") |>
  as_tibble()

effect_by_lead_day = conleyreg(
  ex_ante_spending_per_gdp ~
    i(forecast_precip_cut) + i(forecast_cut)   |
    county + state^hurricane,
  data = df,
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

coef = tidy(effect_by_lead_day, conf.int = TRUE) |>
  filter(str_detect(term, "forecast_precip_cut")) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .before = 1,
  ) |>
  mutate(term = sort(unique(df$forecast_precip_cut))) |>
  mutate(x = row_number(),
         estimate = estimate,
         conf.low = conf.low,
         conf.high = conf.high)

share_plot = ggplot(coef, aes(x, estimate*100)) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(aes(x = x, ymin = conf.low*100, ymax = conf.high*100), width = 0) +
  geom_point(size = 3) +
  scale_x_continuous(labels = coef$term, breaks = coef$x) +
  main_theme +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  labs(
    title = "B. Protective Expenditures/GDP (%)",
    x = "Precipitation Forecast (mm)",
    y = ""
  ) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )

# Load in data
df = readRDS("data/clean/final_dataset.rds") |>
  as_tibble()

effect_by_lead_day = conleyreg(
  ex_ante_spending_pc ~
    i(forecast_precip_cut) + i(forecast_cut) |
    county + state^hurricane,
  data = df,
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

coef = tidy(effect_by_lead_day, conf.int = TRUE) |>
  filter(str_detect(term, "forecast_precip_cut")) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .before = 1,
  ) |>
  mutate(term = sort(unique(df$forecast_precip_cut))) |>
  mutate(x = row_number(),
         estimate = estimate,
         conf.low = conf.low,
         conf.high = conf.high)

pc_plot = ggplot(coef, aes(x, estimate)) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(aes(x = x, ymin = conf.low, ymax = conf.high), width = 0) +
  geom_point(size = 3) +
  scale_x_continuous(labels = coef$term, breaks = coef$x) +
  main_theme +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  labs(
    title = "C. Protective Expenditures/Person ($)",
    x = "Precipitation Forecast (mm)",
    y = ""
  ) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )

plot_out = log_plot + share_plot + pc_plot

ggsave(plot = plot_out, "results/FigureC5.png", width = 3 * 10 * 2 / 3, height = 1 * 8 * 2 / 3) 
ggsave(plot = plot_out, "results/FigureC5.pdf", device = cairo_pdf, width = 3 * 10 * 2 / 3, height = 1 * 8 * 2 / 3)

# ==============================================================================
# 7. EXCLUDING EARLY DECLARATIONS
# ==============================================================================

# Load and process FEMA data
# Source: https://www.fema.gov/openfema-data-page/public-assistance-funded-projects-details-v1
fema_df = fread("data/raw/fema/DisasterDeclarationsSummaries.csv") |> 
  clean_names() |>
  remove_empty(which = c("cols"))

weather_sf = readRDS("data/clean/weather_error_sf.rds") |>
  distinct(hurricane, date)

# Define disaster declaration numbers by hurricane
# Source: https://www.fema.gov/disaster/declarations

# Group disaster numbers by year and hurricane
disaster_nums = list(
  # 2024
  milton = c(4834, 4844),
  helene = c(3618, 4827, 4828, 4829, 4830, 4831, 4832),
  francine = c(3614, 4817),
  debby = c(3606, 3608, 4806, 4815, 4821, 4825, 4835),
  beryl = c(4798),

  # 2023
  idalia = c(3597, 4734, 4738),

  # 2022
  ian = c(4677, 3586, 4673, 3585, 3584),
  nicole = c(4680, 3588, 3587),
  
  # 2021
  ida = c(4627, 4629, 3627, 4626, 4618, 4614, 4615, 3572, 3573, 4611, 3569, 3568),
  nicholas = c(3574), # no financial assistance approved per FEMA
  
  # 2020
  laura = c(4572, 3541, 4559, 3540, 3539, 3538, 3537),
  hanna = c(3530),
  isaias = c(4574, 4568, 4566, 4567, 4560, 3535, 3534, 3533, 4583, 4580),
  delta = c(4570, 3548, 3547),
  zeta = c(3549, 4579, 4573, 4576, 4577),
  sally = c(4564, 4563, 3546, 3543, 3544, 3545),
  
  # 2019
  dorian = c(4468, 4465, 4464, 3423, 3422, 3421, 3420, 3419, 3418),
  barry = c(4458),
  
  # 2018
  michael = c(4412, 4411, 4406, 4401, 4400, 3407, 4399, 3406, 3405),
  florence = c(4393, 3403, 3400, 3401, 4394),
  
  # 2017
  harvey = c(4345, 3382, 4332),
  irma = c(3385, 3383, 3384, 4335, 4338, 3389, 4336, 4337, 3387, 3388, 3386, 4341, 4346),
  nate = c(4350, 4349, 3394, 3395, 3393, 3392),
  
  # 2016
  matthew = c(4291, 4286, 4285, 4283, 4284, 3380, 3378, 3379, 3377),
  hermine = c(4280),
  
  # 2014
  arthur = c(), # no disasters declared
  
  # 2012
  sandy = c(4099, 4098, 4097, 4096, 4095, 4093, 4092, 4091, 4090, 4089, 4086, 4087, 4085, 
            3360, 3355, 3356, 3357, 3358, 3359, 3354, 3349, 3350, 3351, 3352, 3353),
  isaac = c(4084, 4082, 4080, 4081, 3348, 3347),
  
  # 2011
  irene = c(4037, 4036, 4034, 4032, 4028, 4027, 4026, 4025, 
            4024, 4023, 4022, 4021, 4020, 4019, 4017, 3338, 
            3339, 3336, 3337, 3333, 3334, 3335, 3331, 3332, 
            3328, 3329, 3330, 3327, 3326),
  
  # 2008
  ike = c(1791, 1792, 3295, 3294, 3293),
  gustav = c(1806, 1794, 1793, 1789, 1786, 3292, 3291, 3289, 3290),
  dolly = c(1780),
  
  # 2005
  wilma = c(1609),
  katrina = c(3262, 3263, 3258, 3255, 3256, 3257, 3251, 3244, 3252, 3245, 3253, 3246, 
              3248, 3249, 3242, 3250, 3243, 3240, 3241, 3238, 3231, 3239, 3232, 3233, 
              3235, 3236, 3237, 3230, 3225, 3226, 3229, 3220, 3221, 3222, 3217, 3218, 
              3219, 3215, 3216, 1605, 1603, 1604, 3213, 3214, 1602, 3212),
  dennis = c(1593, 1594, 1595),
  rita = c(1606, 1607, 3260, 3261, 3259),
  cindy = c(1601)
)

# Combine all disaster numbers into single vector
disaster_numbers = unlist(disaster_nums)

# Process FEMA data
# Filter and add hurricane names
fema_df = fema_df[disaster_number %in% disaster_numbers] |>
  mutate(
    hurricane = case_when(
      disaster_number %in% disaster_nums$arthur ~ "arthur",
      disaster_number %in% disaster_nums$barry ~ "barry",
      disaster_number %in% disaster_nums$cindy ~ "cindy",
      disaster_number %in% disaster_nums$delta ~ "delta",
      disaster_number %in% disaster_nums$dennis ~ "dennis",
      disaster_number %in% disaster_nums$dolly ~ "dolly",
      disaster_number %in% disaster_nums$dorian ~ "dorian",
      disaster_number %in% disaster_nums$florence ~ "florence",
      disaster_number %in% disaster_nums$gustav ~ "gustav",
      disaster_number %in% disaster_nums$hanna ~ "hanna",
      disaster_number %in% disaster_nums$harvey ~ "harvey",
      disaster_number %in% disaster_nums$helene ~ "helene",
      disaster_number %in% disaster_nums$hermine ~ "hermine",
      disaster_number %in% disaster_nums$ian ~ "ian",
      disaster_number %in% disaster_nums$idalia ~ "idalia",
      disaster_number %in% disaster_nums$ida ~ "ida",
      disaster_number %in% disaster_nums$ike ~ "ike",
      disaster_number %in% disaster_nums$irene ~ "irene",
      disaster_number %in% disaster_nums$irma ~ "irma",
      disaster_number %in% disaster_nums$isaac ~ "isaac",
      disaster_number %in% disaster_nums$isaias ~ "isaias",
      disaster_number %in% disaster_nums$katrina ~ "katrina",
      disaster_number %in% disaster_nums$laura ~ "laura",
      disaster_number %in% disaster_nums$matthew ~ "matthew",
      disaster_number %in% disaster_nums$michael ~ "michael",
      disaster_number %in% disaster_nums$milton ~ "milton",
      disaster_number %in% disaster_nums$nate ~ "nate",
      disaster_number %in% disaster_nums$debby ~ "debby",
      disaster_number %in% disaster_nums$nicholas ~ "nicholas",
      disaster_number %in% disaster_nums$nicole ~ "nicole",
      disaster_number %in% disaster_nums$beryl ~ "beryl",
      disaster_number %in% disaster_nums$francine ~ "francine",
      disaster_number %in% disaster_nums$rita ~ "rita",
      disaster_number %in% disaster_nums$sally ~ "sally",
      disaster_number %in% disaster_nums$sandy ~ "sandy",
      disaster_number %in% disaster_nums$wilma ~ "wilma",
      disaster_number %in% disaster_nums$zeta ~ "zeta"
    )
  )

setDT(fema_df)
setDT(weather_sf)

# Merge with weather data and calculate date differences
fema_df = fema_df[weather_sf, on = c("hurricane"), nomatch = 0L] |>
  mutate(
    date = as.Date(date),
    declaration_date = as.Date(declaration_date),
    incident_begin_date = as.Date(incident_begin_date),
    day_diff_fema = as.numeric(incident_begin_date - declaration_date),
    day_diff_landfall = as.numeric(date - declaration_date)
  ) |>
  select(day_diff_fema, day_diff_landfall, fips_state_code, 
         fips_county_code, hurricane, disaster_number)

# Analyze declaration timing
# Print summary statistics
print(paste("Maximum difference between incident begin and declaration date:",
            max(fema_df$day_diff_fema)))
print(paste("Maximum difference between landfall and declaration date:",
            max(fema_df$day_diff_landfall)))

# Calculate shares within 3 days
fema_df_prior = fema_df |> 
  filter(day_diff_landfall >= 0)

share_within_3_days = mean(fema_df$day_diff_landfall <= 3)
share_within_3_days_prior = mean(fema_df_prior$day_diff_landfall <= 3)

print(paste("Share of declarations within 3 days of landfall:",
            round(share_within_3_days * 100, 2), "%"))
print(paste("Share of prior declarations within 3 days of landfall:",
            round(share_within_3_days_prior * 100, 2), "%"))

# Analyze early declarations
early_declarations = fema_df |>
  filter(day_diff_landfall > 3) |>
  group_by(hurricane) |>
  summarise(earliest_declaration = min(day_diff_landfall)) |>
  arrange(earliest_declaration)

print(early_declarations)

# Count early declarations by hurricane
early_declaration_counts = fema_df |>
  filter(day_diff_landfall > 3) |>
  count(hurricane) |>
  arrange(desc(n))

print("Number of declarations more than 3 days before landfall by hurricane:")
print(early_declaration_counts)

# Detailed breakdown of early declarations
early_declaration_details = fema_df |>
  filter(day_diff_landfall > 3) |>
  select(hurricane, fips_state_code, fips_county_code, 
         day_diff_landfall, disaster_number) |>
  arrange(hurricane, day_diff_landfall)

print("\nDetailed breakdown of early declarations:")
print(early_declaration_details)

# Create Figure 2 excluding early declaration hurricanes
# Get list of hurricanes to exclude
hurricanes_to_exclude = early_declaration_counts$hurricane

# Run regression analyses for each outcome
effect_by_lead_day = conleyreg(
  ex_ante_spending/1e6 ~ i(forecast_cut) + i(forecast_precip_cut) |
    county + state^hurricane + wind_cut + precip_cut,
  data = load_df() |> filter(!hurricane %in% hurricanes_to_exclude),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon",
  batch_ram_opt = "none"
)

effect_by_lead_day_share = conleyreg(
  ex_ante_spending_per_gdp ~ i(forecast_cut) + i(forecast_precip_cut) |
    county + state^hurricane + wind_cut + precip_cut,
  data = load_df() |> filter(!hurricane %in% hurricanes_to_exclude),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon",
  batch_ram_opt = "none"
)

effect_by_lead_day_pc = conleyreg(
  ex_ante_spending_pc ~ i(forecast_cut) + i(forecast_precip_cut) |
    county + state^hurricane + wind_cut + precip_cut,
  data = load_df() |> filter(!hurricane %in% hurricanes_to_exclude),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon",
  batch_ram_opt = "none"
)

# Process regression results
coef_log = tidy(effect_by_lead_day, conf.int = TRUE) |>
  filter(str_detect(term, "forecast_cut")) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .before = 1
  ) |>
  mutate(
    term = sort(unique(df$forecast_cut)),
    x = row_number()
  )

coef_share = tidy(effect_by_lead_day_share, conf.int = TRUE) |>
  filter(str_detect(term, "forecast_cut")) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .before = 1
  ) |>
  mutate(
    term = sort(unique(df$forecast_cut)),
    x = row_number()
  )

coef_pc = tidy(effect_by_lead_day_pc, conf.int = TRUE) |>
  filter(str_detect(term, "forecast_cut")) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .before = 1
  ) |>
  mutate(
    term = sort(unique(df$forecast_cut)),
    x = row_number()
  )

# Create plots
log_plot = ggplot(coef_log, aes(x, estimate)) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(
    aes(x = x, ymin = conf.low, ymax = conf.high),
    width = 0,
    color = "#000000"
  ) +
  geom_point(size = 3, color = "#000000") +
  scale_x_continuous(labels = coef_log$term, breaks = coef_log$x) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  labs(
    title = "A. Protective Expenditures (million $)",
    x = "Wind Speed Forecast (m/s)",
    y = ""
  ) +
  main_theme +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) +
  coord_cartesian(ylim = c(-5, 50))

share_plot = ggplot(coef_share, aes(x, estimate * 100)) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(
    aes(x = x, ymin = conf.low * 100, ymax = conf.high * 100),
    width = 0,
    color = "#000000"
  ) +
  geom_point(size = 3, color = "#000000") +
  scale_x_continuous(labels = coef_share$term, breaks = coef_share$x) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  labs(
    title = "B. Protective Expenditures/GDP (%)",
    x = "Wind Speed Forecast (m/s)",
    y = ""
  ) +
  main_theme +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) +
  coord_cartesian(ylim = c(-0.15, 1.5))

pc_plot = ggplot(coef_pc, aes(x, estimate)) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(
    aes(x = x, ymin = conf.low, ymax = conf.high),
    width = 0,
    color = "#000000"
  ) +
  geom_point(size = 3, color = "#000000") +
  scale_x_continuous(labels = coef_pc$term, breaks = coef_pc$x) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  labs(
    title = "C. Protective Expenditures/Person ($)",
    x = "Wind Speed Forecast (m/s)",
    y = ""
  ) +
  main_theme +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) +
  coord_cartesian(ylim = c(-50, 500))

# Combine plots and save
plot_out = (log_plot + share_plot + pc_plot)

ggsave(plot = plot_out, "results/FigureC6.png", width = 3 * 10 * 2/3, height = 8 * 2/3)
ggsave(  plot = plot_out, "results/FigureC6.pdf", device = cairo_pdf, width = 3 * 10 * 2/3, height = 8 * 2/3)

# ==============================================================================
# 8. CONTROL FOR INTENSITY
# ==============================================================================

# Load data
df = load_df() 

effect_by_lead_day = conleyreg(
  ex_ante_spending/1e6 ~ i(forecast_cut) + i(forecast_precip_cut) |
    county + state^hurricane + wind_cut + precip_cut,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon"
)

effect_by_lead_day_share = conleyreg(
  ex_ante_spending_per_gdp ~ i(forecast_cut) + i(forecast_precip_cut) |
    county + state^hurricane + wind_cut + precip_cut,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon"
)

effect_by_lead_day_pc = conleyreg(
  ex_ante_spending_pc ~ i(forecast_cut) + i(forecast_precip_cut) |
    county + state^hurricane + wind_cut + precip_cut,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon"
)

# Process regression results
coef_log = tidy(effect_by_lead_day, conf.int = TRUE) |>
  filter(str_detect(term, "forecast_cut")) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .before = 1
  ) |>
  mutate(
    term = sort(unique(df$forecast_cut)),
    x = row_number()
  )

coef_share = tidy(effect_by_lead_day_share, conf.int = TRUE) |>
  filter(str_detect(term, "forecast_cut")) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .before = 1
  ) |>
  mutate(
    term = sort(unique(df$forecast_cut)),
    x = row_number()
  )

coef_pc = tidy(effect_by_lead_day_pc, conf.int = TRUE) |>
  filter(str_detect(term, "forecast_cut")) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .before = 1
  ) |>
  mutate(
    term = sort(unique(df$forecast_cut)),
    x = row_number()
  )

# Create plots
log_plot = ggplot(coef_log, aes(x, estimate)) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(
    aes(x = x, ymin = conf.low, ymax = conf.high),
    width = 0,
    color = "#000000"
  ) +
  geom_point(size = 3, color = "#000000") +
  scale_x_continuous(labels = coef_log$term, breaks = coef_log$x) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  labs(
    title = "A. Protective Expenditures (million $)",
    x = "Wind Speed Forecast (m/s)",
    y = ""
  ) +
  main_theme +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) +
  coord_cartesian(ylim = c(-5, 50))

share_plot = ggplot(coef_share, aes(x, estimate * 100)) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(
    aes(x = x, ymin = conf.low * 100, ymax = conf.high * 100),
    width = 0,
    color = "#000000"
  ) +
  geom_point(size = 3, color = "#000000") +
  scale_x_continuous(labels = coef_share$term, breaks = coef_share$x) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  labs(
    title = "B. Protective Expenditures/GDP (%)",
    x = "Wind Speed Forecast (m/s)",
    y = ""
  ) +
  main_theme +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) +
  coord_cartesian(ylim = c(-0.15, 1.5))

pc_plot = ggplot(coef_pc, aes(x, estimate)) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(
    aes(x = x, ymin = conf.low, ymax = conf.high),
    width = 0,
    color = "#000000"
  ) +
  geom_point(size = 3, color = "#000000") +
  scale_x_continuous(labels = coef_pc$term, breaks = coef_pc$x) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  labs(
    title = "C. Protective Expenditures/Person ($)",
    x = "Wind Speed Forecast (m/s)",
    y = ""
  ) +
  main_theme +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) +
  coord_cartesian(ylim = c(-50, 500))

# Combine plots and save
plot_out = (log_plot + share_plot + pc_plot)

ggsave(plot = plot_out, "results/FigureC7.png", width = 3 * 10 * 2/3, height = 8 * 2/3)
ggsave(plot = plot_out, "results/FigureC7.pdf", device = cairo_pdf, width = 3 * 10 * 2/3, height = 8 * 2/3)

# ==============================================================================
# 9. FORECAST STANDARD DEVIATION EFFECT
# ==============================================================================

effect_by_lead_day_sd = conleyreg(
  ex_ante_spending/1e6 ~
    i(forecast_cut) + i(forecast_precip_cut) + i(forecast_sd_cut) + i(forecast_precip_sd_cut) |
    county + state^hurricane,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

coef_sd = tidy(effect_by_lead_day_sd, conf.int = TRUE) |>
  filter(str_detect(term, "forecast_sd_cut")) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .before = 1,
  ) |>
  mutate(term = sort(unique(df$forecast_sd_cut))) |>
  mutate(x = row_number())

sd_plot = ggplot(coef_sd, aes(x, estimate)) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(aes(x = x, ymin = conf.low, ymax = conf.high), width = 0, color = "#000000") +
  geom_point(size = 3, color = "#000000") +
  scale_x_continuous(labels = coef_sd$term, breaks = coef_sd$x) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  main_theme +
  labs(
    title = "A. Protective Expenditures (million $)",
    x = "Wind Speed Forecast SD (m/s)",
    y = ""
  ) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) + 
  coord_cartesian(ylim = c(-1, .1))

# Plot coefficients for precipitation forecast standard deviation

effect_by_lead_day_sd_gdp = conleyreg(
  ex_ante_spending_per_gdp ~
    i(forecast_cut) + i(forecast_precip_cut) + i(forecast_sd_cut) + i(forecast_precip_sd_cut) |
    county + state^hurricane,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

coef_sd_gdp = tidy(effect_by_lead_day_sd_gdp, conf.int = TRUE) |>
  filter(str_detect(term, "forecast_sd_cut")) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .before = 1,
  ) |>
  mutate(term = sort(unique(df$forecast_sd_cut))) |>
  mutate(x = row_number(),
         estimate = estimate,
         conf.low = conf.low,
         conf.high = conf.high)

sd_gdp_plot = ggplot(coef_sd_gdp, aes(x, estimate*100)) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(aes(x = x, ymin = conf.low*100, ymax = conf.high*100), width = 0, color = "#000000") +
  geom_point(size = 3, color = "#000000") +
  scale_x_continuous(labels = coef_sd$term, breaks = coef_sd$x) +
  main_theme +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  labs(
    title = "B. Protective Expenditures/GDP (%)",
    x = "Wind Speed Forecast SD (m/s)",
    y = ""
  ) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )  + 
  coord_cartesian(ylim = c(-0.015, .0015))

# Plot coefficients for wind forecast standard deviation with per capita spending
effect_by_lead_day_sd_pc = conleyreg(
  ex_ante_spending_pc ~
    i(forecast_cut) + i(forecast_precip_cut) + i(forecast_sd_cut) + i(forecast_precip_sd_cut) |
    county + state^hurricane,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

coef_sd_pc = tidy(effect_by_lead_day_sd_pc, conf.int = TRUE) |>
  filter(str_detect(term, "forecast_sd_cut")) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .before = 1,
  ) |>
  mutate(term = sort(unique(df$forecast_sd_cut))) |>
  mutate(x = row_number(),
         estimate = estimate,
         conf.low = conf.low,
         conf.high = conf.high)

sd_pc_plot = ggplot(coef_sd_pc, aes(x, estimate)) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(aes(x = x, ymin = conf.low, ymax = conf.high), width = 0, color = "#000000") +
  geom_point(size = 3, color = "#000000") +
  scale_x_continuous(labels = coef_sd_pc$term, breaks = coef_sd_pc$x) +
  main_theme +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  labs(
    title = "C. Protective Expenditures/Person ($)",
    x = "Wind Speed Forecast SD (m/s)",
    y = ""
  ) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) + 
  coord_cartesian(ylim = c(-10, 1))

plot_out = sd_plot + sd_gdp_plot + sd_pc_plot

ggsave(plot = plot_out, "results/FigureC8.png", width = 3 * 10 * 2 / 3, height = 1 * 8 * 2 / 3)
ggsave(plot = plot_out, "results/FigureC8.pdf", device = cairo_pdf, width = 3 * 10 * 2 / 3, height = 1 * 8 * 2 / 3)

# ==============================================================================
# 10. POPULATION-WEIGHTED
# ==============================================================================

effect_by_lead_day = conleyreg(
  ex_ante_spending/1e6 ~
    i(forecast_cut_popw) + i(forecast_precip_cut) |
    county + state^hurricane,
  data = load_df_popw(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

df_popw = load_df_popw()

coef_log = tidy(effect_by_lead_day, conf.int = TRUE) |>
  filter(str_detect(term, "forecast_cut_popw")) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .before = 1,
  ) |>
  mutate(term = sort(unique(df_popw$forecast_cut_popw))) |>
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
    i(forecast_cut_popw) + i(forecast_precip_cut) |
    county + state^hurricane,
  data = load_df_popw(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

coef_gdp = tidy(effect_by_lead_day_gdp, conf.int = TRUE) |>
  filter(str_detect(term, "forecast_cut_popw")) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .before = 1,
  ) |>
  mutate(term = sort(unique(df_popw$forecast_cut_popw))) |>
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
    i(forecast_cut_popw) + i(forecast_precip_cut) |
    county + state^hurricane,
  data = load_df_popw(),
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
  mutate(term = sort(unique(df_popw$forecast_cut_popw))) |>
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

ggsave(plot = plot_out, "results/FigureC9.png", width = 3 * 10 * 2 / 3, height = 1 * 8 * 2 / 3)
ggsave(plot = plot_out, "results/FigureC9.pdf", device = cairo_pdf, width = 3 * 10 * 2 / 3, height = 1 * 8 * 2 / 3)

# ==============================================================================
# 11. TRACK EFFECTS
# ==============================================================================

df_track = load_df_track()

effect_by_lead_day = conleyreg(
  ex_ante_spending/1e6 ~
    i(forecast_cut) + i(forecast_precip_cut) + i(forecast_track_cut) |
    county + state^hurricane,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

# Regression with only forecast_track_cut
effect_by_lead_day_track_only = conleyreg(
  ex_ante_spending/1e6 ~
    i(forecast_track_cut) |
    county + state^hurricane,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

coef_log = tidy(effect_by_lead_day, conf.int = TRUE) |>
  filter(str_detect(term, "forecast_track_cut")) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .before = 1,
  ) |>
  mutate(term = sort(unique(df_track$forecast_track_cut))) |>
  mutate(x = row_number(),
         model = "Track | Wind + Precip")

coef_log_track_only = tidy(effect_by_lead_day_track_only, conf.int = TRUE) |>
  filter(str_detect(term, "forecast_track_cut")) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .before = 1,
  ) |>
  mutate(term = sort(unique(df_track$forecast_track_cut))) |>
  mutate(x = row_number(),
         model = "Track Only")

# Create position adjustments for jittering
pos_full = position_nudge(x = -0.1)
pos_track = position_nudge(x = 0.1)

log_plot = ggplot() +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(data = coef_log, aes(x = x, ymin = conf.low, ymax = conf.high, color = model), width = 0, position = pos_full) +
  geom_point(data = coef_log, aes(x = x, y = estimate, color = model, shape = model), size = 3, position = pos_full) +
  geom_errorbar(data = coef_log_track_only, aes(x = x, ymin = conf.low, ymax = conf.high, color = model), width = 0, position = pos_track) +
  geom_point(data = coef_log_track_only, aes(x = x, y = estimate, color = model, shape = model), size = 3, position = pos_track) +
  scale_x_continuous(labels = coef_log$term, breaks = coef_log$x) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  scale_color_manual(values = c("Track | Wind + Precip" = "#000000", "Track Only" = "#0072B2")) +
  scale_shape_manual(values = c("Track | Wind + Precip" = 16, "Track Only" = 17)) +
  main_theme +
  labs(
    title = "A. Protective Expenditures (million $)",
    x = "Track Distance Forecast (km)",
    y = ""
  ) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    legend.text = element_text(size = 12),
    legend.position = c(0.07, 0.9),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = "white", color = NA),
    legend.title = element_blank(),
    legend.margin = margin(1, 1, 1, 1),
    legend.key.size = unit(1, "lines")
  ) # + coord_cartesian(ylim = c(-5, 50))

effect_by_lead_day_gdp = conleyreg(
  ex_ante_spending_per_gdp ~
    i(forecast_cut) + i(forecast_precip_cut) + i(forecast_track_cut) |
    county + state^hurricane,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

# Regression with only forecast_track_cut for per_gdp
effect_by_lead_day_gdp_track_only = conleyreg(
  ex_ante_spending_per_gdp ~
    i(forecast_track_cut) |
    county + state^hurricane,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

coef_gdp = tidy(effect_by_lead_day_gdp, conf.int = TRUE) |>
  filter(str_detect(term, "forecast_track_cut")) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .before = 1,
  ) |>
  mutate(term = sort(unique(df_track$forecast_track_cut))) |>
  mutate(x = row_number(),
         estimate = estimate,
         conf.low = conf.low,
         conf.high = conf.high,
         model = "Track | Wind + Precip")

coef_gdp_track_only = tidy(effect_by_lead_day_gdp_track_only, conf.int = TRUE) |>
  filter(str_detect(term, "forecast_track_cut")) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .before = 1,
  ) |>
  mutate(term = sort(unique(df_track$forecast_track_cut))) |>
  mutate(x = row_number(),
         model = "Track Only")

share_plot = ggplot() +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(data = coef_gdp, aes(x = x, ymin = conf.low*100, ymax = conf.high*100, color = model), width = 0, position = pos_full) +
  geom_point(data = coef_gdp, aes(x = x, y = estimate*100, color = model, shape = model), size = 3, position = pos_full) +
  geom_errorbar(data = coef_gdp_track_only, aes(x = x, ymin = conf.low*100, ymax = conf.high*100, color = model), width = 0, position = pos_track) +
  geom_point(data = coef_gdp_track_only, aes(x = x, y = estimate*100, color = model, shape = model), size = 3, position = pos_track) +
  scale_x_continuous(labels = coef_gdp$term, breaks = coef_gdp$x) +
  main_theme +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_color_manual(values = c("Track | Wind + Precip" = "#000000", "Track Only" = "#0072B2")) +
  scale_shape_manual(values = c("Track | Wind + Precip" = 16, "Track Only" = 17)) +
  labs(
    title = "B. Protective Expenditures/GDP (%)",
    x = "Track Distance Forecast (km)",
    y = ""
  ) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    legend.position = "none"
  ) # + coord_cartesian(ylim = c(-0.15, 1.5))

effect_by_lead_day_pc = conleyreg(
  ex_ante_spending_pc ~
    i(forecast_cut) + i(forecast_precip_cut) + i(forecast_track_cut) |
    county + state^hurricane,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

# Regression with only forecast_track_cut for per capita
effect_by_lead_day_pc_track_only = conleyreg(
  ex_ante_spending_pc ~
    i(forecast_track_cut) |
    county + state^hurricane,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

coef_pc = tidy(effect_by_lead_day_pc, conf.int = TRUE) |>
  filter(str_detect(term, "forecast_track_cut")) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .before = 1,
  ) |>
  mutate(term = sort(unique(df_track$forecast_track_cut))) |>
  mutate(x = row_number(),
         estimate = estimate,
         conf.low = conf.low,
         conf.high = conf.high,
         model = "Track | Wind + Precip")

coef_pc_track_only = tidy(effect_by_lead_day_pc_track_only, conf.int = TRUE) |>
  filter(str_detect(term, "forecast_track_cut")) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .before = 1,
  ) |>
  mutate(term = sort(unique(df_track$forecast_track_cut))) |>
  mutate(x = row_number(),
         model = "Track Only")

pc_plot = ggplot() +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(data = coef_pc, aes(x = x, ymin = conf.low, ymax = conf.high, color = model), width = 0, position = pos_full) +
  geom_point(data = coef_pc, aes(x = x, y = estimate, color = model, shape = model), size = 3, position = pos_full) +
  geom_errorbar(data = coef_pc_track_only, aes(x = x, ymin = conf.low, ymax = conf.high, color = model), width = 0, position = pos_track) +
  geom_point(data = coef_pc_track_only, aes(x = x, y = estimate, color = model, shape = model), size = 3, position = pos_track) +
  scale_x_continuous(labels = coef_pc$term, breaks = coef_pc$x) +
  main_theme +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  scale_color_manual(values = c("Track | Wind + Precip" = "#000000", "Track Only" = "#0072B2")) +
  scale_shape_manual(values = c("Track | Wind + Precip" = 16, "Track Only" = 17)) +
  labs(
    title = "C. Protective Expenditures/Person ($)",
    x = "Track Distance Forecast (km)",
    y = ""
  ) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    legend.position = "none"
  ) # + coord_cartesian(ylim = c(-50, 500))

plot_out = log_plot + share_plot + pc_plot

ggsave(plot = plot_out, "results/FigureC10.png", width = 3 * 10 * 2 / 3, height = 1 * 8 * 2 / 3)
ggsave(plot = plot_out, "results/FigureC10.pdf", device = cairo_pdf, width = 3 * 10 * 2 / 3, height = 1 * 8 * 2 / 3)

# ==============================================================================
# 11b. TRACK EFFECTS - ROBUSTNESS CUTOFF 
# ==============================================================================

# Load combined dataset
combined_data <- readRDS("data/clean/pooled_track_errors_all_cutoffs.rds")

# Split by cutoff for analysis
results_list <- list()
for (cutoff in cutoffs) {
  results_list[[as.character(cutoff)]] <- combined_data %>%
    filter(cutoff == !!cutoff)
}

# Define bin order for x-axis
bin_levels <- c("(0,40]", "(40,80]", "(80,120]", "(120,160]", "(160,200]", "(200,240]", "(240,280]", "(280,320]", "(320,360]", "(360,400]")

# Function to run regression and extract coefficients for track error bins (with all controls)
run_track_regression <- function(data, outcome_var) {
  reg <- conleyreg(
    as.formula(paste(outcome_var, "~ i(forecast_cut) + i(forecast_precip_cut) + i(forecast_track_cut) | county + state^hurricane")),
    data = data,
    dist_cutoff = 400,
    lag_cutoff = Inf,
    unit = "county",
    time = "date",
    lat = "lat",
    lon = "lon",
    batch_ram_opt = "none"
  )
  coefs <- tidy(reg, conf.int = TRUE) %>%
    filter(str_detect(term, "forecast_track_cut")) %>%
    mutate(
      bin_label = str_remove(term, "forecast_track_cut::"),
      bin_lower = as.numeric(str_extract(bin_label, "\\d+"))
    ) %>%
    filter(bin_label %in% bin_levels) %>%
    select(-bin_lower)
  # Add the reference bin (0,40] with coefficient 0
  ref_bin <- tibble(
    term = "forecast_track_cut::(0,40]",
    estimate = 0,
    std.error = 0,
    statistic = 0,
    p.value = 1,
    conf.low = 0,
    conf.high = 0,
    bin_label = "(0,40]"
  )
  coefs <- bind_rows(ref_bin, coefs)
  return(coefs)
}

# Only run the full regression for each cutoff and outcome
outcomes <- c("ex_ante_spending/1e6", "ex_ante_spending_per_gdp*100", "ex_ante_spending_pc")
outcome_names <- c("Protective Expenditures (million $)", "Protective Expenditures/GDP (%)", "Protective Expenditures/Person ($)")

all_results <- list()
for (i in seq_along(outcomes)) {
  outcome <- outcomes[i]
  outcome_name <- outcome_names[i]
  cutoff_results <- list()
  for (cutoff in cutoffs) {
    cutoff_str <- as.character(cutoff)
    # Use the loader function for each regression
    data <- load_df_track(cutoff)
    coefs <- run_track_regression(data, outcome)
    coefs$cutoff <- cutoff_str
    coefs$outcome <- outcome_name
    cutoff_results[[cutoff_str]] <- coefs
  }
  all_results[[outcome_name]] <- bind_rows(cutoff_results)
}

# Plot for each outcome independently, then combine

dat1 <- all_results[["Protective Expenditures (million $)"]]
dat2 <- all_results[["Protective Expenditures/GDP (%)"]]
dat3 <- all_results[["Protective Expenditures/Person ($)"]]

# Individual plots (remove legend.position from each)
p1 <- ggplot(dat1, aes(x = bin_label, y = estimate, color = cutoff, group = cutoff, shape = cutoff)) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), position = position_dodge(width = 0.5), width = 0) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  scale_color_manual(values = fe_colors, name = "Distance Cutoff (km):") +
  scale_shape_manual(values = fe_shapes, name = "Distance Cutoff (km):") +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  scale_x_discrete(limits = bin_levels) +
  main_theme +
  labs(
    title = "A. Protective Expenditures (million $)",
    x = "Track Distance Forecast (km)",
    y = "",
    color = "Distance Cutoff (km)",
    shape = "Distance Cutoff (km)"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )

p2 <- ggplot(dat2, aes(x = bin_label, y = estimate, color = cutoff, group = cutoff, shape = cutoff)) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), position = position_dodge(width = 0.5), width = 0) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  scale_color_manual(values = fe_colors, name = "Distance Cutoff (km):") +
  scale_shape_manual(values = fe_shapes, name = "Distance Cutoff (km):") +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  scale_x_discrete(limits = bin_levels) +
  main_theme +
  labs(
    title = "B. Protective Expenditures/GDP (%)",
    x = "Track Distance Forecast (km)",
    y = "",
    color = "Distance Cutoff (km)",
    shape = "Distance Cutoff (km)"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )

p3 <- ggplot(dat3, aes(x = bin_label, y = estimate, color = cutoff, group = cutoff, shape = cutoff)) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), position = position_dodge(width = 0.5), width = 0) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  scale_color_manual(values = fe_colors, name = "Distance Cutoff (km):") +
  scale_shape_manual(values = fe_shapes, name = "Distance Cutoff (km):") +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  scale_x_discrete(limits = bin_levels) +
  main_theme +
  labs(
    title = "C. Protective Expenditures/Person ($)",
    x = "Track Distance Forecast (km)",
    y = "",
    color = "Distance Cutoff (km)",
    shape = "Distance Cutoff (km)"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )

# Combine plots with patchwork and a single, centered legend with extra space
plot_out <- (p1 + p2 + p3) +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18),
    legend.spacing.x = unit(1.5, 'cm'),
    legend.margin = margin(t = 10),
    legend.box.margin = margin(t = 10)
  ) &
  guides(
    color = guide_legend(nrow = 1, byrow = TRUE, override.aes = list(size = 5)),
    shape = guide_legend(nrow = 1, byrow = TRUE)
  )

# Save the plot
ggsave(plot = plot_out, filename = "results/FigureC11.png", width = 3 * 10 * 2 / 3, height = 8 * 2 / 3 + 2)
ggsave(plot = plot_out, filename = "results/FigureC11.pdf", device = cairo_pdf, width = 3 * 10 * 2 / 3, height = 8 * 2 / 3 + 2)

# ==============================================================================
# 12. WIND EFFECTS CONDITIONAL ON TRACK FORECAST
# ==============================================================================

df_track = load_df_track()

effect_by_lead_day = conleyreg(
  ex_ante_spending/1e6 ~
    i(forecast_cut) + i(forecast_precip_cut) + i(forecast_track_cut) |
    county + state^hurricane,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

# Regression without forecast_track_cut
effect_by_lead_day_wind_only = conleyreg(
  ex_ante_spending/1e6 ~
    i(forecast_cut) + i(forecast_precip_cut) |
    county + state^hurricane,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

coef_log = tidy(effect_by_lead_day, conf.int = TRUE) |>
  filter(str_detect(term, "forecast_cut")) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .before = 1,
  ) |>
  mutate(term = sort(unique(df_track$forecast_cut))) |>
  mutate(x = row_number(),
         model = "Wind | Track + Precip")

coef_log_wind_only = tidy(effect_by_lead_day_wind_only, conf.int = TRUE) |>
  filter(str_detect(term, "forecast_cut")) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .before = 1,
  ) |>
  mutate(term = sort(unique(df_track$forecast_cut))) |>
  mutate(x = row_number(),
         model = "Wind | Precip")

# Create position adjustments for jittering
pos_full = position_nudge(x = -0.1)
pos_track = position_nudge(x = 0.1)

log_plot = ggplot() +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(data = coef_log, aes(x = x, ymin = conf.low, ymax = conf.high, color = model), width = 0, position = pos_full) +
  geom_point(data = coef_log, aes(x = x, y = estimate, color = model, shape = model), size = 3, position = pos_full) +
  geom_errorbar(data = coef_log_wind_only, aes(x = x, ymin = conf.low, ymax = conf.high, color = model), width = 0, position = pos_track) +
  geom_point(data = coef_log_wind_only, aes(x = x, y = estimate, color = model, shape = model), size = 3, position = pos_track) +
  scale_x_continuous(labels = coef_log$term, breaks = coef_log$x) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  scale_color_manual(values = c("Wind | Track + Precip" = "#000000", "Wind | Precip" = "#0072B2")) +
  scale_shape_manual(values = c("Wind | Track + Precip" = 16, "Wind | Precip" = 17)) +
  main_theme +
  labs(
    title = "A. Protective Expenditures (million $)",
    x = "Wind Speed Forecast (m/s)",
    y = ""
  ) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    legend.text = element_text(size = 12),
    legend.position = c(0.05, 0.95),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = "white", color = NA),
    legend.title = element_blank(),
    legend.margin = margin(1, 1, 1, 1),
    legend.key.size = unit(0.8, "lines")
  ) + 
  coord_cartesian(ylim = c(-5, 50))

# Original regression with forecast_track_cut
effect_by_lead_day_gdp = conleyreg(
  ex_ante_spending_per_gdp ~
    i(forecast_cut) + i(forecast_precip_cut) + i(forecast_track_cut) |
    county + state^hurricane,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

# Regression without forecast_track_cut
effect_by_lead_day_gdp_wind_only = conleyreg(
  ex_ante_spending_per_gdp ~
    i(forecast_cut) + i(forecast_precip_cut) |
    county + state^hurricane,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

coef_gdp = tidy(effect_by_lead_day_gdp, conf.int = TRUE) |>
  filter(str_detect(term, "forecast_cut")) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .before = 1,
  ) |>
  mutate(term = sort(unique(df_track$forecast_cut))) |>
  mutate(x = row_number(),
         estimate = estimate,
         conf.low = conf.low,
         conf.high = conf.high,
         model = "Wind | Track + Precip")

coef_gdp_wind_only = tidy(effect_by_lead_day_gdp_wind_only, conf.int = TRUE) |>
  filter(str_detect(term, "forecast_cut")) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .before = 1,
  ) |>
  mutate(term = sort(unique(df_track$forecast_cut))) |>
  mutate(x = row_number(),
         model = "Wind | Precip")

share_plot = ggplot() +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(data = coef_gdp, aes(x = x, ymin = conf.low*100, ymax = conf.high*100, color = model), width = 0, position = pos_full) +
  geom_point(data = coef_gdp, aes(x = x, y = estimate*100, color = model, shape = model), size = 3, position = pos_full) +
  geom_errorbar(data = coef_gdp_wind_only, aes(x = x, ymin = conf.low*100, ymax = conf.high*100, color = model), width = 0, position = pos_track) +
  geom_point(data = coef_gdp_wind_only, aes(x = x, y = estimate*100, color = model, shape = model), size = 3, position = pos_track) +
  scale_x_continuous(labels = coef_gdp$term, breaks = coef_gdp$x) +
  main_theme +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_color_manual(values = c("Wind | Track + Precip" = "#000000", "Wind | Precip" = "#0072B2")) +
  scale_shape_manual(values = c("Wind | Track + Precip" = 16, "Wind | Precip" = 17)) +
  labs(
    title = "B. Protective Expenditures/GDP (%)",
    x = "Wind Speed Forecast (m/s)",
    y = ""
  ) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    legend.position = "none"
  ) + 
  coord_cartesian(ylim = c(-0.15, 1.5))

# Original regression with forecast_track_cut
effect_by_lead_day_pc = conleyreg(
  ex_ante_spending_pc ~
    i(forecast_cut) + i(forecast_precip_cut) + i(forecast_track_cut) |
    county + state^hurricane,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

# Regression without forecast_track_cut
effect_by_lead_day_pc_wind_only = conleyreg(
  ex_ante_spending_pc ~
    i(forecast_cut) + i(forecast_precip_cut) |
    county + state^hurricane,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

coef_pc = tidy(effect_by_lead_day_pc, conf.int = TRUE) |>
  filter(str_detect(term, "forecast_cut")) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .before = 1,
  ) |>
  mutate(term = sort(unique(df_track$forecast_cut))) |>
  mutate(x = row_number(),
         estimate = estimate,
         conf.low = conf.low,
         conf.high = conf.high,
         model = "Wind | Track + Precip")

coef_pc_wind_only = tidy(effect_by_lead_day_pc_wind_only, conf.int = TRUE) |>
  filter(str_detect(term, "forecast_cut")) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .before = 1,
  ) |>
  mutate(term = sort(unique(df_track$forecast_cut))) |>
  mutate(x = row_number(),
         model = "Wind | Precip")

pc_plot = ggplot() +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(data = coef_pc, aes(x = x, ymin = conf.low, ymax = conf.high, color = model), width = 0, position = pos_full) +
  geom_point(data = coef_pc, aes(x = x, y = estimate, color = model, shape = model), size = 3, position = pos_full) +
  geom_errorbar(data = coef_pc_wind_only, aes(x = x, ymin = conf.low, ymax = conf.high, color = model), width = 0, position = pos_track) +
  geom_point(data = coef_pc_wind_only, aes(x = x, y = estimate, color = model, shape = model), size = 3, position = pos_track) +
  scale_x_continuous(labels = coef_pc$term, breaks = coef_pc$x) +
  main_theme +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  scale_color_manual(values = c("Wind | Track + Precip" = "#000000", "Wind | Precip" = "#0072B2")) +
  scale_shape_manual(values = c("Wind | Track + Precip" = 16, "Wind | Precip" = 17)) +
  labs(
    title = "C. Protective Expenditures/Person ($)",
    x = "Wind Speed Forecast (m/s)",
    y = ""
  ) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    legend.position = "none"
  ) + 
  coord_cartesian(ylim = c(-50, 500))

plot_out = log_plot + share_plot + pc_plot

ggsave(plot = plot_out, "results/FigureC12.png", width = 3 * 10 * 2 / 3, height = 1 * 8 * 2 / 3)
ggsave(plot = plot_out, "results/FigureC12.pdf", device = cairo_pdf, width = 3 * 10 * 2 / 3, height = 1 * 8 * 2 / 3)
