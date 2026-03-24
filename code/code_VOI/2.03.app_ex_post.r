# ==============================================================================
# EX-POST ERROR ANALYSIS
# ==============================================================================
# OUTLINE:
# 1. Main Table 
# 2. Interaction Tables 
# 3. 600km Conley Robustness
# 4. PDD Robustness
# 5. Coastal States Analysis
# 6. Asinh Transformation Analysis
# 7. Property Damage Analysis (Damage Type Breakdown)
# 8. Precipitation Error Analysis
# 9. Varying Fixed Effects Robustness Check (Intensity Controls)
# 10. Varying Intensity Bins Robustness Check (Bin Number)
# 11. Population Weighting
# 12. Wind Intensity Interactions
# 13. Track Analysis (including robustness cutoff)
# 14. Wind Error Analysis Conditional on Track Controls
# 15. Protective Expenditure as Function of Ex-Post Error
# ==============================================================================
gc()

# ==============================================================================
# 1. MAIN TABLE
# ==============================================================================

regs = list()

regs[[1]] = conleyreg(
  total_cost
  ~ wind_error + precip_error  |
        county + state^hurricane + wind_cut + precip_cut,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs[[2]] = conleyreg(
  total_cost
  ~ wind_error + precip_error  |
        county + state^hurricane + wind_cut + precip_cut + county^month,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs[[3]] = conleyreg(
  total_cost
  ~ wind_error + precip_error  |
        county + state^hurricane + wind_cut + precip_cut + county^year,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs[[4]] = conleyreg(
  total_cost
  ~ wind_error + precip_error  |
        county + state^hurricane + wind_cut + precip_cut + county^year + county^month,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_gdp = list()

regs_gdp[[1]] = conleyreg(
  total_cost_per_gdp*100
  ~ wind_error + precip_error  |
        county + state^hurricane + wind_cut + precip_cut,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_gdp[[2]] = conleyreg(
  total_cost_per_gdp*100
  ~ wind_error + precip_error  |
        county + state^hurricane + wind_cut + precip_cut + county^month,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_gdp[[3]] = conleyreg(
  total_cost_per_gdp*100
  ~ wind_error + precip_error  |
        county + state^hurricane + wind_cut + precip_cut + county^year,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_gdp[[4]] = conleyreg(
  total_cost_per_gdp*100
  ~ wind_error + precip_error  |
        county + state^hurricane + wind_cut + precip_cut + county^year + county^month,
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
  total_cost_pc
  ~ wind_error + precip_error  |
      county + state^hurricane + wind_cut + precip_cut,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pc[[2]] = conleyreg(
  total_cost_pc
  ~ wind_error + precip_error  |
      county + state^hurricane + wind_cut + precip_cut + county^month,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pc[[3]] = conleyreg(
  total_cost_pc
  ~ wind_error + precip_error  |
      county + state^hurricane + wind_cut + precip_cut + county^year,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pc[[4]] = conleyreg(
  total_cost_pc
  ~ wind_error + precip_error  |
      county + state^hurricane + wind_cut + precip_cut + county^year + county^month,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

obs = feols(
  total_cost
  ~ wind_error + precip_error  |
      county + state^hurricane + wind_cut + precip_cut,
  data = load_df()
)$nobs |>
  format(big.mark = ",")

rows = tribble(
  ~term, ~"(1)", ~"(2)", ~"(3)", ~"(4)", 
  "", "", "", "", "", 
  "Observations", obs, obs, obs, obs, 
  "", "", "", "", "", 
  "Realized Wind/Precip Bins", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", 
  "State-Hurricane FE", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark",
  "County FE", "\\checkmark",  "", "",  "",
  "County-Month of Year FE", "", "\\checkmark", "", "\\checkmark",
  "County-Year FE", "", "", "\\checkmark", "\\checkmark"
)

tab = msummary(list("Damages + Recovery Expenditures (million $)" = regs, "(Damages + Recovery Expenditures) / GDP (%)" = regs_gdp, "(Damages + Recovery Expenditures) / Person ($)" = regs_pc

),
  coef_rename = c(
    "Wind Forecast Underestimate (m/s)",
    "Precip Forecast Underestimate (mm)"
  ),
  gof_omit = "R2|AIC|BIC|Log.|RMSE|FE||Std.Errors",
  stars =  c('*' = .1, '**' = .05, '***' = .01),
  fmt = "%.2f",
  add_rows = rows,
  title = "The Effect of Underestimating Wind and Precipitation on Damages and FEMA Recovery Expenditures. \\label{atab:ex_post_error}",
  notes = list("Standard errors are Conley Spatial HAC with a distance radius of 400 km for spatial correlation and arbitrary autocorrelation within counties."),
  threeparttable = TRUE,
  shape = 'rbind',
  escape = FALSE,
  output = "results/TableC2.tex"
)

add_adjust_box("results/TableC2.tex")

# ==============================================================================
# 2. WIND INTENSITY INTERACTION TABLE
# ==============================================================================

regs_log = list()

regs_log[[1]] = conleyreg(
  total_cost
  ~ wind_error:category + precip_error   |
        county + state^hurricane + wind_cut + precip_cut,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_log[[2]] = conleyreg(
  total_cost
  ~ wind_error:category + precip_error   |
        county + state^hurricane + wind_cut + precip_cut + county^month,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_log[[3]] = conleyreg(
  total_cost
  ~ wind_error:category + precip_error   |
        county + state^hurricane + wind_cut + precip_cut + county^year,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_log[[4]] = conleyreg(
  total_cost
  ~ wind_error:category + precip_error   |
        county + state^hurricane + wind_cut + precip_cut + county^year + county^month,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)


regs_gdp = list()

regs_gdp[[1]] = conleyreg(
  total_cost_per_gdp*100
  ~ wind_error:category + precip_error   |
        county + state^hurricane + wind_cut + precip_cut,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_gdp[[2]] = conleyreg(
  total_cost_per_gdp*100
  ~ wind_error:category + precip_error   |
        county + state^hurricane + wind_cut + precip_cut + county^month,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_gdp[[3]] = conleyreg(
  total_cost_per_gdp*100
  ~ wind_error:category + precip_error   |
        county + state^hurricane + wind_cut + precip_cut + county^year,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_gdp[[4]] = conleyreg(
  total_cost_per_gdp*100
  ~ wind_error:category + precip_error   |
        county + state^hurricane + wind_cut + precip_cut + county^year + county^month,
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
  total_cost_pc
  ~ wind_error:category + precip_error   |
      county + state^hurricane + wind_cut + precip_cut,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pc[[2]] = conleyreg(
  total_cost_pc
  ~ wind_error:category + precip_error   |
      county + state^hurricane + wind_cut + precip_cut + county^month,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pc[[3]] = conleyreg(
  total_cost_pc
  ~ wind_error:category + precip_error   |
      county + state^hurricane + wind_cut + precip_cut + county^year,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pc[[4]] = conleyreg(
  total_cost_pc
  ~ wind_error:category + precip_error   |
      county + state^hurricane + wind_cut + precip_cut + county^year + county^month,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

obs = feols(
  total_cost
  ~ wind_error:category + precip_error   |
      county + state^hurricane + wind_cut + precip_cut,
  data = load_df()
)$nobs |>
  format(big.mark = ",")

rows = tribble(
  ~term, ~"(1)", ~"(2)", ~"(3)", ~"(4)", 
  "", "", "", "", "", 
  "Observations", obs, obs, obs, obs, 
  "", "", "", "", "", 
  "Precipitation Underestimate", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", 
  "Realized Wind/Precip Bins", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", 
  "State-Hurricane FE", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark",
  "County FE", "\\checkmark",  "", "",  "",
  "County-Month of Year FE", "", "\\checkmark", "", "\\checkmark",
  "County-Year FE", "", "", "\\checkmark", "\\checkmark"
)

tab = msummary(list("Damages + Recovery Expenditures (million $)" = regs_log, "(Damages + Recovery Expenditures) / GDP (%)" = regs_gdp, "(Damages + Recovery Expenditures) / Person ($)" = regs_pc),
  coef_rename = c(
    "Wind Forecast Underestimate (m/s): Hurricane",
    "Wind Forecast Underestimate (m/s): Sub-Hurricane"
  ),
  coef_omit = c(-2,-3),
  gof_omit = "R2|AIC|BIC|Log.|RMSE|FE||Std.Errors",
  stars =  c('*' = .1, '**' = .05, '***' = .01),
  fmt = "%.2f",
  add_rows = rows,
  title = "The Effect of Underestimating Wind and Precipitation on Damages and FEMA Recovery Expenditures As a Function of Hurricane Intensity. \\label{atab:ex_post_error_inter}",
  notes = list("Standard errors are Conley Spatial HAC with a distance radius of 400 km for spatial correlation and arbitrary autocorrelation within counties. 
               Hurricane wind speeds are those greater than 33 m/s (74 mph), while Sub-Hurricane wind speeds fall below that threshold."),
  threeparttable = TRUE,
  shape = 'rbind',
  escape = FALSE,
  output = "results/TableC3.tex"
)

add_adjust_box("results/TableC3.tex")

# ==============================================================================
# 3. 600KM CONLEY ROBUSTNESS
# ==============================================================================


df = load_df()

reg_dam = conleyreg(
  damages/1e6 ~ i(wind_error_bin, ref = "(-2,0]")
  |
  county + state^hurricane + wind_cut + precip_cut + precip_error_bin,
  data = load_df(),
  dist_cutoff = 600,
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
  dist_cutoff = 600,
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
  dist_cutoff = 600,
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
  dist_cutoff = 600,
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
  dist_cutoff = 600,
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
  dist_cutoff = 600,
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

ggsave(plot = plot_out, "results/FigureC13.png", width = 3 * 10 * 2 / 3, height = 2 * 8 * 2 / 3)
ggsave(plot = plot_out, "results/FigureC13.pdf", device = cairo_pdf, width = 3 * 10 * 2 / 3, height = 2 * 8 * 2 / 3)

# ==============================================================================
# 4. PDD ROBUSTNESS
# ==============================================================================

df = load_pdd()

reg_dam = conleyreg(
  damages/1e6 ~ i(wind_error_bin, ref = "(-2,0]")
   |
    county + state^hurricane + wind_cut + precip_cut + precip_error_bin,
  data = df,
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
  filter(!str_detect(term, "_cut") & !str_detect(term, "precip")) |>
  mutate(term = sort(unique(df$wind_error_bin))) |>
  mutate(x = row_number()) |>
  mutate(type = "Damages")

# Load in data
df = load_pdd()

reg_spend = conleyreg(
  ex_post_spending/1e6 ~ i(wind_error_bin, ref = "(-2,0]") 
   |
    county + state^hurricane + wind_cut + precip_cut + precip_error_bin,
  data = df,
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
  filter(!str_detect(term, "_cut") & !str_detect(term, "precip")) |>
  mutate(term = sort(unique(df$wind_error_bin))) |>
  mutate(x = row_number()) |>
  mutate(type = "Recovery Expenditures")

# Load in data
df = load_pdd()

reg_dam_share = conleyreg(
  damage_per_gdp ~ i(wind_error_bin, ref = "(-2,0]") 
   |
    county + state^hurricane + wind_cut + precip_cut + precip_error_bin,
  data = df,
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
  filter(!str_detect(term, "_cut") & !str_detect(term, "precip")) |>
  mutate(term = sort(unique(df$wind_error_bin))) |>
  mutate(x = row_number()) |>
  mutate(type = "damage")

# Load in data
df = load_pdd()

reg_spend_share = conleyreg(
  ex_post_spending_per_gdp ~ i(wind_error_bin, ref = "(-2,0]")
   |
    county + state^hurricane + wind_cut + precip_cut + precip_error_bin,
  data = df,
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
  filter(!str_detect(term, "_cut") & !str_detect(term, "precip")) |>
  mutate(term = sort(unique(df$wind_error_bin))) |>
  mutate(x = row_number()) |>
  mutate(type = "spending")

# Load in data
df = load_pdd()

reg_dam_pc = conleyreg(
  damage_pc ~ i(wind_error_bin, ref = "(-2,0]") 
   |
    county + state^hurricane + wind_cut + precip_cut + precip_error_bin,
  data = df,
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
  filter(!str_detect(term, "_cut") & !str_detect(term, "precip")) |>
  mutate(term = sort(unique(df$wind_error_bin))) |>
  mutate(x = row_number()) |>
  mutate(type = "damage")

# Load in data
df = load_pdd()

reg_spend_pc = conleyreg(
  ex_post_spending_pc ~ i(wind_error_bin, ref = "(-2,0]") 
   |
    county + state^hurricane + wind_cut + precip_cut + precip_error_bin,
  data = df,
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
  filter(!str_detect(term, "_cut") & !str_detect(term, "precip")) |>
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

ggsave(plot = plot_out, "results/FigureC14.png", width = 3 * 10 * 2 / 3, height = 2 * 8 * 2 / 3)
ggsave(plot = plot_out, "results/FigureC14.pdf", device = cairo_pdf, width = 3 * 10 * 2 / 3, height = 2 * 8 * 2 / 3)

# ==============================================================================
# 5. COASTAL STATES ANALYSIS
# ==============================================================================

# TX, LA, MS, AL, GA, FL, SC, NC, VA, MD, NJ, PA, CT, DE, NY, RI, MA, NH, ME
coastal_states = c(48, 22, 28, 01, 13, 12, 45, 37, 51, 24, 34, 10, 42, 36, 09, 44, 33, 23, 25)

df = readRDS("data/clean/final_dataset.rds") |>
  as_tibble() |> 
  filter(state %in% coastal_states)

reg_dam = conleyreg(
  damages/1e6 ~ i(wind_error_bin, ref = "(-2,0]")
   |
    county + state^hurricane + wind_cut + precip_cut + precip_error_bin,
  data = df,
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
  filter(!str_detect(term, "_cut") & !str_detect(term, "precip")) |>
  mutate(term = sort(unique(df$wind_error_bin))) |>
  mutate(x = row_number()) |>
  mutate(type = "Damages")

# Load in data
df = readRDS("data/clean/final_dataset.rds") |>
  as_tibble() |> 
  filter(state %in% coastal_states)

reg_spend = conleyreg(
  ex_post_spending/1e6 ~ i(wind_error_bin, ref = "(-2,0]") 
   |
    county + state^hurricane + wind_cut + precip_cut + precip_error_bin,
  data = df,
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
  filter(!str_detect(term, "_cut") & !str_detect(term, "precip")) |>
  mutate(term = sort(unique(df$wind_error_bin))) |>
  mutate(x = row_number()) |>
  mutate(type = "Recovery Expenditures")

# Load in data
df = readRDS("data/clean/final_dataset.rds") |>
  as_tibble() |> 
  filter(state %in% coastal_states)

reg_dam_share = conleyreg(
  damage_per_gdp ~ i(wind_error_bin, ref = "(-2,0]") 
   |
    county + state^hurricane + wind_cut + precip_cut + precip_error_bin,
  data = df,
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
  filter(!str_detect(term, "_cut") & !str_detect(term, "precip")) |>
  mutate(term = sort(unique(df$wind_error_bin))) |>
  mutate(x = row_number()) |>
  mutate(type = "damage")

# Load in data
df = readRDS("data/clean/final_dataset.rds") |>
  as_tibble() |> 
  filter(state %in% coastal_states)

reg_spend_share = conleyreg(
  ex_post_spending_per_gdp ~ i(wind_error_bin, ref = "(-2,0]") 
   |
    county + state^hurricane + wind_cut + precip_cut + precip_error_bin,
  data = df,
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
  filter(!str_detect(term, "_cut") & !str_detect(term, "precip")) |>
  mutate(term = sort(unique(df$wind_error_bin))) |>
  mutate(x = row_number()) |>
  mutate(type = "spending")

# Load in data
df = readRDS("data/clean/final_dataset.rds") |>
  as_tibble() |> 
  filter(state %in% coastal_states)

reg_dam_pc = conleyreg(
  damage_pc ~ i(wind_error_bin, ref = "(-2,0]") 
   |
    county + state^hurricane + wind_cut + precip_cut + precip_error_bin,
  data = df,
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
  filter(!str_detect(term, "_cut") & !str_detect(term, "precip")) |>
  mutate(term = sort(unique(df$wind_error_bin))) |>
  mutate(x = row_number()) |>
  mutate(type = "damage")

# Load in data
df = readRDS("data/clean/final_dataset.rds") |>
  as_tibble() |> 
  filter(state %in% coastal_states)

reg_spend_pc = conleyreg(
  ex_post_spending_pc ~ i(wind_error_bin, ref = "(-2,0]") 
   |
    county + state^hurricane + wind_cut + precip_cut + precip_error_bin,
  data = df,
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
  filter(!str_detect(term, "_cut") & !str_detect(term, "precip")) |>
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

ggsave(plot = plot_out, "results/FigureC15.png", width = 3 * 10 * 2 / 3, height = 2 * 8 * 2 / 3)
ggsave(plot = plot_out, "results/FigureC15.pdf", device = cairo_pdf, width = 3 * 10 * 2 / 3, height = 2 * 8 * 2 / 3)

# ==============================================================================
# 6. ASINH TRANSFORMATION ANALYSIS
# ==============================================================================

# Load in data
df = readRDS("data/clean/final_dataset.rds") |>
  as_tibble()

reg_dam = conleyreg(
  asinh_damage ~ i(wind_error_bin, ref = "(-2,0]") + i(precip_error_bin, ref = "(-40,0]")
   |
    county + state^hurricane + wind_cut + precip_cut ,
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
  filter(!str_detect(term, "_cut") & !str_detect(term, "precip")) |>
  mutate(term = sort(unique(df$wind_error_bin))) |>
  mutate(x = row_number()) |>
  mutate(type = "Damages")

coef_dam_precip = tidy(reg_dam, conf.int = TRUE) |>
  filter(!str_detect(term, "_cut") & str_detect(term, "precip")) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .after = 4
  ) |>
  mutate(term = sort(unique(df$precip_error_bin))) |>
  mutate(x = row_number()) |>
  mutate(type = "damage")

# Load in data
df = readRDS("data/clean/final_dataset.rds") |>
  as_tibble()

reg_spend = conleyreg(
  asinh_ex_post_spending ~ i(wind_error_bin, ref = "(-2,0]") + i(precip_error_bin, ref = "(-40,0]")
   |
    county + state^hurricane + wind_cut + precip_cut,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

coef_spend = tidy(reg_spend, conf.int = TRUE) |>
  filter(!str_detect(term, "_cut") & !str_detect(term, "precip")) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .after = 4
  ) |>
  mutate(term = sort(unique(df$wind_error_bin))) |>
  mutate(x = row_number()) |>
  mutate(type = "Recovery Expenditures")

coef_spend_precip = tidy(reg_spend, conf.int = TRUE) |>
  filter(!str_detect(term, "_cut") & str_detect(term, "precip")) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .after = 4
  ) |>
  mutate(term = sort(unique(df$precip_error_bin))) |>
  mutate(x = row_number()) |>
  mutate(type = "spending")

plot_dam = ggplot(coef_dam, aes(x = x, y = estimate, color = type, shape = type)) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(aes(x = x, ymin = conf.low, ymax = conf.high), width = 0,
        position = position_dodge(width = 0.25)) +
  geom_point(size = 3,
        position = position_dodge(width = 0.25)) +
  main_theme +
  scale_x_continuous(labels = coef_dam$term, breaks = coef_dam$x) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  scale_color_manual(values = c("#000000", "#e41a1c")) +
  labs(
    title = "A. Wind and asinh(Damages)",
    x = "",
    y = ""
  ) +
  annotate("text",
           x = 2, y = -3.3,
           family = "Lato",
           label = "Overestimate", size = 6, hjust = 0
  ) +
  annotate("text",
           x = 6.4, y = -3.3,
           family = "Lato",
           label = "Underestimate", size = 6, hjust = 0
  ) +
  geom_segment(aes(x = 1.85, y = -3.3, xend = 1.25, yend = -3.3),
               arrow = arrow(length = unit(0.1, "cm")), color = "black"
  ) +
  geom_segment(aes(x = 9.35, y = -3.3, xend = 9.95, yend = -3.3),
               arrow = arrow(length = unit(0.1, "cm")), color = "black"
  ) +
  main_theme +
  theme(
    axis.text.x = element_text(size = 14, angle = 45, vjust = 1, hjust = 1)
  ) +
  coord_cartesian(ylim = c(-5, 15))

plot_spend = ggplot(coef_spend, aes(x = x, y = estimate, color = type, shape = type)) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(aes(x = x, ymin = conf.low, ymax = conf.high), width = 0,
        position = position_dodge(width = 0.25)) +
  geom_point(size = 3,
        position = position_dodge(width = 0.25)) +
  main_theme +
  scale_x_continuous(labels = coef_spend$term, breaks = coef_spend$x) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  scale_color_manual(values = c("#000000", "#e41a1c")) +
  labs(
    title = "C. Wind and asinh(Recovery Expenditures)",
    x = "Wind Speed Underestimate (m/s)",
    y = ""
  ) +
  main_theme +
  theme(
    axis.text.x = element_text(size = 14, angle = 45, vjust = 1, hjust = 1)
  ) +
  coord_cartesian(ylim = c(-5, 15))

precip_dam = ggplot(coef_dam_precip, aes(x = x, y = estimate, color = type, shape = type)) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(aes(x = x, ymin = conf.low, ymax = conf.high), width = 0,
        position = position_dodge(width = 0.25)) +
  geom_point(size = 3,
        position = position_dodge(width = 0.25)) +
  main_theme +
  scale_x_continuous(labels = coef_dam_precip$term, breaks = coef_dam_precip$x) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  scale_color_manual(values = c("#000000", "#e41a1c")) +
  labs(
    title = "B. Precip and asinh(Damages)",
    x = "",
    y = ""
  ) +
  main_theme +
  theme(
    axis.text.x = element_text(size = 14, angle = 45, vjust = 1, hjust = 1)
  ) +
  coord_cartesian(ylim = c(-5, 15))

precip_spend = ggplot(coef_spend_precip, aes(x = x, y = estimate, color = type, shape = type)) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(aes(x = x, ymin = conf.low, ymax = conf.high), width = 0,
        position = position_dodge(width = 0.25)) +
  geom_point(size = 3,
        position = position_dodge(width = 0.25)) +
  main_theme +
  scale_x_continuous(labels = coef_spend_precip$term, breaks = coef_spend_precip$x) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  scale_color_manual(values = c("#000000", "#e41a1c")) +
  labs(
    title = "D. Precip and asinh(Recovery Expenditures)",
    x = "Precipitation Underestimate (mm)",
    y = ""
  ) +
  main_theme +
  theme(
    axis.text.x = element_text(size = 14, angle = 45, vjust = 1, hjust = 1)
  ) +
  coord_cartesian(ylim = c(-5, 15))

plot_out = (plot_dam + precip_dam) / (plot_spend + precip_spend)

ggsave(plot = plot_out, "results/FigureC16.png", width = 2 * 10 * 2 / 3, height = 2 * 8 * 2 / 3)
ggsave(plot = plot_out, "results/FigureC16.pdf", device = cairo_pdf, width = 2 * 10 * 2 / 3, height = 2 * 8 * 2 / 3)

# ==============================================================================
# 7. PROPERTY DAMAGE ANALYSIS
# ==============================================================================

reg_prop = conleyreg(
  property_damage/1e6 ~ i(wind_error_bin, ref = "(-2,0]") + i(precip_error_bin, ref = "(-40,0]")
   |
    county + state^hurricane + wind_cut + precip_cut,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

coef_prop = tidy(reg_prop, conf.int = TRUE) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .after = 4
  ) |>
  filter(!str_detect(term, "_cut") & !str_detect(term, "precip")) |>
  mutate(term = sort(unique(df$wind_error_bin))) |>
  mutate(x = row_number()) 

reg_crop = conleyreg(
  crop_damage/1e6 ~ i(wind_error_bin, ref = "(-2,0]") + i(precip_error_bin, ref = "(-40,0]")
   |
    county + state^hurricane + wind_cut + precip_cut,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

coef_crop = tidy(reg_crop, conf.int = TRUE) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .after = 4
  ) |>
  filter(!str_detect(term, "_cut") & !str_detect(term, "precip")) |>
  mutate(term = sort(unique(df$wind_error_bin))) |>
  mutate(x = row_number()) 

reg_mortality = conleyreg(
  mortality_damage/1e6 ~ i(wind_error_bin, ref = "(-2,0]") + i(precip_error_bin, ref = "(-40,0]")
   |
    county + state^hurricane + wind_cut + precip_cut,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

coef_mortality = tidy(reg_mortality, conf.int = TRUE) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .after = 4
  ) |>
  filter(!str_detect(term, "_cut") & !str_detect(term, "precip")) |>
  mutate(term = sort(unique(df$wind_error_bin))) |>
  mutate(x = row_number()) 

log_plot_prop = ggplot(coef_prop, aes(x = x, y = estimate)) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(aes(x = x, ymin = conf.low, ymax = conf.high), width = 0,
        position = position_dodge(width = 0.25)) +
  geom_point(size = 3,
        position = position_dodge(width = 0.25)) +
  main_theme +
  scale_x_continuous(labels = coef_prop$term, breaks = coef_prop$x) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  labs(
    title = "A. Property Damages (million $)",
    x = "Wind Speed Underestimate (m/s)",
    y = ""
  ) +
  annotate("text",
           x = 2, y = -150,
           family = "Lato",
           label = "Overestimate", size = 6, hjust = 0
  ) +
  annotate("text",
           x = 6.4, y = -150,
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
  )

log_plot_crop = ggplot(coef_crop, aes(x = x, y = estimate)) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(aes(x = x, ymin = conf.low, ymax = conf.high), width = 0,
        position = position_dodge(width = 0.25)) +
  geom_point(size = 3,
        position = position_dodge(width = 0.25)) +
  main_theme +
  scale_x_continuous(labels = coef_crop$term, breaks = coef_crop$x) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  labs(
    title = "B. Crop Damages (million $)",
    x = "Wind Speed Underestimate (m/s)",
    y = ""
  ) +
  main_theme +
  theme(
    axis.text.x = element_text(size = 14, angle = 45, vjust = 1, hjust = 1)
  ) 

log_plot_mortality = ggplot(coef_mortality, aes(x = x, y = estimate)) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(aes(x = x, ymin = conf.low, ymax = conf.high), width = 0,
        position = position_dodge(width = 0.25)) +
  geom_point(size = 3,
        position = position_dodge(width = 0.25)) +
  main_theme +
  scale_x_continuous(labels = coef_mortality$term, breaks = coef_mortality$x) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  labs(
    title = "C. Mortality Damages (million $)",
    x = "Wind Speed Underestimate (m/s)",
    y = ""
  ) +
  main_theme +
  theme(
    axis.text.x = element_text(size = 14, angle = 45, vjust = 1, hjust = 1)
  ) 

plot_out = (log_plot_prop + log_plot_crop + log_plot_mortality)

ggsave(plot = plot_out, "results/FigureC17.png", width = 3 * 10 * 2 / 3, height = 1 * 8 * 2 / 3)
ggsave(plot = plot_out, "results/FigureC17.pdf", device = cairo_pdf, width = 3 * 10 * 2 / 3, height = 1 * 8 * 2 / 3)

# ==============================================================================
# 8. PRECIPITATION ERROR ANALYSIS
# ==============================================================================

df = readRDS("data/clean/final_dataset.rds") |>
  as_tibble()

reg_dam = conleyreg(
  damages/1e6 ~ i(precip_error_bin, ref = "(-40,0]") 
   |
    county + state^hurricane + wind_cut + precip_cut + wind_error_bin,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

coef_dam = tidy(reg_dam, conf.int = TRUE) |>
  filter(!str_detect(term, "cut") & !str_detect(term, "wind")) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .after = 4
  ) |>
  mutate(term = sort(unique(df$precip_error_bin))) |>
  mutate(x = row_number()) |>
  mutate(type = "Damages")

# Load in data
df = readRDS("data/clean/final_dataset.rds") |>
  as_tibble()

reg_spend = conleyreg(
  ex_post_spending/1e6 ~ i(precip_error_bin, ref = "(-40,0]")
   |
    county + state^hurricane + wind_cut + precip_cut + wind_error_bin,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

coef_spend = tidy(reg_spend, conf.int = TRUE) |>
  filter(!str_detect(term, "cut") & !str_detect(term, "wind")) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .after = 4
  ) |>
  mutate(term = sort(unique(df$precip_error_bin))) |>
  mutate(x = row_number()) |>
  mutate(type = "Recovery Expenditures")

# Load in data
df = readRDS("data/clean/final_dataset.rds") |>
  as_tibble()

reg_dam_share = conleyreg(
  damage_per_gdp ~ i(precip_error_bin, ref = "(-40,0]")
   |
    county + state^hurricane + wind_cut + precip_cut + wind_error_bin,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

coef_dam_share = tidy(reg_dam_share, conf.int = TRUE) |>
  filter(!str_detect(term, "cut") & !str_detect(term, "wind")) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .after = 4
  ) |>
  mutate(term = sort(unique(df$precip_error_bin))) |>
  mutate(x = row_number()) |>
  mutate(type = "damage")

# Load in data
df = readRDS("data/clean/final_dataset.rds") |>
  as_tibble()

reg_spend_share = conleyreg(
  ex_post_spending_per_gdp ~ i(precip_error_bin, ref = "(-40,0]")
   |
    county + state^hurricane + wind_cut + precip_cut + wind_error_bin,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

coef_spend_share = tidy(reg_spend_share, conf.int = TRUE) |>
  filter(!str_detect(term, "cut") & !str_detect(term, "wind")) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .after = 4
  ) |>
  mutate(term = sort(unique(df$precip_error_bin))) |>
  mutate(x = row_number()) |>
  mutate(type = "spending")

# Load in data
df = readRDS("data/clean/final_dataset.rds") |>
  as_tibble()

reg_dam_pc = conleyreg(
  damage_pc ~ i(precip_error_bin, ref = "(-40,0]") 
   |
    county + state^hurricane + wind_cut + precip_cut + wind_error_bin,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

coef_dam_pc = tidy(reg_dam_pc, conf.int = TRUE) |>
  filter(!str_detect(term, "cut") & !str_detect(term, "wind")) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .after = 4
  ) |>
  mutate(term = sort(unique(df$precip_error_bin))) |>
  mutate(x = row_number()) |>
  mutate(type = "damage")

# Load in data
df = readRDS("data/clean/final_dataset.rds") |>
  as_tibble()

reg_spend_pc = conleyreg(
  ex_post_spending_pc ~ i(precip_error_bin, ref = "(-40,0]") 
   |
    county + state^hurricane + wind_cut + precip_cut + wind_error_bin,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

coef_spend_pc = tidy(reg_spend_pc, conf.int = TRUE) |>
  filter(!str_detect(term, "cut") & !str_detect(term, "wind")) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .after = 4
  ) |>
  mutate(term = sort(unique(df$precip_error_bin))) |>
  mutate(x = row_number()) |>
  mutate(type = "spending")

log_plot_dam = ggplot(coef_dam, aes(x = x, y = estimate, color = type, shape = type)) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(aes(x = x, ymin = conf.low, ymax = conf.high), width = 0,
        position = position_dodge(width = 0.25)) +
  geom_point(size = 3,
        position = position_dodge(width = 0.25)) +
  main_theme +
  scale_x_continuous(labels = coef_dam$term, breaks = coef_dam$x) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  scale_color_manual(values = c("#000000", "#e41a1c")) +
  labs(
    title = "A. Damages (million $)",
    x = "",
    y = ""
  )  +
  annotate("text",
           x = 2, y = -450,
           family = "Lato",
           label = "Overestimate", size = 6, hjust = 0
  ) +
  annotate("text",
           x = 6.3, y = 300,
           family = "Lato",
           label = "Underestimate", size = 6, hjust = 0
  ) +
  geom_segment(aes(x = 1.85, y = -450, xend = 1.25, yend = -450),
               arrow = arrow(length = unit(0.1, "cm")), color = "black"
  ) +
  geom_segment(aes(x = 9.35, y = 300, xend = 9.95, yend = 300),
               arrow = arrow(length = unit(0.1, "cm")), color = "black"
  ) +
  main_theme +
  theme(
    axis.text.x = element_text(size = 14, angle = 45, vjust = 1, hjust = 1)
  ) 

log_plot_spend = ggplot(coef_spend, aes(x = x, y = estimate, color = type, shape = type)) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(aes(x = x, ymin = conf.low, ymax = conf.high), width = 0,
        position = position_dodge(width = 0.25)) +
  geom_point(size = 3,
        position = position_dodge(width = 0.25)) +
  main_theme +
  scale_x_continuous(labels = coef_spend$term, breaks = coef_spend$x) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  scale_color_manual(values = c("#000000", "#e41a1c")) +
  labs(
    title = "D. Recovery Expenditures (million $)",
    x = "Precipitation Underestimate (mm)",
    y = ""
  ) +
  main_theme +
  theme(
    axis.text.x = element_text(size = 14, angle = 45, vjust = 1, hjust = 1)
  ) 

share_plot_dam = ggplot(coef_dam_share, aes(x = x, y = estimate*100, color = type, shape = type)) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(aes(x = x, ymin = conf.low*100, ymax = conf.high*100), width = 0,
        position = position_dodge(width = 0.25)) +
  geom_point(size = 3,
        position = position_dodge(width = 0.25)) +
  main_theme +
  scale_x_continuous(labels = coef_dam_share$term, breaks = coef_dam_share$x) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  scale_color_manual(values = c("#000000", "#e41a1c")) +
  labs(
    title = "B. Damages/GDP (%)",
    x = "",
    y = ""
  ) +
  main_theme +
  theme(
    axis.text.x = element_text(size = 14, angle = 45, vjust = 1, hjust = 1)
  ) 

share_plot_spend = ggplot(coef_spend_share, aes(x = x, y = estimate*100, color = type, shape = type)) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(aes(x = x, ymin = conf.low*100, ymax = conf.high*100), width = 0,
        position = position_dodge(width = 0.25)) +
  geom_point(size = 3,
        position = position_dodge(width = 0.25)) +
  main_theme +
  scale_x_continuous(labels = coef_spend_share$term, breaks = coef_spend_share$x) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  scale_color_manual(values = c("#000000", "#e41a1c")) +
  labs(
    title = "E. Recovery Expenditures/GDP (%)",
    x = "Precipitation Underestimate (mm)",
    y = ""
  ) +
  main_theme +
  theme(
    axis.text.x = element_text(size = 14, angle = 45, vjust = 1, hjust = 1)
  ) 

pc_plot_dam = ggplot(coef_dam_pc, aes(x = x, y = estimate, color = type, shape = type)) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(aes(x = x, ymin = conf.low, ymax = conf.high), width = 0,
        position = position_dodge(width = 0.25)) +
  geom_point(size = 3,
        position = position_dodge(width = 0.25)) +
  main_theme +
  scale_x_continuous(labels = coef_dam_pc$term, breaks = coef_dam_pc$x) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  scale_color_manual(values = c("#000000", "#e41a1c")) +
  labs(
    title = "C. Damages/Person ($)",
    x = "",
    y = ""
  ) +
  main_theme +
  theme(
    axis.text.x = element_text(size = 14, angle = 45, vjust = 1, hjust = 1)
  ) 

pc_plot_spend = ggplot(coef_spend_share, aes(x = x, y = estimate*100, color = type, shape = type)) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(aes(x = x, ymin = conf.low*100, ymax = conf.high*100), width = 0,
        position = position_dodge(width = 0.25)) +
  geom_point(size = 3,
        position = position_dodge(width = 0.25)) +
  main_theme +
  scale_x_continuous(labels = coef_spend_share$term, breaks = coef_spend_share$x) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  scale_color_manual(values = c("#000000", "#e41a1c")) +
  labs(
    title = "F. Recovery Expenditures/Person ($)",
    x = "Precipitation Underestimate (mm)",
    y = ""
  ) +
  main_theme +
  theme(
    axis.text.x = element_text(size = 14, angle = 45, vjust = 1, hjust = 1)
  ) 

plot_out = (log_plot_dam + share_plot_dam + pc_plot_dam) / (log_plot_spend + share_plot_spend + pc_plot_spend)

ggsave(plot = plot_out, "results/FigureC18.png", width = 3 * 10 * 2 / 3, height = 2 * 8 * 2 / 3)
ggsave(plot = plot_out, "results/FigureC18.pdf", device = cairo_pdf, width = 3 * 10 * 2 / 3, height = 2 * 8 * 2 / 3)

# ==============================================================================
# 9. VARYING FIXED EFFECTS ROBUSTNESS CHECK
# ==============================================================================

# Initialize lists to store regression results for different panels
damage_regs = list()
damage_share_regs = list()
damage_pc_regs = list()
spending_regs = list()
spending_share_regs = list()
spending_pc_regs = list()

### Vary types of intensity fixed effects to capture intensity

fixed_effects_combinations = list(
  "county + state^hurricane + wind_cut",
  "county + state^hurricane + wind_cut + precip_cut",
  "county + state^hurricane + wind_cut + precip_cut + coastal",
  "county + state^hurricane + wind_cut + precip_cut + coastal + left_of_track",
  "county + state^hurricane + wind_cut^precip_cut",
  "county + state^hurricane + wind_cut^precip_cut^coastal",
  "county + state^hurricane + wind_cut^precip_cut^left_of_track",
  "county + state^hurricane + wind_cut^precip_cut^coastal^left_of_track"
)

# Loop through different fixed effect combinations for Figure 3 panels
for (i in seq_along(fixed_effects_combinations)) {
  # Create formula for damages regression
  fe_formula_damage = as.formula(paste("damages/1e6 ~ i(wind_error_bin, ref = \"(-2,0]\") |", fixed_effects_combinations[[i]], "+ precip_error_bin"))
  
  # Run damages regression
  damage_regs[[i]] = conleyreg(
    formula = fe_formula_damage,
    data = load_df(),
    dist_cutoff = dist,
    lag_cutoff = Inf,
    unit = "county",
    time = "year",
    lat = "lat",
    lon = "lon"
  )
  
  # Create formula for damages/GDP regression
  fe_formula_damage_share = as.formula(paste("damage_per_gdp ~ i(wind_error_bin, ref = \"(-2,0]\") |", fixed_effects_combinations[[i]], "+ precip_error_bin"))
  
  # Run damages/GDP regression
  damage_share_regs[[i]] = conleyreg(
    formula = fe_formula_damage_share,
    data = load_df(),
    dist_cutoff = dist,
    lag_cutoff = Inf,
    unit = "county",
    time = "year",
    lat = "lat",
    lon = "lon"
  )
  
  # Create formula for damages per capita regression
  fe_formula_damage_pc = as.formula(paste("damage_pc ~ i(wind_error_bin, ref = \"(-2,0]\") |", fixed_effects_combinations[[i]], "+ precip_error_bin"))
  
  # Run damages per capita regression
  damage_pc_regs[[i]] = conleyreg(
    formula = fe_formula_damage_pc,
    data = load_df(),
    dist_cutoff = dist,
    lag_cutoff = Inf,
    unit = "county",
    time = "year",
    lat = "lat",
    lon = "lon"
  )
  
  # Create formula for spending regression
  fe_formula_spending = as.formula(paste("ex_post_spending/1e6 ~ i(wind_error_bin, ref = \"(-2,0]\") |", fixed_effects_combinations[[i]], "+ precip_error_bin"))
  
  # Run spending regression
  spending_regs[[i]] = conleyreg(
    formula = fe_formula_spending,
    data = load_df(),
    dist_cutoff = dist,
    lag_cutoff = Inf,
    unit = "county",
    time = "year",
    lat = "lat",
    lon = "lon"
  )
  
  # Create formula for spending/GDP regression
  fe_formula_spending_share = as.formula(paste("ex_post_spending_per_gdp ~ i(wind_error_bin, ref = \"(-2,0]\") |", fixed_effects_combinations[[i]], "+ precip_error_bin"))
  
  # Run spending/GDP regression
  spending_share_regs[[i]] = conleyreg(
    formula = fe_formula_spending_share,
    data = load_df(),
    dist_cutoff = dist,
    lag_cutoff = Inf,
    unit = "county",
    time = "year",
    lat = "lat",
    lon = "lon"
  )
  
  # Create formula for spending per capita regression
  fe_formula_spending_pc = as.formula(paste("ex_post_spending_pc ~ i(wind_error_bin, ref = \"(-2,0]\") |", fixed_effects_combinations[[i]], "+ precip_error_bin"))
  
  # Run spending per capita regression
  spending_pc_regs[[i]] = conleyreg(
    formula = fe_formula_spending_pc,
    data = load_df(),
    dist_cutoff = dist,
    lag_cutoff = Inf,
    unit = "county",
    time = "year",
    lat = "lat",
    lon = "lon"
  )
  
  # Print progress
}

# Process Figure 3 regression results for visualization with varying fixed effects

# Extract coefficients for all damage regressions
coef_dam_fe = lapply(seq_along(damage_regs), function(i) {
  tidy(damage_regs[[i]], conf.int = TRUE) |>
    add_row(
      term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .after = 4
    ) |>
    filter(!str_detect(term, "wind_cut") & !str_detect(term, "precip")) |>
    mutate(term = sort(unique(load_df()$wind_error_bin))) |>
    mutate(
      x = row_number(),
      fe_type = paste0("FE", i)
    )
}) |> bind_rows()

# Extract coefficients for all damage/GDP regressions
coef_dam_share_fe = lapply(seq_along(damage_share_regs), function(i) {
  tidy(damage_share_regs[[i]], conf.int = TRUE) |>
    add_row(
      term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .after = 4
    ) |>
    filter(!str_detect(term, "wind_cut") & !str_detect(term, "precip")) |>
    mutate(term = sort(unique(load_df()$wind_error_bin))) |>
    mutate(
      x = row_number(),
      fe_type = paste0("FE", i)
    )
}) |> bind_rows()

# Extract coefficients for all damage per capita regressions
coef_dam_pc_fe = lapply(seq_along(damage_pc_regs), function(i) {
  tidy(damage_pc_regs[[i]], conf.int = TRUE) |>
    add_row(
      term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .after = 4
    ) |>
    filter(!str_detect(term, "wind_cut") & !str_detect(term, "precip")) |>
    mutate(term = sort(unique(load_df()$wind_error_bin))) |>
    mutate(
      x = row_number(),
      fe_type = paste0("FE", i)
    )
}) |> bind_rows()

# Extract coefficients for all spending regressions
coef_spend_fe = lapply(seq_along(spending_regs), function(i) {
  tidy(spending_regs[[i]], conf.int = TRUE) |>
    add_row(
      term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .after = 4
    ) |>
    filter(!str_detect(term, "wind_cut") & !str_detect(term, "precip")) |>
    mutate(term = sort(unique(load_df()$wind_error_bin))) |>
    mutate(
      x = row_number(),
      fe_type = paste0("FE", i)
    )
}) |> bind_rows()

# Extract coefficients for all spending/GDP regressions
coef_spend_share_fe = lapply(seq_along(spending_share_regs), function(i) {
  tidy(spending_share_regs[[i]], conf.int = TRUE) |>
    add_row(
      term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .after = 4
    ) |>
    filter(!str_detect(term, "wind_cut") & !str_detect(term, "precip")) |>
    mutate(term = sort(unique(load_df()$wind_error_bin))) |>
    mutate(
      x = row_number(),
      fe_type = paste0("FE", i)
    )
}) |> bind_rows()

# Extract coefficients for all spending per capita regressions
coef_spend_pc_fe = lapply(seq_along(spending_pc_regs), function(i) {
  tidy(spending_pc_regs[[i]], conf.int = TRUE) |>
    add_row(
      term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .after = 4
    ) |>
    filter(!str_detect(term, "wind_cut") & !str_detect(term, "precip")) |>
    mutate(term = sort(unique(load_df()$wind_error_bin))) |>
    mutate(
      x = row_number(),
      fe_type = paste0("FE", i)
    )
}) |> bind_rows()

# Define colors and shapes for the different FE specifications
fe_colors = c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33','#a65628','#f781bf')
fe_shapes = c(16, 17, 15, 18, 8, 12, 10, 14)

# Create plots for each panel, similar to Figure 3

# A. Damages (million $)
log_plot_dam_fe = ggplot(coef_dam_fe, aes(x = x, y = estimate, color = fe_type, shape = fe_type, group = fe_type)) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(aes(x = x, ymin = conf.low, ymax = conf.high), width = 0,
                position = position_dodge(width = 0.5)) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  scale_x_continuous(labels = unique(coef_dam_fe$term), breaks = unique(coef_dam_fe$x)) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  scale_color_manual(values = fe_colors, 
                    labels = c("Wind FE", 
                              "Wind+Precip FE", 
                              "Wind+Precip+Coastal FE", 
                              "Wind+Precip+Coastal+West FE",
                              "Wind×Precip FE", 
                              "Wind×Precip×Coastal FE", 
                              "Wind×Precip×West FE", 
                              "Wind×Precip×Coastal×West FE")) +
  scale_shape_manual(values = fe_shapes,
                    labels = c("Wind FE", 
                              "Wind+Precip FE", 
                              "Wind+Precip+Coastal FE", 
                              "Wind+Precip+Coastal+West FE",
                              "Wind×Precip FE", 
                              "Wind×Precip×Coastal FE", 
                              "Wind×Precip×West FE", 
                              "Wind×Precip×Coastal×West FE")) +
  labs(
    title = "A. Damages (million $)",
    x = "",
    y = "",
    color = "FEs",
    shape = "FEs"
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
    legend.position = "none",
    axis.text.x = element_text(size = 14, angle = 45, vjust = 1, hjust = 1)
  ) + 
  coord_cartesian(ylim = c(-200, 400))

# B. Damages/GDP (%)
share_plot_dam_fe = ggplot(coef_dam_share_fe, aes(x = x, y = estimate * 100, color = fe_type, shape = fe_type, group = fe_type)) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(aes(x = x, ymin = conf.low * 100, ymax = conf.high * 100), width = 0,
                position = position_dodge(width = 0.5)) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  scale_x_continuous(labels = unique(coef_dam_share_fe$term), breaks = unique(coef_dam_share_fe$x)) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  scale_color_manual(values = fe_colors,
                    labels = c("Wind FE", 
                              "Wind+Precip FE", 
                              "Wind+Precip+Coastal FE", 
                              "Wind+Precip+Coastal+West FE",
                              "Wind×Precip FE", 
                              "Wind×Precip×Coastal FE", 
                              "Wind×Precip×West FE", 
                              "Wind×Precip×Coastal×West FE")) +
  scale_shape_manual(values = fe_shapes,
                    labels = c("Wind FE", 
                              "Wind+Precip FE", 
                              "Wind+Precip+Coastal FE", 
                              "Wind+Precip+Coastal+West FE",
                              "Wind×Precip FE", 
                              "Wind×Precip×Coastal FE", 
                              "Wind×Precip×West FE", 
                              "Wind×Precip×Coastal×West FE")) +
  labs(
    title = "B. Damages/GDP (%)",
    x = "",
    y = "",
    color = "Fixed Effects Specification",
    shape = "Fixed Effects Specification"
  ) +
  main_theme +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 14, angle = 45, vjust = 1, hjust = 1)
  ) + 
  coord_cartesian(ylim = c(-20, 40))

# C. Damages/Person ($)
pc_plot_dam_fe = ggplot(coef_dam_pc_fe, aes(x = x, y = estimate, color = fe_type, shape = fe_type, group = fe_type)) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(aes(x = x, ymin = conf.low, ymax = conf.high), width = 0,
                position = position_dodge(width = 0.5)) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  scale_x_continuous(labels = unique(coef_dam_pc_fe$term), breaks = unique(coef_dam_pc_fe$x)) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  scale_color_manual(values = fe_colors,
                    labels = c("Wind FE", 
                              "Wind+Precip FE", 
                              "Wind+Precip+Coastal FE", 
                              "Wind+Precip+Coastal+West FE",
                              "Wind×Precip FE", 
                              "Wind×Precip×Coastal FE", 
                              "Wind×Precip×West FE", 
                              "Wind×Precip×Coastal×West FE")) +
  scale_shape_manual(values = fe_shapes,
                    labels = c("Wind FE", 
                              "Wind+Precip FE", 
                              "Wind+Precip+Coastal FE", 
                              "Wind+Precip+Coastal+West FE",
                              "Wind×Precip FE", 
                              "Wind×Precip×Coastal FE", 
                              "Wind×Precip×West FE", 
                              "Wind×Precip×Coastal×West FE")) +
  labs(
    title = "C. Damages/Person ($)",
    x = "",
    y = "",
    color = "Fixed Effects Specification",
    shape = "Fixed Effects Specification"
  ) +
  main_theme +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 14, angle = 45, vjust = 1, hjust = 1)
  ) + 
  coord_cartesian(ylim = c(-5000, 10000))

# D. Recovery Expenditures (million $)
log_plot_spend_fe = ggplot(coef_spend_fe, aes(x = x, y = estimate, color = fe_type, shape = fe_type, group = fe_type)) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(aes(x = x, ymin = conf.low, ymax = conf.high), width = 0,
                position = position_dodge(width = 0.5)) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  scale_x_continuous(labels = unique(coef_spend_fe$term), breaks = unique(coef_spend_fe$x)) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  scale_color_manual(values = fe_colors,
                    labels = c("Wind FE", 
                              "Wind+Precip FE", 
                              "Wind+Precip+Coastal FE", 
                              "Wind+Precip+Coastal+West FE",
                              "Wind×Precip FE", 
                              "Wind×Precip×Coastal FE", 
                              "Wind×Precip×West FE", 
                              "Wind×Precip×Coastal×West FE")) +
  scale_shape_manual(values = fe_shapes,
                    labels = c("Wind FE", 
                              "Wind+Precip FE", 
                              "Wind+Precip+Coastal FE", 
                              "Wind+Precip+Coastal+West FE",
                              "Wind×Precip FE", 
                              "Wind×Precip×Coastal FE", 
                              "Wind×Precip×West FE", 
                              "Wind×Precip×Coastal×West FE")) +
  labs(
    title = "D. Recovery Expenditures (million $)",
    x = "Wind Speed Underestimate (m/s)",
    y = "",
    color = "Fixed Effects Specification",
    shape = "Fixed Effects Specification"
  ) +
  main_theme +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 14, angle = 45, vjust = 1, hjust = 1)
  ) + 
  coord_cartesian(ylim = c(-20, 40))

# E. Recovery Expenditures/GDP (%)
share_plot_spend_fe = ggplot(coef_spend_share_fe, aes(x = x, y = estimate * 100, color = fe_type, shape = fe_type, group = fe_type)) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(aes(x = x, ymin = conf.low * 100, ymax = conf.high * 100), width = 0,
                position = position_dodge(width = 0.5)) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  scale_x_continuous(labels = unique(coef_spend_share_fe$term), breaks = unique(coef_spend_share_fe$x)) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  scale_color_manual(values = fe_colors,
                    labels = c("Wind FE", 
                              "Wind+Precip FE", 
                              "Wind+Precip+Coastal FE", 
                              "Wind+Precip+Coastal+West FE",
                              "Wind×Precip FE", 
                              "Wind×Precip×Coastal FE", 
                              "Wind×Precip×West FE", 
                              "Wind×Precip×Coastal×West FE")) +
  scale_shape_manual(values = fe_shapes,
                    labels = c("Wind FE", 
                              "Wind+Precip FE", 
                              "Wind+Precip+Coastal FE", 
                              "Wind+Precip+Coastal+West FE",
                              "Wind×Precip FE", 
                              "Wind×Precip×Coastal FE", 
                              "Wind×Precip×West FE", 
                              "Wind×Precip×Coastal×West FE")) +
  labs(
    title = "E. Recovery Expenditures/GDP (%)",
    x = "Wind Speed Underestimate (m/s)",
    y = "",
    color = "Fixed Effects Specification",
    shape = "Fixed Effects Specification"
  ) +
  main_theme +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 14, angle = 45, vjust = 1, hjust = 1)
  ) + 
  coord_cartesian(ylim = c(-1.5, 1.0))

# F. Recovery Expenditures/Person ($)
pc_plot_spend_fe = ggplot(coef_spend_pc_fe, aes(x = x, y = estimate, color = fe_type, shape = fe_type, group = fe_type)) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(aes(x = x, ymin = conf.low, ymax = conf.high), width = 0,
                position = position_dodge(width = 0.5)) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  scale_x_continuous(labels = unique(coef_spend_pc_fe$term), breaks = unique(coef_spend_pc_fe$x)) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  scale_color_manual(values = fe_colors,
                    labels = c("Wind FE", 
                              "Wind+Precip FE", 
                              "Wind+Precip+Coastal FE", 
                              "Wind+Precip+Coastal+West FE",
                              "Wind×Precip FE", 
                              "Wind×Precip×Coastal FE", 
                              "Wind×Precip×West FE", 
                              "Wind×Precip×Coastal×West FE")) +
  scale_shape_manual(values = fe_shapes,
                    labels = c("Wind FE", 
                              "Wind+Precip FE", 
                              "Wind+Precip+Coastal FE", 
                              "Wind+Precip+Coastal+West FE",
                              "Wind×Precip FE", 
                              "Wind×Precip×Coastal FE", 
                              "Wind×Precip×West FE", 
                              "Wind×Precip×Coastal×West FE")) +
  labs(
    title = "F. Recovery Expenditures/Person ($)",
    x = "Wind Speed Underestimate (m/s)",
    y = "",
    color = "Fixed Effects Specification",
    shape = "Fixed Effects Specification"
  ) +
  main_theme +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 14, angle = 45, vjust = 1, hjust = 1)
  ) + 
  coord_cartesian(ylim = c(-500, 1000))

# Rebuild plots with shape mapped outside the main aesthetics
# To resolve the duplicate-legend issue

# Set up plots with both shape and color for FE types
log_plot_dam_fe = ggplot(coef_dam_fe, aes(x = x, y = estimate, color = fe_type, shape = fe_type, group = fe_type)) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(aes(x = x, ymin = conf.low, ymax = conf.high), width = 0,
                position = position_dodge(width = 0.5)) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  scale_x_continuous(labels = unique(coef_dam_fe$term), breaks = unique(coef_dam_fe$x)) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  scale_color_manual(values = fe_colors, 
                    labels = c("Wind FE", 
                              "Wind+Precip FE", 
                              "Wind+Precip+Coastal FE", 
                              "Wind+Precip+Coastal+West FE",
                              "Wind×Precip FE", 
                              "Wind×Precip×Coastal FE", 
                              "Wind×Precip×West FE", 
                              "Wind×Precip×Coastal×West FE")) +
  scale_shape_manual(values = fe_shapes,
                    labels = c("Wind FE", 
                              "Wind+Precip FE", 
                              "Wind+Precip+Coastal FE", 
                              "Wind+Precip+Coastal+West FE",
                              "Wind×Precip FE", 
                              "Wind×Precip×Coastal FE", 
                              "Wind×Precip×West FE", 
                              "Wind×Precip×Coastal×West FE")) +
  labs(
    title = "A. Damages (million $)",
    x = "",
    y = "",
    color = "Fixed Effects Specification",
    shape = "Fixed Effects Specification"
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
    legend.position = "none",
    axis.text.x = element_text(size = 14, angle = 45, vjust = 1, hjust = 1)
  ) + 
  coord_cartesian(ylim = c(-200, 400))

# Create a plot with a shared legend
plot_out_fe = (log_plot_dam_fe + share_plot_dam_fe + pc_plot_dam_fe) / 
              (log_plot_spend_fe + share_plot_spend_fe + pc_plot_spend_fe) +
  plot_layout(guides = "collect") & 
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(size = 20),    # Legend title size
    legend.text = element_text(size = 18),     # Legend text size
    legend.spacing.x = unit(1.5, 'cm'),
    legend.margin = margin(t = 10),
    legend.box.margin = margin(t = 10)
  ) & 
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE, override.aes = list(size = 5), 
                        title = "Fixed Effects Specification"),
    shape = guide_legend(nrow = 2, byrow = TRUE, override.aes = list(size = 5), 
                        title = "Fixed Effects Specification")
  )

# Save the figure
ggsave(plot = plot_out_fe, "results/FigureC19.png", width = 3 * 10 * 2 / 3, height = 2 * 8 * 2 / 3 + 2)  # Extra height for legend
ggsave(plot = plot_out_fe, "results/FigureC19.pdf", device = cairo_pdf, width = 3 * 10 * 2 / 3, height = 2 * 8 * 2 / 3 + 2)

# ==============================================================================
# 10. VARYING INTENSITY BINS ROBUSTNESS CHECK
# ==============================================================================

# Define labels for the wind error bins
x_labels = c("[-10, -8]", "(-8, -6]", "(-6, -4]", "(-4, -2]", "(-2, 0]", "(0, 2]", "(2, 4]", "(4, 6]", "(6, 8]", "(8, 10]")

# Define vector of bin numbers to try
num_bins_vec = c(5, 10, 20, 40, 60, 80, 100, 120)

# Initialize list to store regression results
reg_results = list()

for (num_bins in num_bins_vec) {
  # Create binned dataset
  df_binned = load_df() |>
    # Group positive wind observations before binning
    group_by(wind_obs > 0) |>
    mutate(
      # Create wind cuts using ntile, setting 0 observations to bin 0
      wind_cut_new = ntile(wind_obs, num_bins),
      wind_cut_new = ifelse(wind_obs == 0, 0, wind_cut_new)
    ) |>
    ungroup() |>
    group_by(precip_obs > 0) |>
    mutate(
      # Create precip cuts using ntile, setting 0 observations to bin 0
      precip_cut_new = ntile(precip_obs, num_bins),
      precip_cut_new = ifelse(precip_obs == 0, 0, precip_cut_new)
    ) |>
    ungroup()
  
  # Run regression with binned variables
  reg_results[[as.character(num_bins)]] = conleyreg(
    damages/1e6 ~ i(wind_error_bin, ref = "(-2,0]") |
      county + state^hurricane + wind_cut_new^precip_cut_new^coastal^left_of_track + precip_error_bin,
    data = df_binned,
    dist_cutoff = dist,
    lag_cutoff = Inf,
    unit = "county",
    time = "year",
    lat = "lat",
    lon = "lon",
    batch_ram_opt = "none"
  )
  
  # Print progress
}


# Initialize list to store regression results
reg_results_pgdp = list()

for (num_bins in num_bins_vec) {
  # Create binned dataset
  df_binned = load_df() |>
    # Group positive wind observations before binning
    group_by(wind_obs > 0) |>
    mutate(
      # Create wind cuts using ntile, setting 0 observations to bin 0
      wind_cut_new = ntile(wind_obs, num_bins),
      wind_cut_new = ifelse(wind_obs == 0, 0, wind_cut_new)
    ) |>
    ungroup() |>
    group_by(precip_obs > 0) |>
    mutate(
      # Create precip cuts using ntile, setting 0 observations to bin 0
      precip_cut_new = ntile(precip_obs, num_bins),
      precip_cut_new = ifelse(precip_obs == 0, 0, precip_cut_new)
    ) |>
    ungroup()
  
  # Run regression with binned variables
  reg_results_pgdp[[as.character(num_bins)]] = conleyreg(
    damage_per_gdp*100 ~ i(wind_error_bin, ref = "(-2,0]") |
      county + state^hurricane + wind_cut_new^precip_cut_new^coastal^left_of_track + precip_error_bin,
    data = df_binned,
    dist_cutoff = dist,
    lag_cutoff = Inf,
    unit = "county",
    time = "year",
    lat = "lat",
    lon = "lon",
    batch_ram_opt = "none"
  )
  
  # Print progress
}

# Initialize list to store regression results
reg_results_pc = list()

for (num_bins in num_bins_vec) {
  # Create binned dataset
  df_binned = load_df() |>
    # Group positive wind observations before binning
    group_by(wind_obs > 0) |>
    mutate(
      # Create wind cuts using ntile, setting 0 observations to bin 0
      wind_cut_new = ntile(wind_obs, num_bins),
      wind_cut_new = ifelse(wind_obs == 0, 0, wind_cut_new)
    ) |>
    ungroup() |>
    group_by(precip_obs > 0) |>
    mutate(
      # Create precip cuts using ntile, setting 0 observations to bin 0
      precip_cut_new = ntile(precip_obs, num_bins),
      precip_cut_new = ifelse(precip_obs == 0, 0, precip_cut_new)
    ) |>
    ungroup()
  
  # Run regression with binned variables
  reg_results_pc[[as.character(num_bins)]] = conleyreg(
    damage_pc ~ i(wind_error_bin, ref = "(-2,0]") |
      county + state^hurricane + wind_cut_new^precip_cut_new^coastal^left_of_track + precip_error_bin,
    data = df_binned,
    dist_cutoff = dist,
    lag_cutoff = Inf,
    unit = "county",
    time = "year",
    lat = "lat",
    lon = "lon",
    batch_ram_opt = "none"
  )
  
  # Print progress
}

# Compare coefficients across different bin specifications
coef_comparison = lapply(names(reg_results), function(nb) {
  tidy(reg_results[[nb]]) |>
  add_row(
    term = "asdf", estimate = 0, std.error = 0, .after = 4
  ) |>
    mutate(num_bins = as.numeric(nb)) 
}) |>
  bind_rows()

# Create data arrays for each dependent variable
# First for damages and damages per GDP, per capita
wind_error_comparison = coef_comparison |>
  mutate(
    bin_label = rep(1:10, length(num_bins_vec))
  ) 

wind_error_comparison_pgdp = bind_rows(lapply(names(reg_results_pgdp), function(nb) {
  tidy(reg_results_pgdp[[nb]]) |>
  add_row(
    term = "asdf", estimate = 0, std.error = 0, .after = 4
  ) |>
    mutate(num_bins = as.numeric(nb)) 
})) |>
  mutate(
    bin_label = rep(1:10, length(num_bins_vec))
  )

wind_error_comparison_pc = bind_rows(lapply(names(reg_results_pc), function(nb) {
  tidy(reg_results_pc[[nb]]) |>
  add_row(
    term = "asdf", estimate = 0, std.error = 0, .after = 4
  ) |>
    mutate(num_bins = as.numeric(nb)) 
})) |>
  mutate(
    bin_label = rep(1:10, length(num_bins_vec))
  )

# Now for spending variables (ex_post_spending)
# Initialize the regression results lists for spending
spend_results = list()
spend_results_pgdp = list()
spend_results_pc = list()

# Run regressions for spending metrics
for (num_bins in num_bins_vec) {
  # Create binned dataset
  df_binned = load_df() |>
    group_by(wind_obs > 0) |>
    mutate(
      wind_cut_new = ntile(wind_obs, num_bins),
      wind_cut_new = ifelse(wind_obs == 0, 0, wind_cut_new)
    ) |>
    ungroup() |>
    group_by(precip_obs > 0) |>
    mutate(
      precip_cut_new = ntile(precip_obs, num_bins),
      precip_cut_new = ifelse(precip_obs == 0, 0, precip_cut_new)
    ) |>
    ungroup()
  
  # Run regression for ex_post_spending
  spend_results[[as.character(num_bins)]] = conleyreg(
    ex_post_spending/1e6 ~ i(wind_error_bin, ref = "(-2,0]") |
      county + state^hurricane + wind_cut_new^precip_cut_new^coastal^left_of_track + precip_error_bin,
    data = df_binned,
    dist_cutoff = dist,
    lag_cutoff = Inf,
    unit = "county",
    time = "year",
    lat = "lat",
    lon = "lon",
    batch_ram_opt = "none"
  )
  # Print progress
}

for (num_bins in num_bins_vec) {
  # Create binned dataset
  df_binned = load_df() |>
    group_by(wind_obs > 0) |>
    mutate(
      wind_cut_new = ntile(wind_obs, num_bins),
      wind_cut_new = ifelse(wind_obs == 0, 0, wind_cut_new)
    ) |>
    ungroup() |>
    group_by(precip_obs > 0) |>
    mutate(
      precip_cut_new = ntile(precip_obs, num_bins),
      precip_cut_new = ifelse(precip_obs == 0, 0, precip_cut_new)
    ) |>
    ungroup()
  
  # Print progress
}


# Run regressions for spending metrics
for (num_bins in num_bins_vec) {
  # Create binned dataset
  df_binned = load_df() |>
    group_by(wind_obs > 0) |>
    mutate(
      wind_cut_new = ntile(wind_obs, num_bins),
      wind_cut_new = ifelse(wind_obs == 0, 0, wind_cut_new)
    ) |>
    ungroup() |>
    group_by(precip_obs > 0) |>
    mutate(
      precip_cut_new = ntile(precip_obs, num_bins),
      precip_cut_new = ifelse(precip_obs == 0, 0, precip_cut_new)
    ) |>
    ungroup()

  # Run regression for ex_post_spending_per_gdp
  spend_results_pgdp[[as.character(num_bins)]] = conleyreg(
    ex_post_spending_per_gdp*100 ~ i(wind_error_bin, ref = "(-2,0]") |
      county + state^hurricane + wind_cut_new^precip_cut_new^coastal^left_of_track + precip_error_bin,
    data = df_binned,
    dist_cutoff = dist,
    lag_cutoff = Inf,
    unit = "county",
    time = "year",
    lat = "lat",
    lon = "lon",
    batch_ram_opt = "none"
  )
  # Print progress
}

for (num_bins in num_bins_vec) {
  # Create binned dataset
  df_binned = load_df() |>
    group_by(wind_obs > 0) |>
    mutate(
      wind_cut_new = ntile(wind_obs, num_bins),
      wind_cut_new = ifelse(wind_obs == 0, 0, wind_cut_new)
    ) |>
    ungroup() |>
    group_by(precip_obs > 0) |>
    mutate(
      precip_cut_new = ntile(precip_obs, num_bins),
      precip_cut_new = ifelse(precip_obs == 0, 0, precip_cut_new)
    ) |>
    ungroup()
  
  # Print progress
}


# Run regressions for spending metrics
for (num_bins in num_bins_vec) {
  # Create binned dataset
  df_binned = load_df() |>
    group_by(wind_obs > 0) |>
    mutate(
      wind_cut_new = ntile(wind_obs, num_bins),
      wind_cut_new = ifelse(wind_obs == 0, 0, wind_cut_new)
    ) |>
    ungroup() |>
    group_by(precip_obs > 0) |>
    mutate(
      precip_cut_new = ntile(precip_obs, num_bins),
      precip_cut_new = ifelse(precip_obs == 0, 0, precip_cut_new)
    ) |>
    ungroup()

  
  # Run regression for ex_post_spending_pc
  spend_results_pc[[as.character(num_bins)]] = conleyreg(
    ex_post_spending_pc ~ i(wind_error_bin, ref = "(-2,0]") |
      county + state^hurricane + wind_cut_new^precip_cut_new^coastal^left_of_track + precip_error_bin,
    data = df_binned,
    dist_cutoff = dist,
    lag_cutoff = Inf,
    unit = "county",
    time = "year",
    lat = "lat",
    lon = "lon",
    batch_ram_opt = "none"
  )
  
  # Print progress
}

# Process damage regression results
wind_error_comparison = bind_rows(lapply(names(reg_results), function(nb) {
  tidy(reg_results[[nb]]) |>
    filter(str_detect(term, "^wind_error_bin::")) |>  # Only keep wind_error_bin terms
  add_row(
    term = "asdf", estimate = 0, std.error = 0, .after = 4
  ) |>
    mutate(num_bins = as.numeric(nb)) 
})) |>
  mutate(
    bin_label = rep(1:10, length(num_bins_vec)),
    bin_label = factor(bin_label)  # Convert to factor to ensure correct ordering
  )

wind_error_comparison_pgdp = bind_rows(lapply(names(reg_results_pgdp), function(nb) {
  tidy(reg_results_pgdp[[nb]]) |>
 filter(str_detect(term, "^wind_error_bin::")) |>  # Only keep wind_error_bin terms
  add_row(
    term = "asdf", estimate = 0, std.error = 0, .after = 4
  ) |>
    mutate(num_bins = as.numeric(nb)) 
})) |>
  mutate(
    bin_label = rep(1:10, length(num_bins_vec)),
    bin_label = factor(bin_label)  # Convert to factor to ensure correct ordering
  )

wind_error_comparison_pc = bind_rows(lapply(names(reg_results_pc), function(nb) {
  tidy(reg_results_pc[[nb]]) |>
    filter(str_detect(term, "^wind_error_bin::")) |>  # Only keep wind_error_bin terms
  add_row(
    term = "asdf", estimate = 0, std.error = 0, .after = 4
  ) |>
    mutate(num_bins = as.numeric(nb)) 
})) |>
  mutate(
    bin_label = rep(1:10, length(num_bins_vec)),
    bin_label = factor(bin_label)  # Convert to factor to ensure correct ordering
  )

# Process spending regression results
spend_error_comparison = bind_rows(lapply(names(spend_results), function(nb) {
  tidy(spend_results[[nb]]) |>
    filter(str_detect(term, "^wind_error_bin::")) |>  # Only keep wind_error_bin terms
  add_row(
    term = "asdf", estimate = 0, std.error = 0, .after = 4
  ) |>
    mutate(num_bins = as.numeric(nb)) 
})) |>
  mutate(
    bin_label = rep(1:10, length(num_bins_vec)),
    bin_label = factor(bin_label)  # Convert to factor to ensure correct ordering
  )

spend_error_comparison_pgdp = bind_rows(lapply(names(spend_results_pgdp), function(nb) {
  tidy(spend_results_pgdp[[nb]]) |>
    filter(str_detect(term, "^wind_error_bin::")) |>  # Only keep wind_error_bin terms
  add_row(
    term = "asdf", estimate = 0, std.error = 0, .after = 4
  ) |>
    mutate(num_bins = as.numeric(nb)) 
})) |>
  mutate(
    bin_label = rep(1:10, length(num_bins_vec)),
    bin_label = factor(bin_label)  # Convert to factor to ensure correct ordering
  )

spend_error_comparison_pc = bind_rows(lapply(names(spend_results_pc), function(nb) {
  tidy(spend_results_pc[[nb]]) |>
    filter(str_detect(term, "^wind_error_bin::")) |>  # Only keep wind_error_bin terms
  add_row(
    term = "asdf", estimate = 0, std.error = 0, .after = 4
  ) |>
    mutate(num_bins = as.numeric(nb)) 
})) |>
  mutate(
    bin_label = rep(1:10, length(num_bins_vec)),
    bin_label = factor(bin_label)  # Convert to factor to ensure correct ordering
  )

# Create the 6 panel plots
# A. Damages (million $)
dam_plot = ggplot(wind_error_comparison, aes(x = bin_label, y = estimate, color = factor(num_bins), group = factor(num_bins), shape = factor(num_bins))) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(
    aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error),
    position = position_dodge(width = 0.5),
    width = 0
  ) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  scale_x_discrete(labels = x_labels) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  scale_shape_manual(values = c(16, 17, 15, 18, 8, 12, 10, 14)) +
  scale_color_manual(values = c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33','#a65628','#f781bf')) +  
  main_theme +
  labs(
    title = "A. Damages (million $)",
    x = "",
    y = "",
    color = "Number of Intensity Bins",
    shape = "Number of Intensity Bins"
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
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 14, angle = 45, vjust = 1, hjust = 1)
  ) +
  coord_cartesian(ylim = c(-200, 400))

# B. Damages/GDP (%)
dam_plot_pgdp = ggplot(wind_error_comparison_pgdp, aes(x = bin_label, y = estimate, color = factor(num_bins), group = factor(num_bins), shape = factor(num_bins))) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(
    aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error),
    position = position_dodge(width = 0.5),
    width = 0
  ) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  scale_x_discrete(labels = x_labels) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  scale_shape_manual(values = c(16, 17, 15, 18, 8, 12, 10, 14)) +
  scale_color_manual(values = c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33','#a65628','#f781bf')) + 
  main_theme +
  labs(
    title = "B. Damages/GDP (%)",
    x = "",
    y = "",
    color = "Number of Intensity Bins",
    shape = "Number of Intensity Bins"
  ) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 14, angle = 45, vjust = 1, hjust = 1)
  ) +
  coord_cartesian(ylim = c(-20, 40))

# C. Damages/Person ($)
dam_plot_pc = ggplot(wind_error_comparison_pc, aes(x = bin_label, y = estimate, color = factor(num_bins), group = factor(num_bins), shape = factor(num_bins))) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(
    aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error),
    position = position_dodge(width = 0.5),
    width = 0
  ) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  scale_x_discrete(labels = x_labels) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  scale_shape_manual(values = c(16, 17, 15, 18, 8, 12, 10, 14)) +
  scale_color_manual(values = c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33','#a65628','#f781bf')) + 
  main_theme +
  labs(
    title = "C. Damages/Person ($)",
    x = "",
    y = "",
    color = "Number of Intensity Bins",
    shape = "Number of Intensity Bins"
  ) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 14, angle = 45, vjust = 1, hjust = 1)
  ) +
  coord_cartesian(ylim = c(-5000, 10000))

# D. Recovery Expenditures (million $)
spend_plot = ggplot(spend_error_comparison, aes(x = bin_label, y = estimate, color = factor(num_bins), group = factor(num_bins), shape = factor(num_bins))) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(
    aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error),
    position = position_dodge(width = 0.5),
    width = 0
  ) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  scale_x_discrete(labels = x_labels) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  scale_shape_manual(values = c(16, 17, 15, 18, 8, 12, 10, 14)) +
  scale_color_manual(values = c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33','#a65628','#f781bf')) + 
  main_theme +
  labs(
    title = "D. Recovery Expenditures (million $)",
    x = "Wind Speed Underestimate (m/s)",
    y = "",
    color = "Number of Intensity Bins",
    shape = "Number of Intensity Bins"
  ) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 14, angle = 45, vjust = 1, hjust = 1)
  ) +
  coord_cartesian(ylim = c(-20, 40))

# E. Recovery Expenditures/GDP (%)
spend_plot_pgdp = ggplot(spend_error_comparison_pgdp, aes(x = bin_label, y = estimate, color = factor(num_bins), group = factor(num_bins), shape = factor(num_bins))) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(
    aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error),
    position = position_dodge(width = 0.5),
    width = 0
  ) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  scale_x_discrete(labels = x_labels) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  scale_shape_manual(values = c(16, 17, 15, 18, 8, 12, 10, 14)) +
  scale_color_manual(values = c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33','#a65628','#f781bf')) + 
  main_theme +
  labs(
    title = "E. Recovery Expenditures/GDP (%)",
    x = "Wind Speed Underestimate (m/s)",
    y = "",
    color = "Number of Intensity Bins",
    shape = "Number of Intensity Bins"
  ) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 14, angle = 45, vjust = 1, hjust = 1)
  ) +
  coord_cartesian(ylim = c(-1.5, 1.0))

# F. Recovery Expenditures/Person ($)
spend_plot_pc = ggplot(spend_error_comparison_pc, aes(x = bin_label, y = estimate, color = factor(num_bins), group = factor(num_bins), shape = factor(num_bins))) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(
    aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error),
    position = position_dodge(width = 0.5),
    width = 0
  ) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  scale_x_discrete(labels = x_labels) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  scale_shape_manual(values = c(16, 17, 15, 18, 8, 12, 10, 14)) +
  scale_color_manual(values = c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33','#a65628','#f781bf')) + 
  main_theme +
  labs(
    title = "F. Recovery Expenditures/Person ($)",
    x = "Wind Speed Underestimate (m/s)",
    y = "",
    color = "Number of Intensity Bins",
    shape = "Number of Intensity Bins"
  ) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 14, angle = 45, vjust = 1, hjust = 1)
  ) +
  coord_cartesian(ylim = c(-500, 1000))

# Combine the plots in a 2x3 layout with a shared legend
plot_out = (dam_plot + dam_plot_pgdp + dam_plot_pc) / 
           (spend_plot + spend_plot_pgdp + spend_plot_pc) +
  plot_layout(guides = "collect") & 
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(size = 20),    # Legend title size
    legend.text = element_text(size = 18),     # Legend text size
    legend.spacing.x = unit(1.5, 'cm'),
    legend.margin = margin(t = 10),
    legend.box.margin = margin(t = 10)
  ) & 
  guides(
    color = guide_legend(nrow = 1, byrow = TRUE, override.aes = list(size = 5)),
    shape = guide_legend(nrow = 1, byrow = TRUE, override.aes = list(size = 5))
  )

# Save the plot
ggsave(plot = plot_out, "results/FigureC20.png", width = 3*10*2/3, height = 2*8*2/3 + 1)  # Extra height for legend
ggsave(plot = plot_out, "results/FigureC20.pdf", device = cairo_pdf, width = 3*10*2/3, height = 2*8*2/3 + 1)

# ==============================================================================
# 11. POPULATION WEIGHTING
# ==============================================================================

reg_dam = conleyreg(
  damages/1e6 ~ i(wind_error_bin, ref = "(-2,0]")
  |
    county + state^hurricane + wind_cut_popw + precip_cut + precip_error_bin,
  data = load_df_popw(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

df_popw = load_df_popw()

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
    county + state^hurricane + wind_cut_popw + precip_cut + precip_error_bin,
  data = load_df_popw(),
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
    county + state^hurricane + wind_cut_popw + precip_cut + precip_error_bin,
  data = load_df_popw(),
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
    county + state^hurricane + wind_cut_popw + precip_cut + precip_error_bin,
  data = load_df_popw(),
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
    county + state^hurricane + wind_cut_popw + precip_cut + precip_error_bin,
  data = load_df_popw(),
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
    county + state^hurricane + wind_cut_popw + precip_cut + precip_error_bin,
  data = load_df_popw(),
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

ggsave(plot = plot_out, "results/FigureC21.png", width = 3 * 10 * 2 / 3, height = 2 * 8 * 2 / 3)
ggsave(plot = plot_out, "results/FigureC21.pdf", device = cairo_pdf, width = 3 * 10 * 2 / 3, height = 2 * 8 * 2 / 3)

# ==============================================================================
# 12. WIND INTENSITY INTERACTIONS
# ==============================================================================

run_interaction_and_margins = function(outcome_var, wind_obs_levels, data, controls) {
  # Build regression formula
  formula_str = paste0(outcome_var, " ~ i(wind_error_bin, ref = \"(-2,0]\") * asinh(wind_obs) | ", controls)
  formula_obj = as.formula(formula_str)
  
  # Run model for coefficients
  reg = conleyreg(
    formula_obj,
    data = data,
    dist_cutoff = dist,
    lag_cutoff = Inf,
    unit = "county",
    time = "year",
    lat = "lat",
    lon = "lon",
    batch_ram_opt = "none"
  )

  # Get robust variance-covariance matrix
  vcov_matrix = conleyreg(
    formula_obj,
    data = data,
    dist_cutoff = dist,
    lag_cutoff = Inf,
    unit = "county",
    time = "year",
    lat = "lat",
    lon = "lon",
    batch_ram_opt = "none",
    vcov = TRUE
  )
  
  # Extract coefficients and create combinations
  coef_base = tidy(reg) |>
    filter(str_detect(term, "wind_error_bin") & !str_detect(term, "asinh")) |>
    add_row(
      term = "asdf", estimate = 0, std.error = 0, .after = 4
    ) |>
    filter(!str_detect(term, "wind_cut") & !str_detect(term, "precip")) |>
    mutate(bin_label = sort(unique(df$wind_error_bin))) |>
    mutate(x = row_number()) 
  
  
  coef_interact = tidy(reg) |>
    filter(str_detect(term, "wind_error_bin") & str_detect(term, "asinh")) |>
    add_row(
      term = "asdf", estimate = 0, std.error = 0, .after = 4
    ) |>
    filter(!str_detect(term, "wind_cut") & !str_detect(term, "precip")) |>
    mutate(bin_label = sort(unique(df$wind_error_bin))) |>
    mutate(x = row_number()) 
  
  
  # Create a data frame for plotting
  plot_data = expand_grid(
    bin_label = coef_base$bin_label,
    wind_obs = wind_obs_bins
  ) |>
    left_join(
      coef_base |> select(bin_label, base_estimate = estimate, base_term = term),
      by = "bin_label"
    ) |>
    left_join(
      coef_interact |> select(bin_label, interact_estimate = estimate, interact_term = term),
      by = "bin_label"
    ) |>
    mutate(
      # Calculate combined effect
      total_effect = base_estimate + interact_estimate * asinh(wind_obs)
    )
  
  # Function to compute standard error for linear combination
  compute_se = function(base_term, interact_term, wind_obs_val, vcov_matrix) {
    # Get indices in vcov matrix
    base_idx = which(colnames(vcov_matrix) == base_term)
    interact_idx = which(colnames(vcov_matrix) == interact_term)
    
    # Create linear combination vector
    combo_vector = rep(0, ncol(vcov_matrix))
    combo_vector[base_idx] = 1
    combo_vector[interact_idx] = asinh(wind_obs_val)
    
    # Compute standard error
    se = sqrt(t(combo_vector) %*% vcov_matrix %*% combo_vector)
    return(as.numeric(se))
  }
  
  # Add standard errors to plot data
  plot_data = plot_data |>
    rowwise() |>
    mutate(
      se = compute_se(base_term, interact_term, wind_obs, vcov_matrix),
      conf_low = total_effect - 1.96 * se,
      conf_high = total_effect + 1.96 * se
    ) |>
    ungroup()  
  
  return(plot_data)
}

# Setup
controls = "county + state^hurricane + wind_cut^precip_cut^coastal^left_of_track + precip_error_bin"

# Outcomes

plot_data_dam        = run_interaction_and_margins("damages/1e6", wind_obs_bins, load_df(), controls)
plot_data_dam_pgdp   = run_interaction_and_margins("damage_per_gdp*100", wind_obs_bins, load_df(), controls)
plot_data_dam_pc     = run_interaction_and_margins("damage_pc", wind_obs_bins, load_df(), controls)
plot_data_spend      = run_interaction_and_margins("ex_post_spending/1e6", wind_obs_bins, load_df(), controls)
plot_data_spend_pgdp = run_interaction_and_margins("ex_post_spending_per_gdp*100", wind_obs_bins, load_df(), controls)
plot_data_spend_pc   = run_interaction_and_margins("ex_post_spending_pc", wind_obs_bins, load_df(), controls)


make_panel = function(data, title, ylimits = NULL, show_xlabel = FALSE) {
  data = data |> 
    mutate(
      bin_label = factor(bin_label, levels = sort(unique(bin_label))),
      wind_obs = factor(wind_obs, levels = sort(unique(wind_obs)))
    )
  
  p = ggplot(data, aes(x = bin_label, y = total_effect, color = wind_obs, shape = wind_obs)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey55") +
    geom_errorbar(
      aes(ymin = conf_low, ymax = conf_high),
      position = position_dodge(width = 0.5),
      width = 0.5
    ) +
    geom_point(
      position = position_dodge(width = 0.5),
      size = 3
    ) +
    scale_color_manual(values = fe_colors[1:length(unique(data$wind_obs))], name = "Wind Speed (m/s)") +
    scale_shape_manual(values = fe_shapes[1:length(unique(data$wind_obs))], name = "Wind Speed (m/s)") +
    main_theme +
    labs(
      title = title,
      x = if (show_xlabel) "Wind Speed Error Underestimate (m/s)" else NULL
    ) +
    theme(
      text = element_text(family = "Lato"),
      legend.position = "bottom",
      axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
      axis.title.y = element_blank()
    ) +
    coord_cartesian(ylim = ylimits)
  
  if (title == "A. Damages (million $)") {
  p = p +
    annotate("text",
             x = 2, y = -1500,
             family = "Lato",
             label = "Overestimate", size = 6, hjust = 0
    ) +
    annotate("text",
             x = 6.3, y = -1500,
             family = "Lato",
             label = "Underestimate", size = 6, hjust = 0
    ) +
    geom_segment(aes(x = 1.85, y = -1500, xend = 1.25, yend = -1500),
                 arrow = arrow(length = unit(0.1, "cm")), color = "black"
    ) +
    geom_segment(aes(x = 9.35, y = -1500, xend = 9.95, yend = -1500),
                 arrow = arrow(length = unit(0.1, "cm")), color = "black"
    )
}
  return(p)
}


# Generate and save plot

plot_out = (
  make_panel(plot_data_dam,        "A. Damages (million $)") +
    make_panel(plot_data_dam_pgdp,   "B. Damages / GDP (%)") +
    make_panel(plot_data_dam_pc,     "C. Damages / Person ($)")
) /
  (
    make_panel(plot_data_spend,        "D. Recovery Expenditure (million $)", show_xlabel = TRUE) +
      make_panel(plot_data_spend_pgdp,   "E. Recovery Expenditure/ GDP (%)", show_xlabel = TRUE) +
      make_panel(plot_data_spend_pc,     "F. Recovery Expenditure/ Person ($)", show_xlabel = TRUE)
  ) +
  plot_layout(guides = "collect") & theme(
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
    shape = guide_legend(nrow = 1, byrow = TRUE, override.aes = list(size = 5))
  )

ggsave(plot = plot_out, filename = "results/FigureC22.png", width = 3 * 10 * 2 / 3, height = 2 * 8 * 2 / 3)
ggsave(plot = plot_out, filename = "results/FigureC22.pdf", device = cairo_pdf, width = 3 * 10 * 2 / 3, height = 2 * 8 * 2 / 3)

# ==============================================================================
# 13. TRACK ANALYSIS
# ==============================================================================

df_track = load_df_track()

# Original regression with precip_error_bin and wind_error_bin in fixed effects
reg_dam = conleyreg(
  damages/1e6 ~ i(track_error_bin, ref = "(-80,0]") 
  |
    county + state^hurricane + wind_cut + precip_cut + track_cut + precip_error_bin + wind_error_bin,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

# Regression without precip_error_bin and wind_error_bin in fixed effects
reg_dam_track_only = conleyreg(
  damages/1e6 ~ i(track_error_bin, ref = "(-80,0]") 
  |
    county + state^hurricane + wind_cut + precip_cut + track_cut,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

coef_dam = tidy(reg_dam, conf.int = TRUE) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .after = 4
  ) |>
  filter(!str_detect(term, "track_cut") & !str_detect(term, "wind") & !str_detect(term, "precip")) |>
  mutate(term = sort(unique(df_track$track_error_bin))) |>
  mutate(x = row_number()) |>
  mutate(model = "Track | Wind + Precip")

coef_dam_track_only = tidy(reg_dam_track_only, conf.int = TRUE) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .after = 4
  ) |>
  filter(!str_detect(term, "track_cut") & !str_detect(term, "wind") & !str_detect(term, "precip")) |>
  mutate(term = sort(unique(df_track$track_error_bin))) |>
  mutate(x = row_number()) |>
  mutate(model = "Track Only")

# Original regression with precip_error_bin and wind_error_bin in fixed effects
reg_spend = conleyreg(
  ex_post_spending/1e6 ~ i(track_error_bin, ref = "(-80,0]")
  |
    county + state^hurricane + wind_cut + precip_cut + track_cut + precip_error_bin + wind_error_bin,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

# Regression without precip_error_bin and wind_error_bin in fixed effects
reg_spend_track_only = conleyreg(
  ex_post_spending/1e6 ~ i(track_error_bin, ref = "(-80,0]")
  |
    county + state^hurricane + wind_cut + precip_cut + track_cut,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

coef_spend = tidy(reg_spend, conf.int = TRUE) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .after = 4
  ) |>
  filter(!str_detect(term, "track_cut") & !str_detect(term, "wind") & !str_detect(term, "precip")) |>
  mutate(term = sort(unique(df_track$track_error_bin))) |>
  mutate(x = row_number()) |>
  mutate(model = "Track | Wind + Precip")

coef_spend_track_only = tidy(reg_spend_track_only, conf.int = TRUE) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .after = 4
  ) |>
  filter(!str_detect(term, "track_cut") & !str_detect(term, "wind") & !str_detect(term, "precip")) |>
  mutate(term = sort(unique(df_track$track_error_bin))) |>
  mutate(x = row_number()) |>
  mutate(model = "Track Only")

# Original regression with precip_error_bin and wind_error_bin in fixed effects
reg_dam_share = conleyreg(
  damage_per_gdp ~ i(track_error_bin, ref = "(-80,0]")
  |
    county + state^hurricane + wind_cut + precip_cut + track_cut + precip_error_bin + wind_error_bin,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

# Regression without precip_error_bin and wind_error_bin in fixed effects
reg_dam_share_track_only = conleyreg(
  damage_per_gdp ~ i(track_error_bin, ref = "(-80,0]")
  |
    county + state^hurricane + wind_cut + precip_cut + track_cut,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

coef_dam_share = tidy(reg_dam_share, conf.int = TRUE) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .after = 4
  ) |>
  filter(!str_detect(term, "track_cut") & !str_detect(term, "wind") & !str_detect(term, "precip")) |>
  mutate(term = sort(unique(df_track$track_error_bin))) |>
  mutate(x = row_number()) |>
  mutate(model = "Track | Wind + Precip")

coef_dam_share_track_only = tidy(reg_dam_share_track_only, conf.int = TRUE) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .after = 4
  ) |>
  filter(!str_detect(term, "track_cut") & !str_detect(term, "wind") & !str_detect(term, "precip")) |>
  mutate(term = sort(unique(df_track$track_error_bin))) |>
  mutate(x = row_number()) |>
  mutate(model = "Track Only")

# Original regression with precip_error_bin and wind_error_bin in fixed effects
reg_spend_share = conleyreg(
  ex_post_spending_per_gdp ~ i(track_error_bin, ref = "(-80,0]")
  |
    county + state^hurricane + wind_cut + precip_cut + track_cut + precip_error_bin + wind_error_bin,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

# Regression without precip_error_bin and wind_error_bin in fixed effects
reg_spend_share_track_only = conleyreg(
  ex_post_spending_per_gdp ~ i(track_error_bin, ref = "(-80,0]")
  |
    county + state^hurricane + wind_cut + precip_cut + track_cut,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

coef_spend_share = tidy(reg_spend_share, conf.int = TRUE) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .after = 4
  ) |>
  filter(!str_detect(term, "track_cut") & !str_detect(term, "wind") & !str_detect(term, "precip")) |>
  mutate(term = sort(unique(df_track$track_error_bin))) |>
  mutate(x = row_number()) |>
  mutate(model = "Track | Wind + Precip")

coef_spend_share_track_only = tidy(reg_spend_share_track_only, conf.int = TRUE) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .after = 4
  ) |>
  filter(!str_detect(term, "track_cut") & !str_detect(term, "wind") & !str_detect(term, "precip")) |>
  mutate(term = sort(unique(df_track$track_error_bin))) |>
  mutate(x = row_number()) |>
  mutate(model = "Track Only")

# Original regression with precip_error_bin and wind_error_bin in fixed effects
reg_dam_pc = conleyreg(
  damage_pc ~ i(track_error_bin, ref = "(-80,0]")
  |
    county + state^hurricane + wind_cut + precip_cut + track_cut + precip_error_bin + wind_error_bin,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

# Regression without precip_error_bin and wind_error_bin in fixed effects
reg_dam_pc_track_only = conleyreg(
  damage_pc ~ i(track_error_bin, ref = "(-80,0]")
  |
    county + state^hurricane + wind_cut + precip_cut + track_cut,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

coef_dam_pc = tidy(reg_dam_pc, conf.int = TRUE) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .after = 4
  ) |>
  filter(!str_detect(term, "track_cut") & !str_detect(term, "wind") & !str_detect(term, "precip")) |>
  mutate(term = sort(unique(df_track$track_error_bin))) |>
  mutate(x = row_number()) |>
  mutate(model = "Track | Wind + Precip")

coef_dam_pc_track_only = tidy(reg_dam_pc_track_only, conf.int = TRUE) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .after = 4
  ) |>
  filter(!str_detect(term, "track_cut") & !str_detect(term, "wind") & !str_detect(term, "precip")) |>
  mutate(term = sort(unique(df_track$track_error_bin))) |>
  mutate(x = row_number()) |>
  mutate(model = "Track Only")

# Original regression with precip_error_bin and wind_error_bin in fixed effects
reg_spend_pc = conleyreg(
  ex_post_spending_pc ~ i(track_error_bin, ref = "(-80,0]")
  |
    county + state^hurricane + wind_cut + precip_cut + track_cut + precip_error_bin + wind_error_bin,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

# Regression without precip_error_bin and wind_error_bin in fixed effects
reg_spend_pc_track_only = conleyreg(
  ex_post_spending_pc ~ i(track_error_bin, ref = "(-80,0]")
  |
    county + state^hurricane + wind_cut + precip_cut + track_cut,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

coef_spend_pc = tidy(reg_spend_pc, conf.int = TRUE) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .after = 4
  ) |>
  filter(!str_detect(term, "track_cut") & !str_detect(term, "wind") & !str_detect(term, "precip")) |>
  mutate(term = sort(unique(df_track$track_error_bin))) |>
  mutate(x = row_number()) |>
  mutate(model = "Track | Wind + Precip")

coef_spend_pc_track_only = tidy(reg_spend_pc_track_only, conf.int = TRUE) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .after = 4
  ) |>
  filter(!str_detect(term, "track_cut") & !str_detect(term, "wind") & !str_detect(term, "precip")) |>
  mutate(term = sort(unique(df_track$track_error_bin))) |>
  mutate(x = row_number()) |>
  mutate(model = "Track Only")

# Create position adjustments for jittering (same as in ex ante chunk)
pos_full = position_nudge(x = -0.1)
pos_track = position_nudge(x = 0.1)

log_plot_dam = ggplot() +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(data = coef_dam, aes(x = x, ymin = conf.low, ymax = conf.high, color = model), width = 0, position = pos_full) +
  geom_point(data = coef_dam, aes(x = x, y = estimate, color = model, shape = model), size = 3, position = pos_full) +
  geom_errorbar(data = coef_dam_track_only, aes(x = x, ymin = conf.low, ymax = conf.high, color = model), width = 0, position = pos_track) +
  geom_point(data = coef_dam_track_only, aes(x = x, y = estimate, color = model, shape = model), size = 3, position = pos_track) +
  scale_x_continuous(labels = coef_dam$term, breaks = coef_dam$x) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  scale_color_manual(values = c("Track | Wind + Precip" = "#000000", "Track Only" = "#0072B2")) +
  scale_shape_manual(values = c("Track | Wind + Precip" = 16, "Track Only" = 17)) +
  labs(
    title = "A. Damages (million $)",
    x = "",
    y = ""
  )  +
  annotate("text",
           x = 2, y = -100,
           family = "Lato",
           label = "Overestimate", size = 6, hjust = 0
  ) +
  annotate("text",
           x = 6.3, y = -100,
           family = "Lato",
           label = "Underestimate", size = 6, hjust = 0
  ) +
  geom_segment(aes(x = 1.85, y = -100, xend = 1.25, yend = -100),
               arrow = arrow(length = unit(0.1, "cm")), color = "black"
  ) +
  geom_segment(aes(x = 9.35, y = -100, xend = 9.95, yend = -100),
               arrow = arrow(length = unit(0.1, "cm")), color = "black"
  ) +
  main_theme +
  theme(
    axis.text.x = element_text(size = 14, angle = 45, vjust = 1, hjust = 1),
    legend.text = element_text(size = 12),
    legend.position = c(0.05, 0.95),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = "white", color = NA),
    legend.title = element_blank(),
    legend.margin = margin(1, 1, 1, 1),
    legend.key.size = unit(0.8, "lines")
  ) # + coord_cartesian(ylim = c(-200, 400))

log_plot_spend = ggplot() +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(data = coef_spend, aes(x = x, ymin = conf.low, ymax = conf.high, color = model), width = 0, position = pos_full) +
  geom_point(data = coef_spend, aes(x = x, y = estimate, color = model, shape = model), size = 3, position = pos_full) +
  geom_errorbar(data = coef_spend_track_only, aes(x = x, ymin = conf.low, ymax = conf.high, color = model), width = 0, position = pos_track) +
  geom_point(data = coef_spend_track_only, aes(x = x, y = estimate, color = model, shape = model), size = 3, position = pos_track) +
  scale_x_continuous(labels = coef_spend$term, breaks = coef_spend$x) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  scale_color_manual(values = c("Track | Wind + Precip" = "#000000", "Track Only" = "#0072B2")) +
  scale_shape_manual(values = c("Track | Wind + Precip" = 16, "Track Only" = 17)) +
  labs(
    title = "D. Recovery Expenditures (million $)",
    x = "Track Distance Underestimate (km)",
    y = ""
  ) +
  main_theme +
  theme(
    axis.text.x = element_text(size = 14, angle = 45, vjust = 1, hjust = 1),
    legend.position = "none"
  ) #+ coord_cartesian(ylim = c(-20, 40))

share_plot_dam = ggplot() +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(data = coef_dam_share, aes(x = x, ymin = conf.low * 100, ymax = conf.high * 100, color = model), width = 0, position = pos_full) +
  geom_point(data = coef_dam_share, aes(x = x, y = estimate * 100, color = model, shape = model), size = 3, position = pos_full) +
  geom_errorbar(data = coef_dam_share_track_only, aes(x = x, ymin = conf.low * 100, ymax = conf.high * 100, color = model), width = 0, position = pos_track) +
  geom_point(data = coef_dam_share_track_only, aes(x = x, y = estimate * 100, color = model, shape = model), size = 3, position = pos_track) +
  scale_x_continuous(labels = coef_dam_share$term, breaks = coef_dam_share$x) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  scale_color_manual(values = c("Track | Wind + Precip" = "#000000", "Track Only" = "#0072B2")) +
  scale_shape_manual(values = c("Track | Wind + Precip" = 16, "Track Only" = 17)) +
  labs(
    title = "B. Damages/GDP (%)",
    x = "",
    y = ""
  ) +
  main_theme +
  theme(
    axis.text.x = element_text(size = 14, angle = 45, vjust = 1, hjust = 1),
    legend.position = "none"
  ) #+  coord_cartesian(ylim = c(-20, 40))

share_plot_spend = ggplot() +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(data = coef_spend_share, aes(x = x, ymin = conf.low * 100, ymax = conf.high * 100, color = model), width = 0, position = pos_full) +
  geom_point(data = coef_spend_share, aes(x = x, y = estimate * 100, color = model, shape = model), size = 3, position = pos_full) +
  geom_errorbar(data = coef_spend_share_track_only, aes(x = x, ymin = conf.low * 100, ymax = conf.high * 100, color = model), width = 0, position = pos_track) +
  geom_point(data = coef_spend_share_track_only, aes(x = x, y = estimate * 100, color = model, shape = model), size = 3, position = pos_track) +
  scale_x_continuous(labels = coef_spend_share$term, breaks = coef_spend_share$x) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  scale_color_manual(values = c("Track | Wind + Precip" = "#000000", "Track Only" = "#0072B2")) +
  scale_shape_manual(values = c("Track | Wind + Precip" = 16, "Track Only" = 17)) +
  labs(
    title = "E. Recovery Expenditures/GDP (%)",
    x = "Track Distance Underestimate (km)",
    y = ""
  ) +
  main_theme +
  theme(
    axis.text.x = element_text(size = 14, angle = 45, vjust = 1, hjust = 1),
    legend.position = "none"
  ) #+   coord_cartesian(ylim = c(-1.5, 1.0))

pc_plot_dam = ggplot() +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(data = coef_dam_pc, aes(x = x, ymin = conf.low, ymax = conf.high, color = model), width = 0, position = pos_full) +
  geom_point(data = coef_dam_pc, aes(x = x, y = estimate, color = model, shape = model), size = 3, position = pos_full) +
  geom_errorbar(data = coef_dam_pc_track_only, aes(x = x, ymin = conf.low, ymax = conf.high, color = model), width = 0, position = pos_track) +
  geom_point(data = coef_dam_pc_track_only, aes(x = x, y = estimate, color = model, shape = model), size = 3, position = pos_track) +
  scale_x_continuous(labels = coef_dam_pc$term, breaks = coef_dam_pc$x) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  scale_color_manual(values = c("Track | Wind + Precip" = "#000000", "Track Only" = "#0072B2")) +
  scale_shape_manual(values = c("Track | Wind + Precip" = 16, "Track Only" = 17)) +
  labs(
    title = "C. Damages/Person ($)",
    x = "",
    y = ""
  ) +
  main_theme +
  theme(
    axis.text.x = element_text(size = 14, angle = 45, vjust = 1, hjust = 1),
    legend.position = "none"
  ) #+   coord_cartesian(ylim = c(-5000, 10000))

pc_plot_spend = ggplot() +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(data = coef_spend_pc, aes(x = x, ymin = conf.low, ymax = conf.high, color = model), width = 0, position = pos_full) +
  geom_point(data = coef_spend_pc, aes(x = x, y = estimate, color = model, shape = model), size = 3, position = pos_full) +
  geom_errorbar(data = coef_spend_pc_track_only, aes(x = x, ymin = conf.low, ymax = conf.high, color = model), width = 0, position = pos_track) +
  geom_point(data = coef_spend_pc_track_only, aes(x = x, y = estimate, color = model, shape = model), size = 3, position = pos_track) +
  scale_x_continuous(labels = coef_spend_pc$term, breaks = coef_spend_pc$x) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  scale_color_manual(values = c("Track | Wind + Precip" = "#000000", "Track Only" = "#0072B2")) +
  scale_shape_manual(values = c("Track | Wind + Precip" = 16, "Track Only" = 17)) +
  labs(
    title = "F. Recovery Expenditures/Person ($)",
    x = "Track Distance Underestimate (km)",
    y = ""
  ) +
  main_theme +
  theme(
    axis.text.x = element_text(size = 14, angle = 45, vjust = 1, hjust = 1),
    legend.position = "none"
  ) #+   coord_cartesian(ylim = c(-500, 1000))

plot_out = (log_plot_dam + share_plot_dam + pc_plot_dam) / (log_plot_spend + share_plot_spend + pc_plot_spend)

# Save plot
ggsave(plot = plot_out, "results/FigureC23.png", width = 3*10*2/3, height = 2*8*2/3)
ggsave(plot = plot_out, "results/FigureC23.pdf", device = cairo_pdf, width = 3*10*2/3, height = 2*8*2/3)

# ==============================================================================
# 13b. TRACK ANALYSIS - ROBUSTNESS CUTOFF 
# ==============================================================================

# Function to run regression and extract coefficients for track error bins (ex post analysis)
run_track_error_regression <- function(data, outcome_var) {
  reg <- conleyreg(
    as.formula(paste(outcome_var, "~ i(track_error_bin, ref = \"(-80,0]\") | county + state^hurricane + wind_cut + precip_cut + track_cut + precip_error_bin + wind_error_bin")),
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
    filter(str_detect(term, "track_error_bin")) %>%
    mutate(
      bin_label = str_remove(term, "track_error_bin::")
    )
  # Add the reference bin (-80,0] with coefficient 0
  ref_bin <- tibble(
    term = "track_error_bin::(-80,0]",
    estimate = 0,
    std.error = 0,
    statistic = 0,
    p.value = 1,
    conf.low = 0,
    conf.high = 0,
    bin_label = "(-80,0]"
  )
  coefs <- bind_rows(ref_bin, coefs)
  
  fixed_bin_levels <- c(
    "[-400,-320]", "(-320,-240]", "(-240,-160]", "(-160,-80]", "(-80,0]",
    "(0,80]", "(80,160]", "(160,240]", "(240,320]", "(320,400]"
  )
  
  coefs <- coefs %>%
    mutate(
      clean_bin = str_extract(term, "\\[?-?\\d+,-?\\d+\\]?|\\(-?\\d+,-?\\d+\\]"),
      
      clean_bin = ifelse(is.na(clean_bin), "(-80,0]", clean_bin),
      
      clean_bin = factor(clean_bin, levels = fixed_bin_levels, ordered = TRUE)
    ) %>%
    arrange(clean_bin)
  return(coefs)
}

# Define ex post outcomes
ex_post_outcomes <- c("damages/1e6", "ex_post_spending/1e6", "damage_per_gdp", "ex_post_spending_per_gdp", "damage_pc", "ex_post_spending_pc")
ex_post_outcome_names <- c("Damages (million $)", "Recovery Expenditures (million $)", "Damages/GDP (%)", "Recovery Expenditures/GDP (%)", "Damages/Person ($)", "Recovery Expenditures/Person ($)")

# Run ex post regressions for each cutoff and outcome
ex_post_all_results <- list()
for (i in seq_along(ex_post_outcomes)) {
  outcome <- ex_post_outcomes[i]
  outcome_name <- ex_post_outcome_names[i]
  print(sprintf("\nRunning ex post regressions for: %s", outcome_name))
  cutoff_results <- list()
  for (cutoff in cutoffs) {
    cutoff_str <- as.character(cutoff)
    print(sprintf("  Cutoff %s km...", cutoff_str))
    # Use the loader function for each regression
    data <- load_df_track(cutoff)
    coefs <- run_track_error_regression(data, outcome)
    coefs$cutoff <- cutoff_str
    coefs$outcome <- outcome_name
    cutoff_results[[cutoff_str]] <- coefs
  }
  ex_post_all_results[[outcome_name]] <- bind_rows(cutoff_results)
}

# Create individual plots for each outcome
dat1 <- ex_post_all_results[["Damages (million $)"]]
dat2 <- ex_post_all_results[["Damages/GDP (%)"]]
dat3 <- ex_post_all_results[["Damages/Person ($)"]]
dat4 <- ex_post_all_results[["Recovery Expenditures (million $)"]]
dat5 <- ex_post_all_results[["Recovery Expenditures/GDP (%)"]]
dat6 <- ex_post_all_results[["Recovery Expenditures/Person ($)"]]

# Individual plots using clean_bin as x
p1 <- ggplot(dat1, aes(x = clean_bin, y = estimate, color = cutoff, group = cutoff, shape = cutoff)) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), position = position_dodge(width = 0.5), width = 0) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  scale_color_manual(values = fe_colors, name = "Distance Cutoff (km):") +
  scale_shape_manual(values = fe_shapes, name = "Distance Cutoff (km):") +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  main_theme +
  labs(
    title = "A. Damages (million $)",
    x = "Track Distance Underestimate (km)",
    y = "",
    color = "Distance Cutoff (km)",
    shape = "Distance Cutoff (km)"
  ) +
  annotate("text",
           x = 2, y = -100,
           family = "Lato",
           label = "Overestimate", size = 6, hjust = 0
  ) +
  annotate("text",
           x = 6.3, y = -100,
           family = "Lato",
           label = "Underestimate", size = 6, hjust = 0
  ) +
  geom_segment(aes(x = 1.85, y = -100, xend = 1.25, yend = -100),
               arrow = arrow(length = unit(0.1, "cm")), color = "black"
  ) +
  geom_segment(aes(x = 9.35, y = -100, xend = 9.95, yend = -100),
               arrow = arrow(length = unit(0.1, "cm")), color = "black"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )

p2 <- ggplot(dat2, aes(x = clean_bin, y = estimate * 100, color = cutoff, group = cutoff, shape = cutoff)) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(aes(ymin = conf.low * 100, ymax = conf.high * 100), position = position_dodge(width = 0.5), width = 0) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  scale_color_manual(values = fe_colors, name = "Distance Cutoff (km):") +
  scale_shape_manual(values = fe_shapes, name = "Distance Cutoff (km):") +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  main_theme +
  labs(
    title = "B. Damages/GDP (%)",
    x = "Track Distance Underestimate (km)",
    y = "",
    color = "Distance Cutoff (km)",
    shape = "Distance Cutoff (km)"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )

p3 <- ggplot(dat3, aes(x = clean_bin, y = estimate, color = cutoff, group = cutoff, shape = cutoff)) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), position = position_dodge(width = 0.5), width = 0) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  scale_color_manual(values = fe_colors, name = "Distance Cutoff (km):") +
  scale_shape_manual(values = fe_shapes, name = "Distance Cutoff (km):") +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  main_theme +
  labs(
    title = "C. Damages/Person ($)",
    x = "Track Distance Underestimate (km)",
    y = "",
    color = "Distance Cutoff (km)",
    shape = "Distance Cutoff (km)"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )

p4 <- ggplot(dat4, aes(x = clean_bin, y = estimate, color = cutoff, group = cutoff, shape = cutoff)) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), position = position_dodge(width = 0.5), width = 0) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  scale_color_manual(values = fe_colors, name = "Distance Cutoff (km):") +
  scale_shape_manual(values = fe_shapes, name = "Distance Cutoff (km):") +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  main_theme +
  labs(
    title = "D. Recovery Expenditures (million $)",
    x = "Track Distance Underestimate (km)",
    y = "",
    color = "Distance Cutoff (km)",
    shape = "Distance Cutoff (km)"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )

p5 <- ggplot(dat5, aes(x = clean_bin, y = estimate * 100, color = cutoff, group = cutoff, shape = cutoff)) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(aes(ymin = conf.low * 100, ymax = conf.high * 100), position = position_dodge(width = 0.5), width = 0) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  scale_color_manual(values = fe_colors, name = "Distance Cutoff (km):") +
  scale_shape_manual(values = fe_shapes, name = "Distance Cutoff (km):") +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  main_theme +
  labs(
    title = "E. Recovery Expenditures/GDP (%)",
    x = "Track Distance Underestimate (km)",
    y = "",
    color = "Distance Cutoff (km)",
    shape = "Distance Cutoff (km)"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )

p6 <- ggplot(dat6, aes(x = clean_bin, y = estimate, color = cutoff, group = cutoff, shape = cutoff)) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), position = position_dodge(width = 0.5), width = 0) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  scale_color_manual(values = fe_colors, name = "Distance Cutoff (km):") +
  scale_shape_manual(values = fe_shapes, name = "Distance Cutoff (km):") +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  main_theme +
  labs(
    title = "F. Recovery Expenditures/Person ($)",
    x = "Track Distance Underestimate (km)",
    y = "",
    color = "Distance Cutoff (km)",
    shape = "Distance Cutoff (km)"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )

# Combine plots with patchwork and a single, centered legend with extra space
ex_post_plot_out <- (p1 + p2 + p3) / (p4 + p5 + p6) +
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
ggsave(plot = ex_post_plot_out, filename = "results/FigureC24.png", width = 3 * 10 * 2 / 3, height = 2 * 8 * 2 / 3 + 2)
ggsave(plot = ex_post_plot_out, filename = "results/FigureC24.pdf", device = cairo_pdf, width = 3 * 10 * 2 / 3, height = 2 * 8 * 2 / 3 + 2)

# ==============================================================================
# 14. WIND ERROR ANALYSIS CONDITIONAL ON TRACK CONTROLS
# ==============================================================================

df_track = load_df_track()

# Original regression with track_cut and track_error_bin
reg_dam = conleyreg(
  damages/1e6 ~ i(wind_error_bin, ref = "(-2,0]") 
  |
    county + state^hurricane + wind_cut + precip_cut + precip_error_bin + track_cut + track_error_bin,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

# Regression without track_cut and track_error_bin
reg_dam_wind_only = conleyreg(
  damages/1e6 ~ i(wind_error_bin, ref = "(-2,0]") 
  |
    county + state^hurricane + wind_cut + precip_cut + precip_error_bin,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

coef_dam = tidy(reg_dam, conf.int = TRUE) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .after = 4
  ) |>
  filter(!str_detect(term, "wind_cut") & !str_detect(term, "precip") & !str_detect(term, "track")) |>
  mutate(term = sort(unique(df_track$wind_error_bin))) |>
  mutate(x = row_number()) |>
  mutate(model = "Wind | Track + Precip")

coef_dam_wind_only = tidy(reg_dam_wind_only, conf.int = TRUE) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .after = 4
  ) |>
  filter(!str_detect(term, "wind_cut") & !str_detect(term, "precip") & !str_detect(term, "track")) |>
  mutate(term = sort(unique(df_track$wind_error_bin))) |>
  mutate(x = row_number()) |>
  mutate(model = "Wind | Precip")

# Original regression with track_cut and track_error_bin
reg_spend = conleyreg(
  ex_post_spending/1e6 ~ i(wind_error_bin, ref = "(-2,0]")
  |
    county + state^hurricane + wind_cut + precip_cut + precip_error_bin + track_cut + track_error_bin,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

# Regression without track_cut and track_error_bin
reg_spend_wind_only = conleyreg(
  ex_post_spending/1e6 ~ i(wind_error_bin, ref = "(-2,0]")
  |
    county + state^hurricane + wind_cut + precip_cut + precip_error_bin,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

coef_spend = tidy(reg_spend, conf.int = TRUE) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .after = 4
  ) |>
  filter(!str_detect(term, "wind_cut") & !str_detect(term, "precip") & !str_detect(term, "track")) |>
  mutate(term = sort(unique(df_track$wind_error_bin))) |>
  mutate(x = row_number()) |>
  mutate(model = "Wind | Track + Precip")

coef_spend_wind_only = tidy(reg_spend_wind_only, conf.int = TRUE) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .after = 4
  ) |>
  filter(!str_detect(term, "wind_cut") & !str_detect(term, "precip") & !str_detect(term, "track")) |>
  mutate(term = sort(unique(df_track$wind_error_bin))) |>
  mutate(x = row_number()) |>
  mutate(model = "Wind | Precip")

# Original regression with track_cut and track_error_bin
reg_dam_share = conleyreg(
  damage_per_gdp ~ i(wind_error_bin, ref = "(-2,0]")
  |
    county + state^hurricane + wind_cut + precip_cut + precip_error_bin + track_cut + track_error_bin,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

# Regression without track_cut and track_error_bin
reg_dam_share_wind_only = conleyreg(
  damage_per_gdp ~ i(wind_error_bin, ref = "(-2,0]")
  |
    county + state^hurricane + wind_cut + precip_cut + precip_error_bin,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

coef_dam_share = tidy(reg_dam_share, conf.int = TRUE) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .after = 4
  ) |>
  filter(!str_detect(term, "wind_cut") & !str_detect(term, "precip") & !str_detect(term, "track")) |>
  mutate(term = sort(unique(df_track$wind_error_bin))) |>
  mutate(x = row_number()) |>
  mutate(model = "Wind | Track + Precip")

coef_dam_share_wind_only = tidy(reg_dam_share_wind_only, conf.int = TRUE) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .after = 4
  ) |>
  filter(!str_detect(term, "wind_cut") & !str_detect(term, "precip") & !str_detect(term, "track")) |>
  mutate(term = sort(unique(df_track$wind_error_bin))) |>
  mutate(x = row_number()) |>
  mutate(model = "Wind | Precip")

# Original regression with track_cut and track_error_bin
reg_spend_share = conleyreg(
  ex_post_spending_per_gdp ~ i(wind_error_bin, ref = "(-2,0]")
  |
    county + state^hurricane + wind_cut + precip_cut + precip_error_bin + track_cut + track_error_bin,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

# Regression without track_cut and track_error_bin
reg_spend_share_wind_only = conleyreg(
  ex_post_spending_per_gdp ~ i(wind_error_bin, ref = "(-2,0]")
  |
    county + state^hurricane + wind_cut + precip_cut + precip_error_bin,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

coef_spend_share = tidy(reg_spend_share, conf.int = TRUE) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .after = 4
  ) |>
  filter(!str_detect(term, "wind_cut") & !str_detect(term, "precip") & !str_detect(term, "track")) |>
  mutate(term = sort(unique(df_track$wind_error_bin))) |>
  mutate(x = row_number()) |>
  mutate(model = "Wind | Track + Precip")

coef_spend_share_wind_only = tidy(reg_spend_share_wind_only, conf.int = TRUE) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .after = 4
  ) |>
  filter(!str_detect(term, "wind_cut") & !str_detect(term, "precip") & !str_detect(term, "track")) |>
  mutate(term = sort(unique(df_track$wind_error_bin))) |>
  mutate(x = row_number()) |>
  mutate(model = "Wind | Precip")

# Original regression with track_cut and track_error_bin
reg_dam_pc = conleyreg(
  damage_pc ~ i(wind_error_bin, ref = "(-2,0]")
  |
    county + state^hurricane + wind_cut + precip_cut + precip_error_bin + track_cut + track_error_bin,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

# Regression without track_cut and track_error_bin
reg_dam_pc_wind_only = conleyreg(
  damage_pc ~ i(wind_error_bin, ref = "(-2,0]")
  |
    county + state^hurricane + wind_cut + precip_cut + precip_error_bin,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

coef_dam_pc = tidy(reg_dam_pc, conf.int = TRUE) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .after = 4
  ) |>
  filter(!str_detect(term, "wind_cut") & !str_detect(term, "precip") & !str_detect(term, "track")) |>
  mutate(term = sort(unique(df_track$wind_error_bin))) |>
  mutate(x = row_number()) |>
  mutate(model = "Wind | Track + Precip")

coef_dam_pc_wind_only = tidy(reg_dam_pc_wind_only, conf.int = TRUE) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .after = 4
  ) |>
  filter(!str_detect(term, "wind_cut") & !str_detect(term, "precip") & !str_detect(term, "track")) |>
  mutate(term = sort(unique(df_track$wind_error_bin))) |>
  mutate(x = row_number()) |>
  mutate(model = "Wind | Precip")

# Original regression with track_cut and track_error_bin
reg_spend_pc = conleyreg(
  ex_post_spending_pc ~ i(wind_error_bin, ref = "(-2,0]")
  |
    county + state^hurricane + wind_cut + precip_cut + precip_error_bin + track_cut + track_error_bin,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

# Regression without track_cut and track_error_bin
reg_spend_pc_wind_only = conleyreg(
  ex_post_spending_pc ~ i(wind_error_bin, ref = "(-2,0]")
  |
    county + state^hurricane + wind_cut + precip_cut + precip_error_bin,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

coef_spend_pc = tidy(reg_spend_pc, conf.int = TRUE) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .after = 4
  ) |>
  filter(!str_detect(term, "wind_cut") & !str_detect(term, "precip") & !str_detect(term, "track")) |>
  mutate(term = sort(unique(df_track$wind_error_bin))) |>
  mutate(x = row_number()) |>
  mutate(model = "Wind | Track + Precip")

coef_spend_pc_wind_only = tidy(reg_spend_pc_wind_only, conf.int = TRUE) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .after = 4
  ) |>
  filter(!str_detect(term, "wind_cut") & !str_detect(term, "precip") & !str_detect(term, "track")) |>
  mutate(term = sort(unique(df_track$wind_error_bin))) |>
  mutate(x = row_number()) |>
  mutate(model = "Wind | Precip")

# Create position adjustments for jittering
pos_full = position_nudge(x = -0.1)
pos_wind = position_nudge(x = 0.1)

log_plot_dam = ggplot() +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(data = coef_dam, aes(x = x, ymin = conf.low, ymax = conf.high, color = model), width = 0, position = pos_full) +
  geom_point(data = coef_dam, aes(x = x, y = estimate, color = model, shape = model), size = 3, position = pos_full) +
  geom_errorbar(data = coef_dam_wind_only, aes(x = x, ymin = conf.low, ymax = conf.high, color = model), width = 0, position = pos_wind) +
  geom_point(data = coef_dam_wind_only, aes(x = x, y = estimate, color = model, shape = model), size = 3, position = pos_wind) +
  scale_x_continuous(labels = coef_dam$term, breaks = coef_dam$x) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  scale_color_manual(values = c("Wind | Track + Precip" = "#000000", "Wind | Precip" = "#0072B2")) +
  scale_shape_manual(values = c("Wind | Track + Precip" = 16, "Wind | Precip" = 17)) +
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
    axis.text.x = element_text(size = 14, angle = 45, vjust = 1, hjust = 1),
    legend.position = c(0.05, 0.95),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = "white", color = NA),
    legend.title = element_blank(),
    legend.margin = margin(1, 1, 1, 1),
    legend.key.size = unit(0.8, "lines")
  ) + 
  coord_cartesian(ylim = c(-200, 400))

log_plot_spend = ggplot() +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(data = coef_spend, aes(x = x, ymin = conf.low, ymax = conf.high, color = model), width = 0, position = pos_full) +
  geom_point(data = coef_spend, aes(x = x, y = estimate, color = model, shape = model), size = 3, position = pos_full) +
  geom_errorbar(data = coef_spend_wind_only, aes(x = x, ymin = conf.low, ymax = conf.high, color = model), width = 0, position = pos_wind) +
  geom_point(data = coef_spend_wind_only, aes(x = x, y = estimate, color = model, shape = model), size = 3, position = pos_wind) +
  scale_x_continuous(labels = coef_spend$term, breaks = coef_spend$x) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  scale_color_manual(values = c("Wind | Track + Precip" = "#000000", "Wind | Precip" = "#0072B2")) +
  scale_shape_manual(values = c("Wind | Track + Precip" = 16, "Wind | Precip" = 17)) +
  labs(
    title = "D. Recovery Expenditures (million $)",
    x = "Wind Speed Underestimate (m/s)",
    y = ""
  ) +
  main_theme +
  theme(
    axis.text.x = element_text(size = 14, angle = 45, vjust = 1, hjust = 1),
    legend.position = "none"
  ) + 
  coord_cartesian(ylim = c(-20, 40))

share_plot_dam = ggplot() +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(data = coef_dam_share, aes(x = x, ymin = conf.low * 100, ymax = conf.high * 100, color = model), width = 0, position = pos_full) +
  geom_point(data = coef_dam_share, aes(x = x, y = estimate * 100, color = model, shape = model), size = 3, position = pos_full) +
  geom_errorbar(data = coef_dam_share_wind_only, aes(x = x, ymin = conf.low * 100, ymax = conf.high * 100, color = model), width = 0, position = pos_wind) +
  geom_point(data = coef_dam_share_wind_only, aes(x = x, y = estimate * 100, color = model, shape = model), size = 3, position = pos_wind) +
  scale_x_continuous(labels = coef_dam_share$term, breaks = coef_dam_share$x) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  scale_color_manual(values = c("Wind | Track + Precip" = "#000000", "Wind | Precip" = "#0072B2")) +
  scale_shape_manual(values = c("Wind | Track + Precip" = 16, "Wind | Precip" = 17)) +
  labs(
    title = "B. Damages/GDP (%)",
    x = "",
    y = ""
  ) +
  main_theme +
  theme(
    axis.text.x = element_text(size = 14, angle = 45, vjust = 1, hjust = 1),
    legend.position = "none"
  ) + 
  coord_cartesian(ylim = c(-20, 40))

share_plot_spend = ggplot() +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(data = coef_spend_share, aes(x = x, ymin = conf.low * 100, ymax = conf.high * 100, color = model), width = 0, position = pos_full) +
  geom_point(data = coef_spend_share, aes(x = x, y = estimate * 100, color = model, shape = model), size = 3, position = pos_full) +
  geom_errorbar(data = coef_spend_share_wind_only, aes(x = x, ymin = conf.low * 100, ymax = conf.high * 100, color = model), width = 0, position = pos_wind) +
  geom_point(data = coef_spend_share_wind_only, aes(x = x, y = estimate * 100, color = model, shape = model), size = 3, position = pos_wind) +
  scale_x_continuous(labels = coef_spend_share$term, breaks = coef_spend_share$x) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  scale_color_manual(values = c("Wind | Track + Precip" = "#000000", "Wind | Precip" = "#0072B2")) +
  scale_shape_manual(values = c("Wind | Track + Precip" = 16, "Wind | Precip" = 17)) +
  labs(
    title = "E. Recovery Expenditures/GDP (%)",
    x = "Wind Speed Underestimate (m/s)",
    y = ""
  ) +
  main_theme +
  theme(
    axis.text.x = element_text(size = 14, angle = 45, vjust = 1, hjust = 1),
    legend.position = "none"
  ) + 
  coord_cartesian(ylim = c(-1.5, 1.0))

pc_plot_dam = ggplot() +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(data = coef_dam_pc, aes(x = x, ymin = conf.low, ymax = conf.high, color = model), width = 0, position = pos_full) +
  geom_point(data = coef_dam_pc, aes(x = x, y = estimate, color = model, shape = model), size = 3, position = pos_full) +
  geom_errorbar(data = coef_dam_pc_wind_only, aes(x = x, ymin = conf.low, ymax = conf.high, color = model), width = 0, position = pos_wind) +
  geom_point(data = coef_dam_pc_wind_only, aes(x = x, y = estimate, color = model, shape = model), size = 3, position = pos_wind) +
  scale_x_continuous(labels = coef_dam_pc$term, breaks = coef_dam_pc$x) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  scale_color_manual(values = c("Wind | Track + Precip" = "#000000", "Wind | Precip" = "#0072B2")) +
  scale_shape_manual(values = c("Wind | Track + Precip" = 16, "Wind | Precip" = 17)) +
  labs(
    title = "C. Damages/Person ($)",
    x = "",
    y = ""
  ) +
  main_theme +
  theme(
    axis.text.x = element_text(size = 14, angle = 45, vjust = 1, hjust = 1),
    legend.position = "none"
  ) + 
  coord_cartesian(ylim = c(-5000, 10000))

pc_plot_spend = ggplot() +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(data = coef_spend_pc, aes(x = x, ymin = conf.low, ymax = conf.high, color = model), width = 0, position = pos_full) +
  geom_point(data = coef_spend_pc, aes(x = x, y = estimate, color = model, shape = model), size = 3, position = pos_full) +
  geom_errorbar(data = coef_spend_pc_wind_only, aes(x = x, ymin = conf.low, ymax = conf.high, color = model), width = 0, position = pos_wind) +
  geom_point(data = coef_spend_pc_wind_only, aes(x = x, y = estimate, color = model, shape = model), size = 3, position = pos_wind) +
  scale_x_continuous(labels = coef_spend_pc$term, breaks = coef_spend_pc$x) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  scale_color_manual(values = c("Wind | Track + Precip" = "#000000", "Wind | Precip" = "#0072B2")) +
  scale_shape_manual(values = c("Wind | Track + Precip" = 16, "Wind | Precip" = 17)) +
  labs(
    title = "F. Recovery Expenditures/Person ($)",
    x = "Wind Speed Underestimate (m/s)",
    y = ""
  ) +
  main_theme +
  theme(
    axis.text.x = element_text(size = 14, angle = 45, vjust = 1, hjust = 1),
    legend.position = "none"
  ) + 
  coord_cartesian(ylim = c(-500, 1000))

plot_out = (log_plot_dam + share_plot_dam + pc_plot_dam) / (log_plot_spend + share_plot_spend + pc_plot_spend)

# Save plot
ggsave(plot = plot_out, "results/FigureC25.png", width = 3*10*2/3, height = 2*8*2/3)
ggsave(plot = plot_out, "results/FigureC25.pdf", device = cairo_pdf, width = 3*10*2/3, height = 2*8*2/3)

# ==============================================================================
# 15. PROTECTIVE EXPENDITURE AS A FUNCTION OF EX POST ERROR
# ==============================================================================


reg_protect = conleyreg(
  ex_ante_spending/1e6 ~ i(wind_error_bin, ref = "(-2,0]")
  |
    county + state^hurricane + forecast_cut + forecast_precip_cut + precip_error_bin,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

coef_protect = tidy(reg_protect, conf.int = TRUE) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .after = 4
  ) |>
  filter(!str_detect(term, "wind_cut") & !str_detect(term, "precip")) |>
  mutate(term = sort(unique(df$wind_error_bin))) |>
  mutate(x = row_number()) |>
  mutate(type = "Protective Expenditures")

reg_protect_share = conleyreg(
  ex_ante_spending_per_gdp ~ i(wind_error_bin, ref = "(-2,0]")
  |
    county + state^hurricane + forecast_cut + forecast_precip_cut + precip_error_bin,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

coef_protect_share = tidy(reg_protect_share, conf.int = TRUE) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .after = 4
  ) |>
  filter(!str_detect(term, "wind_cut") & !str_detect(term, "precip")) |>
  mutate(term = sort(unique(df$wind_error_bin))) |>
  mutate(x = row_number()) |>
  mutate(type = "Protective Expenditures")

reg_protect_pc = conleyreg(
  ex_ante_spending_pc ~ i(wind_error_bin, ref = "(-2,0]")
  |
    county + state^hurricane + forecast_cut + forecast_precip_cut + precip_error_bin,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

coef_protect_pc = tidy(reg_protect_pc, conf.int = TRUE) |>
  add_row(
    term = "asdf", estimate = 0, conf.high = 0, conf.low = 0, .after = 4
  ) |>
  filter(!str_detect(term, "wind_cut") & !str_detect(term, "precip")) |>
  mutate(term = sort(unique(df$wind_error_bin))) |>
  mutate(x = row_number()) |>
  mutate(type = "Protective Expenditures")

log_plot_protect = 
  ggplot(coef_protect, aes(x = x, y = estimate)) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(aes(x = x, ymin = conf.low, ymax = conf.high), width = 0,
                position = position_dodge(width = 0.25)) +
  geom_point(size = 3,
             position = position_dodge(width = 0.25)) +
  main_theme +
  scale_x_continuous(labels = coef_protect$term, breaks = coef_protect$x) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  labs(
    title = "A. Protective Expenditures (million $)",
    x = "",
    y = ""
  ) +
  annotate("text",
           x = 2, y = 10,
           family = "Lato",
           label = "Overestimate", size = 6, hjust = 0
  ) +
  annotate("text",
           x = 6.3, y = 10,
           family = "Lato",
           label = "Underestimate", size = 6, hjust = 0
  ) +
  annotate("segment", x = 1.85, y = 10, xend = 1.25, yend = 10,
           arrow = arrow(length = unit(0.1, "cm")), color = "black") +
  annotate("segment", x = 9.35, y = 10, xend = 9.95, yend = 10,
           arrow = arrow(length = unit(0.1, "cm")), color = "black") +
  theme(
    axis.text.x = element_text(size = 14, angle = 45, vjust = 1, hjust = 1)
  ) + 
  coord_cartesian(ylim = c(0, 50))

share_plot_protect = 
  ggplot(coef_protect_share, aes(x = x, y = estimate * 100)) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(aes(x = x, ymin = conf.low * 100, ymax = conf.high * 100), width = 0,
                position = position_dodge(width = 0.25)) +
  geom_point(size = 3,
             position = position_dodge(width = 0.25)) +
  main_theme +
  scale_x_continuous(labels = coef_protect_share$term, breaks = coef_protect_share$x) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  labs(
    title = "B. Protective Expenditures/GDP (%)",
    x = "",
    y = ""
  ) +
  main_theme +
  theme(
    axis.text.x = element_text(size = 14, angle = 45, vjust = 1, hjust = 1)
  ) + 
  coord_cartesian(ylim = c(0, 1.5))

pc_plot_protect = 
  ggplot(coef_protect_pc, aes(x = x, y = estimate )) +
  geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
  geom_errorbar(aes(x = x, ymin = conf.low , ymax = conf.high), width = 0,
                position = position_dodge(width = 0.25)) +
  geom_point(size = 3,
             position = position_dodge(width = 0.25)) +
  main_theme +
  scale_x_continuous(labels = coef_protect_pc$term, breaks = coef_protect_pc$x) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  labs(
    title = "C. Protective Expenditures/Person ($)",
    x = "",
    y = ""
  ) +
  main_theme +
  theme(
    axis.text.x = element_text(size = 14, angle = 45, vjust = 1, hjust = 1)
  ) + 
  coord_cartesian(ylim = c(0, 500))

plot_out = (log_plot_protect + share_plot_protect + pc_plot_protect)

ggsave(plot = plot_out, "results/FigureC26.png", width = 3 * 10 * 2 / 3, height = 1 * 8 * 2 / 3)
ggsave(plot = plot_out, "results/FigureC26.pdf", device = cairo_pdf, width = 3 * 10 * 2 / 3, height = 1 * 8 * 2 / 3)
