# ======================
# 0. Table 1
# ======================

df = readRDS("data/clean_denial/final_dataset_denial.rds")

data_summary = df |>
  filter(wind_obs > 0 & precip_obs > 0) |> # only keep affected counties
  group_by(hurricane, county, augmented_obs) |>
  mutate(
    wind_obs = max(wind_obs),
    precip_obs = max(precip_obs)
  ) |>
  ungroup() |>
  arrange(hurricane, year, county, augmented_obs) |>
  distinct(
    year, hurricane, state, county, augmented_obs,
    wind_obs, wind_fc, wind_error,
    precip_obs, precip_fc, precip_error,
    damages, deaths, ex_post_spending, ex_ante_spending
  ) |>
  group_by(hurricane, year, augmented_obs) |>
  summarise(
    max_wind_speed = paste0(round(mean(wind_obs), 2), " (", round(sd(wind_obs), 2), ")"),
    max_wind_error = paste0(round(mean(abs(wind_error)), 2), " (", round(sd(abs(wind_error)), 2), ")"),
    max_prec = paste0(round(mean(precip_obs), 2), " (", round(sd(precip_obs), 2), ")"),
    max_prec_error = paste0(round(mean(abs(precip_error)), 2), " (", round(sd(abs(precip_error)), 2), ")"),
    sum_damage = sum(damages) / 10^9,
    ex_ante_spending = sum(ex_ante_spending) / 10^9,
    ex_post_spending = sum(ex_post_spending) / 10^9
  ) |>
  arrange(year) |>
  mutate(hurricane = str_to_title(hurricane),
        augmented_obs = ifelse(augmented_obs == 1, "Yes", "No")) |> # Recast augmented_obs as strings "Yes" and "No"
  rename(
    Hurricane = hurricane,
    Year = year,
    `Augmented Obs?` = augmented_obs,
    `Wind Speed` = max_wind_speed,
    `Wind Speed Error` = max_wind_error,
    `Precipitation` = max_prec,
    `Precipitation Error` = max_prec_error,
    `Total Damage` = sum_damage,
    `Protective Exp.` = ex_ante_spending,
    `Recovery Exp.` = ex_post_spending
  )

# The summary statistics table is broken down by augmented_obs for each hurricane

summary_table = kbl(data_summary,
  format = "latex",
  caption = "Summary Statistics by Hurricane.",
  label = "summary_dmg",
  booktabs = T,
  digits = 2,
  escape = FALSE,
  linesep = c(""),
  col.names = names(data_summary),
  align = c("l", rep("c", dim(data_summary)[2] - 1))
) |>
  footnote(
    general = "Wind speed, precipitation, and their associated errors are averaged across counties that experienced positive wind speeds only. Standard deviations for these variables are reported in parentheses. Damages and expenditures are summed across counties for each storm. ``Precip'' is short for precipitation, and ``Exp'' is short for Expenditure. Wind speed is the maximum sustained wind speed in meters per second (m/s).",
    threeparttable = T,
    fixed_small_size = T
  )

fileConn = file("output/augmented_obs/table_1.tex")
writeLines(summary_table, fileConn)
close(fileConn)

add_adjust_box("output/augmented_obs/table_1.tex")

# ======================
# 2. Figure 2
# ======================

df = readRDS("data/clean_denial/final_dataset_denial.rds")


print("================")
print("Making Figure 2.")
print("================")

# Function to create plots
run_fig2_reg <- function(formula, this_data) {
  # Run regression for augmented_obs = 1
  reg_all = conleyreg(
    formula,
    data = this_data |> filter(augmented_obs == 1),
    dist_cutoff = dist,
    lag_cutoff = Inf,
    unit = "county",
    time = "year",
    lat = "lat",
    lon = "lon",
    batch_ram_opt = "none"
  )
  
  # Run regression for augmented_obs = 0
  reg_no = conleyreg(
    formula,
    data = this_data |> filter(augmented_obs == 0),
    dist_cutoff = dist,
    lag_cutoff = Inf,
    unit = "county",
    time = "year",
    lat = "lat",
    lon = "lon",
    batch_ram_opt = "none"
  )
  
  # Process regression results for augmented_obs = 1
  coef_all = tidy(reg_all, conf.int = TRUE) |>
    filter(str_detect(term, "forecast_cut")) |>
    mutate(term = str_extract(term, "(?<=forecast_cut::).*")) |>
    mutate(term = factor(term, levels = levels(this_data$forecast_cut))) |>
    mutate(data_type = "Yes")
    
  # Process regression results for augmented_obs = 0
  coef_no = tidy(reg_no, conf.int = TRUE) |>
    filter(str_detect(term, "forecast_cut")) |>
    mutate(term = str_extract(term, "(?<=forecast_cut::).*")) |>
    mutate(term = factor(term, levels = levels(this_data$forecast_cut))) |>
    mutate(data_type = "No")
  
  # Combine results
  coef = rbind(coef_all, coef_no)

  return(coef)
}

create_fig2_panel_plot <- function(coef, title, y_label = "", ylim_range = NULL, bar_disp = 0.1, scaling = 1.0) {
  # Create x points and displace "No"s by -bar_disp and "Yes"s by bar_disp
  coef = coef |>
    mutate(x = as.integer(term)) |>
    mutate(x = ifelse(data_type == "Yes", x + bar_disp, x - bar_disp))

  # Extract X labels fomr only "Yes"
  x_labels = coef |> filter(data_type == "Yes") |> pull(term)
  x_breaks = coef |> filter(data_type == "Yes") |> pull(x) - bar_disp

  # If null, set ylim_range to range of coef
  #if (is.null(ylim_range)) {
  #  ylim_range = c(min(coef$conf.low)*1.01, max(coef$conf.high)*1.01)
  #}

  # Create plot
  plot = ggplot(coef, aes(x, estimate*scaling)) +
    geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
    geom_errorbar(aes(x = x, ymin = conf.low*scaling, ymax = conf.high*scaling, color = data_type), width = 0) +
    geom_point(aes(color = data_type), size = 3) +
    scale_x_continuous(labels = x_labels, breaks = x_breaks) +
    scale_y_continuous(breaks = scales::pretty_breaks()) +
    scale_color_manual(values = c("Yes" = "blue", "No" = "red"), name = "Aug. Obs?") +
    main_theme +
    labs(title = title, x = "Wind Speed Forecast (m/s)", y = y_label) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          legend.position = "right")
    #coord_cartesian(ylim = ylim_range)
  
  return(plot)
}

# Create plots

# Figure 2A - Protective Spending
fig2_a_coef = run_fig2_reg(formula(ex_ante_spending/1e6 ~ i(forecast_cut) + i(forecast_precip_cut) | county + state^hurricane), df)
fig2_a_plot = create_fig2_panel_plot(coef = fig2_a_coef, "A. Protective Spending (million $)")
fig2_a_plot

# Figure 2B - Protective Spending/GDP
fig2_b_coef = run_fig2_reg(formula(ex_ante_spending_per_gdp ~ i(forecast_cut) + i(forecast_precip_cut) | county + state^hurricane), df)
fig2_b_plot = create_fig2_panel_plot(coef = fig2_b_coef, "B. Protective Spending/GDP (%)", "", scaling = 100)
fig2_b_plot

# Figure 2C - Protective Spending/Person
fig2_c_coef = run_fig2_reg(formula(ex_ante_spending_pc ~ i(forecast_cut) + i(forecast_precip_cut) | county + state^hurricane), df)
fig2_c_plot = create_fig2_panel_plot(coef = fig2_c_coef, "C. Protective Spending/Person ($)", "")
fig2_c_plot


# Combine plots and add color legend at the bottom
fig2_panel = plot_grid(
  fig2_a_plot, fig2_b_plot, fig2_c_plot,
  ncol = 3,
  rel_widths = c(1, 1, 1.2)
)

ggsave(plot = fig2_panel, filename = "output/augmented_obs/figure_2.png", width = 3 * 10 * 2 / 3, height = 1 * 8 * 2 / 3)

# Stack coefs for each panel in a single tibble and export coefs table in CSV
fig2_coefs <- bind_rows(
  fig2_a_coef %>% mutate(reg = "Spending"),
  fig2_b_coef %>% mutate(reg = "Spending/GDP"),
  fig2_c_coef %>% mutate(reg = "Spending/Person"))

write_csv(fig2_coefs, "output/augmented_obs/figure_2_coefs.csv")




# # ======================
# # 3. Figure 3
# # ======================

print("================")
print("Making Figure 3.")
print("================")


run_fig3_reg <- function(this_formula, this_data) {
  reg_no <- conleyreg(
    this_formula,
    data = this_data |> filter(augmented_obs == 0),
    dist_cutoff = dist,
    lag_cutoff = Inf,
    unit = "county",
    time = "year",
    lat = "lat",
    lon = "lon",
    batch_ram_opt = "none"
  )
  
  reg_all <- conleyreg(
    this_formula,
    data = this_data |> filter(augmented_obs == 1),
    dist_cutoff = dist,
    lag_cutoff = Inf,
    unit = "county",
    time = "year",
    lat = "lat",
    lon = "lon",
    batch_ram_opt = "none"
  )

  # Process regression results for augmented_obs = 1
  coef_all = tidy(reg_all, conf.int = TRUE) |>
    filter(str_detect(term, "wind_error_bin")) |>
    mutate(term = str_extract(term, "\\(.*?\\]")) |>
    add_row(term = "(-2,0]", estimate = 0, conf.high = 0, conf.low = 0, .after = 4) |>
    mutate(term = factor(term, levels = levels(this_data$wind_error_bin))) |>
    mutate(data_type = "Yes")
    
  # Process regression results for augmented_obs = 0
  coef_no = tidy(reg_no, conf.int = TRUE) |>
    filter(str_detect(term, "wind_error_bin")) |>
    mutate(term = str_extract(term, "\\(.*?\\]")) |>
    add_row(term = "(-2,0]", estimate = 0, conf.high = 0, conf.low = 0, .after = 4) |>
    mutate(term = factor(term, levels = levels(this_data$wind_error_bin))) |>
    mutate(data_type = "No")
  
  coef <- rbind(coef_all, coef_no)

}

create_fig3_panel_plot <- function(coef, title, y_label = "", ylim_range = NULL, bar_disp = 0.1, scaling = 1.0) {
  # Add x points and displace "No"s by -bar_disp and "Yes"s by bar_disp
  coef = coef |>
    mutate(x = as.integer(term)) |>
    mutate(x = ifelse(data_type == "Yes", x + bar_disp, x - bar_disp))

  # Extract X labels
  x_labels = coef |> filter(data_type == "Yes") |> pull(term)
  x_breaks = coef |> filter(data_type == "Yes") |> pull(x) - bar_disp

  plot = ggplot(coef, aes(x, estimate*scaling)) +
    geom_hline(yintercept = 0, color = "grey55", linetype = "dashed") +
    geom_errorbar(aes(x = x, ymin = conf.low*scaling, ymax = conf.high*scaling, color = data_type), width = 0) +
    geom_point(aes(color = data_type), size = 3) +
    scale_x_continuous(labels = x_labels, breaks = x_breaks) +
    scale_y_continuous(breaks = scales::pretty_breaks()) +
    scale_color_manual(values = c("Yes" = "blue", "No" = "red"), name = "Aug. Obs?") +
    main_theme +
    labs(title = title, x = "Wind Speed Underestimate (m/s)", y = y_label) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          legend.position = "right") +
    coord_cartesian(ylim = ylim_range)

  return(plot)
} 



# Panel A) Damages by forecast error
fig3_a_coef = run_fig3_reg(formula(damages/1e6 ~ i(wind_error_bin, ref = "(-2,0]") | county + state^hurricane + wind_cut + precip_cut + precip_error_bin), df)
fig3_a_plot = create_fig3_panel_plot(coef = fig3_a_coef, "A. Damages (million $)")
fig3_a_plot

# Panel B) Damages/GDP by forecast error
fig3_b_coef = run_fig3_reg(formula(damage_per_gdp ~ i(wind_error_bin, ref = "(-2,0]") | county + state^hurricane + wind_cut + precip_cut + precip_error_bin), df)
fig3_b_plot = create_fig3_panel_plot(coef = fig3_b_coef, "B. Damages/GDP (%)", scaling = 100)
fig3_b_plot

# Panel C) Damages per capita by forecast error
fig3_c_coef = run_fig3_reg(formula(damage_pc ~ i(wind_error_bin, ref = "(-2,0]") | county + state^hurricane + wind_cut + precip_cut + precip_error_bin), df)
fig3_c_plot = create_fig3_panel_plot(coef = fig3_c_coef, "C. Damages/Person ($)")
fig3_c_plot

# Panel D) Recovery Spending by forecast error
fig3_d_coef = run_fig3_reg(formula(ex_post_spending/1e6 ~ i(wind_error_bin, ref = "(-2,0]") | county + state^hurricane + wind_cut + precip_cut + precip_error_bin), df)
fig3_d_plot = create_fig3_panel_plot(coef = fig3_d_coef, "D. Recovery Spending (million $)")
fig3_d_plot

# Panel E) Recovery Spending/GDP by forecast error
fig3_e_coef = run_fig3_reg(formula(ex_post_spending_per_gdp ~ i(wind_error_bin, ref = "(-2,0]") | county + state^hurricane + wind_cut + precip_cut + precip_error_bin), df)
fig3_e_plot = create_fig3_panel_plot(coef = fig3_e_coef, "E. Recovery Spending/GDP (%)", scaling = 100)
fig3_e_plot

# Panel F) Recovery Spending per capita by forecast error
fig3_f_coef = run_fig3_reg(formula(ex_post_spending_pc ~ i(wind_error_bin, ref = "(-2,0]") | county + state^hurricane + wind_cut + precip_cut + precip_error_bin), df)
fig3_f_plot = create_fig3_panel_plot(coef = fig3_f_coef, "F. Recovery Spending/Person ($)")
fig3_f_plot


fig3_panel = plot_grid(
  fig3_a_plot, fig3_b_plot, fig3_c_plot, fig3_d_plot, fig3_e_plot, fig3_f_plot,
  ncol = 3,
  rel_widths = c(1, 1, 1.2)
)

fig3_panel

ggsave(plot = fig3_panel, "output/augmented_obs/figure_3.png", width = 3 * 10 * 2 / 3, height = 2 * 8 * 2 / 3)

# Stack coefs for each panel in a single tibble and export coefs table in CSV
fig3_coefs <- bind_rows(
  fig3_a_coef %>% mutate(reg = "Damages"),
  fig3_b_coef %>% mutate(reg = "Damages/GDP"),
  fig3_c_coef %>% mutate(reg = "Damages/Person"),
  fig3_d_coef %>% mutate(reg = "Recovery Spending"),
  fig3_e_coef %>% mutate(reg = "Recovery Spending/GDP"),
  fig3_f_coef %>% mutate(reg = "Recovery Spending/Person"))

write_csv(fig3_coefs, "output/augmented_obs/figure_3_coefs.csv")



# ======================
# 4. Table 2
# ======================

print("================")
print("Making Table 2.")
print("================")

run_conleyreg <- function(this_formula, this_data) {
    conleyreg(
      this_formula,
      data = this_data,
      dist_cutoff = dist,
      lag_cutoff = Inf,
      unit = "county",
      time = "year",
      lat = "lat",
      lon = "lon",
      batch_ram_opt = "none"
      )
}


# Regressions in level
regs_level = list()
  
# Regressions for Model 6 (Yes and No cases)
regs_level[[1]] = run_conleyreg(
  formula(total_cost ~ squared_error  + squared_error_precip + level_error + level_error_precip | 
              county + state^hurricane +
              county^year + county^month + wind_cut + precip_cut),
  df |> filter(augmented_obs == 1)
)
regs_level[[2]] = run_conleyreg(
  formula(total_cost ~ squared_error  + squared_error_precip + level_error + level_error_precip | 
              county + state^hurricane +
              county^year + county^month + wind_cut + precip_cut),
  df |> filter(augmented_obs == 0)
)

# Regressions for Model 7
regs_level[[3]] = run_conleyreg(
  formula(total_cost ~ squared_error:category + squared_error_precip + level_error + level_error_precip | 
              county + state^hurricane + wind_cut + precip_cut),
  df |> filter(augmented_obs == 1)
)

regs_level[[4]] = run_conleyreg(
  formula(total_cost ~ squared_error:category + squared_error_precip + level_error + level_error_precip | 
              county + state^hurricane + wind_cut + precip_cut),
  df |> filter(augmented_obs == 0)
)


# Regressions per GDP
regs_pgdp = list()

# Regressions for Model 6 (Yes and No cases)
regs_pgdp[[1]] = run_conleyreg(
  formula(total_cost_per_gdp*100 ~ squared_error  + squared_error_precip + level_error + level_error_precip | 
              county + state^hurricane +
              county^year + county^month + wind_cut + precip_cut),
  df |> filter(augmented_obs == 1)
)
regs_pgdp[[2]] = run_conleyreg(
  formula(total_cost_per_gdp*100 ~ squared_error  + squared_error_precip + level_error + level_error_precip | 
              county + state^hurricane +
              county^year + county^month + wind_cut + precip_cut),
  df |> filter(augmented_obs == 0)
)

# Regressions for Model 7 (Yes and No cases)
regs_pgdp[[3]] = run_conleyreg(
  formula(total_cost_per_gdp*100 ~ squared_error:category + squared_error_precip + level_error + level_error_precip | 
              county + state^hurricane + wind_cut + precip_cut),
  df |> filter(augmented_obs == 1)
)
regs_pgdp[[4]] = run_conleyreg(
  formula(total_cost_per_gdp*100 ~ squared_error:category + squared_error_precip + level_error + level_error_precip | 
              county + state^hurricane + wind_cut + precip_cut),
  df |> filter(augmented_obs == 0)
)


# Regressions per capita
regs_pc = list()

# Regressions for Model 6 (Yes and No cases)
regs_pc[[1]] = run_conleyreg(
  formula(total_cost_pc ~ squared_error  + squared_error_precip + level_error + level_error_precip | 
              county + state^hurricane +
              county^year + county^month + wind_cut + precip_cut),
  df |> filter(augmented_obs == 1)
)
regs_pc[[2]] = run_conleyreg(
  formula(total_cost_pc ~ squared_error  + squared_error_precip + level_error + level_error_precip | 
              county + state^hurricane +
              county^year + county^month + wind_cut + precip_cut),
  df |> filter(augmented_obs == 0)
)

# Regressions for Model 7 (Yes and No cases)
regs_pc[[3]] = run_conleyreg(
  formula(total_cost_pc ~ squared_error:category + squared_error_precip + level_error + level_error_precip | 
              county + state^hurricane + wind_cut + precip_cut),
  df |> filter(augmented_obs == 1)
)
regs_pc[[4]] = run_conleyreg(
  formula(total_cost_pc ~ squared_error:category + squared_error_precip + level_error + level_error_precip | 
              county + state^hurricane + wind_cut + precip_cut),
  df |> filter(augmented_obs == 0)
)



obs_yes = feols(
  total_cost ~ squared_error  + squared_error_precip | 
              county + state^hurricane +
              county^year + county^month + wind_cut + precip_cut,
  data = df |> filter(augmented_obs == 1))$nobs |>
  format(big.mark = ",")
  
obs_no = feols(
  total_cost ~ squared_error  + squared_error_precip | 
              county + state^hurricane +
              county^year + county^month + wind_cut + precip_cut,
  data = df |> filter(augmented_obs == 1))$nobs |>
  format(big.mark = ",")


rows = tribble(
  ~term, ~"(1)", ~"(2)", ~"(3)", ~"(4)",
  "Augmented observations?", "Yes", "No", "Yes", "No",
  "", "", "", "", "",
  "Observations", obs_yes[1], obs_no[1], obs_yes[1], obs_no[1], 
  "", "", "", "", "",
  "Realized Wind/Precip Bins", "X", "X", "X", "X",
  "Level Wind/Precip Error", "X", "X", "X", "X",
  "State-Hurricane FE", "X", "X", "X", "X",
  "County FE", "", "",  "X", "X",
  "County-Month of Year FE", "X", "X", "", "",
  "County-Year FE", "X", "X", "", "",
)

msummary(list("Damages + Recovery Spending (million $)" = regs_level, "(Damages + Recovery Spending) / GDP (%)" = regs_pgdp, "Damages + Recovery Spending Per Capita ($/person)" = regs_pc),
  coef_rename = c("$\\beta_2: (e - \\mu)^2$", "$\\text{Hurricane } \\beta_2: (e - \\mu)^2$", "$\\text{Sub-Hurricane } \\beta_2: (e - \\mu)^2$"),
  coef_omit = c(-1, -5, -6),
  gof_omit = "R2|AIC|BIC|Log.|RMSE|FE|Std.Errors",
  stars =  c('*' = .1, '**' = .05, '***' = .01),
  fmt = "%.2f",
  add_rows = rows,
  title = "The Value of a Wind Speed Forecast Improvement.",
  notes = list("Standard errors are Conley Spatial HAC with a distance radius of 400 km for spatial correlation and arbitrary autocorrelation within counties."),
  threeparttable = TRUE,
  shape = 'rbind',
  escape = FALSE,
  output = "output/augmented_obs/table_2.tex"
)


msummary(list("Damages + Recovery Spending (million $)" = regs_level, "(Damages + Recovery Spending) / GDP (%)" = regs_pgdp, "Damages + Recovery Spending Per Capita ($/person)" = regs_pc),
  coef_rename = c("$\\beta_2: (e - \\mu)^2$", "$\\text{Hurricane } \\beta_2: (e - \\mu)^2$", "$\\text{Sub-Hurricane } \\beta_2: (e - \\mu)^2$"),
  coef_omit = c(-1, -5, -6),
  gof_omit = "R2|AIC|BIC|Log.|RMSE|FE|Std.Errors",
  stars =  c('*' = .1, '**' = .05, '***' = .01),
  fmt = "%.2f",
  add_rows = rows,
  title = "The Value of a Wind Speed Forecast Improvement.",
  shape = 'rbind',
  escape = FALSE,
  output = "output/augmented_obs/table_2.md"
)
