# ==============================================================================
# WHAT IS THE EX ANTE VALUE OF IMPROVING HURRICANE FORECASTS?
# ==============================================================================
# OUTLINE:
# 1. Conley Robustness (600km cutoff)
# 2. PDD Robustness (drop counties with PDD but no damage)
# 3. Coastal States Analysis
# 4. Data Split Analysis (property, crop, other damage)
# 5. No Demean Analysis
# 6. Category Error Analysis
# 7. Precipitation Analysis
# 8. Intensity Controls Robustness
# 9. Intensity Bin Robustness
# 10. Population Weighting
# 11. Wind Interaction Analysis
# 12. VOI Track Forecasts
# 13. VOI Wind Conditional on Track
# 14. Normalization Analysis (global, hurricane-specific, bin-specific)
# 15. Conley Distance Cutoff Testing
# 16. Drop Hurricanes Analysis
# 17. Centered Analysis
# ==============================================================================
gc()

# ==============================================================================
# 1. CONLEY ROBUSTNESS (600km cutoff)
# ==============================================================================

regs = list()

regs[[1]] = conleyreg(
  total_cost ~ squared_error + squared_error_precip + level_error + level_error_precip | 
    county + state^hurricane + wind_cut + precip_cut,
  data = load_df(),
  dist_cutoff = 600,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs[[2]] = conleyreg(
  total_cost ~ squared_error + squared_error_precip + level_error + level_error_precip | 
    county + state^hurricane + wind_cut + precip_cut + county^month,
  data = load_df(),
  dist_cutoff = 600,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs[[3]] = conleyreg(
  total_cost ~ squared_error + squared_error_precip + level_error + level_error_precip | 
    county + state^hurricane + wind_cut + precip_cut + county^year,
  data = load_df(),
  dist_cutoff = 600,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs[[4]] = conleyreg(
  total_cost ~ squared_error + squared_error_precip + level_error + level_error_precip | 
    county + state^hurricane + wind_cut + precip_cut +
    county^year + county^month,
  data = load_df(),
  dist_cutoff = 600,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs[[5]] = conleyreg(
  total_cost ~ squared_error:category + squared_error_precip + level_error + level_error_precip | 
    county + state^hurricane + wind_cut + precip_cut,
  data = load_df(),
  dist_cutoff = 600,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pc = list()

regs_pc[[1]] = conleyreg(
  total_cost_pc ~ squared_error + squared_error_precip + level_error + level_error_precip | 
    county + state^hurricane + wind_cut + precip_cut,
  data = load_df(),
  dist_cutoff = 600,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pc[[2]] = conleyreg(
  total_cost_pc ~ squared_error + squared_error_precip + level_error + level_error_precip | 
    county + state^hurricane + wind_cut + precip_cut + county^month,
  data = load_df(),
  dist_cutoff = 600,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pc[[3]] = conleyreg(
  total_cost_pc ~ squared_error + squared_error_precip + level_error + level_error_precip | 
    county + state^hurricane + wind_cut + precip_cut + county^year,
  data = load_df(),
  dist_cutoff = 600,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pc[[4]] = conleyreg(
  total_cost_pc ~ squared_error + squared_error_precip + level_error + level_error_precip | 
    county + state^hurricane + wind_cut + precip_cut +
    county^year + county^month,
  data = load_df(),
  dist_cutoff = 600,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pc[[5]] = conleyreg(
  total_cost_pc ~ squared_error:category + squared_error_precip + level_error + level_error_precip | 
    county + state^hurricane + wind_cut + precip_cut,
  data = load_df(),
  dist_cutoff = 600,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pgdp = list()

regs_pgdp[[1]] = conleyreg(
  total_cost_per_gdp*100 ~ squared_error + squared_error_precip + level_error + level_error_precip | 
    county + state^hurricane + wind_cut + precip_cut,
  data = load_df(),
  dist_cutoff = 600,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pgdp[[2]] = conleyreg(
  total_cost_per_gdp*100 ~ squared_error + squared_error_precip + level_error + level_error_precip | 
    county + state^hurricane + wind_cut + precip_cut + county^month,
  data = load_df(),
  dist_cutoff = 600,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pgdp[[3]] = conleyreg(
  total_cost_per_gdp*100 ~ squared_error + squared_error_precip + level_error + level_error_precip | 
    county + state^hurricane + wind_cut + precip_cut + county^year,
  data = load_df(),
  dist_cutoff = 600,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pgdp[[4]] = conleyreg(
  total_cost_per_gdp*100 ~ squared_error + squared_error_precip + level_error + level_error_precip | 
    county + state^hurricane + wind_cut + precip_cut +
    county^year + county^month,
  data = load_df(),
  dist_cutoff = 600,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pgdp[[5]] = conleyreg(
  total_cost_per_gdp*100 ~ squared_error:category + squared_error_precip + level_error + level_error_precip | 
    county + state^hurricane + wind_cut + precip_cut,
  data = load_df(),
  dist_cutoff = 600,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

obs = feols(
  total_cost ~ squared_error + squared_error_precip + level_error + level_error_precip |
    county^year + state^hurricane + wind_cut + precip_cut,
  data = load_df()
)$nobs |>
  format(big.mark = ",")

rows = tribble(
  ~term, ~"(1)", ~"(2)", ~"(3)", ~"(4)", ~"(5)",
  "", "", "", "", "", "", 
  "Observations", obs, obs, obs, obs, obs,
  "", "", "", "", "",  "",
  "Realized Wind/Precip Bins", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark",  "\\checkmark", 
  "Level Wind/Precip Error", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark",  "\\checkmark", 
  "State-Hurricane FE", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark",  "\\checkmark", 
  "County FE", "\\checkmark", "", "",  "", "\\checkmark",
  "County-Month of Year FE", "",  "\\checkmark", "", "\\checkmark", "", 
  "County-Year FE", "", "",  "\\checkmark", "\\checkmark", ""
)

msummary(list("Damages + Recovery Expenditures (million $)" = regs, "(Damages + Recovery Expenditures) / GDP (%)" = regs_pgdp, "Damages + Recovery Expenditures Per Capita ($/person)" = regs_pc),
               coef_rename = c("$\\beta_2: (e - \\mu)^2$", "$\\text{Hurricane } \\beta_2: (e - \\mu)^2$", "$\\text{Sub-Hurricane } \\beta_2: (e - \\mu)^2$"),
               coef_omit = c(-1, -5, -6),
               gof_omit = "R2|AIC|BIC|Log.|RMSE|FE|Std.Errors",
               stars =  c('*' = .1, '**' = .05, '***' = .01),
               fmt = "%.2f",
               add_rows = rows,
               title = "The Value of a Wind Speed Forecast Improvement: Conley Robustness. \\label{atab:voi-table-600}",
               notes = list("Standard errors are Conley Spatial HAC with a distance radius of 600 km for spatial correlation and arbitrary autocorrelation within counties. 
               $e$ is the county-hurricane-specific forecast error, and $\\\\mu$ is the hurricane-intensity bin-specific mean forecast error. 
               Hurricane wind speeds are those greater than 33 m/s (74 mph), while Sub-Hurricane wind speeds fall below that threshold."),
               threeparttable = TRUE,
               shape = 'rbind',
               escape = FALSE,
               output = "results/TableC4.tex"
)

add_adjust_box("results/TableC4.tex")

# ==============================================================================
# 2. PDD ROBUSTNESS
# ==============================================================================

regs = list()

regs[[1]] = conleyreg(
  total_cost ~ squared_error + squared_error_precip + level_error + level_error_precip | 
    county + state^hurricane + wind_cut + precip_cut,
  data = load_pdd(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs[[2]] = conleyreg(
  total_cost ~ squared_error + squared_error_precip + level_error + level_error_precip | 
    county + state^hurricane + wind_cut + precip_cut + county^month,
  data = load_pdd(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs[[3]] = conleyreg(
  total_cost ~ squared_error + squared_error_precip + level_error + level_error_precip | 
    county + state^hurricane + wind_cut + precip_cut + county^year,
  data = load_pdd(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs[[4]] = conleyreg(
  total_cost ~ squared_error + squared_error_precip + level_error + level_error_precip | 
    county + state^hurricane + wind_cut + precip_cut +
    county^year + county^month,
  data = load_pdd(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs[[5]] = conleyreg(
  total_cost ~ squared_error:category + squared_error_precip + level_error + level_error_precip | 
    county + state^hurricane + wind_cut + precip_cut,
  data = load_pdd(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pc = list()

regs_pc[[1]] = conleyreg(
  total_cost_pc ~ squared_error + squared_error_precip + level_error + level_error_precip | 
    county + state^hurricane + wind_cut + precip_cut,
  data = load_pdd(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pc[[2]] = conleyreg(
  total_cost_pc ~ squared_error + squared_error_precip + level_error + level_error_precip | 
    county + state^hurricane + wind_cut + precip_cut + county^month,
  data = load_pdd(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pc[[3]] = conleyreg(
  total_cost_pc ~ squared_error + squared_error_precip + level_error + level_error_precip | 
    county + state^hurricane + wind_cut + precip_cut + county^year,
  data = load_pdd(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pc[[4]] = conleyreg(
  total_cost_pc ~ squared_error + squared_error_precip + level_error + level_error_precip | 
    county + state^hurricane + wind_cut + precip_cut +
    county^year + county^month,
  data = load_pdd(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pc[[5]] = conleyreg(
  total_cost_pc ~ squared_error:category + squared_error_precip + level_error + level_error_precip | 
    county + state^hurricane + wind_cut + precip_cut,
  data = load_pdd(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pgdp = list()

regs_pgdp[[1]] = conleyreg(
  total_cost_per_gdp*100 ~ squared_error + squared_error_precip + level_error + level_error_precip | 
    county + state^hurricane + wind_cut + precip_cut,
  data = load_pdd(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pgdp[[2]] = conleyreg(
  total_cost_per_gdp*100 ~ squared_error + squared_error_precip + level_error + level_error_precip | 
    county + state^hurricane + wind_cut + precip_cut + county^month,
  data = load_pdd(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pgdp[[3]] = conleyreg(
  total_cost_per_gdp*100 ~ squared_error + squared_error_precip + level_error + level_error_precip | 
    county + state^hurricane + wind_cut + precip_cut + county^year,
  data = load_pdd(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pgdp[[4]] = conleyreg(
  total_cost_per_gdp*100 ~ squared_error + squared_error_precip + level_error + level_error_precip | 
    county + state^hurricane + wind_cut + precip_cut +
    county^year + county^month,
  data = load_pdd(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pgdp[[5]] = conleyreg(
  total_cost_per_gdp*100 ~ squared_error:category + squared_error_precip + level_error + level_error_precip | 
    county + state^hurricane + wind_cut + precip_cut,
  data = load_pdd(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

obs = feols(
  total_cost ~ squared_error + squared_error_precip + level_error + level_error_precip |
    county^year + state^hurricane + wind_cut + precip_cut,
  data = load_pdd(),
)$nobs |>
  format(big.mark = ",")

rows = tribble(
  ~term, ~"(1)", ~"(2)", ~"(3)", ~"(4)", ~"(5)",
  "", "", "", "", "", "", 
  "Observations", obs, obs, obs, obs, obs,
  "", "", "", "", "",  "",
  "Realized Wind/Precip Bins", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark",  "\\checkmark", 
  "Level Wind/Precip Error", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark",  "\\checkmark", 
  "State-Hurricane FE", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark",  "\\checkmark", 
  "County FE", "\\checkmark", "", "",  "", "\\checkmark",
  "County-Month of Year FE", "",  "\\checkmark", "", "\\checkmark", "", 
  "County-Year FE", "", "",  "\\checkmark", "\\checkmark", ""
)

msummary(list("Damages + Recovery Expenditures (million $)" = regs, "(Damages + Recovery Expenditures) / GDP (%)" = regs_pgdp, "Damages + Recovery Expenditures Per Capita ($/person)" = regs_pc),
               coef_rename = c("$\\beta_2: (e - \\mu)^2$", "$\\text{Hurricane } \\beta_2: (e - \\mu)^2$", "$\\text{Sub-Hurricane } \\beta_2: (e - \\mu)^2$"),
               coef_omit = c(-1, -5, -6),
               gof_omit = "R2|AIC|BIC|Log.|RMSE|FE|Std.Errors",
               stars =  c('*' = .1, '**' = .05, '***' = .01),
               fmt = "%.2f",
               add_rows = rows,
               title = "The Value of a Wind Speed Forecast Improvement: PDD Robustness. \\label{atab:voi-table-pdd}",
               notes = list("Standard errors are Conley Spatial HAC with a distance radius of 400 km for spatial correlation and arbitrary autocorrelation within counties. 
               $e$ is the county-hurricane-specific forecast error, and $\\\\mu$ is the hurricane-intensity bin-specific mean forecast error. 
               Hurricane wind speeds are those greater than 33 m/s (74 mph), while Sub-Hurricane wind speeds fall below that threshold. 
               Counties issued a Presidential Disaster Declaration but without reported SHELDUS damage are dropped from the sample."),
               threeparttable = TRUE,
               shape = 'rbind',
               escape = FALSE,
               output = "results/TableC5.tex"
)

add_adjust_box("results/TableC5.tex")

# ==============================================================================
# 3. COASTAL STATES ANALYSIS
# ==============================================================================

regs = list()

regs[[1]] = conleyreg(
  total_cost ~ squared_error + squared_error_precip + level_error + level_error_precip | 
    county + state^hurricane + wind_cut + precip_cut,
  data = load_df() |> filter(state %in% coastal_states),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs[[2]] = conleyreg(
  total_cost ~ squared_error + squared_error_precip + level_error + level_error_precip | 
    county + state^hurricane + wind_cut + precip_cut + county^month,
  data = load_df() |> filter(state %in% coastal_states),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs[[3]] = conleyreg(
  total_cost ~ squared_error + squared_error_precip + level_error + level_error_precip | 
    county + state^hurricane + wind_cut + precip_cut + county^year,
  data = load_df() |> filter(state %in% coastal_states),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs[[4]] = conleyreg(
  total_cost ~ squared_error + squared_error_precip + level_error + level_error_precip | 
    county + state^hurricane + wind_cut + precip_cut +
    county^year + county^month,
  data = load_df() |> filter(state %in% coastal_states),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs[[5]] = conleyreg(
  total_cost ~ squared_error:category + squared_error_precip + level_error + level_error_precip | 
    county + state^hurricane + wind_cut + precip_cut,
  data = load_df() |> filter(state %in% coastal_states),
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
  data = load_df() |> filter(state %in% coastal_states),
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
  data = load_df() |> filter(state %in% coastal_states),
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
  data = load_df() |> filter(state %in% coastal_states),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pc[[4]] = conleyreg(
  total_cost_pc ~ squared_error + squared_error_precip + level_error + level_error_precip | 
    county + state^hurricane + wind_cut + precip_cut +
    county^year + county^month,
  data = load_df() |> filter(state %in% coastal_states),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pc[[5]] = conleyreg(
  total_cost_pc ~ squared_error:category + squared_error_precip + level_error + level_error_precip | 
    county + state^hurricane + wind_cut + precip_cut,
  data = load_df() |> filter(state %in% coastal_states),
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
  data = load_df() |> filter(state %in% coastal_states),
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
  data = load_df() |> filter(state %in% coastal_states),
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
  data = load_df() |> filter(state %in% coastal_states),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pgdp[[4]] = conleyreg(
  total_cost_per_gdp*100 ~ squared_error + squared_error_precip + level_error + level_error_precip | 
    county + state^hurricane + county^month + wind_cut + precip_cut,
  data = load_df() |> filter(state %in% coastal_states),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pgdp[[5]] = conleyreg(
  total_cost_per_gdp*100 ~ squared_error:category + squared_error_precip + level_error + level_error_precip | 
    county + state^hurricane + county^year + wind_cut + precip_cut,
  data = load_df() |> filter(state %in% coastal_states),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

obs = feols(
  total_cost ~ squared_error + squared_error_precip + level_error + level_error_precip |
    county^year + state^hurricane + wind_cut + precip_cut,
  data = load_df() |> filter(state %in% coastal_states),
)$nobs |>
  format(big.mark = ",")

rows = tribble(
  ~term, ~"(1)", ~"(2)", ~"(3)", ~"(4)", ~"(5)",
  "", "", "", "", "", "", 
  "Observations", obs, obs, obs, obs, obs,
  "", "", "", "", "",  "",
  "Realized Wind/Precip Bins", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark",  "\\checkmark", 
  "Level Wind/Precip Error", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark",  "\\checkmark", 
  "State-Hurricane FE", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark",  "\\checkmark", 
  "County FE", "\\checkmark", "", "",  "", "\\checkmark",
  "County-Month of Year FE", "",  "\\checkmark", "", "\\checkmark", "", 
  "County-Year FE", "", "",  "\\checkmark", "\\checkmark", ""
)

msummary(list("Damages + Recovery Expenditures (million $)" = regs, "(Damages + Recovery Expenditures) / GDP (%)" = regs_pgdp, "Damages + Recovery Expenditures Per Capita ($/person)" = regs_pc),
               coef_rename = c("$\\beta_2: (e - \\mu)^2$", "$\\text{Hurricane } \\beta_2: (e - \\mu)^2$", "$\\text{Sub-Hurricane } \\beta_2: (e - \\mu)^2$"),
               coef_omit = c(-1, -5, -6),
               gof_omit = "R2|AIC|BIC|Log.|RMSE|FE|Std.Errors",
               stars =  c('*' = .1, '**' = .05, '***' = .01),
               fmt = "%.2f",
               add_rows = rows,
               title = "The Value of a Wind Speed Forecast Improvement: Coastal States. \\label{atab:voi-table-coastal}",
               notes = list("Standard errors are Conley Spatial HAC with a distance radius of 400 km for spatial correlation and arbitrary autocorrelation within counties. 
               $e$ is the county-hurricane-specific forecast error, and $\\\\mu$ is the hurricane-intensity bin-specific mean forecast error. 
               Hurricane wind speeds are those greater than 33 m/s (74 mph), while Sub-Hurricane wind speeds fall below that threshold. 
               Only the following states are included in the sample: Texas, Louisiana, Mississippi, Alabama, Georgia, Florida, South Carolina, North Carolina, Virginia, Maryland, New Jersey, Pennsylvania, Connecticut, Delaware, New York, Rhode Island, Massachusetts, New Hampshire, and Maine."),
               threeparttable = TRUE,
               shape = 'rbind',
               escape = FALSE,
               output = "results/TableC6.tex"
)

add_adjust_box("results/TableC6.tex")

# ==============================================================================
# 4. DATA SPLIT ANALYSIS
# ==============================================================================

regs_prop = list()

regs_prop[[1]] = conleyreg(
  property_damage/1e6 ~ squared_error + squared_error_precip + level_error + level_error_precip | 
    county + state^hurricane + wind_cut + precip_cut,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_prop[[2]] = conleyreg(
  property_damage/1e6 ~ squared_error + squared_error_precip + level_error + level_error_precip | 
    county + state^hurricane + wind_cut + precip_cut + county^month,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_prop[[3]] = conleyreg(
  property_damage/1e6 ~ squared_error + squared_error_precip + level_error + level_error_precip | 
    county + state^hurricane + wind_cut + precip_cut + county^year,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_prop[[4]] = conleyreg(
  property_damage/1e6 ~ squared_error + squared_error_precip + level_error + level_error_precip | 
    county + state^hurricane + wind_cut + precip_cut +
    county^year + county^month,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_prop[[5]] = conleyreg(
  property_damage/1e6 ~ squared_error:category + squared_error_precip + level_error + level_error_precip | 
    county + state^hurricane + wind_cut + precip_cut,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_crop = list()

regs_crop[[1]] = conleyreg(
  crop_damage/1e6 ~ squared_error + squared_error_precip + level_error + level_error_precip | 
    county + state^hurricane + wind_cut + precip_cut,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_crop[[2]] = conleyreg(
  crop_damage/1e6 ~ squared_error + squared_error_precip + level_error + level_error_precip | 
    county + state^hurricane + wind_cut + precip_cut + county^month,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_crop[[3]] = conleyreg(
  crop_damage/1e6 ~ squared_error + squared_error_precip + level_error + level_error_precip | 
    county + state^hurricane + wind_cut + precip_cut + county^year,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_crop[[4]] = conleyreg(
  crop_damage/1e6 ~ squared_error + squared_error_precip + level_error + level_error_precip | 
    county + state^hurricane + wind_cut + precip_cut +
    county^year + county^month,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_crop[[5]] = conleyreg(
  crop_damage/1e6 ~ squared_error:category + squared_error_precip + level_error + level_error_precip | 
    county + state^hurricane + wind_cut + precip_cut,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_mort = list()

regs_mort[[1]] = conleyreg(
  mortality_damage/1e6 ~ squared_error + squared_error_precip + level_error + level_error_precip | 
    county + state^hurricane + wind_cut + precip_cut,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_mort[[2]] = conleyreg(
  mortality_damage/1e6 ~ squared_error + squared_error_precip + level_error + level_error_precip | 
    county + state^hurricane + wind_cut + precip_cut + county^month,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_mort[[3]] = conleyreg(
  mortality_damage/1e6 ~ squared_error + squared_error_precip + level_error + level_error_precip | 
    county + state^hurricane + wind_cut + precip_cut + county^year,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_mort[[4]] = conleyreg(
  mortality_damage/1e6 ~ squared_error + squared_error_precip + level_error + level_error_precip | 
    county + state^hurricane + wind_cut + precip_cut +
    county^year + county^month,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_mort[[5]] = conleyreg(
  mortality_damage/1e6 ~ squared_error:category + squared_error_precip + level_error + level_error_precip | 
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
  total_cost ~ squared_error + squared_error_precip + level_error + level_error_precip |
    county^year + state^hurricane + wind_cut + precip_cut,
  data = load_df()
)$nobs |>
  format(big.mark = ",")

rows = tribble(
  ~term, ~"(1)", ~"(2)", ~"(3)", ~"(4)", ~"(5)",
  "", "", "", "", "", "", 
  "Observations", obs, obs, obs, obs, obs,
  "", "", "", "", "",  "",
  "Realized Wind/Precip Bins", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark",  "\\checkmark", 
  "Level Wind/Precip Error", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark",  "\\checkmark", 
  "State-Hurricane FE", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark",  "\\checkmark", 
  "County FE", "\\checkmark", "", "",  "", "\\checkmark",
  "County-Month of Year FE", "",  "\\checkmark", "", "\\checkmark", "", 
  "County-Year FE", "", "",  "\\checkmark", "\\checkmark", ""
)

msummary(list("Property Damages (million $)" = regs_prop, "Crop Damages (million $)" = regs_crop, "Mortality Damages (million $)" = regs_mort),
         coef_rename = c("$\\beta_2: (e - \\mu)^2$", "$\\text{Hurricane } \\beta_2: (e - \\mu)^2$", "$\\text{Sub-Hurricane } \\beta_2: (e - \\mu)^2$"),
         coef_omit = c(-1, -5, -6),
         gof_omit = "R2|AIC|BIC|Log.|RMSE|FE|Std.Errors",
         stars =  c('*' = .1, '**' = .05, '***' = .01),
         fmt = "%.2f",
         add_rows = rows,
         title = "The Value of a Wind Speed Forecast Improvement by Damage Type. \\label{atab:voi-table-split}",
         notes = list("Standard errors are Conley Spatial HAC with a distance radius of 400 km for spatial correlation and arbitrary autocorrelation within counties. 
               $e$ is the county-hurricane-specific forecast error, and $\\\\mu$ is the hurricane-intensity bin-specific mean forecast error. 
               Hurricane wind speeds are those greater than 33 m/s (74 mph), while Sub-Hurricane wind speeds fall below that threshold."),
         threeparttable = TRUE,
         shape = 'rbind',
         escape = FALSE,
         output = "results/TableC7.tex"
)

add_adjust_box("results/TableC7.tex")

# ==============================================================================
# 5. NO DEMEAN ANALYSIS
# ==============================================================================

regs = list()

regs[[1]] = conleyreg(
  total_cost ~ squared_error_nodemean + squared_error_precip_nodemean + level_error + level_error_precip | 
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
  total_cost ~ squared_error_nodemean + squared_error_precip_nodemean + level_error + level_error_precip | 
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
  total_cost ~ squared_error_nodemean + squared_error_precip_nodemean + level_error + level_error_precip | 
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
  total_cost ~ squared_error_nodemean + squared_error_precip_nodemean + level_error + level_error_precip | 
    county + state^hurricane + wind_cut + precip_cut +
    county^year + county^month,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs[[5]] = conleyreg(
  total_cost ~ squared_error_nodemean:category + squared_error_precip_nodemean + level_error + level_error_precip | 
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
  total_cost_pc ~ squared_error_nodemean + squared_error_precip_nodemean + level_error + level_error_precip | 
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
  total_cost_pc ~ squared_error_nodemean + squared_error_precip_nodemean + level_error + level_error_precip | 
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
  total_cost_pc ~ squared_error_nodemean + squared_error_precip_nodemean + level_error + level_error_precip | 
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
  total_cost_pc ~ squared_error_nodemean + squared_error_precip_nodemean + level_error + level_error_precip | 
    county + state^hurricane + wind_cut + precip_cut +
    county^year + county^month,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pc[[5]] = conleyreg(
  total_cost_pc ~ squared_error_nodemean:category + squared_error_precip_nodemean + level_error + level_error_precip | 
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
  total_cost_per_gdp*100 ~ squared_error_nodemean + squared_error_precip_nodemean + level_error + level_error_precip | 
    county + state^hurricane + wind_cut + precip_cut,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pgdp[[2]] = conleyreg(
  total_cost_per_gdp*100 ~ squared_error_nodemean + squared_error_precip_nodemean + level_error + level_error_precip | 
    county + state^hurricane + wind_cut + precip_cut + county^month,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pgdp[[3]] = conleyreg(
  total_cost_per_gdp*100 ~ squared_error_nodemean + squared_error_precip_nodemean + level_error + level_error_precip | 
    county + state^hurricane + wind_cut + precip_cut + county^year,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pgdp[[4]] = conleyreg(
  total_cost_per_gdp*100 ~ squared_error_nodemean + squared_error_precip_nodemean + level_error + level_error_precip | 
    county + state^hurricane + wind_cut + precip_cut +
    county^year + county^month,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pgdp[[5]] = conleyreg(
  total_cost_per_gdp*100 ~ squared_error_nodemean:category + squared_error_precip_nodemean + level_error + level_error_precip | 
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
  total_cost ~ squared_error_nodemean + squared_error_precip_nodemean + level_error + level_error_precip |
    county^year + state^hurricane + wind_cut + precip_cut,
  data = load_df()
)$nobs |>
  format(big.mark = ",")

rows = tribble(
  ~term, ~"(1)", ~"(2)", ~"(3)", ~"(4)", ~"(5)",
  "", "", "", "", "", "", 
  "Observations", obs, obs, obs, obs, obs,
  "", "", "", "", "",  "",
  "Realized Wind/Precip Bins", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark",  "\\checkmark", 
  "Level Wind/Precip Error", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark",  "\\checkmark", 
  "State-Hurricane FE", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark",  "\\checkmark", 
  "County FE", "\\checkmark", "", "",  "", "\\checkmark",
  "County-Month of Year FE", "",  "\\checkmark", "", "\\checkmark", "", 
  "County-Year FE", "", "",  "\\checkmark", "\\checkmark", ""
)

msummary(list("Damages + Recovery Expenditures (million $)" = regs, "(Damages + Recovery Expenditures) / GDP (%)" = regs_pgdp, "Damages + Recovery Expenditures Per Capita ($/person)" = regs_pc),
         coef_rename = c("$\\beta_2: (e - \\mu)^2$", "$\\text{Hurricane } \\beta_2: (e - \\mu)^2$", "$\\text{Sub-Hurricane } \\beta_2: (e - \\mu)^2$"),
         coef_omit = c(-1, -5, -6),
         gof_omit = "R2|AIC|BIC|Log.|RMSE|FE|Std.Errors",
         stars =  c('*' = .1, '**' = .05, '***' = .01),
         fmt = "%.2f",
         add_rows = rows,
         title = "The Value of a Wind Speed Forecast Improvement without Demeaning. \\label{atab:voi-table-no-demean}",
         notes = list("Standard errors are Conley Spatial HAC with a distance radius of 400 km for spatial correlation and arbitrary autocorrelation within counties. 
               The squared error terms are not demeaned. 
               $e$ is the county-hurricane-specific forecast error, and $\\\\mu$ is the hurricane-intensity bin-specific mean forecast error. 
               Hurricane wind speeds are those greater than 33 m/s (74 mph), while Sub-Hurricane wind speeds fall below that threshold."),
         threeparttable = TRUE,
         shape = 'rbind',
         escape = FALSE,
         output = "results/TableC8.tex"
)

add_adjust_box("results/TableC8.tex")

# ==============================================================================
# 6. CATEGORY ERROR ANALYSIS
# ==============================================================================


regs = list()

conleyreg(
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

regs[[1]] = conleyreg(
  total_cost ~ squared_error + squared_error_precip | 
    county + state^hurricane + category_error,
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
    county + state^hurricane + category_error,
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
    county + state^hurricane + category_error,
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
    county + state^hurricane + category_error,
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
    county + state^hurricane + county^year + category_error,
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
    county^year + county^month + category_error,
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
    county + state^hurricane + category_error,
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
    county + state^hurricane + category_error,
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
    county + state^hurricane + wind_cut + precip_cut + category_error,
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
    county + state^hurricane + wind_cut + precip_cut + category_error,
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
    county + state^hurricane + county^month + wind_cut + precip_cut + category_error,
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
    county + state^hurricane + county^year + wind_cut + precip_cut + category_error,
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
    county^year + county^month + wind_cut + precip_cut + category_error,
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
    county + state^hurricane + category_error,
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
    county + state^hurricane + category_error,
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
    county + state^hurricane + wind_cut + precip_cut + category_error,
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
    county + state^hurricane + wind_cut + precip_cut + category_error,
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
    county + state^hurricane + county^month + wind_cut + precip_cut + category_error,
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
    county + state^hurricane + county^year + wind_cut + precip_cut + category_error,
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
    county^year + county^month + wind_cut + precip_cut + category_error,
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
    county + state^hurricane + category_error,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

obs = feols(
  total_cost ~ squared_error + squared_error_precip + level_error + level_error_precip |
    county^year + state^hurricane + wind_cut + precip_cut + category_error,
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
         stars =  c('*' = .1, '**' = .05, '***' = .01),
         fmt = "%.2f",
         add_rows = rows,
         title = "The Value of a Wind Speed Forecast Improvement: Category Error Controls. \\label{atab:voi-table-category-error}",
         notes = list("Standard errors are Conley Spatial HAC with a distance radius of 400 km for spatial correlation and arbitrary autocorrelation within counties. 
               $e$ is the county-hurricane-specific forecast error, and $\\\\mu$ is the hurricane-intensity bin-specific mean forecast error. 
               Hurricane wind speeds are those greater than 33 m/s (74 mph), while Sub-Hurricane wind speeds fall below that threshold. 
               All specifications control for an interaction between an indicator for forecast hurricane-force winds and an indicator for realized hurricane-force winds."),
         threeparttable = TRUE,
         shape = 'rbind',
         escape = FALSE,
         output = "results/TableC9.tex"
)

add_adjust_box("results/TableC9.tex")

# ==============================================================================
# 7. PRECIPITATION ANALYSIS
# ==============================================================================

regs = list()

regs[[1]] = conleyreg(
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

regs[[2]] = conleyreg(
  total_cost ~ squared_error + squared_error_precip + level_error + level_error_precip | 
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
  total_cost ~ squared_error + squared_error_precip + level_error + level_error_precip | 
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
  total_cost ~ squared_error + squared_error_precip + level_error + level_error_precip | 
    county + state^hurricane + wind_cut + precip_cut +
    county^year + county^month,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs[[5]] = conleyreg(
  total_cost ~ squared_error + squared_error_precip:category + level_error + level_error_precip | 
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

regs_pc[[2]] = conleyreg(
  total_cost_pc ~ squared_error + squared_error_precip + level_error + level_error_precip | 
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
  total_cost_pc ~ squared_error + squared_error_precip + level_error + level_error_precip | 
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
  total_cost_pc ~ squared_error + squared_error_precip + level_error + level_error_precip | 
    county + state^hurricane + wind_cut + precip_cut +
    county^year + county^month,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pc[[5]] = conleyreg(
  total_cost_pc ~ squared_error + squared_error_precip:category + level_error + level_error_precip | 
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

regs_pgdp[[2]] = conleyreg(
  total_cost_per_gdp*100 ~ squared_error + squared_error_precip + level_error + level_error_precip | 
    county + state^hurricane + wind_cut + precip_cut + county^month,
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
    county + state^hurricane + wind_cut + precip_cut + county^year,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pgdp[[4]] = conleyreg(
  total_cost_per_gdp*100 ~ squared_error + squared_error_precip + level_error + level_error_precip | 
    county + state^hurricane + wind_cut + precip_cut +
    county^year + county^month,
  data = load_df(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pgdp[[5]] = conleyreg(
  total_cost_per_gdp*100 ~ squared_error + squared_error_precip:category + level_error + level_error_precip | 
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
  total_cost ~  squared_error + squared_error_precip + level_error + level_error_precip |
    county^year + state^hurricane + wind_cut + precip_cut,
  data = load_df()
)$nobs |>
  format(big.mark = ",")

rows = tribble(
  ~term, ~"(1)", ~"(2)", ~"(3)", ~"(4)", ~"(5)",
  "", "", "", "", "", "", 
  "Observations", obs, obs, obs, obs, obs,
  "", "", "", "", "",  "",
  "Realized Wind/Precip Bins", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark",  "\\checkmark", 
  "Level Wind/Precip Error", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark",  "\\checkmark", 
  "State-Hurricane FE", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark",  "\\checkmark", 
  "County FE", "\\checkmark", "", "",  "", "\\checkmark",
  "County-Month of Year FE", "",  "\\checkmark", "", "\\checkmark", "", 
  "County-Year FE", "", "",  "\\checkmark", "\\checkmark", ""
)

msummary(list("Damages + Recovery Expenditures (million $)" = regs, "(Damages + Recovery Expenditures) / GDP (%)" = regs_pgdp, "Damages + Recovery Expenditures Per Capita ($/person)" = regs_pc),
               coef_rename = c("$\\beta_2: (e - \\mu)^2$", "$\\text{Hurricane } \\beta_2: (e - \\mu)^2$", "$\\text{Sub-Hurricane } \\beta_2: (e - \\mu)^2$"),
               coef_omit = c(-2, -5, -6),
               gof_omit = "R2|AIC|BIC|Log.|RMSE|FE|Std.Errors",
               stars =  c('*' = .1, '**' = .05, '***' = .01),
               fmt = "%.2f",
               add_rows = rows,
               title = "The Value of a Precipitation Forecast Improvement. \\label{atab:voi-table-precip}",
               notes = list("Standard errors are Conley Spatial HAC with a distance radius of 400 km for spatial correlation and arbitrary autocorrelation within counties. 
               $e$ is the county-hurricane-specific forecast error, and $\\\\mu$ is the hurricane-intensity bin-specific mean forecast error. 
               Hurricane wind speeds are those greater than 33 m/s (74 mph), while Sub-Hurricane wind speeds fall below that threshold."),
               threeparttable = TRUE,
               shape = 'rbind',
               escape = FALSE,
               output = "results/TableC10.tex"
)

add_adjust_box("results/TableC10.tex")

# ==============================================================================
# 8. INTENSITY CONTROLS ROBUSTNESS
# ==============================================================================

gc()  # free memory before parallel cluster setup

reg_results = list()

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

# Loop over each combination of fixed effects
for (i in seq_along(fixed_effects_combinations)) {
  # Create the fixed effects formula
  fe_formula = as.formula(paste("total_cost ~ squared_error:category + squared_error_precip + level_error + level_error_precip |", fixed_effects_combinations[[i]]))
  
  # Run the regression
  reg_results[[i]] = conleyreg(
    formula = fe_formula,
    data = load_df(),
    dist_cutoff = dist,
    lag_cutoff = Inf,
    unit = "county",
    time = "year",
    lat = "lat",
    lon = "lon",
    batch_ram_opt = "none"
  )
}

# Initialize a list to store regression results
reg_results_pgdp = list()

# Loop over each combination of fixed effects
for (i in seq_along(fixed_effects_combinations)) {
  # Create the fixed effects formula
  fe_formula = as.formula(paste("total_cost_per_gdp*100 ~ squared_error:category + squared_error_precip + level_error + level_error_precip |", fixed_effects_combinations[[i]]))
  
  # Run the regression
  reg_results_pgdp[[i]] = conleyreg(
    formula = fe_formula,
    data = load_df(),
    dist_cutoff = dist,
    lag_cutoff = Inf,
    unit = "county",
    time = "year",
    lat = "lat",
    lon = "lon",
    batch_ram_opt = "none"
  )
}

# Initialize a list to store regression results
reg_results_pc = list()

# Loop over each combination of fixed effects
for (i in seq_along(fixed_effects_combinations)) {
  # Create the fixed effects formula
  fe_formula = as.formula(paste("total_cost_pc ~ squared_error:category + squared_error_precip + level_error + level_error_precip |", fixed_effects_combinations[[i]]))
  
  # Run the regression
  reg_results_pc[[i]] = conleyreg(
    formula = fe_formula,
    data = load_df(),
    dist_cutoff = dist,
    lag_cutoff = Inf,
    unit = "county",
    time = "year",
    lat = "lat",
    lon = "lon",
    batch_ram_opt = "none"
  )
}


obs = feols(
  total_cost ~ squared_error  + squared_error_precip | 
    county + state^hurricane +
    county + state^hurricane + wind_cut^precip_cut^coastal^left_of_track,
  data = load_df()
)$nobs |>
  format(big.mark = ",")

rows = tribble(
  ~term, ~"(1)", ~"(2)", ~"(3)", ~"(4)", ~"(5)", ~"(6)", ~"(7)", ~"(8)",
  "", "", "", "", "", "", "", "", "", 
  "Observations", obs, obs, obs, obs, obs, obs, obs, obs, 
  "", "", "", "", "", "", "", "", "", 
  "State-Hurricane FE", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark",  "\\checkmark", 
  "County FE", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark",  "\\checkmark", 
  "Realized Wind Bins", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "", "", "", "",
  "Realized Precip Bins", "", "\\checkmark", "\\checkmark", "\\checkmark", "", "", "", "",
  "Coastal Indicator", "", "", "\\checkmark", "\\checkmark", "", "", "", "",
  "West of Track Indicator", "", "", "", "\\checkmark", "", "", "", "",
  "Wind-by-Precip Bins", "", "", "", "", "\\checkmark", "", "", "",
  "Wind-by-Precip-by-Coastal Indicator", "", "", "", "", "", "\\checkmark", "", "",
  "Wind-by-Precip-by-West of Track Indicator", "", "", "", "", "", "", "\\checkmark", "",
  "Wind-by-Precip-by-Coastal-by-West of Track Indicator", "", "", "", "", "", "", "", "\\checkmark",
)


msummary(list("Damages + Recovery Expenditures (million $)" = reg_results, "(Damages + Recovery Expenditures) / GDP (%)" = reg_results_pgdp, "Damages + Recovery Expenditures Per Capita ($/person)" = reg_results_pc),
         coef_rename = c("$\\text{Hurricane } \\beta_2: (e - \\mu)^2$", "$\\text{Sub-Hurricane } \\beta_2: (e - \\mu)^2$"),
         coef_omit = c(-4, -5),
         gof_omit = "R2|AIC|BIC|Log.|RMSE|FE|Std.Errors",
         stars =  c('*' = .1, '**' = .05, '***' = .01),
         fmt = "%.2f",
         add_rows = rows,
         title = "The Value of a Wind Speed Forecast Improvement. \\label{atab:voi-table-fe-robust}",
         notes = list("Standard errors are Conley Spatial HAC with a distance radius of 400 km for spatial correlation and arbitrary autocorrelation within counties. 
               $e$ is the county-hurricane-specific forecast error, and $\\\\mu$ is the hurricane-intensity bin-specific mean forecast error. 
               Hurricane wind speeds are those greater than 33 m/s (74 mph), while Sub-Hurricane wind speeds fall below that threshold."),
         threeparttable = TRUE,
         shape = 'rbind',
         escape = FALSE,
         output = "results/TableC11.tex"
)

add_adjust_box("results/TableC11.tex")

# ==============================================================================
# 9. INTENSITY BIN ROBUSTNESS
# ==============================================================================

reg_results = list()

# Define vector of bin numbers to try
num_bins_vec = c(5, 10, 20, 40, 60, 80, 100, 120)

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
    total_cost ~ squared_error:category + squared_error_precip + level_error + level_error_precip |
      county + state^hurricane + wind_cut_new^precip_cut_new^coastal^left_of_track,
    data = df_binned,
    dist_cutoff = dist,
    lag_cutoff = Inf,
    unit = "county",
    time = "year",
    lat = "lat",
    lon = "lon",
    batch_ram_opt = "none"
  )
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
    total_cost_pc ~ squared_error:category + squared_error_precip + level_error + level_error_precip |
      county + state^hurricane + wind_cut_new^precip_cut_new^coastal^left_of_track,
    data = df_binned,
    dist_cutoff = dist,
    lag_cutoff = Inf,
    unit = "county",
    time = "year",
    lat = "lat",
    lon = "lon",
    batch_ram_opt = "none"
  )
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
    total_cost_per_gdp*100 ~ squared_error:category + squared_error_precip + level_error + level_error_precip |
      county + state^hurricane + wind_cut_new^precip_cut_new^coastal^left_of_track,
    data = df_binned,
    dist_cutoff = dist,
    lag_cutoff = Inf,
    unit = "county",
    time = "year",
    lat = "lat",
    lon = "lon",
    batch_ram_opt = "none"
  )
}

obs = feols(
  total_cost ~ squared_error  + squared_error_precip | 
    county + state^hurricane +
    county + state^hurricane + wind_cut^precip_cut^coastal^left_of_track,
  data = load_df()
)$nobs |>
  format(big.mark = ",")

rows = tribble(
  ~term, ~"(1)", ~"(2)", ~"(3)", ~"(4)", ~"(5)", ~"(6)", ~"(7)", ~"(8)", 
  "", "", "", "", "", "", "",  "", "",
  "Observations", obs, obs, obs, obs, obs, obs, obs, obs, 
  "", "", "", "", "", "", "", "", "",
  "State-Hurricane FE", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", 
  "County FE", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", 
  "Wind-by-Precip-by-Coastal-by-West of Track Indicator", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark"
)

msummary(
  list(
    "Damages + Recovery Expenditures (million $)" = reg_results, 
    "(Damages + Recovery Expenditures) / GDP (%)" = reg_results_pgdp, 
    "Damages + Recovery Expenditures Per Capita ($/person)" = reg_results_pc
  ),
  coef_rename = c(
    "$\\text{Hurricane } \\beta_2: (e - \\mu)^2$", 
    "$\\text{Sub-Hurricane } \\beta_2: (e - \\mu)^2$"
  ),
  col.names = c("", "5 Bins", "10 Bins", "20 Bins", "40 Bins", "60 Bins", "80 Bins", "100 Bins", "120 Bins"),
  coef_omit = c(-4, -5),
  gof_omit = "R2|AIC|BIC|Log.|RMSE|FE|Std.Errors",
  stars = c('*' = .1, '**' = .05, '***' = .01),
  fmt = "%.2f",
  add_rows = rows,
  title = "The Value of a Wind Speed Forecast Improvement Under Different Wind Bins. \\label{atab:voi-table-bins-robust}",
  notes = list(
    "* p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01",
    "Standard errors are Conley Spatial HAC with a distance radius of 400 km for spatial correlation and arbitrary autocorrelation within counties.",
    "$e$ is the county-hurricane-specific forecast error, and $\\\\mu$ is the hurricane-intensity bin-specific mean forecast error.",
    "Each column shows estimates using a different number of bins to control for wind and precipitation intensity, ranging from 5 to 120 bins.",
    "Hurricane wind speeds are those greater than 33 m/s (74 mph), while Sub-Hurricane wind speeds fall below that threshold."
  ),
  threeparttable = TRUE,
  shape = 'rbind',
  escape = FALSE,
  output = "results/TableC12.tex"
)

add_adjust_box("results/TableC12.tex")

# ==============================================================================
# 10. POPULATION WEIGHTING
# ==============================================================================


regs = list()

regs[[1]] = conleyreg(
  total_cost ~ squared_error + squared_error_precip | 
    county + state^hurricane,
  data = load_df_popw(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs[[2]] = conleyreg(
  total_cost ~ squared_error + squared_error_precip | 
    county + state^hurricane + wind_cut_popw + precip_cut,
  data = load_df_popw(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs[[3]] = conleyreg(
  total_cost ~ squared_error + squared_error_precip + level_error + level_error_precip | 
    county + state^hurricane + wind_cut_popw + precip_cut,
  data = load_df_popw(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs[[4]] = conleyreg(
  total_cost ~ squared_error  + squared_error_precip + level_error + level_error_precip | 
    county + state^hurricane + county^month + wind_cut_popw + precip_cut,
  data = load_df_popw(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs[[5]] = conleyreg(
  total_cost ~ squared_error  + squared_error_precip + level_error + level_error_precip | 
    county + state^hurricane + county^year + wind_cut_popw + precip_cut,
  data = load_df_popw(),
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
    county^year + county^month + wind_cut_popw + precip_cut,
  data = load_df_popw(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs[[7]] = conleyreg(
  total_cost ~ squared_error:category + squared_error_precip + level_error + level_error_precip | 
    county + state^hurricane + wind_cut_popw + precip_cut,
  data = load_df_popw(),
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
  data = load_df_popw(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pc[[2]] = conleyreg(
  total_cost_pc ~ squared_error + squared_error_precip | 
    county + state^hurricane + wind_cut_popw + precip_cut,
  data = load_df_popw(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pc[[3]] = conleyreg(
  total_cost_pc ~ squared_error + squared_error_precip + level_error + level_error_precip | 
    county + state^hurricane + wind_cut_popw + precip_cut,
  data = load_df_popw(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pc[[4]] = conleyreg(
  total_cost_pc ~ squared_error  + squared_error_precip + level_error + level_error_precip | 
    county + state^hurricane + county^month + wind_cut_popw + precip_cut,
  data = load_df_popw(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pc[[5]] = conleyreg(
  total_cost_pc ~ squared_error  + squared_error_precip + level_error + level_error_precip | 
    county + state^hurricane + county^year + wind_cut_popw + precip_cut,
  data = load_df_popw(),
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
    county^year + county^month + wind_cut_popw + precip_cut,
  data = load_df_popw(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pc[[7]] = conleyreg(
  total_cost_pc ~ squared_error:category + squared_error_precip + level_error + level_error_precip | 
    county + state^hurricane + wind_cut_popw + precip_cut,
  data = load_df_popw(),
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
  data = load_df_popw(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pgdp[[2]] = conleyreg(
  total_cost_per_gdp*100 ~ squared_error + squared_error_precip | 
    county + state^hurricane + wind_cut_popw + precip_cut,
  data = load_df_popw(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pgdp[[3]] = conleyreg(
  total_cost_per_gdp*100 ~ squared_error + squared_error_precip + level_error + level_error_precip | 
    county + state^hurricane + wind_cut_popw + precip_cut,
  data = load_df_popw(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pgdp[[4]] = conleyreg(
  total_cost_per_gdp*100 ~ squared_error  + squared_error_precip + level_error + level_error_precip | 
    county + state^hurricane + county^month + wind_cut_popw + precip_cut,
  data = load_df_popw(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pgdp[[5]] = conleyreg(
  total_cost_per_gdp*100 ~ squared_error  + squared_error_precip + level_error + level_error_precip | 
    county + state^hurricane + county^year + wind_cut_popw + precip_cut,
  data = load_df_popw(),
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
    county^year + county^month + wind_cut_popw + precip_cut,
  data = load_df_popw(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pgdp[[7]] = conleyreg(
  total_cost_per_gdp*100 ~ squared_error:category + squared_error_precip + level_error + level_error_precip | 
    county + state^hurricane + wind_cut_popw + precip_cut,
  data = load_df_popw(),
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
    county^year + county^month + wind_cut_popw + precip_cut,
  data = load_df_popw()
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
         stars =  c('*' = .1, '**' = .05, '***' = .01),
         fmt = "%.2f",
         add_rows = rows,
         title = "The Value of a Wind Speed Forecast Improvement with Population-Weighted Wind Aggregation. \\label{atab:voi-table-popw}",
         notes = list("Wind speed forecasts and wind speed intensities are population-weighted within-county when constructing the county-level variables.
                       Standard errors are Conley Spatial HAC with a distance radius of 400 km for spatial correlation and arbitrary autocorrelation within counties. 
                       Estimates are conditional on an interaction between an indicator variable whether the county was forecast to be hit by hurricane-force winds, 
                       and an indicator variable whether the county was hit by hurricane-force winds. 
                       $e$ is the county-hurricane-specific forecast error, and $\\\\mu$ is the hurricane-intensity bin-specific mean forecast error. 
                       Hurricane wind speeds are those greater than 33 m/s (74 mph), while Sub-Hurricane wind speeds fall below that threshold."),
         threeparttable = TRUE,
         shape = 'rbind',
         escape = FALSE,
         output = "results/TableC13.tex"
)

add_adjust_box("results/TableC13.tex")

# ==============================================================================
# 11. WIND INTERACTION ANALYSIS
# ==============================================================================

run_interaction_and_margins_table = function(outcome_var, wind_obs_levels, data, controls) {
  # Build regression formula
  formula_str = paste0(outcome_var, " ~ squared_error*asinh(wind_obs) | ", controls)
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
  
  # Extract coefficients
  coefs = tidy(reg)
  beta_1 = coefs |> filter(term == "squared_error") |> pull(estimate)
  beta_3 = coefs |> filter(term == "squared_error:asinh(wind_obs)") |> pull(estimate)
  
  # Create list to store models for each wind speed
  models = list()
  
  for(w in wind_obs_levels) {
    # Calculate marginal effect
    asinh_w = asinh(w)
    marginal_effect = beta_1 + beta_3 * asinh_w
    
    # Calculate standard error using delta method
    idx_b1 = which(colnames(vcov_matrix) == "squared_error")
    idx_b3 = which(colnames(vcov_matrix) == "squared_error:asinh(wind_obs)")
    L = rep(0, ncol(vcov_matrix))
    L[idx_b1] = 1
    L[idx_b3] = asinh_w
    se = sqrt(as.numeric(t(L) %*% vcov_matrix %*% L))
    
    # Create modelsummary_list object
    ti = data.frame(
      term = "Marginal Effect",
      estimate = marginal_effect,
      std.error = se,
      statistic = marginal_effect/se,
      p.value = 2 * (1 - pnorm(abs(marginal_effect/se)))
    )
    
    gl = data.frame(
      nobs = nrow(data)
    )
    
    mod = list(tidy = ti, glance = gl)
    class(mod) = "modelsummary_list"
    
    models[[paste0(w, " m/s")]] = mod
  }
  
  return(models)
}

# Run regressions for each outcome
controls = "county + state^hurricane + wind_cut^precip_cut^coastal^left_of_track + precip_error_bin"
wind_obs_bins = c(18, 33, 43, 50, 58, 70)

# Get number of observations
obs = nrow(load_df()) |> format(big.mark = ",")

# Create models for each outcome
models_cost = run_interaction_and_margins_table("total_cost", wind_obs_bins, load_df(), controls)
models_cost_pc = run_interaction_and_margins_table("total_cost_pc", wind_obs_bins, load_df(), controls)
models_pgdp = run_interaction_and_margins_table("total_cost_per_gdp*100", wind_obs_bins, load_df(), controls)

# Create rows for the bottom of the table
rows = tribble(
  ~term, ~"18", ~"33", ~"43", ~"50", ~"58", ~"70",
  "", "", "", "", "", "", "",
  "Observations", obs, obs, obs, obs, obs, obs,
  "", "", "", "", "", "", "",
  "State-Hurricane FE", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark",
  "County FE", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark",
  "Wind-by-Precip-by-Coastal-by-West of Track Indicator", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark"
)

# Create the table
msummary(
  list(
    "Damages + Recovery Expenditures (million $)" = models_cost,
    "(Damages + Recovery Expenditures) / GDP (%)" = models_pgdp,
    "Damages + Recovery Expenditures Per Capita ($/person)" = models_cost_pc
  ),
  coef_rename = c("Marginal Effect" = "$\\beta_2: (e - \\mu)^2$"),
  gof_omit = ".*",
  stars = c('*' = .1, '**' = .05, '***' = .01),
  fmt = "%.2f",
  add_rows = rows,
  title = "Marginal Effect of Squared Forecast Error by Wind Intensity \\label{atab:voi-table-mfx}",
  notes = list(
    "* p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01",
    "Standard errors are Conley Spatial HAC with a distance radius of 400 km for spatial correlation and arbitrary autocorrelation within counties.",
    "$e$ is the county-hurricane-specific forecast error, and $\\\\mu$ is the hurricane-intensity bin-specific mean forecast error.",
    "Each column reports the marginal effect of squared forecast error $(e - \\\\mu)^2$ evaluated at different observed wind speeds, ranging from tropical storm (18 m/s) to major hurricane strength (70 m/s)."
  ),
  threeparttable = TRUE,
  escape = FALSE,
  shape = 'rbind',
  output = "results/TableC14.tex"
)

add_adjust_box("results/TableC14.tex")

# ==============================================================================
# 12. VOI TRACK FORECASTS
# ==============================================================================

# ==============================================================================
# 12a. Track Forecast Improvement with Wind and Precipitation Controls
# ==============================================================================

regs = list()

regs[[1]] = conleyreg(
  total_cost ~ squared_error + squared_error_precip + squared_error_track | 
    county + state^hurricane,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs[[2]] = conleyreg(
  total_cost ~ squared_error + squared_error_precip + squared_error_track | 
    county + state^hurricane + wind_cut + precip_cut + track_cut,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs[[3]] = conleyreg(
  total_cost ~ squared_error + squared_error_precip + squared_error_track + level_error + level_error_precip + level_error_track| 
    county + state^hurricane + wind_cut + precip_cut + track_cut,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs[[4]] = conleyreg(
  total_cost ~ squared_error + squared_error_precip + squared_error_track + level_error + level_error_precip + level_error_track | 
    county + state^hurricane + county^month + wind_cut + precip_cut + track_cut,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs[[5]] = conleyreg(
  total_cost ~ squared_error  + squared_error_precip + squared_error_track + level_error + level_error_precip + level_error_track | 
    county + state^hurricane + county^year + wind_cut + precip_cut + track_cut,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs[[6]] = conleyreg(
  total_cost ~ squared_error  + squared_error_precip + squared_error_track + level_error + level_error_precip + level_error_track | 
    county + state^hurricane +
    county^year + county^month + wind_cut + precip_cut + track_cut,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs[[7]] = conleyreg(
  total_cost ~ squared_error + squared_error_precip + squared_error_track:category + level_error + level_error_precip + level_error_track | 
    county + state^hurricane + wind_cut + precip_cut + track_cut,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pc = list()

regs_pc[[1]] = conleyreg(
  total_cost_pc ~ squared_error + squared_error_precip + squared_error_track | 
    county + state^hurricane,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pc[[2]] = conleyreg(
  total_cost_pc ~ squared_error + squared_error_precip + squared_error_track | 
    county + state^hurricane + wind_cut + precip_cut + track_cut,  
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pc[[3]] = conleyreg(
  total_cost_pc ~ squared_error + squared_error_precip + squared_error_track + level_error + level_error_precip + level_error_track | 
    county + state^hurricane + wind_cut + precip_cut + track_cut,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pc[[4]] = conleyreg(
  total_cost_pc ~ squared_error  + squared_error_precip + squared_error_track + level_error + level_error_precip + level_error_track | 
    county + state^hurricane + county^month + wind_cut + precip_cut + track_cut,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pc[[5]] = conleyreg(
  total_cost_pc ~ squared_error  + squared_error_precip + squared_error_track + level_error + level_error_precip + level_error_track | 
    county + state^hurricane + county^year + wind_cut + precip_cut + track_cut,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pc[[6]] = conleyreg(
  total_cost_pc ~ squared_error  + squared_error_precip + squared_error_track + level_error + level_error_precip + level_error_track | 
    county + state^hurricane +
    county^year + county^month + wind_cut + precip_cut + track_cut,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pc[[7]] = conleyreg(
  total_cost_pc ~ squared_error + squared_error_precip + squared_error_track:category + level_error + level_error_precip + level_error_track | 
    county + state^hurricane + wind_cut + precip_cut + track_cut,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pgdp = list()

regs_pgdp[[1]] = conleyreg(
  total_cost_per_gdp*100 ~ squared_error + squared_error_precip + squared_error_track | 
    county + state^hurricane,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pgdp[[2]] = conleyreg(
  total_cost_per_gdp*100 ~ squared_error + squared_error_precip + squared_error_track | 
    county + state^hurricane + wind_cut + precip_cut + track_cut,
  data = load_df_track (),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pgdp[[3]] = conleyreg(
  total_cost_per_gdp*100 ~ squared_error + squared_error_precip + squared_error_track + level_error + level_error_precip + level_error_track | 
    county + state^hurricane + wind_cut + precip_cut + track_cut,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pgdp[[4]] = conleyreg(
  total_cost_per_gdp*100 ~ squared_error  + squared_error_precip + squared_error_track + level_error + level_error_precip + level_error_track | 
    county + state^hurricane + county^month + wind_cut + precip_cut + track_cut,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pgdp[[5]] = conleyreg(
  total_cost_per_gdp*100 ~ squared_error  + squared_error_precip + squared_error_track + level_error + level_error_precip + level_error_track | 
    county + state^hurricane + county^year + wind_cut + precip_cut + track_cut,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pgdp[[6]] = conleyreg(
  total_cost_per_gdp*100 ~ squared_error  + squared_error_precip + squared_error_track + level_error + level_error_precip + level_error_track | 
    county + state^hurricane +
    county^year + county^month + wind_cut + precip_cut + track_cut,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pgdp[[7]] = conleyreg(
  total_cost_per_gdp*100 ~ squared_error + squared_error_precip + squared_error_track:category + level_error + level_error_precip + level_error_track | 
    county + state^hurricane + wind_cut + precip_cut + track_cut,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

obs = feols(
  total_cost ~ squared_error  + squared_error_precip | 
    county + state^hurricane +
    county^year + county^month + wind_cut + precip_cut + track_cut,
  data = load_df_track(),
)$nobs |>
  format(big.mark = ",")

rows = tribble(
  ~term, ~"(1)", ~"(2)", ~"(3)", ~"(4)", ~"(5)", ~"(6)", ~"(7)",
  "", "", "", "", "", "", "", "", 
  "Observations", obs, obs, obs, obs, obs, obs, obs, 
  "", "", "", "", "", "", "", "", 
  "Realized Wind/Precip/Track Bins", "", "\\checkmark", "\\checkmark", "\\checkmark",  "\\checkmark", "\\checkmark", "\\checkmark",
  "Level Wind/Precip/Track Error", "", "", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", 
  "State-Hurricane FE", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", 
  "County FE", "\\checkmark", "\\checkmark", "\\checkmark",  "", "",  "", "\\checkmark",
  "County-Month of Year FE", "", "", "", "\\checkmark", "", "\\checkmark", "",
  "County-Year FE", "", "", "", "",  "\\checkmark", "\\checkmark", ""
)

msummary(list("Damages + Recovery Expenditures (million $)" = regs, "(Damages + Recovery Expenditures) / GDP (%)" = regs_pgdp, "Damages + Recovery Expenditures Per Capita ($/person)" = regs_pc),
         coef_rename = c("$\\beta_2: (e - \\mu)^2$", "$\\text{Hurricane } \\beta_2: (e - \\mu)^2$", "$\\text{Sub-Hurricane } \\beta_2: (e - \\mu)^2$"),
         coef_omit = c(-3, -7, -8),
         gof_omit = "R2|AIC|BIC|Log.|RMSE|FE|Std.Errors",
         stars =  c('*' = .1, '**' = .05, '***' = .01),
         fmt = "%.3f",
         add_rows = rows,
         title = "The Value of a Track Distance Forecast Improvement Conditional on Wind and Precipitation. \\label{atab:voi-table-side-track}",
         notes = list("Standard errors are Conley Spatial HAC with a distance radius of 400 km for spatial correlation and arbitrary autocorrelation within counties. 
                      $e$ is the county-hurricane-specific forecast error, and $\\\\mu$ is the hurricane-intensity bin-specific mean forecast error. 
                      Hurricane wind speeds are those greater than 33 m/s (74 mph), while Sub-Hurricane wind speeds fall below that threshold."),
         threeparttable = TRUE,
         shape = 'rbind',
         escape = FALSE,
         output = "results/TableC15.tex"
)

add_adjust_box("results/TableC15.tex")

# ==============================================================================
# 12b. Track Forecast Improvement Unconditional
# ==============================================================================

regs = list()

regs[[1]] = conleyreg(
  total_cost ~  squared_error_track | 
    county + state^hurricane,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs[[2]] = conleyreg(
  total_cost ~  squared_error_track | 
    county + state^hurricane + track_cut,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs[[3]] = conleyreg(
  total_cost ~  squared_error_track + level_error_track| 
    county + state^hurricane + track_cut,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs[[4]] = conleyreg(
  total_cost ~  squared_error_track + level_error_track | 
    county + state^hurricane + county^month + track_cut,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs[[5]] = conleyreg(
  total_cost ~ squared_error_track + level_error_track | 
    county + state^hurricane + county^year + track_cut,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs[[6]] = conleyreg(
  total_cost ~ squared_error_track + level_error_track | 
    county + state^hurricane +
    county^year + county^month + track_cut,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs[[7]] = conleyreg(
  total_cost ~  squared_error_track:category + level_error_track | 
    county + state^hurricane + track_cut,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pc = list()

regs_pc[[1]] = conleyreg(
  total_cost_pc ~  squared_error_track | 
    county + state^hurricane,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pc[[2]] = conleyreg(
  total_cost_pc ~  squared_error_track | 
    county + state^hurricane + track_cut,  
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pc[[3]] = conleyreg(
  total_cost_pc ~  squared_error_track + level_error_track | 
    county + state^hurricane + track_cut,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pc[[4]] = conleyreg(
  total_cost_pc ~ squared_error_track + level_error_track | 
    county + state^hurricane + county^month + track_cut,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pc[[5]] = conleyreg(
  total_cost_pc ~ squared_error_track + level_error_track | 
    county + state^hurricane + county^year + track_cut,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pc[[6]] = conleyreg(
  total_cost_pc ~ squared_error_track + level_error_track | 
    county + state^hurricane +
    county^year + county^month + track_cut,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pc[[7]] = conleyreg(
  total_cost_pc ~  squared_error_track:category + level_error_track | 
    county + state^hurricane + track_cut,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pgdp = list()

regs_pgdp[[1]] = conleyreg(
  total_cost_per_gdp*100 ~  squared_error_track | 
    county + state^hurricane,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pgdp[[2]] = conleyreg(
  total_cost_per_gdp*100 ~  squared_error_track | 
    county + state^hurricane + track_cut,
  data = load_df_track (),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pgdp[[3]] = conleyreg(
  total_cost_per_gdp*100 ~  squared_error_track + level_error_track | 
    county + state^hurricane + track_cut,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pgdp[[4]] = conleyreg(
  total_cost_per_gdp*100 ~ squared_error_track + level_error_track | 
    county + state^hurricane + county^month + track_cut,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pgdp[[5]] = conleyreg(
  total_cost_per_gdp*100 ~ squared_error_track + level_error_track | 
    county + state^hurricane + county^year + track_cut,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pgdp[[6]] = conleyreg(
  total_cost_per_gdp*100 ~ squared_error_track + level_error_track | 
    county + state^hurricane +
    county^year + county^month + track_cut,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pgdp[[7]] = conleyreg(
  total_cost_per_gdp*100 ~  squared_error_track:category + level_error_track | 
    county + state^hurricane + track_cut,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

obs = feols(
  total_cost ~ squared_error_track | 
    county + state^hurricane +
    county^year + county^month + track_cut,
  data = load_df_track(),
)$nobs |>
  format(big.mark = ",")

rows = tribble(
  ~term, ~"(1)", ~"(2)", ~"(3)", ~"(4)", ~"(5)", ~"(6)", ~"(7)",
  "", "", "", "", "", "", "", "", 
  "Observations", obs, obs, obs, obs, obs, obs, obs, 
  "", "", "", "", "", "", "", "", 
  "Realized Wind/Precip/Track Bins", "", "\\checkmark", "\\checkmark", "\\checkmark",  "\\checkmark", "\\checkmark", "\\checkmark",
  "Level Wind/Precip/Track Error", "", "", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", 
  "State-Hurricane FE", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", 
  "County FE", "\\checkmark", "\\checkmark", "\\checkmark",  "", "",  "", "\\checkmark",
  "County-Month of Year FE", "", "", "", "\\checkmark", "", "\\checkmark", "",
  "County-Year FE", "", "", "", "",  "\\checkmark", "\\checkmark", ""
)

msummary(list("Damages + Recovery Expenditures (million $)" = regs, "(Damages + Recovery Expenditures) / GDP (%)" = regs_pgdp, "Damages + Recovery Expenditures Per Capita ($/person)" = regs_pc),
         coef_rename = c("$\\beta_2: (e - \\mu)^2$", "$\\text{Hurricane } \\beta_2: (e - \\mu)^2$", "$\\text{Sub-Hurricane } \\beta_2: (e - \\mu)^2$"),
         coef_omit = c(-1, -3, -4),
         gof_omit = "R2|AIC|BIC|Log.|RMSE|FE|Std.Errors",
         stars =  c('*' = .1, '**' = .05, '***' = .01),
         fmt = "%.3f",
         add_rows = rows,
         title = "The Unconditional Value of a Track Distance Forecast Improvement. \\label{atab:voi-table-side-track-uncond}",
         notes = list("Standard errors are Conley Spatial HAC with a distance radius of 400 km for spatial correlation and arbitrary autocorrelation within counties. 
                      $e$ is the county-hurricane-specific forecast error, and $\\\\mu$ is the hurricane-intensity bin-specific mean forecast error. 
                      Hurricane wind speeds are those greater than 33 m/s (74 mph), while Sub-Hurricane wind speeds fall below that threshold."),
         threeparttable = TRUE,
         shape = 'rbind',
         escape = FALSE,
         output = "results/TableC16.tex"
)

add_adjust_box("results/TableC16.tex")

# ==============================================================================
# 12c. Track Forecast Improvement Conditional on Wind + Precip - ROBUSTNESS CUTOFF
# ==============================================================================

reg_list <- list()
reg_list_pc <- list()
reg_list_pgdp <- list()
obs_vec <- c()

for (cutoff in cutoffs) {
  reg_list[[as.character(cutoff)]] <- conleyreg(
    total_cost ~ squared_error + squared_error_precip + squared_error_track:category + level_error + level_error_precip + level_error_track | 
      county + state^hurricane + wind_cut + precip_cut + track_cut,
    data = load_df_track(cutoff),
    dist_cutoff = 400,
    lag_cutoff = Inf,
    unit = "county",
    time = "date",
    lat = "lat",
    lon = "lon", batch_ram_opt = "none"
  )
  reg_list_pc[[as.character(cutoff)]] <- conleyreg(
    total_cost_pc ~ squared_error + squared_error_precip + squared_error_track:category + level_error + level_error_precip + level_error_track |
      county + state^hurricane + wind_cut + precip_cut + track_cut,
    data = load_df_track(cutoff),
    dist_cutoff = 400,
    lag_cutoff = Inf,
    unit = "county",
    time = "date",
    lat = "lat",
    lon = "lon", batch_ram_opt = "none"
  )
  reg_list_pgdp[[as.character(cutoff)]] <- conleyreg(
    total_cost_per_gdp*100 ~ squared_error + squared_error_precip + squared_error_track:category + level_error + level_error_precip + level_error_track |
      county + state^hurricane + wind_cut + precip_cut + track_cut,
    data = load_df_track(cutoff),
    dist_cutoff = 400,
    lag_cutoff = Inf,
    unit = "county",
    time = "date",
    lat = "lat",
    lon = "lon", batch_ram_opt = "none"
  )
  obs_vec[as.character(cutoff)] <- nrow(load_df_track(cutoff))
}

obs_vec <- format(as.numeric(obs_vec), big.mark = ",")

rows = tribble(
  ~term, ~"400 km", ~"500 km", ~"600 km", ~"700 km", ~"800 km",
  "", "", "", "", "", "",
  "Observations", obs_vec[1], obs_vec[2], obs_vec[3], obs_vec[4], obs_vec[5],
  "", "", "", "", "", "",
  "Realized Wind/Precip/Track Bins", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark",
  "Level Wind/Precip/Track Error", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark",
  "State-Hurricane FE", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark",
  "County FE", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark"
)

msummary(
  list(
    "Damages + Recovery Expenditures (million $)" = reg_list,
    "(Damages + Recovery Expenditures) / GDP (%)" = reg_list_pgdp,
    "Damages + Recovery Expenditures Per Capita ($/person)" = reg_list_pc
  ),
  coef_rename = c("$\\text{Hurricane } \\beta_2: (e - \\mu)^2$", "$\\text{Sub-Hurricane } \\beta_2: (e - \\mu)^2$"),
  col.names = c("", "400 km", "500 km", "600 km", "700 km", "800 km"),
  coef_omit = c(-6,-7),
  gof_omit = "R2|AIC|BIC|Log.|RMSE|FE|Std.Errors",
  stars =  c('*' = .1, '**' = .05, '***' = .01),
  fmt = "%.3f",
  add_rows = rows,
  title = "The Value of a Track Distance Forecast Improvement Conditional on Wind and Precipitation: Cutoff Robustness. \\label{atab:voi-table-side-track-robust}",
  notes = list("Standard errors are Conley Spatial HAC with a distance radius of 400 km for spatial correlation and arbitrary autocorrelation within counties.  $e$ is forecast error per county for a given hurricane, $\\\\mu$ is the mean forecast error per hurricane/hurricane-intensity bin.  Hurricane wind speeds are those greater than 33 m/s (74 mph), while Sub-Hurricane wind speeds fall below that threshold. Each column corresponds to a different distance cutoff (in kilometers) used to define the maximum allowable distance between county centroids and the hurricane track."),
  threeparttable = TRUE,
  shape = 'rbind',
  escape = FALSE,
  output = "results/TableC17.tex")

add_adjust_box("results/TableC17.tex")

# ==============================================================================
# 12d. Track Forecast Improvement Unconditional - ROBUSTNESS CUTOFF
# ==============================================================================

reg_list <- list()
reg_list_pc <- list()
reg_list_pgdp <- list()
obs_vec <- c()

for (cutoff in cutoffs) {
  reg_list[[as.character(cutoff)]] <- conleyreg(
    total_cost ~ squared_error_track:category + level_error_track |
      county + state^hurricane + track_cut,
    data = load_df_track(cutoff),
    dist_cutoff = 400,
    lag_cutoff = Inf,
    unit = "county",
    time = "date",
    lat = "lat",
    lon = "lon", batch_ram_opt = "none"
  )
  reg_list_pc[[as.character(cutoff)]] <- conleyreg(
    total_cost_pc ~ squared_error_track:category + level_error_track |
      county + state^hurricane + track_cut,
    data = load_df_track(cutoff),
    dist_cutoff = 400,
    lag_cutoff = Inf,
    unit = "county",
    time = "date",
    lat = "lat",
    lon = "lon", batch_ram_opt = "none"
  )
  reg_list_pgdp[[as.character(cutoff)]] <- conleyreg(
    total_cost_per_gdp*100 ~ squared_error_track:category + level_error_track |
      county + state^hurricane + track_cut,
    data = load_df_track(cutoff),
    dist_cutoff = 400,
    lag_cutoff = Inf,
    unit = "county",
    time = "date",
    lat = "lat",
    lon = "lon", batch_ram_opt = "none"
  )
  
  obs_vec[as.character(cutoff)] <- nrow(load_df_track(cutoff))
}

obs_vec <- format(as.numeric(obs_vec), big.mark = ",")

rows = tribble(
  ~term, ~"400 km", ~"500 km", ~"600 km", ~"700 km", ~"800 km",
  "", "", "", "", "", "",
  "Observations", obs_vec[1], obs_vec[2], obs_vec[3], obs_vec[4], obs_vec[5],
  "", "", "", "", "", "",
  "Realized Wind/Precip/Track Bins", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark",
  "Level Wind/Precip/Track Error", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark",
  "State-Hurricane FE", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark",
  "County FE", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark"
)

msummary(
  list(
    "Damages + Recovery Expenditures (million $)" = reg_list,
    "(Damages + Recovery Expenditures) / GDP (%)" = reg_list_pgdp,
    "Damages + Recovery Expenditures Per Capita ($/person)" = reg_list_pc
  ),
  coef_rename = c("$\\text{Hurricane } \\beta_2: (e - \\mu)^2$", "$\\text{Sub-Hurricane } \\beta_2: (e - \\mu)^2$"),
  col.names = c("", "400 km", "500 km", "600 km", "700 km", "800 km"),
  coef_omit = c(-2,-3),
  gof_omit = "R2|AIC|BIC|Log.|RMSE|FE|Std.Errors",
  stars =  c('*' = .1, '**' = .05, '***' = .01),
  fmt = "%.3f",
  add_rows = rows,
  title = "The Unconditional Value of a Track Distance Forecast Improvement: Cutoff Robustness. \\label{atab:voi-table-side-track-uncond-robust}",
  notes = list("Standard errors are Conley Spatial HAC with a distance radius of 400 km for spatial correlation and arbitrary autocorrelation within counties.  $e$ is forecast error per county for a given hurricane, $\\\\mu$ is the mean forecast error per hurricane/hurricane-intensity bin.  Hurricane wind speeds are those greater than 33 m/s (74 mph), while Sub-Hurricane wind speeds fall below that threshold. Each column corresponds to a different distance cutoff (in kilometers) used to define the maximum allowable distance between county centroids and the hurricane track."),
  threeparttable = TRUE,
  shape = 'rbind',
  escape = FALSE, 
  output = "results/TableC18.tex")

add_adjust_box("results/TableC18.tex")

# ==============================================================================
# 13. VOI WIND CONDITIONAL ON TRACK
# ==============================================================================

regs = list()

regs[[1]] = conleyreg(
  total_cost ~ squared_error + squared_error_precip + squared_error_track | 
    county + state^hurricane,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs[[2]] = conleyreg(
  total_cost ~ squared_error + squared_error_precip + squared_error_track | 
    county + state^hurricane + wind_cut + precip_cut + track_cut,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs[[3]] = conleyreg(
  total_cost ~ squared_error + squared_error_precip + squared_error_track + level_error + level_error_precip + level_error_track| 
    county + state^hurricane + wind_cut + precip_cut + track_cut,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs[[4]] = conleyreg(
  total_cost ~ squared_error + squared_error_precip + squared_error_track + level_error + level_error_precip + level_error_track | 
    county + state^hurricane + county^month + wind_cut + precip_cut + track_cut,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs[[5]] = conleyreg(
  total_cost ~ squared_error  + squared_error_precip + squared_error_track + level_error + level_error_precip + level_error_track | 
    county + state^hurricane + county^year + wind_cut + precip_cut + track_cut,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs[[6]] = conleyreg(
  total_cost ~ squared_error  + squared_error_precip + squared_error_track + level_error + level_error_precip + level_error_track | 
    county + state^hurricane +
    county^year + county^month + wind_cut + precip_cut + track_cut,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs[[7]] = conleyreg(
  total_cost ~ squared_error:category + squared_error_precip + squared_error_track + level_error + level_error_precip + level_error_track | 
    county + state^hurricane + wind_cut + precip_cut + track_cut,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pc = list()

regs_pc[[1]] = conleyreg(
  total_cost_pc ~ squared_error + squared_error_precip + squared_error_track | 
    county + state^hurricane,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pc[[2]] = conleyreg(
  total_cost_pc ~ squared_error + squared_error_precip + squared_error_track | 
    county + state^hurricane + wind_cut + precip_cut + track_cut,  
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pc[[3]] = conleyreg(
  total_cost_pc ~ squared_error + squared_error_precip + squared_error_track + level_error + level_error_precip + level_error_track | 
    county + state^hurricane + wind_cut + precip_cut + track_cut,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pc[[4]] = conleyreg(
  total_cost_pc ~ squared_error  + squared_error_precip + squared_error_track + level_error + level_error_precip + level_error_track | 
    county + state^hurricane + county^month + wind_cut + precip_cut + track_cut,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pc[[5]] = conleyreg(
  total_cost_pc ~ squared_error  + squared_error_precip + squared_error_track + level_error + level_error_precip + level_error_track | 
    county + state^hurricane + county^year + wind_cut + precip_cut + track_cut,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pc[[6]] = conleyreg(
  total_cost_pc ~ squared_error  + squared_error_precip + squared_error_track + level_error + level_error_precip + level_error_track | 
    county + state^hurricane +
    county^year + county^month + wind_cut + precip_cut + track_cut,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pc[[7]] = conleyreg(
  total_cost_pc ~ squared_error:category + squared_error_precip + squared_error_track + level_error + level_error_precip + level_error_track | 
    county + state^hurricane + wind_cut + precip_cut + track_cut,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pgdp = list()

regs_pgdp[[1]] = conleyreg(
  total_cost_per_gdp*100 ~ squared_error + squared_error_precip + squared_error_track | 
    county + state^hurricane,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pgdp[[2]] = conleyreg(
  total_cost_per_gdp*100 ~ squared_error + squared_error_precip + squared_error_track | 
    county + state^hurricane + wind_cut + precip_cut + track_cut,
  data = load_df_track (),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pgdp[[3]] = conleyreg(
  total_cost_per_gdp*100 ~ squared_error + squared_error_precip + squared_error_track + level_error + level_error_precip + level_error_track | 
    county + state^hurricane + wind_cut + precip_cut + track_cut,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pgdp[[4]] = conleyreg(
  total_cost_per_gdp*100 ~ squared_error  + squared_error_precip + squared_error_track + level_error + level_error_precip + level_error_track | 
    county + state^hurricane + county^month + wind_cut + precip_cut + track_cut,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pgdp[[5]] = conleyreg(
  total_cost_per_gdp*100 ~ squared_error  + squared_error_precip + squared_error_track + level_error + level_error_precip + level_error_track | 
    county + state^hurricane + county^year + wind_cut + precip_cut + track_cut,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pgdp[[6]] = conleyreg(
  total_cost_per_gdp*100 ~ squared_error  + squared_error_precip + squared_error_track + level_error + level_error_precip + level_error_track | 
    county + state^hurricane +
    county^year + county^month + wind_cut + precip_cut + track_cut,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

regs_pgdp[[7]] = conleyreg(
  total_cost_per_gdp*100 ~ squared_error:category + squared_error_precip + squared_error_track + level_error + level_error_precip + level_error_track | 
    county + state^hurricane + wind_cut + precip_cut + track_cut,
  data = load_df_track(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "date",
  lat = "lat",
  lon = "lon", batch_ram_opt = "none"
)

obs = feols(
  total_cost ~ squared_error  + squared_error_precip | 
    county + state^hurricane +
    county^year + county^month + wind_cut + precip_cut + track_cut,
  data = load_df_track(),
)$nobs |>
  format(big.mark = ",")

rows = tribble(
  ~term, ~"(1)", ~"(2)", ~"(3)", ~"(4)", ~"(5)", ~"(6)", ~"(7)",
  "", "", "", "", "", "", "", "", 
  "Observations", obs, obs, obs, obs, obs, obs, obs, 
  "", "", "", "", "", "", "", "", 
  "Realized Wind/Precip/Track Bins", "", "\\checkmark", "\\checkmark", "\\checkmark",  "\\checkmark", "\\checkmark", "\\checkmark",
  "Level Wind/Precip/Track Error", "", "", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", 
  "State-Hurricane FE", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", 
  "County FE", "\\checkmark", "\\checkmark", "\\checkmark",  "", "",  "", "\\checkmark",
  "County-Month of Year FE", "", "", "", "\\checkmark", "", "\\checkmark", "",
  "County-Year FE", "", "", "", "",  "\\checkmark", "\\checkmark", ""
)

msummary(list("Damages + Recovery Expenditures (million $)" = regs, "(Damages + Recovery Expenditures) / GDP (%)" = regs_pgdp, "Damages + Recovery Expenditures Per Capita ($/person)" = regs_pc),
         coef_rename = c("$\\beta_2: (e - \\mu)^2$", "$\\text{Hurricane } \\beta_2: (e - \\mu)^2$", "$\\text{Sub-Hurricane } \\beta_2: (e - \\mu)^2$"),
         coef_omit = c(-1, -7, -8),
         gof_omit = "R2|AIC|BIC|Log.|RMSE|FE|Std.Errors",
         stars =  c('*' = .1, '**' = .05, '***' = .01),
         fmt = "%.2f",
         add_rows = rows,
         title = "The Value of a Wind Speed Forecast Improvement with Track Controls. \\label{atab:voi-table-track}",
         notes = list("Standard errors are Conley Spatial HAC with a distance radius of 400 km for spatial correlation and arbitrary autocorrelation within counties. 
                      $e$ is forecast error per county for a given hurricanes, $\\\\mu$ is the mean forecast error per hurricane/hurricane-intensity bin. 
                      Hurricane wind speeds are those greater than 33 m/s (74 mph), while Sub-Hurricane wind speeds fall below that threshold."),
         threeparttable = TRUE,
         shape = 'rbind',
         escape = FALSE, 
         output = "results/TableC19.tex"
)

add_adjust_box("results/TableC19.tex")

# ==============================================================================
# 14. NORMALIZATION ANALYSIS
# ==============================================================================

# ==============================================================================
# 14a. Global Normalization (across full sample)
# ==============================================================================

regs = list()

regs[[1]] = conleyreg(
  total_cost ~ squared_error_norm + squared_error_precip | 
    county + state^hurricane,
  data = load_df_normalized_global(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon"
)

regs[[2]] = conleyreg(
  total_cost ~ squared_error_norm + squared_error_precip | 
    county + state^hurricane + wind_cut + precip_cut,
  data = load_df_normalized_global(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon"
)

regs[[3]] = conleyreg(
  total_cost ~ squared_error_norm + squared_error_precip + 
    level_error_norm + level_error_precip | 
    county + state^hurricane + wind_cut + precip_cut,
  data = load_df_normalized_global(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon"
)

regs[[4]] = conleyreg(
  total_cost ~ squared_error_norm + squared_error_precip + 
    level_error_norm + level_error_precip | 
    county + state^hurricane + county^month + wind_cut + precip_cut,
  data = load_df_normalized_global(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon"
)

regs[[5]] = conleyreg(
  total_cost ~ squared_error_norm + squared_error_precip + 
    level_error_norm + level_error_precip | 
    county + state^hurricane + county^year + wind_cut + precip_cut,
  data = load_df_normalized_global(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon"
)

regs[[6]] = conleyreg(
  total_cost ~ squared_error_norm + squared_error_precip + 
    level_error_norm + level_error_precip | 
    county + state^hurricane + county^year + county^month + wind_cut + precip_cut,
  data = load_df_normalized_global(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon"
)

regs[[7]] = conleyreg(
  total_cost ~ squared_error_norm:category + squared_error_precip + 
    level_error_norm + level_error_precip | 
    county + state^hurricane + wind_cut + precip_cut,
  data = load_df_normalized_global(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon"
)

# Panel B: (Damages + Recovery Expenditures)/GDP (%)
regs_pgdp = list()

regs_pgdp[[1]] = conleyreg(
  total_cost_per_gdp*100 ~ squared_error_norm + squared_error_precip | 
    county + state^hurricane,
  data = load_df_normalized_global(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon"
)

regs_pgdp[[2]] = conleyreg(
  total_cost_per_gdp*100 ~ squared_error_norm + squared_error_precip | 
    county + state^hurricane + wind_cut + precip_cut,
  data = load_df_normalized_global(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon"
)

regs_pgdp[[3]] = conleyreg(
  total_cost_per_gdp*100 ~ squared_error_norm + squared_error_precip + 
    level_error_norm + level_error_precip | 
    county + state^hurricane + wind_cut + precip_cut,
  data = load_df_normalized_global(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon"
)

regs_pgdp[[4]] = conleyreg(
  total_cost_per_gdp*100 ~ squared_error_norm + squared_error_precip + 
    level_error_norm + level_error_precip | 
    county + state^hurricane + county^month + wind_cut + precip_cut,
  data = load_df_normalized_global(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon"
)

regs_pgdp[[5]] = conleyreg(
  total_cost_per_gdp*100 ~ squared_error_norm + squared_error_precip + 
    level_error_norm + level_error_precip | 
    county + state^hurricane + county^year + wind_cut + precip_cut,
  data = load_df_normalized_global(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon"
)

regs_pgdp[[6]] = conleyreg(
  total_cost_per_gdp*100 ~ squared_error_norm + squared_error_precip + 
    level_error_norm + level_error_precip | 
    county + state^hurricane + county^year + county^month + wind_cut + precip_cut,
  data = load_df_normalized_global(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon"
)

regs_pgdp[[7]] = conleyreg(
  total_cost_per_gdp*100 ~ squared_error_norm:category + squared_error_precip + 
    level_error_norm + level_error_precip | 
    county + state^hurricane + wind_cut + precip_cut,
  data = load_df_normalized_global(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon"
)

# Panel C: Damages + Recovery Expenditures Per Capita ($/person)
regs_pc = list()

regs_pc[[1]] = conleyreg(
  total_cost_pc ~ squared_error_norm + squared_error_precip | 
    county + state^hurricane,
  data = load_df_normalized_global(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon"
)

regs_pc[[2]] = conleyreg(
  total_cost_pc ~ squared_error_norm + squared_error_precip | 
    county + state^hurricane + wind_cut + precip_cut,
  data = load_df_normalized_global(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon"
)

regs_pc[[3]] = conleyreg(
  total_cost_pc ~ squared_error_norm + squared_error_precip + 
    level_error_norm + level_error_precip | 
    county + state^hurricane + wind_cut + precip_cut,
  data = load_df_normalized_global(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon"
)

regs_pc[[4]] = conleyreg(
  total_cost_pc ~ squared_error_norm + squared_error_precip + 
    level_error_norm + level_error_precip | 
    county + state^hurricane + county^month + wind_cut + precip_cut,
  data = load_df_normalized_global(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon"
)

regs_pc[[5]] = conleyreg(
  total_cost_pc ~ squared_error_norm + squared_error_precip + 
    level_error_norm + level_error_precip | 
    county + state^hurricane + county^year + wind_cut + precip_cut,
  data = load_df_normalized_global(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon"
)

regs_pc[[6]] = conleyreg(
  total_cost_pc ~ squared_error_norm + squared_error_precip + 
    level_error_norm + level_error_precip | 
    county + state^hurricane + county^year + county^month + wind_cut + precip_cut,
  data = load_df_normalized_global(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon"
)

regs_pc[[7]] = conleyreg(
  total_cost_pc ~ squared_error_norm:category + squared_error_precip + 
    level_error_norm + level_error_precip | 
    county + state^hurricane + wind_cut + precip_cut,
  data = load_df_normalized_global(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon"
)

# Get observation count
obs = feols(
  total_cost ~ squared_error_norm + squared_error_precip | 
    county + state^hurricane + county^year + county^month + wind_cut + precip_cut,
  data = load_df_normalized_global(),
)$nobs |>
  format(big.mark = ",")

# Create rows for table footer
rows = tribble(
  ~term, ~"(1)", ~"(2)", ~"(3)", ~"(4)", ~"(5)", ~"(6)", ~"(7)",
  "", "", "", "", "", "", "", "",
  "Observations", obs, obs, obs, obs, obs, obs, obs,
  "", "", "", "", "", "", "", "",
  "Realized Wind/Precip Bins", "", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark",
  "Level Wind/Precip Error", "", "", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark",
  "State-Hurricane FE", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark",
  "County FE", "\\checkmark", "\\checkmark", "\\checkmark", "", "", "", "\\checkmark",
  "County-Month of Year FE", "", "", "", "\\checkmark", "", "\\checkmark", "",
  "County-Year FE", "", "", "", "", "\\checkmark", "\\checkmark", ""
)

# Generate table
msummary(
  list(
    "Damages + Recovery Expenditures (million $)" = regs,
    "(Damages + Recovery Expenditures) / GDP (%)" = regs_pgdp,
    "Damages + Recovery Expenditures Per Capita ($/person)" = regs_pc
  ),
  coef_rename = c(
    "$\\beta_2: (e - \\mu)^2$",
    "$\\text{Hurricane } \\beta_2: (e - \\mu)^2$",
    "$\\text{Sub-Hurricane } \\beta_2: (e - \\mu)^2$"
  ),
  coef_omit = c(-1, -5, -6),
  gof_omit = "R2|AIC|BIC|Log.|RMSE|FE|Std.Errors",
  stars = c('*' = .1, '**' = .05, '***' = .01),
  fmt = "%.2f",
  add_rows = rows,
  title = "The Value of a Wind Speed Forecast Improvement: Global Inverse Normal Transformation. \\label{atab:voi-table-norm-global}",
  notes = list("Standard errors are Conley Spatial HAC with a distance radius of 400 km for spatial correlation and arbitrary autocorrelation within counties. 
               Forecast errors are normalized across hurricanes. 
               $e$ is the county-hurricane-specific forecast error, and $\\\\mu$ is the hurricane-intensity bin-specific mean forecast error. 
               Hurricane wind speeds are those greater than 33 m/s (74 mph), while Sub-Hurricane wind speeds fall below that threshold."),
  threeparttable = TRUE,
  shape = 'rbind',
  escape = FALSE,
  output = "results/TableC20.tex"
)

add_adjust_box("results/TableC20.tex")

# ==============================================================================
# 14b. Hurricane-Specific Normalization (within each hurricane)
# ==============================================================================

regs = list()

regs[[1]] = conleyreg(
  total_cost ~ squared_error_norm + squared_error_precip | 
    county + state^hurricane,
  data = load_df_normalized(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon"
)

regs[[2]] = conleyreg(
  total_cost ~ squared_error_norm + squared_error_precip | 
    county + state^hurricane + wind_cut + precip_cut,
  data = load_df_normalized(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon"
)

regs[[3]] = conleyreg(
  total_cost ~ squared_error_norm + squared_error_precip + 
    level_error_norm + level_error_precip | 
    county + state^hurricane + wind_cut + precip_cut,
  data = load_df_normalized(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon"
)

regs[[4]] = conleyreg(
  total_cost ~ squared_error_norm + squared_error_precip + 
    level_error_norm + level_error_precip | 
    county + state^hurricane + county^month + wind_cut + precip_cut,
  data = load_df_normalized(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon"
)

regs[[5]] = conleyreg(
  total_cost ~ squared_error_norm + squared_error_precip + 
    level_error_norm + level_error_precip | 
    county + state^hurricane + county^year + wind_cut + precip_cut,
  data = load_df_normalized(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon"
)

regs[[6]] = conleyreg(
  total_cost ~ squared_error_norm + squared_error_precip + 
    level_error_norm + level_error_precip | 
    county + state^hurricane + county^year + county^month + wind_cut + precip_cut,
  data = load_df_normalized(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon"
)

regs[[7]] = conleyreg(
  total_cost ~ squared_error_norm:category + squared_error_precip + 
    level_error_norm + level_error_precip | 
    county + state^hurricane + wind_cut + precip_cut,
  data = load_df_normalized(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon"
)

# Panel B: (Damages + Recovery Expenditures)/GDP (%)
regs_pgdp = list()

regs_pgdp[[1]] = conleyreg(
  total_cost_per_gdp*100 ~ squared_error_norm + squared_error_precip | 
    county + state^hurricane,
  data = load_df_normalized(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon"
)

regs_pgdp[[2]] = conleyreg(
  total_cost_per_gdp*100 ~ squared_error_norm + squared_error_precip | 
    county + state^hurricane + wind_cut + precip_cut,
  data = load_df_normalized(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon"
)

regs_pgdp[[3]] = conleyreg(
  total_cost_per_gdp*100 ~ squared_error_norm + squared_error_precip + 
    level_error_norm + level_error_precip | 
    county + state^hurricane + wind_cut + precip_cut,
  data = load_df_normalized(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon"
)

regs_pgdp[[4]] = conleyreg(
  total_cost_per_gdp*100 ~ squared_error_norm + squared_error_precip + 
    level_error_norm + level_error_precip | 
    county + state^hurricane + county^month + wind_cut + precip_cut,
  data = load_df_normalized(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon"
)

regs_pgdp[[5]] = conleyreg(
  total_cost_per_gdp*100 ~ squared_error_norm + squared_error_precip + 
    level_error_norm + level_error_precip | 
    county + state^hurricane + county^year + wind_cut + precip_cut,
  data = load_df_normalized(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon"
)

regs_pgdp[[6]] = conleyreg(
  total_cost_per_gdp*100 ~ squared_error_norm + squared_error_precip + 
    level_error_norm + level_error_precip | 
    county + state^hurricane + county^year + county^month + wind_cut + precip_cut,
  data = load_df_normalized(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon"
)

regs_pgdp[[7]] = conleyreg(
  total_cost_per_gdp*100 ~ squared_error_norm:category + squared_error_precip + 
    level_error_norm + level_error_precip | 
    county + state^hurricane + wind_cut + precip_cut,
  data = load_df_normalized(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon"
)

# Panel C: Damages + Recovery Expenditures Per Capita ($/person)
regs_pc = list()

regs_pc[[1]] = conleyreg(
  total_cost_pc ~ squared_error_norm + squared_error_precip | 
    county + state^hurricane,
  data = load_df_normalized(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon"
)

regs_pc[[2]] = conleyreg(
  total_cost_pc ~ squared_error_norm + squared_error_precip | 
    county + state^hurricane + wind_cut + precip_cut,
  data = load_df_normalized(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon"
)

regs_pc[[3]] = conleyreg(
  total_cost_pc ~ squared_error_norm + squared_error_precip + 
    level_error_norm + level_error_precip | 
    county + state^hurricane + wind_cut + precip_cut,
  data = load_df_normalized(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon"
)

regs_pc[[4]] = conleyreg(
  total_cost_pc ~ squared_error_norm + squared_error_precip + 
    level_error_norm + level_error_precip | 
    county + state^hurricane + county^month + wind_cut + precip_cut,
  data = load_df_normalized(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon"
)

regs_pc[[5]] = conleyreg(
  total_cost_pc ~ squared_error_norm + squared_error_precip + 
    level_error_norm + level_error_precip | 
    county + state^hurricane + county^year + wind_cut + precip_cut,
  data = load_df_normalized(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon"
)

regs_pc[[6]] = conleyreg(
  total_cost_pc ~ squared_error_norm + squared_error_precip + 
    level_error_norm + level_error_precip | 
    county + state^hurricane + county^year + county^month + wind_cut + precip_cut,
  data = load_df_normalized(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon"
)

regs_pc[[7]] = conleyreg(
  total_cost_pc ~ squared_error_norm:category + squared_error_precip + 
    level_error_norm + level_error_precip | 
    county + state^hurricane + wind_cut + precip_cut,
  data = load_df_normalized(),
  dist_cutoff = dist,
  lag_cutoff = Inf,
  unit = "county",
  time = "year",
  lat = "lat",
  lon = "lon"
)

# Get observation count
obs = feols(
  total_cost ~ squared_error_norm + squared_error_precip | 
    county + state^hurricane + county^year + county^month + wind_cut + precip_cut,
  data = load_df_normalized()
)$nobs |>
  format(big.mark = ",")

# Create rows for table footer
rows = tribble(
  ~term, ~"(1)", ~"(2)", ~"(3)", ~"(4)", ~"(5)", ~"(6)", ~"(7)",
  "", "", "", "", "", "", "", "",
  "Observations", obs, obs, obs, obs, obs, obs, obs,
  "", "", "", "", "", "", "", "",
  "Realized Wind/Precip Bins", "", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark",
  "Level Wind/Precip Error", "", "", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark",
  "State-Hurricane FE", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark",
  "County FE", "\\checkmark", "\\checkmark", "\\checkmark", "", "", "", "\\checkmark",
  "County-Month of Year FE", "", "", "", "\\checkmark", "", "\\checkmark", "",
  "County-Year FE", "", "", "", "", "\\checkmark", "\\checkmark", ""
)

# Generate table
msummary(
  list(
    "Damages + Recovery Expenditures (million $)" = regs,
    "(Damages + Recovery Expenditures) / GDP (%)" = regs_pgdp,
    "Damages + Recovery Expenditures Per Capita ($/person)" = regs_pc
  ),
  coef_rename = c(
    "$\\beta_2: (e - \\mu)^2$",
    "$\\text{Hurricane } \\beta_2: (e - \\mu)^2$",
    "$\\text{Sub-Hurricane } \\beta_2: (e - \\mu)^2$"
  ),
  coef_omit = c(-1, -5, -6),
  gof_omit = "R2|AIC|BIC|Log.|RMSE|FE|Std.Errors",
  stars = c('*' = .1, '**' = .05, '***' = .01),
  fmt = "%.2f",
  add_rows = rows,
  title = "The Value of a Wind Speed Forecast Improvement: Inverse Normal Transformation by Hurricane. \\label{atab:voi-table-norm}",
  notes = list("Standard errors are Conley Spatial HAC with a distance radius of 400 km for spatial correlation and arbitrary autocorrelation within counties. 
               Forecast errors are normalized by hurricane. 
               $e$ is the county-hurricane-specific forecast error, and $\\\\mu$ is the hurricane-intensity bin-specific mean forecast error. 
               Hurricane wind speeds are those greater than 33 m/s (74 mph), while Sub-Hurricane wind speeds fall below that threshold."),
  threeparttable = TRUE,
  shape = 'rbind',
  escape = FALSE,
  output = "results/TableC21.tex"
)

add_adjust_box("results/TableC21.tex")

# ==============================================================================
# 14c. Bin-Specific Normalization (within wind speed bins: 2, 5, 10, 20 m/s)
# ==============================================================================

bin_widths = c(2, 5, 10, 20)

# Save the normalized dataset for future use

for (bin_width in bin_widths) {
  # Generate binned dataset
  df_binned = load_df() |>
    mutate(
      forecast_bin = cut_width(wind_obs, width = bin_width, boundary = 0)
    ) |>
    group_by(hurricane, forecast_bin) |>
    mutate(
      # Calculate bin-specific error statistics
      mean_error = mean(wind_error),
      sd_error = sd(wind_error),
      
      # Convert to percentiles
      r = rank(wind_error, ties.method = "first"),
      n = n(),
      p = (r - 0.5) / n,
      
      # Convert to z-scores and rescale
      z = qnorm(p),
      level_error_star = mean_error + sd_error * z,
      wind_star = wind_fc + level_error_star
    ) |>
    ungroup() |>
    mutate(
      squared_error_star = level_error_star^2
    )
  
  saveRDS(df_binned, paste0("data/clean/df_binned_", bin_width, ".rds"))
  
  load_df_binned = function() {
    # Load in data
    df = readRDS(paste0("data/clean/df_binned_", bin_width, ".rds")) |>
      as_tibble()
    return(df)
  }
  
  # Panel A: Damages and Recovery Expenditures (million $)
  regs = list()
  
  regs[[1]] = conleyreg(
    total_cost ~ squared_error_star + squared_error_precip | 
      county + state^hurricane,
    data = load_df_binned(),
    dist_cutoff = dist,
    lag_cutoff = Inf,
    unit = "county",
    time = "year",
    lat = "lat",
    lon = "lon"
  )
  
  regs[[2]] = conleyreg(
    total_cost ~ squared_error_star + squared_error_precip | 
      county + state^hurricane + wind_cut + precip_cut,
    data = load_df_binned(),
    dist_cutoff = dist,
    lag_cutoff = Inf,
    unit = "county",
    time = "year",
    lat = "lat",
    lon = "lon"
  )
  
  regs[[3]] = conleyreg(
    total_cost ~ squared_error_star + squared_error_precip + 
      level_error_star + level_error_precip | 
      county + state^hurricane + wind_cut + precip_cut,
    data = load_df_binned(),
    dist_cutoff = dist,
    lag_cutoff = Inf,
    unit = "county",
    time = "year",
    lat = "lat",
    lon = "lon"
  )
  
  regs[[4]] = conleyreg(
    total_cost ~ squared_error_star + squared_error_precip + 
      level_error_star + level_error_precip | 
      county + state^hurricane + county^month + wind_cut + precip_cut,
    data = load_df_binned(),
    dist_cutoff = dist,
    lag_cutoff = Inf,
    unit = "county",
    time = "year",
    lat = "lat",
    lon = "lon"
  )
  
  regs[[5]] = conleyreg(
    total_cost ~ squared_error_star + squared_error_precip + 
      level_error_star + level_error_precip | 
      county + state^hurricane + county^year + wind_cut + precip_cut,
    data = load_df_binned(),
    dist_cutoff = dist,
    lag_cutoff = Inf,
    unit = "county",
    time = "year",
    lat = "lat",
    lon = "lon"
  )
  
  regs[[6]] = conleyreg(
    total_cost ~ squared_error_star + squared_error_precip + 
      level_error_star + level_error_precip | 
      county + state^hurricane + county^year + county^month + wind_cut + precip_cut,
    data = load_df_binned(),
    dist_cutoff = dist,
    lag_cutoff = Inf,
    unit = "county",
    time = "year",
    lat = "lat",
    lon = "lon"
  )
  
  regs[[7]] = conleyreg(
    total_cost ~ squared_error_star:category + squared_error_precip + 
      level_error_star + level_error_precip | 
      county + state^hurricane + wind_cut + precip_cut,
    data = load_df_binned(),
    dist_cutoff = dist,
    lag_cutoff = Inf,
    unit = "county",
    time = "year",
    lat = "lat",
    lon = "lon"
  )
  
  # Panel B: (Damages + Recovery Expenditures)/GDP (%)
  regs_pgdp = list()
  
  regs_pgdp[[1]] = conleyreg(
    total_cost_per_gdp*100 ~ squared_error_star + squared_error_precip | 
      county + state^hurricane,
    data = load_df_binned(),
    dist_cutoff = dist,
    lag_cutoff = Inf,
    unit = "county",
    time = "year",
    lat = "lat",
    lon = "lon"
  )
  
  regs_pgdp[[2]] = conleyreg(
    total_cost_per_gdp*100 ~ squared_error_star + squared_error_precip | 
      county + state^hurricane + wind_cut + precip_cut,
    data = load_df_binned(),
    dist_cutoff = dist,
    lag_cutoff = Inf,
    unit = "county",
    time = "year",
    lat = "lat",
    lon = "lon"
  )
  
  regs_pgdp[[3]] = conleyreg(
    total_cost_per_gdp*100 ~ squared_error_star + squared_error_precip + 
      level_error_star + level_error_precip | 
      county + state^hurricane + wind_cut + precip_cut,
    data = load_df_binned(),
    dist_cutoff = dist,
    lag_cutoff = Inf,
    unit = "county",
    time = "year",
    lat = "lat",
    lon = "lon"
  )
  
  regs_pgdp[[4]] = conleyreg(
    total_cost_per_gdp*100 ~ squared_error_star + squared_error_precip + 
      level_error_star + level_error_precip | 
      county + state^hurricane + county^month + wind_cut + precip_cut,
    data = load_df_binned(),
    dist_cutoff = dist,
    lag_cutoff = Inf,
    unit = "county",
    time = "year",
    lat = "lat",
    lon = "lon"
  )
  
  regs_pgdp[[5]] = conleyreg(
    total_cost_per_gdp*100 ~ squared_error_star + squared_error_precip + 
      level_error_star + level_error_precip | 
      county + state^hurricane + county^year + wind_cut + precip_cut,
    data = load_df_binned(),
    dist_cutoff = dist,
    lag_cutoff = Inf,
    unit = "county",
    time = "year",
    lat = "lat",
    lon = "lon"
  )
  
  regs_pgdp[[6]] = conleyreg(
    total_cost_per_gdp*100 ~ squared_error_star + squared_error_precip + 
      level_error_star + level_error_precip | 
      county + state^hurricane + county^year + county^month + wind_cut + precip_cut,
    data = load_df_binned(),
    dist_cutoff = dist,
    lag_cutoff = Inf,
    unit = "county",
    time = "year",
    lat = "lat",
    lon = "lon"
  )
  
  regs_pgdp[[7]] = conleyreg(
    total_cost_per_gdp*100 ~ squared_error_star:category + squared_error_precip + 
      level_error_star + level_error_precip | 
      county + state^hurricane + wind_cut + precip_cut,
    data = load_df_binned(),
    dist_cutoff = dist,
    lag_cutoff = Inf,
    unit = "county",
    time = "year",
    lat = "lat",
    lon = "lon"
  )
  
  # Panel C: Damages + Recovery Expenditures Per Capita ($/person)
  regs_pc = list()
  
  regs_pc[[1]] = conleyreg(
    total_cost_pc ~ squared_error_star + squared_error_precip | 
      county + state^hurricane,
    data = load_df_binned(),
    dist_cutoff = dist,
    lag_cutoff = Inf,
    unit = "county",
    time = "year",
    lat = "lat",
    lon = "lon"
  )
  
  regs_pc[[2]] = conleyreg(
    total_cost_pc ~ squared_error_star + squared_error_precip | 
      county + state^hurricane + wind_cut + precip_cut,
    data = load_df_binned(),
    dist_cutoff = dist,
    lag_cutoff = Inf,
    unit = "county",
    time = "year",
    lat = "lat",
    lon = "lon"
  )
  
  regs_pc[[3]] = conleyreg(
    total_cost_pc ~ squared_error_star + squared_error_precip + 
      level_error_star + level_error_precip | 
      county + state^hurricane + wind_cut + precip_cut,
    data = load_df_binned(),
    dist_cutoff = dist,
    lag_cutoff = Inf,
    unit = "county",
    time = "year",
    lat = "lat",
    lon = "lon"
  )
  
  regs_pc[[4]] = conleyreg(
    total_cost_pc ~ squared_error_star + squared_error_precip + 
      level_error_star + level_error_precip | 
      county + state^hurricane + county^month + wind_cut + precip_cut,
    data = load_df_binned(),
    dist_cutoff = dist,
    lag_cutoff = Inf,
    unit = "county",
    time = "year",
    lat = "lat",
    lon = "lon"
  )
  
  regs_pc[[5]] = conleyreg(
    total_cost_pc ~ squared_error_star + squared_error_precip + 
      level_error_star + level_error_precip | 
      county + state^hurricane + county^year + wind_cut + precip_cut,
    data = load_df_binned(),
    dist_cutoff = dist,
    lag_cutoff = Inf,
    unit = "county",
    time = "year",
    lat = "lat",
    lon = "lon"
  )
  
  regs_pc[[6]] = conleyreg(
    total_cost_pc ~ squared_error_star + squared_error_precip + 
      level_error_star + level_error_precip | 
      county + state^hurricane + county^year + county^month + wind_cut + precip_cut,
    data = load_df_binned(),
    dist_cutoff = dist,
    lag_cutoff = Inf,
    unit = "county",
    time = "year",
    lat = "lat",
    lon = "lon"
  )
  
  regs_pc[[7]] = conleyreg(
    total_cost_pc ~ squared_error_star:category + squared_error_precip + 
      level_error_star + level_error_precip | 
      county + state^hurricane + wind_cut + precip_cut,
    data = load_df_binned(),
    dist_cutoff = dist,
    lag_cutoff = Inf,
    unit = "county",
    time = "year",
    lat = "lat",
    lon = "lon"
  )
  
  # Get observation count
  obs = feols(
    total_cost ~ squared_error_star + squared_error_precip | 
      county + state^hurricane + county^year + county^month + wind_cut + precip_cut,
    data = load_df_binned()
  )$nobs |>
    format(big.mark = ",")
  
  # Create rows for table footer
  rows = tribble(
    ~term, ~"(1)", ~"(2)", ~"(3)", ~"(4)", ~"(5)", ~"(6)", ~"(7)",
    "", "", "", "", "", "", "", "",
    "Observations", obs, obs, obs, obs, obs, obs, obs,
    "", "", "", "", "", "", "", "",
    "Realized Wind/Precip Bins", "", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark",
    "Level Wind/Precip Error", "", "", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark",
    "State-Hurricane FE", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark",
    "County FE", "\\checkmark", "\\checkmark", "\\checkmark", "", "", "", "\\checkmark",
    "County-Month of Year FE", "", "", "", "\\checkmark", "", "\\checkmark", "",
    "County-Year FE", "", "", "", "", "\\checkmark", "\\checkmark", ""
  )
  
  # Generate table
  msummary(
    list(
      "Damages + Recovery Expenditures (million $)" = regs,
      "(Damages + Recovery Expenditures) / GDP (%)" = regs_pgdp,
      "Damages + Recovery Expenditures Per Capita ($/person)" = regs_pc
    ),
    coef_rename = c(
      "$\\beta_2: (e - \\mu)^2$",
      "$\\text{Hurricane } \\beta_2: (e - \\mu)^2$",
      "$\\text{Sub-Hurricane } \\beta_2: (e - \\mu)^2$"
    ),
    coef_omit = c(-1, -5, -6),
    gof_omit = "R2|AIC|BIC|Log.|RMSE|FE|Std.Errors",
    stars = c('*' = .1, '**' = .05, '***' = .01),
    fmt = "%.2f",
    add_rows = rows,
    title = paste0("The Value of a Wind Speed Forecast Improvement Using Binned Normal Transform with ", bin_width, " m/s bins. \\label{atab:voi-table-bin", bin_width, "}"),
    notes = list("Standard errors are Conley Spatial HAC with a distance radius of 400 km for spatial correlation and arbitrary autocorrelation within counties. 
               $e$ is the county-hurricane-specific forecast error, and $\\\\mu$ is the hurricane-intensity bin-specific mean forecast error. 
               Hurricane wind speeds are those greater than 33 m/s (74 mph), while Sub-Hurricane wind speeds fall below that threshold."),
    threeparttable = TRUE,
    shape = 'rbind',
    escape = FALSE,
    output = paste0("results/TableC", 21 + which(bin_widths == bin_width), ".tex")
  )
  
  add_adjust_box(paste0("results/TableC", 21 + which(bin_widths == bin_width), ".tex"))
  
}

# ==============================================================================
# 15. CONLEY DISTANCE CUTOFF TESTING
# ==============================================================================

df = readRDS("data/clean/final_dataset.rds") |> 
  drop_na(squared_error, lat, lon)

kms = seq(50, 1050, 100)
years = c(Inf)

loop_vec = expand_grid(kms, years)

# Loop over distance and lag cutoffs
out = map2(loop_vec$kms, loop_vec$years, function(km, year) {
  print(km)
  print(year)
  est = conleyreg(
    total_cost ~ squared_error:category + squared_error_precip + level_error + level_error_precip | county + state^hurricane + wind_cut + precip_cut, 
    data = load_df(), 
    dist_cutoff = km, 
    lag_cutoff = year,
    unit = "county",
    time = "year",
    lat = "lat", 
    lon = "lon"
  ) |> 
    tidy(conf.int = TRUE) |> 
    slice(4) |>
    mutate(distance = km,
           year = year)
  print(est)
  return(est)
})

out = out |> 
  bind_rows() 

fwrite(out, "data/clean/conley_estimates.csv")

ggplot(out) + 
  geom_hline(yintercept = 2.575, color = "grey55", linetype = "dashed") +
  geom_hline(yintercept = 1.96, color = "grey55", linetype = "dashed") +
  geom_hline(yintercept = 1.645, color = "grey55", linetype = "dashed") +
  geom_point(aes(x = distance, y = statistic), color = "black", size = 5) +
  main_theme +
  scale_x_continuous() +
  scale_y_continuous(breaks = c(1.645, 1.96, 2.575, 3, 4)) +
  labs(
    x = "Spatial Correlation Distance Cutoff (kilometers)",
    y = "Estimate T-Statistic"
  ) 

ggsave("results/FigureC27.png", width = 1 * 10 * 4 / 5, height = 1 * 8 * 4 / 5)
ggsave("results/FigureC27.pdf", device = cairo_pdf, width = 1 * 10 * 4 / 5, height = 1 * 8 * 4 / 5)

# ==============================================================================
# 16. DROP HURRICANES ANALYSIS
# ==============================================================================

mean_voi = feols(
  total_cost ~ squared_error:category + squared_error_precip + level_error + level_error_precip |
    county + state^hurricane + wind_cut + precip_cut,
  load_df(),
  cluster = c("county", "state^hurricane")
) |> 
  tidy()  |> 
  slice(4) |>
  pull(estimate)

vois = map_dfr(
  unique(df$hurricane),
  function(hur){
    feols(
      total_cost ~ squared_error:category + squared_error_precip + level_error + level_error_precip |
        county + state^hurricane + wind_cut + precip_cut,
      load_df() |> filter(hurricane != hur),
      cluster = c("county", "state^hurricane")
    ) |> 
      tidy() |> 
      slice(4) |>
      mutate(hurricane = hur)
    
  }
)

ggplot(vois, aes(x = estimate)) +
  geom_histogram(
    fill = "#377eb8", color = "#555555"
  ) +
  geom_vline(xintercept = mean_voi, linetype = 'dashed') +
  main_theme +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  scale_y_continuous(breaks = scales::pretty_breaks(), expand = c(0,0)) +
  labs(
    title = "Estimates Dropping Hurricanes",
    y = "# Hurricanes",
    x = ~ paste(beta[2], ": Squared Error Coefficient")
  )

ggsave("results/FigureC28.png", width = 10 * 2 / 3, height = 8 * 2 / 3)
ggsave("results/FigureC28.pdf", device = cairo_pdf, width = 10 * 2 / 3, height = 8 * 2 / 3)

# ==============================================================================
# 17. CENTERED ANALYSIS
# ==============================================================================

regs = list()

regs[[1]] = conleyreg(
  total_cost ~ I(squared_error - wind_fc_sd^2) + squared_error_precip | 
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
  total_cost ~ I(squared_error - wind_fc_sd^2) + squared_error_precip | 
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
  total_cost ~ I(squared_error - wind_fc_sd^2) + squared_error_precip + level_error + level_error_precip | 
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
  total_cost ~ I(squared_error - wind_fc_sd^2)  + squared_error_precip + level_error + level_error_precip | 
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
  total_cost ~ I(squared_error - wind_fc_sd^2)  + squared_error_precip + level_error + level_error_precip | 
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
  total_cost ~ I(squared_error - wind_fc_sd^2)  + squared_error_precip + level_error + level_error_precip | 
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
  total_cost ~ I(squared_error - wind_fc_sd^2):category + squared_error_precip + level_error + level_error_precip | 
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
  total_cost_pc ~ I(squared_error - wind_fc_sd^2) + squared_error_precip | 
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
  total_cost_pc ~ I(squared_error - wind_fc_sd^2) + squared_error_precip | 
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
  total_cost_pc ~ I(squared_error - wind_fc_sd^2) + squared_error_precip + level_error + level_error_precip | 
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
  total_cost_pc ~ I(squared_error - wind_fc_sd^2)  + squared_error_precip + level_error + level_error_precip | 
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
  total_cost_pc ~ I(squared_error - wind_fc_sd^2)  + squared_error_precip + level_error + level_error_precip | 
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
  total_cost_pc ~ I(squared_error - wind_fc_sd^2)  + squared_error_precip + level_error + level_error_precip | 
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
  total_cost_pc ~ I(squared_error - wind_fc_sd^2):category + squared_error_precip + level_error + level_error_precip | 
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
  total_cost_per_gdp*100 ~ I(squared_error - wind_fc_sd^2) + squared_error_precip | 
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
  total_cost_per_gdp*100 ~ I(squared_error - wind_fc_sd^2) + squared_error_precip | 
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
  total_cost_per_gdp*100 ~ I(squared_error - wind_fc_sd^2) + squared_error_precip + level_error + level_error_precip | 
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
  total_cost_per_gdp*100 ~ I(squared_error - wind_fc_sd^2)  + squared_error_precip + level_error + level_error_precip | 
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
  total_cost_per_gdp*100 ~ I(squared_error - wind_fc_sd^2)  + squared_error_precip + level_error + level_error_precip | 
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
  total_cost_per_gdp*100 ~ I(squared_error - wind_fc_sd^2)  + squared_error_precip + level_error + level_error_precip | 
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
  total_cost_per_gdp*100 ~ I(squared_error - wind_fc_sd^2):category + squared_error_precip + level_error + level_error_precip | 
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
  total_cost ~ I(squared_error - wind_fc_sd)  + squared_error_precip | 
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
  coef_rename = c("$\\beta_2: (e - \\mu)^2-\\sigma^2$", "$\\text{Hurricane } \\beta_2: (e - \\mu)^2-\\sigma^2$", "$\\text{Sub-Hurricane } \\beta_2: (e - \\mu)^2-\\sigma^2$"),
  coef_omit = c(-1, -5, -6),
  gof_omit = "R2|AIC|BIC|Log.|RMSE|FE|Std.Errors",
  stars =  c('*' = .1, '**' = .05, '***' = .01),
  fmt = "%.2f",
  add_rows = rows,
  title = "The Before-Signal Value of a Wind Speed Forecast Improvement. \\label{tab:voi-table-centered}",
  notes = list("Standard errors are Conley Spatial HAC with a distance radius of 400 km for spatial correlation and arbitrary autocorrelation within counties. 
               $e$ is the county-hurricane-specific forecast error, $\\\\mu$ is the hurricane-intensity bin-specific mean forecast error, and $\\\\sigma$ is the county-hurricane-specific ex ante forecast standard deviation. 
               Hurricane wind speeds are those greater than 33 m/s (74 mph), while Sub-Hurricane wind speeds fall below that threshold."),
  threeparttable = TRUE,
  shape = 'rbind',
  escape = FALSE,
  output = "results/TableD2.tex"
)

add_adjust_box("results/TableD2.tex")
