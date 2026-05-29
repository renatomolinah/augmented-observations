# ==============================================================================
# 2.0 Paper Tables - Full (non-abbreviated) regression tables for the manuscript
# ------------------------------------------------------------------------------
# The NOAA *report* (code/NOAA_report.r) renders compact placeholder tables. For
# the *paper* we want the publication versions:
#   - output/paper_table_voi.tex (Stage 1, label tab:voi-table) mirrors Table 2 of
#     Molina & Rudik ("The Social Value of Hurricane Forecasts"): three stacked
#     outcome panels -- total cost, cost / county GDP, cost per capita -- across
#     the seven specifications, reporting beta2 / Hurricane beta2 / Sub-Hurricane
#     beta2 with controls and fixed effects shown as a shared checkmark block
#     (shape = "rbind", coef_omit = c(-1, -5, -6)).
#   - output/paper_controls_table.tex (Stage 2, label tab:noaa-controls) reports
#     the full delta regressions: every coefficient (including the wind control)
#     plus the N / R2 / within-R2 block.
# The estimation code matches NOAA_report.r so the numbers are identical; only the
# table outcomes/rendering differ. Labels match the report's so the manuscript
# \ref{}s keep resolving when its \input lines point at these files.
#
# Run:  Rscript --vanilla code/2.0.paper_tables.r   (requires the Box data symlink)
# Make: make paper_tables
# ==============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  data.table, tidyverse, here, fixest, conleyreg, broom,
  modelsummary, kableExtra
)
options(conleyreg.verbose = FALSE)
# Render numbers plainly (no siunitx \num{} wrapping), matching the Molina & Rudik
# tables; the main VOI table uses shape = "rbind" which is plain regardless.
options(modelsummary_format_numeric_latex = "plain")

source(here("code/functions_table.r"))

voi_path    <- here("data/clean/final_dataset.rds")
denial_path <- here("data/clean_denial/forecast_denial_data_with_lead_times.csv")

stopifnot(file.exists(voi_path))
stopifnot(file.exists(denial_path))

# ==============================================================================
# Stage 1 - Marginal Cost of Forecast Errors (beta) -- estimation copied verbatim
# ==============================================================================

# conleyreg mutates its input data.table by reference, so each call must
# receive a fresh load.
load_voi <- function() {
  readRDS(voi_path) |>
    as_tibble() |>
    drop_na(lat, lon, total_cost_pc, total_cost_per_gdp)
}

run_reg <- function(formula) {
  conleyreg(
    formula       = formula,
    data          = load_voi(),
    dist_cutoff   = 400,
    lag_cutoff    = Inf,
    unit          = "county",
    time          = "year",
    lat           = "lat",
    lon           = "lon",
    batch_ram_opt = "none"
  )
}

# Three outcomes (total cost, cost / county GDP, cost per capita) x seven
# specifications, mirroring Table 2 of Molina & Rudik (Hurricane Forecast repo,
# code/2.01.main_results.r): stacked outcome panels via shape = "rbind", showing
# only beta2 / Hurricane beta2 / Sub-Hurricane beta2, with controls and fixed
# effects collapsed into a single shared checkmark block.
voi_rhs <- c(
  "squared_error + squared_error_precip | county + state^hurricane",
  "squared_error + squared_error_precip | county + state^hurricane + wind_cut + precip_cut",
  "squared_error + squared_error_precip + level_error + level_error_precip | county + state^hurricane + wind_cut + precip_cut",
  "squared_error + squared_error_precip + level_error + level_error_precip | county + state^hurricane + county^month + wind_cut + precip_cut",
  "squared_error + squared_error_precip + level_error + level_error_precip | county + state^hurricane + county^year + wind_cut + precip_cut",
  "squared_error + squared_error_precip + level_error + level_error_precip | county + state^hurricane + county^year + county^month + wind_cut + precip_cut",
  "squared_error:category + squared_error_precip + level_error + level_error_precip | county + state^hurricane + wind_cut + precip_cut"
)
voi_specs <- function(lhs) lapply(voi_rhs, function(r) as.formula(paste(lhs, "~", r)))

# 7 specs x 3 outcomes (each conleyreg call reloads the panel; conleyreg mutates
# its data.table input by reference).
regs      <- lapply(voi_specs("total_cost"),               run_reg)
regs_pgdp <- lapply(voi_specs("total_cost_per_gdp * 100"), run_reg)
regs_pc   <- lapply(voi_specs("total_cost_pc"),            run_reg)

# Single panel N reused across columns (as in the Molina & Rudik table).
obs <- feols(
  total_cost ~ squared_error + squared_error_precip |
    county + state^hurricane + county^year + county^month + wind_cut + precip_cut,
  data = load_voi()
)$nobs |> format(big.mark = ",")

rows <- tribble(
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

msummary(
  list("Damages + Recovery Expenditures (million $)"           = regs,
       "(Damages + Recovery Expenditures) / GDP (%)"           = regs_pgdp,
       "Damages + Recovery Expenditures Per Capita ($/person)" = regs_pc),
  coef_rename = c("$\\beta_2: (e - \\mu)^2$",
                  "$\\text{Hurricane } \\beta_2: (e - \\mu)^2$",
                  "$\\text{Sub-Hurricane } \\beta_2: (e - \\mu)^2$"),
  coef_omit = c(-1, -5, -6),
  gof_omit  = "R2|AIC|BIC|Log.|RMSE|FE|Std.Errors",
  fmt       = "%.2f",
  add_rows  = rows,
  title     = "The Value of a Wind Speed Forecast Improvement. \\label{tab:voi-table}",
  notes     = list(
    "Standard errors are Conley Spatial HAC with a distance radius of 400 km for spatial correlation
     and arbitrary autocorrelation within counties. $e$ is the county-hurricane-specific forecast
     error, and $\\\\mu$ is the hurricane-intensity bin-specific mean forecast error. Hurricane wind
     speeds are those greater than 33 m/s (74 mph), while Sub-Hurricane wind speeds fall below that
     threshold."
  ),
  threeparttable = TRUE,
  shape = "rbind",
  escape = FALSE,
  output = here("output/paper_table_voi.tex")
)

add_adjust_box(here("output/paper_table_voi.tex"))

# --- Appendix A: outcome robustness (col-7 spec), paper format ----------------
# Reuses the column (7) fits already estimated above (no new regressions). Same
# beta2 labelling as the main table; controls/FE identical to column (7).
msummary(
  list("Total cost (million \\$)"     = regs[[7]],
       "Cost / county GDP (\\%)"      = regs_pgdp[[7]],
       "Cost per capita (\\$/person)" = regs_pc[[7]]),
  coef_rename = c("$\\text{Hurricane } \\beta_2: (e - \\mu)^2$",
                  "$\\text{Sub-Hurricane } \\beta_2: (e - \\mu)^2$"),
  coef_omit = "(?i)(precip|level_error)",
  gof_omit  = "R2|AIC|BIC|Log.|RMSE|FE|Std.Errors|Num.Obs.",
  fmt       = "%.2f",
  stars     = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  title     = "Outcome robustness for the column (7) specification (2005--2024 panel). \\label{tab:appA_outcomes}",
  notes     = list("All specifications match column (7) of Table @REF{tab:voi-table} with county and state-by-hurricane fixed effects plus realized wind and precipitation bins. Standard errors are Conley Spatial HAC with a 400 km distance cutoff."),
  align     = "lccc",
  threeparttable = TRUE,
  escape    = FALSE,
  output    = here("output/paper_appA_outcomes.tex")
)
# modelsummary strips backslashes in the notes pass; restore the cross-ref.
.aoa <- here("output/paper_appA_outcomes.tex")
writeLines(gsub("@REF\\{", "\\\\ref{", readLines(.aoa)), .aoa)
add_adjust_box(.aoa)

# ==============================================================================
# Stage 2 - Augmented Observations and Forecast Precision (delta)
# ==============================================================================

forecast_data_lead_times <- fread(denial_path) |>
  filter(name != "maria")

reg_data <- forecast_data_lead_times |>
  filter(!is.na(augmented_obs), lead_days == 3, wind_obs > 33)

regs_delta <- list()
regs_delta[[1]] <- feols(wind_fc_sd ~ augmented_obs | county,
                         data = reg_data, vcov = ~county)
regs_delta[[2]] <- feols(wind_fc_sd ~ augmented_obs | county + name,
                         data = reg_data, vcov = ~county)
regs_delta[[3]] <- feols(wind_fc_sd ~ augmented_obs + wind_obs | county,
                         data = reg_data, vcov = ~county)
regs_delta[[4]] <- feols(wind_fc_sd ~ augmented_obs + wind_obs | county + name,
                         data = reg_data, vcov = ~county)

# Full rendering: show the wind_obs control coefficient (the report hides it) and
# keep the R2 / within-R2 block.
delta_coef_rename <- c(
  "augmented_obs" = "$\\delta$: Augmented Observations",
  "wind_obs"      = "Realized wind speed (m/s)"
)

rows_delta <- tribble(
  ~term, ~"(1)", ~"(2)", ~"(3)", ~"(4)",
  "County FE",    "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark",
  "Hurricane FE", "", "\\checkmark", "", "\\checkmark"
)

msummary(
  regs_delta,
  coef_rename = delta_coef_rename,
  gof_omit    = "AIC|BIC|Log.|RMSE|Std.Errors|FE",
  fmt         = "%.3f",
  stars       = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  add_rows    = rows_delta,
  title       = "Effect of Augmented Observations on Ex-Ante Uncertainty (full specification). \\label{tab:noaa-controls}",
  notes       = list(
    "Full coefficient table for the Stage 2 precision regressions. Standard errors clustered
     at the county level. The sample is the 2017--2020 augmented-observations denial archive at
     the 3-day lead time and hurricane-force realizations ($\\\\geq$ 33 m/s); Hurricane Maria is
     excluded because of its Puerto Rico landfall. $\\\\delta$ is the average reduction in the
     ex-ante wind-speed forecast standard deviation attributable to dropsonde assimilation (the
     ALL configuration of \\\\citet{DitchekEA2023}); the negative sign reflects that AO assimilation
     lowers ensemble spread relative to the no-dropsonde (NO) counterfactual."
  ),
  threeparttable = TRUE,
  escape      = FALSE,
  output      = here("output/paper_controls_table.tex")
)

add_adjust_box(here("output/paper_controls_table.tex"))

# ==============================================================================
# Lead x strength value surface  ->  output/paper_value_surface.tex
# ------------------------------------------------------------------------------
# Regression-based justification for the (3-day, hurricane-force) headline cell.
# For each lead x intensity cell we estimate the precision gain delta (county +
# storm FE, county-clustered SE) and map it into the implied per-county social
# value  V = beta2 * ((sigma_ALL + delta)^2 - sigma_ALL^2), using the Stage 1
# column (7) cost parameters: beta2_hurr for hurricane-force cells, beta2_sub for
# sub-hurricane cells. Value is large only where BOTH delta is identified (longer
# lead) AND beta2 != 0 (hurricane-force), so the headline cell falls out of the
# estimates rather than out of a figure.
# ==============================================================================
b7         <- coef(regs[[7]])
beta2_hurr <- unname(b7[grep("categoryHurricane", names(b7))])
beta2_sub  <- unname(b7[grep("category(Tropical|Sub)", names(b7))])

surface_cell <- function(dat, lead, hf) {
  d <- dat |> filter(lead_days == lead, !is.na(augmented_obs))
  d <- if (hf) filter(d, wind_obs > 33) else filter(d, wind_obs <= 33)
  out <- tibble(strength = if (hf) "Hurricane-force" else "Sub-hurricane",
                lead = lead, n = nrow(d), sigma_all = NA_real_,
                delta = NA_real_, se = NA_real_, p = NA_real_, value_M = NA_real_)
  if (nrow(d) < 10 || length(unique(d$augmented_obs)) < 2) return(out)
  m <- tryCatch(feols(wind_fc_sd ~ augmented_obs | county + name,
                      data = d, vcov = ~county),
                error = function(e) NULL)
  if (is.null(m) || !("augmented_obs" %in% names(coef(m)))) return(out)
  dl <- abs(coef(m)[["augmented_obs"]])
  se <- sqrt(diag(vcov(m)))[["augmented_obs"]]
  s  <- mean(d$wind_fc_sd[d$augmented_obs == 1], na.rm = TRUE)
  b2 <- if (hf) beta2_hurr else beta2_sub
  out$sigma_all <- s
  out$delta     <- dl
  out$se        <- se
  out$p         <- 2 * (1 - pnorm(dl / se))
  out$value_M   <- b2 * ((s + dl)^2 - s^2)
  out
}

surface <- bind_rows(
  lapply(1:3, function(l) surface_cell(forecast_data_lead_times, l, TRUE)),
  lapply(1:3, function(l) surface_cell(forecast_data_lead_times, l, FALSE))
)

fmt_or_dash <- function(x, d) ifelse(is.na(x), "---", sprintf(paste0("%.", d, "f"), x))
surf_disp <- tibble(
  `Lead (days)`                          = surface$lead,
  `Obs.`                                 = surface$n,
  `$\\bar{\\sigma}_{\\text{ALL}}$ (m/s)` = fmt_or_dash(surface$sigma_all, 2),
  `$\\hat{\\delta}$ (m/s)`               = fmt_or_dash(surface$delta, 3),
  `SE`                                   = ifelse(is.na(surface$se), "", sprintf("(%.3f)", surface$se)),
  `$p$`                                  = fmt_or_dash(surface$p, 3),
  `Value/county (\\$M)`                  = fmt_or_dash(surface$value_M, 2)
)

vs_tex <- kbl(surf_disp, format = "latex", booktabs = TRUE, escape = FALSE,
              align = "ccccccc", linesep = "",
              caption = "Lead time $\\times$ intensity value surface for augmented observations. \\label{tab:value-surface}") |>
  pack_rows("Hurricane-force ($\\geq$ 33 m/s)", 1, 3, escape = FALSE) |>
  pack_rows("Sub-hurricane ($<$ 33 m/s)", 4, 6, escape = FALSE) |>
  footnote(
    general = paste(
      "Each cell estimates the precision gain $\\\\hat{\\\\delta}=|\\\\text{coef}(\\\\text{AO})|$ from the Stage 2",
      "specification $\\\\sigma_{csh}=\\\\delta\\\\,\\\\text{AO}_{csh}+\\\\alpha_c+\\\\gamma_h+\\\\varepsilon_{csh}$ on the indicated",
      "lead-time / intensity subset of the 2017--2020 continental-US denial archive (Maria excluded), with",
      "county and storm fixed effects and county-clustered standard errors. $\\\\bar{\\\\sigma}_{\\\\text{ALL}}$ is the mean",
      "ex-ante forecast standard deviation with dropsondes assimilated. Value per county is",
      "$\\\\hat{\\\\beta}_2[(\\\\bar{\\\\sigma}_{\\\\text{ALL}}+\\\\hat{\\\\delta})^2-\\\\bar{\\\\sigma}_{\\\\text{ALL}}^2]$, using the Stage 1 column (7)",
      sprintf("cost parameters $\\\\hat{\\\\beta}_2^{Hurr}=%.2f$ and $\\\\hat{\\\\beta}_2^{Sub}=%.2f$ (\\\\$M per (m/s)$^2$).", beta2_hurr, beta2_sub),
      "The social value is identified and maximized in the 3-day hurricane-force cell; sub-hurricane value is",
      "near zero because $\\\\hat{\\\\beta}_2^{Sub}\\\\approx0$."
    ),
    threeparttable = TRUE, escape = FALSE, general_title = ""
  )
writeLines(as.character(vs_tex), here("output/paper_value_surface.tex"))
add_adjust_box(here("output/paper_value_surface.tex"))

# --- Appendix B: full per-cell precision regressions  -> paper_appB_precision_grid
# The regressions underlying the value surface, in full: one column per
# lead x intensity cell, with the realized-wind control, FE, and GOF. Supersedes
# the report's appB_leadtime (delta-by-lead) by adding intensity and full output.
grid_cell_fit <- function(dat, lead, hf) {
  d <- dat |> filter(lead_days == lead, !is.na(augmented_obs))
  d <- if (hf) filter(d, wind_obs > 33) else filter(d, wind_obs <= 33)
  feols(wind_fc_sd ~ augmented_obs + wind_obs | county + name, data = d, vcov = ~county)
}
grid_specs <- list(
  "HF / 1d"  = grid_cell_fit(forecast_data_lead_times, 1, TRUE),
  "HF / 2d"  = grid_cell_fit(forecast_data_lead_times, 2, TRUE),
  "HF / 3d"  = grid_cell_fit(forecast_data_lead_times, 3, TRUE),
  "Sub / 1d" = grid_cell_fit(forecast_data_lead_times, 1, FALSE),
  "Sub / 2d" = grid_cell_fit(forecast_data_lead_times, 2, FALSE),
  "Sub / 3d" = grid_cell_fit(forecast_data_lead_times, 3, FALSE)
)

grid_rows <- tribble(
  ~term, ~"HF / 1d", ~"HF / 2d", ~"HF / 3d", ~"Sub / 1d", ~"Sub / 2d", ~"Sub / 3d",
  "County FE", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark",
  "Storm FE",  "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark"
)

msummary(
  grid_specs,
  coef_rename = c("augmented_obs" = "$\\delta$: Augmented Observations",
                  "wind_obs"      = "Realized wind speed (m/s)"),
  gof_omit  = "AIC|BIC|Log.|RMSE|Std.Errors|FE",
  fmt       = "%.3f",
  stars     = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  add_rows  = grid_rows,
  title     = "Per-cell precision regressions across the lead $\\times$ intensity grid. \\label{tab:appB_precision_grid}",
  notes     = list("Each column estimates $\\\\sigma_{csh}=\\\\delta\\\\,\\\\text{AO}_{csh}+\\\\theta\\\\,x_{csh}+\\\\alpha_c+\\\\gamma_h+\\\\varepsilon_{csh}$ on the indicated lead-time / intensity subset of the 2017--2020 continental-US denial archive (Maria excluded), with county and storm fixed effects and county-clustered standard errors. HF denotes hurricane-force realizations ($\\\\geq$ 33 m/s). These are the regressions summarized as per-county value in Table \\\\ref{tab:value-surface}; the value surface reports $|\\\\hat{\\\\delta}|$ from the parsimonious specification (omitting the realized-wind control), materially identical to the $\\\\delta$ shown here."),
  threeparttable = TRUE,
  escape    = FALSE,
  output    = here("output/paper_appB_precision_grid.tex")
)
add_adjust_box(here("output/paper_appB_precision_grid.tex"))

cat("\n[paper_tables] Wrote full tables:\n",
    "  output/paper_table_voi.tex\n",
    "  output/paper_appA_outcomes.tex\n",
    "  output/paper_controls_table.tex\n",
    "  output/paper_value_surface.tex\n",
    "  output/paper_appB_precision_grid.tex\n",
    "Copy them into 'augmented_observations/figures and tables/' and point the\n",
    "manuscript \\input lines at them to swap the abbreviated report tables for the\n",
    "full paper versions.\n", sep = "")
