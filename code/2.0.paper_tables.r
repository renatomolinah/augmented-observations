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
  # Main text shows only the headline dollar-cost outcome; the cost/GDP and
  # cost-per-capita normalizations are reported as outcome robustness in
  # Appendix A (tab:cost_outcomes and tab:implied_value). coef_map keeps only
  # the wind squared-error coefficient (pooled in cols 1-6, hurricane/sub split
  # in col 7); precipitation and level errors are omitted (precip is reported in
  # tab:precip_costs).
  regs,
  coef_map = c(
    "squared_error"                        = "$\\beta_2: (e - \\mu)^2$",
    "squared_error:categoryHurricane"      = "$\\text{Hurricane } \\beta_2: (e - \\mu)^2$",
    "squared_error:categoryTropical Storm" = "$\\text{Sub-Hurricane } \\beta_2: (e - \\mu)^2$"),
  gof_omit  = "R2|AIC|BIC|Log.|RMSE|FE|Std.Errors",
  fmt       = "%.2f",
  stars     = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  add_rows  = rows,
  title     = "The Value of a Wind Speed Forecast Improvement (damages plus recovery expenditures, \\$ millions). \\label{tab:voi-table}",
  notes     = list(
    "$e$ is the county-hurricane-specific forecast error, and $\\\\mu$ is the
     hurricane-intensity bin-specific mean forecast error. Hurricane wind speeds are
     those greater than 33 m/s (74 mph), while Sub-Hurricane wind speeds fall below that
     threshold. Standard errors are Conley Spatial HAC with a distance radius of 400 km
     for spatial correlation and arbitrary autocorrelation within counties."
  ),
  threeparttable = TRUE,
  escape = FALSE,
  output = here("output/paper_table_voi.tex")
)

add_adjust_box(here("output/paper_table_voi.tex"))

# --- Appendix A: full VOI table reporting the precipitation coefficient -------
# Same seven total-cost specifications as the main table (reuse `regs`), but the
# coef_map keeps BOTH squared-error coefficients so the precipitation parameter
# beta^p_2 is shown. Motivates focusing the valuation on wind: beta^p_2 is tiny
# and statistically indistinguishable from zero across specifications.
msummary(
  regs,
  coef_map = c(
    "squared_error"                        = "Wind, $\\beta^w_2: (e^w - \\mu^w)^2$",
    "squared_error:categoryHurricane"      = "Wind, Hurricane $\\beta^w_2$",
    "squared_error:categoryTropical Storm" = "Wind, Sub-Hurricane $\\beta^w_2$",
    "squared_error_precip"                 = "Precipitation, $\\beta^p_2: (e^p - \\mu^p)^2$"),
  gof_omit  = "R2|AIC|BIC|Log.|RMSE|FE|Std.Errors",
  fmt       = "%.3f",
  stars     = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  add_rows  = rows,
  title     = "Wind and Precipitation Forecast-Error Costs. \\label{tab:precip_costs}",
  notes     = list(
    "Full coefficient table for the Stage 1 value-of-information regressions, reporting both the
     wind ($\\\\beta^w_2$) and precipitation ($\\\\beta^p_2$) squared demeaned forecast-error
     coefficients; the main table omits precipitation. $e$ is the county-hurricane-specific forecast
     error and $\\\\mu$ the hurricane-intensity bin-specific mean forecast error. Hurricane wind
     speeds exceed 33 m/s (74 mph); Sub-Hurricane wind speeds fall below that threshold. Standard
     errors are Conley Spatial HAC with a 400 km distance radius."
  ),
  threeparttable = TRUE,
  escape = FALSE,
  output = here("output/paper_precip_costs.tex")
)
add_adjust_box(here("output/paper_precip_costs.tex"))

# --- Appendix A: outcome robustness (preferred spec), paper format -----------
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
  title     = "Outcome robustness for the preferred specification (2005--2024 panel). \\label{tab:cost_outcomes}",
  notes     = list("All specifications match our preferred specification (Table @REF{tab:voi-table}), with county and state-by-hurricane fixed effects and realized wind and precipitation bins. Standard errors are Conley Spatial HAC with a 400 km distance cutoff."),
  align     = "lccc",
  threeparttable = TRUE,
  escape    = FALSE,
  output    = here("output/paper_cost_outcomes.tex")
)
# modelsummary strips backslashes in the notes pass; restore the cross-ref.
.aoa <- here("output/paper_cost_outcomes.tex")
writeLines(gsub("@REF\\{", "\\\\ref{", readLines(.aoa)), .aoa)
add_adjust_box(.aoa)

# ==============================================================================
# Stage 2 - Augmented Observations and Forecast Precision (delta)
# ==============================================================================

# --- Controls table (tab:noaa-controls) --------------------------------------
# Generated SEPARATELY by code/2.3.interaction_bootstrap.r: like the interaction
# grid, its significance must come from a restricted wild cluster bootstrap (too
# few storm clusters for analytical cluster-robust p-values), so it is rendered
# there alongside the other bootstrap tables.


# --- Full AO x intensity x lead interaction (tab:interaction-grid) ------------
# The progressive build of eq. 7 is generated SEPARATELY by
# code/2.3.interaction_bootstrap.r, because with only ten storm clusters its
# significance stars must come from a restricted wild cluster bootstrap (Webb
# weights), not analytical cluster-robust p-values (which over-reject badly: the
# 3-day x HF triple interaction is p=0.0004 analytically vs ~0.03 bootstrapped).
# That script writes output/paper_interaction_grid.tex and the cached p-values
# output/interaction_wcb_pvalues.csv. Run it after this script (or via the Make
# target) so the table reflects bootstrap inference.

cat("\n[paper_tables] Wrote full tables:\n",
    "  output/paper_interaction_grid.tex\n",
    "  output/paper_table_voi.tex\n",
    "  output/paper_cost_outcomes.tex\n",
    "  output/paper_controls_table.tex\n",
    "Copy them into 'augmented_observations/figures and tables/' and point the\n",
    "manuscript \\input lines at them to swap the abbreviated report tables for the\n",
    "full paper versions.\n", sep = "")
