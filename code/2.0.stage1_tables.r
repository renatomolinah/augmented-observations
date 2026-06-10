# ==============================================================================
# 2.0 Stage 1 tables: the value-of-information table (tab:voi-table) and the two
# Appendix A cost tables -- precipitation coefficient (tab:precip_costs) and
# outcome robustness (tab:cost_outcomes). Estimation matches NOAA_report.r, so the
# numbers are identical and the shared \label{}s keep the manuscript refs valid.
# ==============================================================================
if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, tidyverse, here, fixest, conleyreg, modelsummary, kableExtra)
options(conleyreg.verbose = FALSE)
options(modelsummary_format_numeric_latex = "plain")
source(here("code/functions_table.r"))

# conleyreg mutates its data.table input by reference, so reload per fit.
load_voi <- function() {
  readRDS(here("data/clean/final_dataset.rds")) |>
    as_tibble() |>
    drop_na(lat, lon, total_cost_pc, total_cost_per_gdp)
}
run_reg <- function(formula) {
  conleyreg(formula, load_voi(), dist_cutoff = 400, lag_cutoff = Inf,
            unit = "county", time = "year", lat = "lat", lon = "lon",
            batch_ram_opt = "none")
}

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

regs      <- lapply(voi_specs("total_cost"),               run_reg)
regs_pgdp <- lapply(voi_specs("total_cost_per_gdp * 100"), run_reg)
regs_pc   <- lapply(voi_specs("total_cost_pc"),            run_reg)

obs <- feols(total_cost ~ squared_error + squared_error_precip |
               county + state^hurricane + county^year + county^month + wind_cut + precip_cut,
             data = load_voi())$nobs |> format(big.mark = ",")

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

# Main VOI table (tab:voi-table): wind squared-error coefficient, hurricane/sub split in col 7.
msummary(
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
    "$e$ is the county-hurricane-specific forecast error, and $\\\\mu$ is the hurricane-intensity bin-specific mean forecast error. Hurricane wind speeds exceed 33 m/s (74 mph); sub-hurricane wind speeds fall below. Conley spatial HAC standard errors with a 400 km cutoff."),
  threeparttable = TRUE, escape = FALSE,
  output = here("output/paper_table_voi.tex")
)
add_adjust_box(here("output/paper_table_voi.tex"))

# Appendix A: full specification reporting the precipitation coefficient (tab:precip_costs).
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
    "Full Stage 1 specification reporting both the wind ($\\\\beta^w_2$) and precipitation ($\\\\beta^p_2$) squared demeaned forecast-error coefficients, the latter omitted from the main table. Conley spatial HAC standard errors with a 400 km cutoff."),
  threeparttable = TRUE, escape = FALSE,
  output = here("output/paper_precip_costs.tex")
)
add_adjust_box(here("output/paper_precip_costs.tex"))

# Appendix A: outcome robustness, preferred spec (tab:cost_outcomes). Reuses the col (7) fits.
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
  notes     = list("Preferred specification (Table @REF{tab:voi-table}), with Conley spatial HAC standard errors with a 400 km cutoff."),
  align     = "lccc",
  threeparttable = TRUE, escape = FALSE,
  output    = here("output/paper_cost_outcomes.tex")
)
.f <- here("output/paper_cost_outcomes.tex")
writeLines(gsub("@REF\\{", "\\\\ref{", readLines(.f)), .f)   # modelsummary strips the backslash; restore the cross-ref
add_adjust_box(.f)

cat("Wrote paper_table_voi.tex, paper_precip_costs.tex, paper_cost_outcomes.tex\n")
