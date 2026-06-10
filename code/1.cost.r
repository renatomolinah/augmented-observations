# ==============================================================================
# Stage 1 - Marginal cost of forecast error (beta)
# Estimates the loss-function curvature beta_2 (Conley spatial HAC) on the
# 2005-2024 continental-US county-by-storm panel and builds the Stage 1 tables.
# Inputs:  data/clean/final_dataset.rds; data/raw/{wind,precip}_error/*.csv (3-day check)
# Outputs: output/{paper_table_voi,paper_cost_outcomes,paper_precip_costs,
#          cost_jackknife,cost_3day_robustness}.tex; output/stage1_numbers.txt
# Run:     Rscript --vanilla code/1.cost.r
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
run_reg <- function(formula, data = load_voi()) {
  conleyreg(formula, data, dist_cutoff = 400, lag_cutoff = Inf,
            unit = "county", time = "year", lat = "lat", lon = "lon",
            batch_ram_opt = "none")
}

# ------------------------------------------------------------------------------
# Estimate seven specifications across three cost outcomes
# ------------------------------------------------------------------------------
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

# ------------------------------------------------------------------------------
# Table: value of information (tab:voi-table)
# ------------------------------------------------------------------------------
msummary(
  regs,
  coef_map = c(
    "squared_error"                        = "$\\beta_2: (e - \\mu)^2$",
    "squared_error:categoryHurricane"      = "$\\text{Hurricane } \\beta_2: (e - \\mu)^2$",
    "squared_error:categoryTropical Storm" = "$\\text{Sub-Hurricane } \\beta_2: (e - \\mu)^2$"),
  gof_omit = "R2|AIC|BIC|Log.|RMSE|FE|Std.Errors", fmt = "%.2f",
  stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01), add_rows = rows,
  title = "The Value of a Wind Speed Forecast Improvement (damages plus recovery expenditures, \\$ millions). \\label{tab:voi-table}",
  notes = list("$e$ is the county-hurricane-specific forecast error, and $\\\\mu$ is the hurricane-intensity bin-specific mean forecast error. Hurricane wind speeds exceed 33 m/s (74 mph); sub-hurricane wind speeds fall below. Conley spatial HAC standard errors with a 400 km cutoff."),
  threeparttable = TRUE, escape = FALSE,
  output = here("output/paper_table_voi.tex")
)
add_adjust_box(here("output/paper_table_voi.tex"))

# ------------------------------------------------------------------------------
# Table: precipitation coefficient (tab:precip_costs)
# ------------------------------------------------------------------------------
msummary(
  regs,
  coef_map = c(
    "squared_error"                        = "Wind, $\\beta^w_2: (e^w - \\mu^w)^2$",
    "squared_error:categoryHurricane"      = "Wind, Hurricane $\\beta^w_2$",
    "squared_error:categoryTropical Storm" = "Wind, Sub-Hurricane $\\beta^w_2$",
    "squared_error_precip"                 = "Precipitation, $\\beta^p_2: (e^p - \\mu^p)^2$"),
  gof_omit = "R2|AIC|BIC|Log.|RMSE|FE|Std.Errors", fmt = "%.3f",
  stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01), add_rows = rows,
  title = "Wind and Precipitation Forecast-Error Costs. \\label{tab:precip_costs}",
  notes = list("Full Stage 1 specification reporting both the wind ($\\\\beta^w_2$) and precipitation ($\\\\beta^p_2$) squared demeaned forecast-error coefficients, the latter omitted from the main table. Conley spatial HAC standard errors with a 400 km cutoff."),
  threeparttable = TRUE, escape = FALSE,
  output = here("output/paper_precip_costs.tex")
)
add_adjust_box(here("output/paper_precip_costs.tex"))

# ------------------------------------------------------------------------------
# Table: outcome robustness, preferred spec (tab:cost_outcomes). Reuses col (7).
# ------------------------------------------------------------------------------
msummary(
  list("Total cost (million \\$)"     = regs[[7]],
       "Cost / county GDP (\\%)"      = regs_pgdp[[7]],
       "Cost per capita (\\$/person)" = regs_pc[[7]]),
  coef_rename = c("$\\text{Hurricane } \\beta_2: (e - \\mu)^2$",
                  "$\\text{Sub-Hurricane } \\beta_2: (e - \\mu)^2$"),
  coef_omit = "(?i)(precip|level_error)",
  gof_omit = "R2|AIC|BIC|Log.|RMSE|FE|Std.Errors|Num.Obs.", fmt = "%.2f",
  stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  title = "Outcome robustness for the preferred specification (2005--2024 panel). \\label{tab:cost_outcomes}",
  notes = list("Preferred specification (Table @REF{tab:voi-table}), with Conley spatial HAC standard errors with a 400 km cutoff."),
  align = "lccc", threeparttable = TRUE, escape = FALSE,
  output = here("output/paper_cost_outcomes.tex")
)
.f <- here("output/paper_cost_outcomes.tex")
writeLines(gsub("@REF\\{", "\\\\ref{", readLines(.f)), .f)   # modelsummary strips the backslash; restore the cross-ref
add_adjust_box(.f)

# ------------------------------------------------------------------------------
# Table: one-storm-out jackknife of the preferred spec (tab:cost_jackknife)
# ------------------------------------------------------------------------------
spec7 <- voi_specs("total_cost")[[7]]
top_storms <- c("katrina", "harvey", "sandy", "ian", "ida", "michael",
                "laura", "irma", "wilma", "rita", "helene")
jack <- bind_rows(lapply(c("none", top_storms), function(s) {
  d  <- if (s == "none") load_voi() else load_voi() |> filter(hurricane != s)
  ce <- as.matrix(run_reg(spec7, d))
  hi <- grep("categoryHurricane", rownames(ce)); si <- grep("categoryTropical Storm", rownames(ce))
  tibble(dropped = s, n_rows = nrow(d),
         beta_hurr = ce[hi, "Estimate"], se_hurr = ce[hi, "Std. Error"],
         beta_sub  = ce[si, "Estimate"], se_sub  = ce[si, "Std. Error"])
}))
jack_disp <- jack |>
  mutate(across(c(beta_hurr, se_hurr, beta_sub, se_sub), \(x) sprintf("%.3f", x)),
         dropped = if_else(dropped == "none", "(none)", str_to_title(dropped)),
         n_rows  = format(n_rows, big.mark = ",")) |>
  transmute(Dropped = dropped, `Obs.` = n_rows,
            `$\\beta_2$: Hurricane` = paste0(beta_hurr, " (", se_hurr, ")"),
            `$\\beta_2$: Sub-Hurr.` = paste0(beta_sub, " (", se_sub, ")"))
kbl(jack_disp, format = "latex", booktabs = TRUE, escape = FALSE, linesep = "",
    caption = "One-storm-out jackknife of the marginal cost of forecast error. \\label{tab:cost_jackknife}",
    align = c("l", "r", "r", "r")) |>
  footnote(general = "Preferred-specification estimate after dropping each storm, with Conley spatial HAC standard errors (400 km cutoff) in parentheses.",
           general_title = "", threeparttable = TRUE, fixed_small_size = TRUE, escape = FALSE) |>
  as.character() |> writeLines(here("output/cost_jackknife.tex"))
add_adjust_box(here("output/cost_jackknife.tex"))

# ------------------------------------------------------------------------------
# Table: 3-day-ahead robustness (tab:cost_3day). Rebuilds the forecast error at
# the 3-day lead only, from the raw per-lead files, re-demeaned in the same bins.
# ------------------------------------------------------------------------------
panel <- load_voi()
wfiles <- list.files(here("data/raw/wind_error"),   pattern = "csv$", full.names = TRUE)
wfiles <- wfiles[!grepl("all_forecasts", wfiles)]
wind3  <- rbindlist(lapply(wfiles, function(f) {
  fread(f)[lead_days == 3, .(wind_error_3 = -mean(vmax_sust_dif_mean, na.rm = TRUE)),
           by = .(hurricane = name, county = as.numeric(gridid))]
}))
pfiles <- list.files(here("data/raw/precip_error"), pattern = "csv$", full.names = TRUE)
pfiles <- pfiles[!grepl("all_forecasts", pfiles)]
precip3 <- rbindlist(lapply(pfiles, function(f) {
  h <- gsub("_precip_errors?\\.csv$", "", basename(f))
  fread(f)[lead_days == 3, .(precip_error_3 = -mean(prec_dif_mean, na.rm = TRUE)),
           by = .(county = as.numeric(gridid))][, hurricane := h][]
}))
p3 <- panel |>
  inner_join(wind3, by = c("hurricane", "county")) |>
  inner_join(precip3, by = c("hurricane", "county")) |>
  group_by(wind_cut, hurricane) |>
  mutate(mean_w = mean(wind_error_3, na.rm = TRUE), mean_p = mean(precip_error_3, na.rm = TRUE)) |>
  ungroup() |>
  mutate(squared_error = (wind_error_3 - mean_w)^2, level_error = wind_error_3,
         squared_error_precip = (precip_error_3 - mean_p)^2, level_error_precip = precip_error_3)

reg_base <- run_reg(spec7, as.data.table(panel))
reg_3day <- run_reg(spec7, as.data.table(p3))
obs3 <- list(format(feols(spec7, as.data.table(panel))$nobs, big.mark = ","),
             format(feols(spec7, as.data.table(p3))$nobs,    big.mark = ","))
rows3 <- tribble(~term, ~"(1)", ~"(2)",
  "", "", "", "Observations", obs3[[1]], obs3[[2]],
  "Realized Wind/Precip Bins", "\\checkmark", "\\checkmark",
  "State-Hurricane FE", "\\checkmark", "\\checkmark", "County FE", "\\checkmark", "\\checkmark")
msummary(
  list("Baseline" = reg_base, "3-day only" = reg_3day),
  coef_map = c("squared_error:categoryHurricane"      = "$\\text{Hurricane } \\beta^{w}_{2}: (e-\\mu)^2$",
               "squared_error:categoryTropical Storm" = "$\\text{Sub-Hurricane } \\beta^{w}_{2}: (e-\\mu)^2$"),
  gof_omit = "R2|AIC|BIC|Log.|RMSE|FE|Std.Errors|Num.Obs.", fmt = "%.2f",
  stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01), add_rows = rows3,
  title = "Marginal cost of forecast error using 3-day-ahead forecasts only. \\label{tab:cost_3day}",
  notes = list("Preferred specification (Table @REF{tab:voi-table}) on total cost. Column (1) uses the 1--3 day average forecast error, column (2) the 3-day-ahead error only. Conley spatial HAC standard errors with a 400 km cutoff."),
  align = "lcc", threeparttable = TRUE, escape = FALSE,
  output = here("output/cost_3day_robustness.tex")
)
.f3 <- here("output/cost_3day_robustness.tex")
writeLines(gsub("@REF\\{", "\\\\ref{", readLines(.f3)), .f3)
add_adjust_box(.f3)
.l <- readLines(.f3); .ci <- grep("\\\\centering", .l)[1]
writeLines(append(.l, "\\setlength{\\tabcolsep}{26pt}", after = .ci), .f3)

# ------------------------------------------------------------------------------
# Stage 1 numbers (Hurricane beta_2 per cost outcome; consumed by Stage 3)
# ------------------------------------------------------------------------------
bhurr <- function(m) unname(coef(m)[grep("categoryHurricane", names(coef(m)))])
# full precision: Stage 3 reads these for the dollar valuation (the paper rounds when citing)
writeLines(c(
  sprintf("beta_hurr (Hurricane, $M per (m/s)^2)  = %.6f", bhurr(regs[[7]])),
  sprintf("beta_gdp_hurr (%% per (m/s)^2)          = %.6f", bhurr(regs_pgdp[[7]])),
  sprintf("beta_pc_hurr ($/person per (m/s)^2)     = %.6f", bhurr(regs_pc[[7]]))
), here("output/stage1_numbers.txt"))
cat("Wrote Stage 1 tables + output/stage1_numbers.txt\n")
