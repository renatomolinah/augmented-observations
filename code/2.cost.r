# ==============================================================================
# Stage 2 - Marginal cost of forecast error (beta)
# Estimates the loss-function curvature beta_2 (Conley spatial HAC) on the
# continental-US county-by-storm panel and builds the Stage 2 tables. The main
# specification uses the 3-day-ahead forecast error, the lead at which Stage 1
# finds the precision gain. Five storms (barry, cindy, hanna, harvey, nicholas)
# never received a 3-day forecast and so leave the main sample; the lead
# robustness table separates that sample change from the change in lead.
# Inputs:  data/clean/final_dataset.rds; data/raw/{wind,precip}_error/*.csv
# Outputs: output/{paper_table_voi,paper_voi_other_costs,paper_cost_outcomes,paper_precip_costs,
#          cost_jackknife,cost_lead_robustness}.tex; output/stage2_numbers.txt
# Run:     Rscript --vanilla code/2.cost.r
# ==============================================================================
if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, tidyverse, here, fixest, conleyreg, modelsummary, kableExtra)
options(conleyreg.verbose = FALSE)
options(modelsummary_format_numeric_latex = "plain")
source(here("code/functions_table.r"))

# ------------------------------------------------------------------------------
# Panels. load_voi() is the 37-storm panel carrying the 1--3 day average error;
# load_3day() rebuilds the error at the 3-day lead only from the raw per-lead
# files, re-demeaned in the same (wind bin x hurricane) cells as the cleaning.
# conleyreg mutates its data.table input by reference, so reload per fit.
# ------------------------------------------------------------------------------
# storm_time indexes the panel's true time dimension. A year holds up to six
# landfalls, so (county, year) does not identify an observation while
# (county, storm) does. conleyreg only checks this when nrow is divisible by the
# number of periods, so a year index passes silently on some samples and errors
# on others. The index is chronological, though with lag_cutoff = Inf every lag
# enters at weight one, so the ordering does not affect the variance matrix.
load_voi <- function() {
  readRDS(here("data/clean/final_dataset.rds")) |>
    as_tibble() |>
    drop_na(lat, lon, total_cost_pc, total_cost_per_gdp) |>
    mutate(storm_time = as.integer(factor(paste(year, hurricane))))
}

read_lead3 <- function() {
  wfiles <- list.files(here("data/raw/wind_error"), pattern = "csv$", full.names = TRUE)
  wfiles <- wfiles[!grepl("all_forecasts", wfiles)]
  wind3 <- rbindlist(lapply(wfiles, function(f) {
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
  list(wind = wind3, precip = precip3)
}
LEAD3 <- read_lead3()

# 3-day panel: attach the per-lead errors, then re-demean within (wind bin x storm)
make_3day <- function(panel) {
  panel |>
    inner_join(LEAD3$wind,   by = c("hurricane", "county")) |>
    inner_join(LEAD3$precip, by = c("hurricane", "county")) |>
    group_by(wind_cut, hurricane) |>
    mutate(mean_w = mean(wind_error_3, na.rm = TRUE),
           mean_p = mean(precip_error_3, na.rm = TRUE)) |>
    ungroup() |>
    mutate(squared_error        = (wind_error_3 - mean_w)^2,
           level_error          = wind_error_3,
           squared_error_precip = (precip_error_3 - mean_p)^2,
           level_error_precip   = precip_error_3)
}
load_3day <- function() make_3day(load_voi())

# storms the main (3-day) sample carries, and the ones that never got a 3-day forecast
storms_3day <- sort(unique(load_3day()$hurricane))
storms_all  <- sort(unique(load_voi()$hurricane))
storms_lost <- setdiff(storms_all, storms_3day)

run_reg <- function(formula, data = load_3day()) {
  conleyreg(formula, data, dist_cutoff = 400, lag_cutoff = Inf,
            unit = "county", time = "storm_time", lat = "lat", lon = "lon",
            batch_ram_opt = "none")
}

# ------------------------------------------------------------------------------
# Estimate seven specifications across three cost outcomes, on the 3-day panel
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
             data = load_3day())$nobs |> format(big.mark = ",")

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
restore_refs <- function(f) writeLines(gsub("@REF\\{", "\\\\ref{", readLines(f)), f)

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
  notes = list(sprintf("$e$ is the county-hurricane-specific forecast error at the 3-day-ahead lead, and $\\\\mu$ is the hurricane-intensity bin-specific mean forecast error. Hurricane wind speeds exceed 33 m/s (74 mph); sub-hurricane wind speeds fall below. The panel covers the %d landfalling hurricanes that received a 3-day forecast. Conley spatial HAC standard errors with a 400 km cutoff.", length(storms_3day))),
  threeparttable = TRUE, escape = FALSE,
  output = here("output/paper_table_voi.tex")
)
restore_refs(here("output/paper_table_voi.tex"))
add_adjust_box(here("output/paper_table_voi.tex"))

# ------------------------------------------------------------------------------
# Appendix table: the other two cost outcomes (per-GDP, per-capita) under the
# same fixed-effect ladder as the main total-cost table (tab:voi-other-costs)
# ------------------------------------------------------------------------------
msummary(
  list("(Damages + Recovery) / GDP (%)" = regs_pgdp,
       "(Damages + Recovery) Per Capita ($/person)" = regs_pc),
  coef_map = c(
    "squared_error"                        = "$\\beta_2: (e - \\mu)^2$",
    "squared_error:categoryHurricane"      = "$\\text{Hurricane } \\beta_2: (e - \\mu)^2$",
    "squared_error:categoryTropical Storm" = "$\\text{Sub-Hurricane } \\beta_2: (e - \\mu)^2$"),
  gof_omit = "R2|AIC|BIC|Log.|RMSE|FE|Std.Errors", fmt = "%.2f",
  stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01), add_rows = rows,
  title = "The Value of a Wind Speed Forecast Improvement: Cost per GDP and per Capita. \\label{tab:voi-other-costs}",
  notes = list(sprintf("The other two cost outcomes, cost per county GDP in percent and cost per capita in dollars per person, under the fixed-effect ladder of the main total-cost table on the 3-day-ahead forecast error. $e$ is the county-hurricane-specific forecast error at the 3-day-ahead lead, and $\\\\mu$ is the hurricane-intensity bin-specific mean forecast error. Hurricane wind speeds exceed 33 m/s (74 mph); sub-hurricane wind speeds fall below. The panel covers the %d landfalling hurricanes that received a 3-day forecast. Conley spatial HAC standard errors with a 400 km cutoff.", length(storms_3day))),
  threeparttable = TRUE, escape = FALSE, shape = "rbind",
  output = here("output/paper_voi_other_costs.tex")
)
add_adjust_box(here("output/paper_voi_other_costs.tex"))

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
  notes = list("The forecast-error cost specifications on the 3-day-ahead forecast error, reporting both the wind ($\\\\beta^w_2$) and precipitation ($\\\\beta^p_2$) squared demeaned forecast-error coefficients, the latter omitted from the main table. Conley spatial HAC standard errors with a 400 km cutoff."),
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
  title = "Outcome robustness for the preferred specification. \\label{tab:cost_outcomes}",
  notes = list("Preferred specification on the 3-day-ahead forecast error, with Conley spatial HAC standard errors with a 400 km cutoff."),
  align = "lccc", threeparttable = TRUE, escape = FALSE,
  output = here("output/paper_cost_outcomes.tex")
)
restore_refs(here("output/paper_cost_outcomes.tex"))   # modelsummary strips the backslash; restore the cross-ref
add_adjust_box(here("output/paper_cost_outcomes.tex"))

# ------------------------------------------------------------------------------
# Table: one-storm-out jackknife of the preferred spec (tab:cost_jackknife)
# ------------------------------------------------------------------------------
spec7 <- voi_specs("total_cost")[[7]]
top_storms <- intersect(c("katrina", "sandy", "ian", "ida", "michael",
                          "laura", "irma", "wilma", "rita", "helene"), storms_3day)
jack <- bind_rows(lapply(c("none", top_storms), function(s) {
  d  <- if (s == "none") load_3day() else load_3day() |> filter(hurricane != s)
  ce <- as.matrix(run_reg(spec7, d))
  hi <- grep("categoryHurricane", rownames(ce)); si <- grep("categoryTropical Storm", rownames(ce))
  tibble(dropped = s, n_rows = nrow(d),
         beta_hurr = ce[hi, "Estimate"], se_hurr = ce[hi, "Std. Error"],
         beta_sub  = ce[si, "Estimate"], se_sub  = ce[si, "Std. Error"])
}))
jstar <- function(b, se) { pv <- 2 * (1 - pnorm(abs(b / se))); ifelse(pv < 0.01, "***", ifelse(pv < 0.05, "**", ifelse(pv < 0.1, "*", ""))) }
jack_disp <- jack |>
  mutate(sh = jstar(beta_hurr, se_hurr), ss = jstar(beta_sub, se_sub),
         across(c(beta_hurr, se_hurr, beta_sub, se_sub), \(x) sprintf("%.3f", x)),
         dropped = if_else(dropped == "none", "(none)", str_to_title(dropped)),
         n_rows  = format(n_rows, big.mark = ",")) |>
  transmute(Dropped = dropped, `Obs.` = n_rows,
            `$\\beta_2$: Hurricane` = paste0(beta_hurr, sh, " (", se_hurr, ")"),
            `$\\beta_2$: Sub-Hurr.` = paste0(beta_sub, ss, " (", se_sub, ")"))
kbl(jack_disp, format = "latex", booktabs = TRUE, escape = FALSE, linesep = "",
    caption = "One-storm-out jackknife of the marginal cost of forecast error. \\label{tab:cost_jackknife}",
    align = c("l", "r", "r", "r")) |>
  footnote(general = "* p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01 Preferred specification on the 3-day-ahead forecast error, re-estimated after dropping each storm, with Conley spatial HAC standard errors (400 km cutoff) in parentheses.",
           general_title = "", threeparttable = TRUE, fixed_small_size = TRUE, escape = FALSE) |>
  as.character() |> writeLines(here("output/cost_jackknife.tex"))
add_adjust_box(here("output/cost_jackknife.tex"))

# ------------------------------------------------------------------------------
# Table: forecast-lead robustness (tab:cost_lead). Splits the move from the 1--3
# day average to the 3-day error into a lead change (col 1 vs 2, same storms) and
# a sample change (col 2 vs 3, the storms with no 3-day forecast).
# ------------------------------------------------------------------------------
# conleyreg reorders and adds columns to its input by reference, so hand every
# fit its own freshly built panel rather than reusing one object across calls.
load_same <- function() load_voi() |> filter(hurricane %in% storms_3day)
reg_lead3    <- run_reg(spec7, load_3day())
reg_avg_same <- run_reg(spec7, load_same())
reg_avg_full <- run_reg(spec7, load_voi())
nobs_of <- function(d) format(feols(spec7, as.data.table(d))$nobs, big.mark = ",")
rows3 <- tribble(~term, ~"(1)", ~"(2)", ~"(3)",
  "", "", "", "",
  "Observations", nobs_of(load_3day()), nobs_of(load_same()), nobs_of(load_voi()),
  "Storms", as.character(length(storms_3day)), as.character(length(storms_3day)),
    as.character(length(storms_all)),
  "Realized Wind/Precip Bins", "\\checkmark", "\\checkmark", "\\checkmark",
  "State-Hurricane FE", "\\checkmark", "\\checkmark", "\\checkmark",
  "County FE", "\\checkmark", "\\checkmark", "\\checkmark")
msummary(
  list("3-day error (main)" = reg_lead3, "1--3 day average, same storms" = reg_avg_same,
       "1--3 day average, all storms" = reg_avg_full),
  coef_map = c("squared_error:categoryHurricane"      = "$\\text{Hurricane } \\beta^{w}_{2}: (e-\\mu)^2$",
               "squared_error:categoryTropical Storm" = "$\\text{Sub-Hurricane } \\beta^{w}_{2}: (e-\\mu)^2$"),
  gof_omit = "R2|AIC|BIC|Log.|RMSE|FE|Std.Errors|Num.Obs.", fmt = "%.2f",
  stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01), add_rows = rows3,
  title = "Marginal cost of forecast error by forecast lead. \\label{tab:cost_lead}",
  notes = list(sprintf("Preferred specification on total cost. Column (1) is the main specification, using the 3-day-ahead forecast error. Column (2) pools the 1--3 day lead forecasts into an average error on the same %d storms, which also brings in the county-storms that never received a 3-day forecast. Column (3) pools the same leads on all %d storms, restoring the %d storms that never received a 3-day forecast (%s). Conley spatial HAC standard errors with a 400 km cutoff.",
    length(storms_3day), length(storms_all), length(storms_lost),
    paste(str_to_title(storms_lost), collapse = ", "))),
  align = "lccc", threeparttable = TRUE, escape = FALSE,
  output = here("output/cost_lead_robustness.tex")
)
restore_refs(here("output/cost_lead_robustness.tex"))
add_adjust_box(here("output/cost_lead_robustness.tex"))

# ------------------------------------------------------------------------------
# Stage 2 numbers (Hurricane beta_2 per cost outcome; consumed by Stage 3)
# ------------------------------------------------------------------------------
bhurr <- function(m) unname(coef(m)[grep("categoryHurricane", names(coef(m)))])
# full precision: Stage 3 reads these for the dollar valuation (the paper rounds when citing)
writeLines(c(
  sprintf("beta_hurr (Hurricane, $M per (m/s)^2)  = %.6f", bhurr(regs[[7]])),
  sprintf("beta_gdp_hurr (%% per (m/s)^2)          = %.6f", bhurr(regs_pgdp[[7]])),
  sprintf("beta_pc_hurr ($/person per (m/s)^2)     = %.6f", bhurr(regs_pc[[7]])),
  "",
  "-- main sample: 3-day-ahead forecast error --",
  sprintf("  storms                               = %d", length(storms_3day)),
  sprintf("  storms with no 3-day forecast        = %s", paste(storms_lost, collapse = ", ")),
  "",
  "-- lead decomposition, Hurricane beta_2 --",
  sprintf("  3-day error, %d storms               = %.6f", length(storms_3day), bhurr(reg_lead3)),
  sprintf("  1-3 day average, %d storms           = %.6f", length(storms_3day), bhurr(reg_avg_same)),
  sprintf("  1-3 day average, %d storms           = %.6f", length(storms_all),  bhurr(reg_avg_full))
), here("output/stage2_numbers.txt"))
cat("Wrote Stage 2 tables + output/stage2_numbers.txt\n")
