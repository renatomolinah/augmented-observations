# ==============================================================================
# 2.2 Stage 1 robustness using only 3-day-ahead forecast errors (vs the headline
# 1-3 day average), rebuilt from the raw per-lead files and re-demeaned in the same
# bins. Output: figures and tables/cost_3day_robustness.tex. Needs the Box mount.
# ==============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, tidyverse, here, fixest, conleyreg, modelsummary, kableExtra)
options(conleyreg.verbose = FALSE)
options(modelsummary_format_numeric_latex = "plain")
source(here("code/functions_table.r"))

panel <- readRDS(here("data/clean/final_dataset.rds")) |>
  as_tibble() |>
  drop_na(lat, lon, total_cost_pc, total_cost_per_gdp)

# --- Build 3-day-lead forecast errors from the raw per-lead files -------------
# Sign convention matches the panel (1.07.merge_data.r): error = -(forecast - obs),
# so a positive value is a forecast underestimate.
wind_files <- list.files(here("data/raw/wind_error"), pattern = "csv$", full.names = TRUE)
wind_files <- wind_files[!grepl("all_forecasts", wind_files)]
wind3 <- rbindlist(lapply(wind_files, function(f) {
  x <- fread(f)[lead_days == 3]
  x[, .(wind_error_3 = -mean(vmax_sust_dif_mean, na.rm = TRUE)),
    by = .(hurricane = name, county = as.numeric(gridid))]
}))

precip_files <- list.files(here("data/raw/precip_error"), pattern = "csv$", full.names = TRUE)
precip_files <- precip_files[!grepl("all_forecasts", precip_files)]
precip3 <- rbindlist(lapply(precip_files, function(f) {
  h <- gsub("_precip_errors?\\.csv$", "", basename(f))
  x <- fread(f)[lead_days == 3]
  x[, .(precip_error_3 = -mean(prec_dif_mean, na.rm = TRUE)),
    by = .(county = as.numeric(gridid))][, hurricane := h][]
}))

# --- Merge onto the panel and re-demean within the panel's (wind_cut, hurricane)
# bins, exactly as 1.07.merge_data.r builds squared_error from wind_error. --------
p3 <- panel |>
  inner_join(wind3,   by = c("hurricane", "county")) |>
  inner_join(precip3, by = c("hurricane", "county")) |>
  group_by(wind_cut, hurricane) |>
  mutate(
    mean_wind_error_3   = mean(wind_error_3,   na.rm = TRUE),
    mean_precip_error_3 = mean(precip_error_3, na.rm = TRUE)
  ) |>
  ungroup() |>
  mutate(
    # overwrite the panel error terms with their 3-day analogs so the column (7)
    # formula and coefficient names are identical across the two columns
    squared_error        = (wind_error_3   - mean_wind_error_3)^2,
    level_error          = wind_error_3,
    squared_error_precip = (precip_error_3 - mean_precip_error_3)^2,
    level_error_precip   = precip_error_3
  )

cat(sprintf("panel rows: %s   3-day rows: %s\n",
            format(nrow(panel), big.mark = ","), format(nrow(p3), big.mark = ",")))

# --- Estimate column (7) on each sample --------------------------------------
spec7 <- total_cost ~ squared_error:category + squared_error_precip +
  level_error + level_error_precip | county + state^hurricane + wind_cut + precip_cut

run_reg <- function(d) {
  conleyreg(
    formula = spec7, data = as.data.table(d),
    dist_cutoff = 400, lag_cutoff = Inf,
    unit = "county", time = "year", lat = "lat", lon = "lon",
    batch_ram_opt = "none"
  )
}

reg_base <- run_reg(panel)   # baseline: 1-3 day average (reproduces headline)
reg_3day <- run_reg(p3)      # robustness: 3-day lead only

# conleyreg only adjusts the vcov, so point estimates and N equal the OLS fit;
# use feols for a reliable coefficient / sample-size readout.
fe_base <- feols(spec7, data = as.data.table(panel))
fe_3day <- feols(spec7, data = as.data.table(p3))
cat(sprintf("\nHurricane beta_2  baseline: %.3f   3-day: %.3f\n",
            coef(fe_base)["squared_error:categoryHurricane"],
            coef(fe_3day)["squared_error:categoryHurricane"]))
obs_base <- format(fe_base$nobs, big.mark = ",")
obs_3day <- format(fe_3day$nobs, big.mark = ",")

rows <- tribble(
  ~term, ~"(1)", ~"(2)",
  "", "", "",
  "Observations", obs_base, obs_3day,
  "Realized Wind/Precip Bins", "\\checkmark", "\\checkmark",
  "State-Hurricane FE", "\\checkmark", "\\checkmark",
  "County FE", "\\checkmark", "\\checkmark"
)

msummary(
  list("Baseline" = reg_base, "3-day only" = reg_3day),
  coef_map = c(
    "squared_error:categoryHurricane"      = "$\\text{Hurricane } \\beta^{w}_{2}: (e-\\mu)^2$",
    "squared_error:categoryTropical Storm" = "$\\text{Sub-Hurricane } \\beta^{w}_{2}: (e-\\mu)^2$"
  ),
  gof_omit = "R2|AIC|BIC|Log.|RMSE|FE|Std.Errors|Num.Obs.",
  fmt = "%.2f",
  stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  add_rows = rows,
  title = "Marginal cost of forecast error using 3-day-ahead forecasts only. \\label{tab:cost_3day}",
  notes = list("Preferred specification (Table @REF{tab:voi-table}) on total cost. Column (1) uses the 1--3 day average forecast error, column (2) the 3-day-ahead error only. Conley spatial HAC standard errors with a 400 km cutoff."),
  align = "lcc",
  threeparttable = TRUE,
  escape = FALSE,
  output = here("augmented_observations/figures and tables/cost_3day_robustness.tex")
)
.f <- here("augmented_observations/figures and tables/cost_3day_robustness.tex")
# modelsummary strips backslashes in the notes pass; restore the \ref cross-ref.
writeLines(gsub("@REF\\{", "\\\\ref{", readLines(.f)), .f)
add_adjust_box(.f)
# widen inter-column spacing for this narrow two-column table
.lines <- readLines(.f)
.ci <- grep("\\\\centering", .lines)[1]
.lines <- append(.lines, "\\setlength{\\tabcolsep}{26pt}", after = .ci)
writeLines(.lines, .f)
cat("\nWrote", .f, "\n")
