# ==============================================================================
# NOAA Report - Main Pipeline
# ------------------------------------------------------------------------------
# Stage 0: assemble manuscript Data-section tables (summary_dmg, data_denial)
# Stage 1: estimate the marginal cost of forecast errors (beta) on the VOI panel
# Stage 2: estimate the precision impact of augmented observations (delta)
# Stage 3: combine into a per-county-per-storm valuation
# ==============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  data.table, tidyverse, here, fixest, conleyreg, broom,
  patchwork, modelsummary, kableExtra
)
options(conleyreg.verbose = FALSE)

source(here("code/functions_table.r"))

# Resolve analysis inputs from the project data folder
voi_path <- here("data/clean/final_dataset.rds")
denial_path <- here("data/clean_denial/forecast_denial_data_with_lead_times.csv")

stopifnot(file.exists(voi_path))
stopifnot(file.exists(denial_path))

# ==============================================================================
# Stage 0 - Manuscript Data-section tables
# ==============================================================================

# --- summary_dmg.tex: per-hurricane summary stats (panel-driven) -------------
voi_full <- readRDS(voi_path) |>
  as_tibble() |>
  drop_na(lat, lon, total_cost_pc, total_cost_per_gdp)

data_summary <- voi_full |>
  group_by(hurricane, county) |>
  mutate(
    wind_obs   = max(wind_obs,   na.rm = TRUE),
    precip_obs = max(precip_obs, na.rm = TRUE)
  ) |>
  ungroup() |>
  arrange(hurricane, year, county) |>
  distinct(
    year, hurricane, state, county,
    wind_obs, wind_fc, wind_error,
    precip_obs, precip_fc, precip_error,
    damages, deaths, ex_post_spending, ex_ante_spending
  ) |>
  group_by(hurricane, year) |>
  summarise(
    max_wind_speed   = sprintf("%.2f", mean(wind_obs)),
    max_wind_error   = sprintf("%.2f", mean(abs(wind_error))),
    max_prec         = sprintf("%.2f", mean(precip_obs)),
    max_prec_error   = sprintf("%.2f", mean(abs(precip_error))),
    sum_damage       = sprintf("%.2f", sum(damages) / 10^9),
    ex_post_spending = sprintf("%.2f", sum(ex_post_spending) / 10^9),
    .groups = "drop"
  ) |>
  arrange(year) |>
  mutate(hurricane = str_to_title(hurricane)) |>
  rename(
    Hurricane               = hurricane,
    Year                    = year,
    `Wind Speed`            = max_wind_speed,
    `Wind Speed Error`      = max_wind_error,
    `Precipitation`         = max_prec,
    `Precipitation Error`   = max_prec_error,
    `Total Damage`          = sum_damage,
    `Recovery Expenditures` = ex_post_spending
  )

summary_units <- c("", "", "(m/s)", "abs(m/s)", "(mm)", "abs(mm)",
                   "(Billion \\$)", "(Billion \\$)")

summary_table <- kbl(
  data_summary,
  format    = "latex",
  caption   = "Summary Statistics by Hurricane.",
  label     = "summary_dmg",
  booktabs  = TRUE,
  escape    = FALSE,
  linesep   = c(""),
  col.names = summary_units,
  align     = c("l", rep("c", ncol(data_summary) - 1))
) |>
  add_header_above(
    setNames(rep(1, length(names(data_summary))), names(data_summary)),
    escape = FALSE
  ) |>
  footnote(
    general = paste(
      "The table includes all category 1 and greater hurricanes (maximum wind speeds",
      "$\\\\geq$ 33 m/s) that made landfall in the continental US.",
      "Wind speed, precipitation, and their associated errors are averaged across counties",
      "to the hurricane level. Damages and expenditures are summed across counties to the",
      "hurricane level. Wind speed is the maximum sustained wind speed in m/s, precipitation",
      "is the total precipitation in mm. Wind speed and precipitation errors are averages",
      "of the absolute values of county-level errors. All damages and expenditures are",
      "reported in billions of (2019) US\\\\$."
    ),
    threeparttable   = TRUE,
    fixed_small_size = TRUE,
    escape           = FALSE
  )

writeLines(as.character(summary_table), here("output/summary_dmg.tex"))
add_adjust_box(here("output/summary_dmg.tex"), before = "\\begin{threeparttable}", after = "\\end{threeparttable}")

# --- data_denial.tex: 8 landfalling hurricanes from Ditchek 2023 ------------
denial_summary <- tribble(
  ~Year, ~Storm,      ~Intensity,    ~Landfall,
  2017,  "Harvey",    "Category 4",  "26 Aug 0000",
  2017,  "Irma",      "Category 4",  "10 Sep 1200",
  2018,  "Florence",  "Category 1",  "14 Sep 0600",
  2018,  "Michael",   "Category 5",  "10 Oct 1200",
  2020,  "Hanna",     "Category 1",  "25 Jul 1800",
  2020,  "Isaias",    "Category 1",  " 4 Aug 0000",
  2020,  "Laura",     "Category 4",  "27 Aug 0600",
  2020,  "Sally",     "Category 2",  "16 Sep 0600"
)

denial_tex <- c(
  "\\begin{table}",
  "    \\centering",
  "    \\begin{threeparttable}",
  "      \\caption{Summary of the cases used.}",
  "      \\label{tab:data_denial}",
  "      \\begin{tabular}{c c c c c c}",
  "        \\hline",
  "        Year & Storm Name & Landfall Intensity & Landfall Time\\\\",
  "        \\hline",
  paste0("        ", denial_summary$Year, " & ", denial_summary$Storm,
         " & ", denial_summary$Intensity, " & ", denial_summary$Landfall, "\\\\"),
  "        \\hline",
  "      \\end{tabular}",
  "\\begin{tablenotes}",
  "\\small",
  "\\item \\textit{Note: }",
  "\\item The landfall intensity is categorized according to the Saffir-Simpson Hurricane Wind Scale, the landfall time is rounded to the next-earliest synoptic time (UTC).",
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)
writeLines(denial_tex, here("output/data_denial.tex"))

# ==============================================================================
# Stage 1 - Marginal Cost of Forecast Errors (beta)
# ==============================================================================
# Single-panel replica of Molina2024 Table 2 (total_cost outcome only),
# on the AO panel (2005-2024, 37 hurricanes).
# ==============================================================================

voi_df <- readRDS(voi_path) |>
  as_tibble() |>
  drop_na(lat, lon, total_cost_pc, total_cost_per_gdp)

cat("\n[Stage 1] VOI panel: ", nrow(voi_df), " rows, ",
    length(unique(voi_df$hurricane)), " hurricanes, ",
    min(voi_df$year), "-", max(voi_df$year), "\n", sep = "")


# conleyreg mutates its input data.table by reference, so each call must
# receive a fresh load (matches the pattern used in code_VOI/2.01.main_results.r).
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

regs <- list()
regs[[1]] <- run_reg(total_cost ~ squared_error + squared_error_precip |
                       county + state^hurricane)
regs[[2]] <- run_reg(total_cost ~ squared_error + squared_error_precip |
                       county + state^hurricane + wind_cut + precip_cut)
regs[[3]] <- run_reg(total_cost ~ squared_error + squared_error_precip +
                       level_error + level_error_precip |
                       county + state^hurricane + wind_cut + precip_cut)
regs[[4]] <- run_reg(total_cost ~ squared_error + squared_error_precip +
                       level_error + level_error_precip |
                       county + state^hurricane + county^month + wind_cut + precip_cut)
regs[[5]] <- run_reg(total_cost ~ squared_error + squared_error_precip +
                       level_error + level_error_precip |
                       county + state^hurricane + county^year + wind_cut + precip_cut)
regs[[6]] <- run_reg(total_cost ~ squared_error + squared_error_precip +
                       level_error + level_error_precip |
                       county + state^hurricane + county^year + county^month +
                       wind_cut + precip_cut)
regs[[7]] <- run_reg(total_cost ~ squared_error:category + squared_error_precip +
                       level_error + level_error_precip |
                       county + state^hurricane + wind_cut + precip_cut)

voi_coef_name <- grep("squared_error:categoryHurricane",
                      names(coef(regs[[7]])), value = TRUE)
beta_hurr <- unname(coef(regs[[7]])[voi_coef_name])

cat(sprintf("[Stage 1] beta (Hurricane, total_cost, col 7) = %.3f million $ per (m/s)^2\n",
            beta_hurr))

obs <- fixest::feols(
  total_cost ~ squared_error + squared_error_precip |
    county + state^hurricane + county^year + county^month + wind_cut + precip_cut,
  data = voi_df
)$nobs |> format(big.mark = ",")

rows <- tribble(
  ~term, ~"(1)", ~"(2)", ~"(3)", ~"(4)", ~"(5)", ~"(6)", ~"(7)",
  "", "", "", "", "", "", "", "",
  "Observations", obs, obs, obs, obs, obs, obs, obs,
  "", "", "", "", "", "", "", "",
  "Realized Wind/Precip Bins", "", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark",
  "Level Wind/Precip Error",  "", "", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark",
  "State-Hurricane FE",       "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark",
  "County FE",                "\\checkmark", "\\checkmark", "\\checkmark", "", "", "", "\\checkmark",
  "County-Month of Year FE",  "", "", "", "\\checkmark", "", "\\checkmark", "",
  "County-Year FE",           "", "", "", "", "\\checkmark", "\\checkmark", ""
)

msummary(
  regs,
  coef_rename = c(
    "$\\beta_2: (e - \\mu)^2$",
    "$\\text{Hurricane } \\beta_2: (e - \\mu)^2$",
    "$\\text{Sub-Hurricane } \\beta_2: (e - \\mu)^2$"
  ),
  coef_omit = c(-1, -5, -6),
  gof_omit  = "R2|AIC|BIC|Log.|RMSE|FE|Std.Errors",
  fmt       = "%.2f",
  stars     = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  add_rows  = rows,
  title     = "The Value of a Wind Speed Forecast Improvement. \\label{tab:voi-table}",
  notes     = list(
    "Standard errors are Conley Spatial HAC with a distance radius of 400 km for spatial correlation
     and arbitrary autocorrelation within counties. $e$ is the county-hurricane-specific forecast error,
     and $\\\\mu$ is the hurricane-intensity bin-specific mean forecast error. Hurricane wind speeds are
     those $\\\\geq$ 33 m/s (74 mph), while Sub-Hurricane wind speeds fall below that threshold."
  ),
  threeparttable = TRUE,
  escape = FALSE,
  output = here("output/table_voi.tex")
)

add_adjust_box(here("output/table_voi.tex"))

# ==============================================================================
# Stage 2 - Augmented Observations and Forecast Precision (delta)
# ==============================================================================

forecast_data_lead_times <- fread(denial_path) |>
  filter(name != "maria")

# Augmented panel figure ----------------------------------------------------
plot_top <- forecast_data_lead_times |>
  mutate(
    intensity_panel = case_when(
      wind_obs > 33   ~ "≥ 33 m/s",
      wind_obs > 17.5 ~ "≥ 17.5 m/s",
      wind_obs > 0    ~ "> 0 m/s"
    )
  ) |>
  filter(!is.na(augmented_obs)) |>
  ggplot(aes(x = wind_fc_sd, color = factor(lead_days))) +
  geom_density(linewidth = 1.1, alpha = 0.6) +
  scale_color_brewer(palette = "Dark2", name = "Lead days") +
  labs(x = "Wind speed SD (m/s)", y = "Density") +
  facet_wrap(~ intensity_panel, nrow = 1) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position  = "bottom",
    panel.grid.minor = element_blank(),
    plot.title       = element_text(face = "bold", size = 16),
    axis.title       = element_text(size = 13)
  )

plot_bottom <- forecast_data_lead_times |>
  filter(!is.na(augmented_obs), wind_obs > 33) |>
  mutate(
    augmented_obs_label = ifelse(augmented_obs == 1, "With Aug Obs", "Without Aug Obs")
  ) |>
  ggplot(aes(x = wind_fc_sd, color = augmented_obs_label)) +
  geom_density(linewidth = 1.1, alpha = 0.6) +
  scale_color_manual(
    values = c("With Aug Obs" = "#1B9E77", "Without Aug Obs" = "#D95F02"),
    name   = ""
  ) +
  labs(x = "Wind speed SD (m/s)", y = "Density") +
  facet_wrap(
    ~ factor(lead_days),
    nrow     = 1,
    labeller = labeller(.cols = function(x) paste("Lead time:", x, "days"))
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position  = "bottom",
    panel.grid.minor = element_blank(),
    plot.title       = element_text(face = "bold", size = 16),
    axis.title       = element_text(size = 13)
  )

augmented_panel <- plot_top / plot_bottom

ggsave(
  plot     = augmented_panel,
  filename = here("output/augmented_panel.png"),
  width    = 12,
  height   = 8,
  dpi      = 300
)

# Stage 2 estimation sample -------------------------------------------------
reg_data <- forecast_data_lead_times |>
  filter(!is.na(augmented_obs), lead_days == 3, wind_obs > 33)

mod_precision <- feols(wind_fc_sd ~ augmented_obs | county + name, data = reg_data)
delta <- abs(coef(mod_precision)["augmented_obs"])

# Controls table ------------------------------------------------------------
regs_delta <- list()
regs_delta[[1]] <- feols(wind_fc_sd ~ augmented_obs | county,
                         data = reg_data, vcov = ~county)
regs_delta[[2]] <- feols(wind_fc_sd ~ augmented_obs | county + name,
                         data = reg_data, vcov = ~county)
regs_delta[[3]] <- feols(wind_fc_sd ~ augmented_obs + wind_obs | county,
                         data = reg_data, vcov = ~county)
regs_delta[[4]] <- feols(wind_fc_sd ~ augmented_obs + wind_obs | county + name,
                         data = reg_data, vcov = ~county)

obs_delta <- regs_delta[[1]]$nobs |> format(big.mark = ",")

rows_delta <- tribble(
  ~term, ~"(1)", ~"(2)", ~"(3)", ~"(4)",
  "", "", "", "", "",
  "Observations",  obs_delta, obs_delta, obs_delta, obs_delta,
  "", "", "", "", "",
  "Wind Controls", "", "", "\\checkmark", "\\checkmark",
  "County FE",     "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark",
  "Hurricane FE",  "", "\\checkmark", "", "\\checkmark"
)

msummary(
  regs_delta,
  coef_rename = c("augmented_obs" = "$\\delta$: Augmented Observations"),
  coef_omit   = "wind_obs",
  gof_omit    = "Num.Obs.|R2|AIC|BIC|Log.|RMSE|FE|Std.Errors",
  fmt         = "%.3f",
  stars       = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  add_rows    = rows_delta,
  title       = "Regression Analysis of the Effect of Augmented Observations in Ex-Ante Uncertainty \\label{tab:noaa-controls}",
  notes       = list(
    "Standard errors clustered at the county level. The sample is restricted to the 2017--2020 augmented-observations denial archive
     at the 3-day lead time and hurricane-force realizations ($\\\\geq$ 33 m/s); Hurricane Maria is excluded because of its Puerto Rico landfall.
     The coefficient $\\\\delta$ is the average reduction in the ex-ante wind-speed forecast standard deviation attributable to the
     assimilation of dropsonde profiles (the ALL configuration of \\\\citet{DitchekEA2023}); the negative sign reflects that AO assimilation
     lowers ensemble spread relative to the no-dropsonde (NO) counterfactual. Wind Controls includes the realized wind speed (m/s) as a
     continuous covariate."
  ),
  threeparttable = TRUE,
  escape      = FALSE,
  output      = here("output/noaa_report_controls_table.tex")
)

add_adjust_box(here("output/noaa_report_controls_table.tex"))

# Stage 2 in-text numbers (noise context for the manuscript paragraph) -------
denial_full <- forecast_data_lead_times |> filter(!is.na(augmented_obs))

delta_se   <- sqrt(diag(vcov(mod_precision)))["augmented_obs"]
delta_p    <- 2 * (1 - pnorm(abs(coef(mod_precision)["augmented_obs"]) / delta_se))

# (i) Stage-3 baseline: HF + 3-day, AO-only
hf3_ao    <- denial_full |> filter(lead_days == 3, wind_obs > 33, augmented_obs == 1)
sigma_bar_hf3ao <- mean(hf3_ao$wind_fc_sd, na.rm = TRUE)
sigma_sd_hf3ao  <- sd(hf3_ao$wind_fc_sd,   na.rm = TRUE)

# (ii) 3-day lead, all wind speeds, AO-only (broader noise benchmark)
lead3     <- denial_full |> filter(lead_days == 3, augmented_obs == 1)
sigma_bar_lead3 <- mean(lead3$wind_fc_sd, na.rm = TRUE)
sigma_sd_lead3  <- sd(lead3$wind_fc_sd,   na.rm = TRUE)

stage2_lines <- c(
  sprintf("delta                              = %.3f m/s",       delta),
  sprintf("delta SE                           = %.3f",           delta_se),
  sprintf("delta p-value                      = %.4f",           delta_p),
  "",
  "-- HF + 3-day lead, AO-only (Stage 3 baseline) --",
  sprintf("  N                                = %d",             nrow(hf3_ao)),
  sprintf("  sigma_bar                        = %.2f m/s",       sigma_bar_hf3ao),
  sprintf("  sd(sigma)                        = %.2f m/s",       sigma_sd_hf3ao),
  sprintf("  delta / sigma_bar                = %.2f%% of mean", 100 * delta / sigma_bar_hf3ao),
  sprintf("  delta / sd(sigma)                = %.2f SDs",       delta / sigma_sd_hf3ao),
  "",
  "-- 3-day lead, all wind speeds, pooled (broader noise benchmark) --",
  sprintf("  N                                = %d",             nrow(lead3)),
  sprintf("  sigma_bar                        = %.2f m/s",       sigma_bar_lead3),
  sprintf("  sd(sigma)                        = %.2f m/s",       sigma_sd_lead3),
  sprintf("  delta / sigma_bar                = %.2f%% of mean", 100 * delta / sigma_bar_lead3),
  sprintf("  delta / sd(sigma)                = %.2f SDs",       delta / sigma_sd_lead3)
)

cat("\n--- Stage 2 in-text numbers ---\n")
cat(paste(stage2_lines, collapse = "\n"), "\n")
writeLines(stage2_lines, here("output/stage2_numbers.txt"))

# ==============================================================================
# Stage 3 - Projecting Economic Gains
# ==============================================================================

baseline_sd <- reg_data |>
  filter(augmented_obs == 1) |>
  summarise(mean_sd = mean(wind_fc_sd, na.rm = TRUE)) |>
  pull(mean_sd)

delta_sigma_sq <- (baseline_sd + delta)^2 - baseline_sd^2

value_per_county <- beta_hurr * delta_sigma_sq

sample_benefits <- reg_data |>
  group_by(name) |>
  summarise(
    affected_counties = n(),
    total_benefit_M   = n() * value_per_county
  ) |>
  arrange(desc(total_benefit_M))

total_value_B <- sum(sample_benefits$total_benefit_M) / 1000
total_value_M <- total_value_B * 1000

stage3_lines <- c(
  sprintf("beta_hurr (Hurricane, $M per (m/s)^2)  = %.3f",     beta_hurr),
  sprintf("N (HF impacts at 3-day lead)           = %d",       nrow(reg_data)),
  sprintf("delta (m/s)                            = %.3f",     delta),
  sprintf("baseline sigma_All (m/s)               = %.2f",     baseline_sd),
  sprintf("delta_sigma_sq = (sigma+delta)^2-sigma^2 = %.2f (m/s)^2", delta_sigma_sq),
  sprintf("Value per Affected County ($M)         = %.2f",     value_per_county),
  sprintf("Affected hurricane-force counties      = %d",       nrow(reg_data)),
  sprintf("Total Retrospective Value ($B)         = %.2f",     total_value_B)
)
cat("\n--- Valuation Summary (in-sample, 2017-2020 denial experiments) ---\n")
cat(paste(stage3_lines, collapse = "\n"), "\n")
writeLines(stage3_lines, here("output/stage3_numbers.txt"))

# ==============================================================================
# Stage 4 - Cost-Benefit benchmark vs ANSER (2016) and NOAA Aircraft Plan (2022)
# ==============================================================================
# Reference values are treated as fixed literature inputs. Update only if a
# newer official NOAA cost-benefit report supersedes the 2016 AoA.

anser2016 <- list(
  # 2016 ANSER External Benefits Report, BY$2024 NPV at 7% discount, 2030–2055.
  # Figure 34, p.60.
  npv_evacuation    = 138.84,
  npv_air_traffic   = 134.18,
  npv_maritime      =   0.18,
  npv_chemistry     = 227.65,
  npv_total         = 500.86,
  lcc_alt2_c130j_M  = 680.00,   # Alt 2 (two modified C-130J) – least-cost alt, Fig 33 p.59
  bcr_base          =   0.75,   # base-case BCR, Section 4 p.61
  years_horizon     =  25       # 2030 (retirement) through 2055 (end of service life)
)

# 2022 NOAA Aircraft Recapitalization Plan + GAO-25-107210 (Jan 2025):
# total acquisition cost for four C-130J aircraft.
noaa_capital <- list(
  total_acq_B  = 1.40,          # GAO-25-107210, undiscounted total acquisition for 4 C-130J
  remaining_B  = 0.678          # remaining to be funded as of January 2025
)

cb_lines <- c(
  "-- ANSER (2016) External Benefits AoA, NPV BY$2024 @ 7%, 2030-2055 --",
  sprintf("  Evacuation avoided NPV ($M)             = %.2f", anser2016$npv_evacuation),
  sprintf("  Air traffic disruption NPV ($M)         = %.2f", anser2016$npv_air_traffic),
  sprintf("  Maritime traffic disruption NPV ($M)    = %.2f", anser2016$npv_maritime),
  sprintf("  Air chemistry research NPV ($M)         = %.2f", anser2016$npv_chemistry),
  sprintf("  TOTAL benefit NPV ($M)                  = %.2f", anser2016$npv_total),
  sprintf("  LCC Alt 2 (C-130J) NPV ($M)             = %.2f", anser2016$lcc_alt2_c130j_M),
  sprintf("  Base-case BCR                           = %.2f", anser2016$bcr_base),
  "",
  "-- Our retrospective valuation (2017-2020) --",
  sprintf("  Total retrospective value ($M)          = %.2f", total_value_M),
  sprintf("  Active years covered                    = %d",   4L),
  sprintf("  Annualized rate ($M/yr)                 = %.2f", total_value_M / 4),
  "",
  "-- Comparisons --",
  sprintf("  Our 2017-2020 value / ANSER 25-yr NPV   = %.2fx",
          total_value_M / anser2016$npv_total),
  sprintf("  Our annualized / ANSER annualized       = %.2fx",
          (total_value_M / 4) / (anser2016$npv_total / anser2016$years_horizon)),
  sprintf("  Our 2017-2020 value / ANSER Alt 2 LCC   = %.2fx",
          total_value_M / anser2016$lcc_alt2_c130j_M),
  sprintf("  Our 2017-2020 value / NOAA total acq    = %.2fx",
          total_value_B / noaa_capital$total_acq_B),
  "",
  "-- NOAA Aircraft Recapitalization (NOAA 2022 / GAO 2025) --",
  sprintf("  Total acquisition, 4x C-130J ($B)       = %.2f", noaa_capital$total_acq_B),
  sprintf("  Remaining to fund (Jan 2025, $B)        = %.2f", noaa_capital$remaining_B)
)
cat("\n--- Stage 4 cost-benefit benchmark ---\n")
cat(paste(cb_lines, collapse = "\n"), "\n")
writeLines(cb_lines, here("output/stage4_costbenefit_numbers.txt"))
