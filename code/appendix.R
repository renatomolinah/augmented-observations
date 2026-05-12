# ==============================================================================
# NOAA Report - Appendices A, B, C
# ------------------------------------------------------------------------------
# A. Beta robustness on the VOI panel:
#    - Sample comparison (2005-2022 vs 2005-2024)
#    - Three outcomes (total_cost, per_GDP, per_capita) in col-7 spec
#    - One-storm-out jackknife on the col-7 Hurricane beta
# B. Delta robustness on the denial panel:
#    - Lead-time grid (1/2/3/4 days)
#    - Intensity threshold grid (>25, >33, >40 m/s)
#    - One-storm-out jackknife across the 8 hurricane denials
#    - Maria-in vs Maria-out
# C. Storm-level decomposition of the $2.91B total valuation.
#
# Uses feols with cluster SEs throughout (point estimates identical to the
# conleyreg main table; SE methodology noted in each table caption).
# ==============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  data.table, tidyverse, here, fixest, conleyreg, broom,
  patchwork, modelsummary, kableExtra, ggplot2
)

source(here("code/functions_table.r"))
options(conleyreg.verbose = FALSE)

# -- paths ---------------------------------------------------------------------
voi_path <- here("data/clean/final_dataset.rds")
denial_path <- here("data/clean_denial/forecast_denial_data_with_lead_times.csv")
stopifnot(file.exists(voi_path), file.exists(denial_path))

# Stars for all msummary tables
stars_econ <- c("*" = 0.1, "**" = 0.05, "***" = 0.01)

# ==============================================================================
# Appendix A — beta robustness (VOI panel)
# ==============================================================================

voi_load <- function() {
  readRDS(voi_path) |>
    as_tibble() |>
    drop_na(lat, lon, total_cost_pc, total_cost_per_gdp)
}
voi_load_drop  <- function(storm) voi_load() |> filter(hurricane != storm)
voi_load_until <- function(yr)    voi_load() |> filter(year <= yr)

# Spec used throughout: paper's preferred col (7) specification.
spec_col7 <- function(lhs = "total_cost") {
  as.formula(sprintf(
    "%s ~ squared_error:category + squared_error_precip + level_error + level_error_precip | county + state^hurricane + wind_cut + precip_cut",
    lhs
  ))
}

# Conley HAC 400 km / unbounded lag — matches main Table 2 col (7) in
# NOAA_report.R. Each call gets a fresh load to avoid data.table by-reference
# mutation inside conleyreg.
run_conley <- function(formula, data) {
  conleyreg(
    formula = formula, data = data,
    dist_cutoff = 400, lag_cutoff = Inf,
    unit = "county", time = "year",
    lat = "lat", lon = "lon",
    batch_ram_opt = "none"
  )
}

# A.1 — sample window comparison ----------------------------------------------
mA1_2024 <- run_conley(spec_col7("total_cost"), voi_load())
mA1_2022 <- run_conley(spec_col7("total_cost"), voi_load_until(2022))

voi_full <- voi_load()  # for downstream non-regression summaries

# conleyreg returns a coeftest object, not a fixest; nobs lives in an attribute
n_obs <- function(m) {
  if (!is.null(attr(m, "nobs"))) attr(m, "nobs")
  else if (!is.null(m$nobs))     m$nobs
  else NA_integer_
}
rowsA1 <- tribble(
  ~term, ~"(1) 2005-2022", ~"(2) 2005-2024",
  "Observations",
    format(nrow(voi_load_until(2022)), big.mark = ","),
    format(nrow(voi_load()),           big.mark = ","),
  "Storms",
    as.character(length(unique(voi_load_until(2022)$hurricane))),
    as.character(length(unique(voi_load()$hurricane)))
)

msummary(
  list("2005-2022 (paper sample)" = mA1_2022,
       "2005-2024 (updated sample)" = mA1_2024),
  coef_rename = c(
    "$\\beta_2$: Hurricane",
    "$\\beta_2$: Sub-Hurricane"
  ),
  coef_omit = "(?i)(precip|level_error)",
  gof_omit  = "R2|AIC|BIC|Log.|RMSE|FE|Std.Errors|Num.Obs.",
  fmt       = "%.3f",
  stars     = stars_econ,
  add_rows  = rowsA1,
  title     = "Sample-window robustness: $\\hat{\\beta}^w_{Hurr}$ before and after adding 2023--2024 storms. \\label{tab:appA_sample}",
  notes     = list("Coefficients on level errors and the precipitation squared error are absorbed into the controls and omitted from display. Standard errors are Conley Spatial HAC with a 400~km distance cutoff."),
  threeparttable = TRUE,
  escape    = FALSE,
  output    = here("output/appA_sample_compare.tex")
)
add_adjust_box(here("output/appA_sample_compare.tex"))

# A.2 — three outcomes (col-7 spec) -------------------------------------------
mA2_tot <- run_conley(spec_col7("total_cost"),    voi_load())
mA2_gdp <- run_conley(
  total_cost_per_gdp * 100 ~ squared_error:category + squared_error_precip +
    level_error + level_error_precip |
    county + state^hurricane + wind_cut + precip_cut,
  voi_load()
)
mA2_pc  <- run_conley(spec_col7("total_cost_pc"), voi_load())

msummary(
  list("Total cost (\\$M)"        = mA2_tot,
       "Cost / county GDP (\\%)" = mA2_gdp,
       "Cost per capita (\\$)"   = mA2_pc),
  coef_rename = c(
    "$\\beta_2$: Hurricane",
    "$\\beta_2$: Sub-Hurricane"
  ),
  coef_omit = "(?i)(precip|level_error)",
  gof_omit  = "R2|AIC|BIC|Log.|RMSE|FE|Std.Errors|Num.Obs.",
  fmt       = "%.3f",
  stars     = stars_econ,
  title     = "Outcome robustness: $\\hat{\\beta}^w_{Hurr}$ across cost outcomes (2005--2024 panel). \\label{tab:appA_outcomes}",
  notes     = list("All specifications match column (7) of Table~\\ref{tab:voi-table} with county and state-by-hurricane fixed effects plus realized wind and precipitation bins. Standard errors are Conley Spatial HAC with a 400~km distance cutoff."),
  threeparttable = TRUE,
  escape    = FALSE,
  output    = here("output/appA_outcomes.tex")
)
add_adjust_box(here("output/appA_outcomes.tex"))

# A.3 — one-storm-out jackknife on Hurricane beta -----------------------------
top_storms <- c("katrina", "harvey", "sandy", "ian", "ida",
                "michael", "laura", "irma", "wilma", "rita")
jack_rows <- list()
for (s in c("none", top_storms)) {
  d <- if (s == "none") voi_load() else voi_load_drop(s)
  m <- run_conley(spec_col7("total_cost"), d)
  cf <- coef(m); se <- sqrt(diag(vcov(m)))
  hurr_ix <- grep("categoryHurricane", names(cf))
  sub_ix  <- grep("categoryTropical Storm", names(cf))
  jack_rows[[s]] <- tibble(
    dropped = s,
    n_storms = length(unique(d$hurricane)),
    n_rows = nrow(d),
    beta_hurr = cf[hurr_ix],
    se_hurr = se[hurr_ix],
    beta_subhurr = cf[sub_ix],
    se_subhurr = se[sub_ix]
  )
}
jack_df <- bind_rows(jack_rows) |>
  mutate(across(c(beta_hurr, se_hurr, beta_subhurr, se_subhurr), \(x) sprintf("%.3f", x))) |>
  mutate(
    dropped = if_else(dropped == "none", "(none)", str_to_title(dropped)),
    n_rows  = format(n_rows, big.mark = ",")
  )

jack_disp <- jack_df |>
  transmute(
    Dropped        = dropped,
    `Storms`       = n_storms,
    `Obs.`         = n_rows,
    `$\\beta_2$: Hurricane` = paste0(beta_hurr, " (", se_hurr, ")"),
    `$\\beta_2$: Sub-Hurr.` = paste0(beta_subhurr, " (", se_subhurr, ")")
  )

jack_tex <- kbl(
  jack_disp,
  format    = "latex",
  booktabs  = TRUE,
  escape    = FALSE,
  caption   = "One-storm-out jackknife: stability of column (7) estimates. \\label{tab:appA_jackknife}",
  linesep   = "",
  align     = c("l", "c", "c", "c", "c")
) |>
  footnote(
    general = "Each row reports column (7) estimates after dropping the indicated storm from the panel. Conley Spatial HAC standard errors (400 km cutoff) in parentheses. The baseline row drops no storm.",
    threeparttable = TRUE,
    fixed_small_size = TRUE,
    escape = FALSE
  )
writeLines(as.character(jack_tex), here("output/appA_jackknife.tex"))
add_adjust_box(here("output/appA_jackknife.tex"))

# ==============================================================================
# Appendix B — delta robustness (denial panel)
# ==============================================================================

denial <- fread(denial_path) |>
  filter(name != "maria", !is.na(augmented_obs)) |>
  as_tibble()

denial_with_maria <- fread(denial_path) |>
  filter(!is.na(augmented_obs)) |>
  as_tibble()

delta_spec <- function(d) {
  feols(wind_fc_sd ~ augmented_obs | county + name, data = d, vcov = ~county)
}

# B.1 — lead-time grid --------------------------------------------------------
B1 <- map_dfr(c(1, 2, 3, 4), function(lt) {
  d <- denial |> filter(lead_days == lt, wind_obs > 33)
  if (nrow(d) < 10) return(NULL)
  m <- delta_spec(d)
  cf <- coef(m)["augmented_obs"]; se <- sqrt(diag(vcov(m)))["augmented_obs"]
  tibble(lead_days = lt, n = nrow(d),
         delta = abs(cf), se = se,
         p = 2*(1 - pnorm(abs(cf)/se)),
         sigma_bar = mean(d$wind_fc_sd, na.rm=TRUE))
})

B1_disp <- B1 |>
  transmute(
    `Lead (days)` = lead_days,
    `Obs.` = format(n, big.mark = ","),
    `$\\bar{\\sigma}$ (m/s)` = sprintf("%.2f", sigma_bar),
    `$\\delta$ (m/s)` = sprintf("%.3f", delta),
    `SE` = sprintf("(%.3f)", se),
    `p` = ifelse(p < 0.001, "$<$0.001", sprintf("%.3f", p))
  )

B1_tex <- kbl(B1_disp, format = "latex", booktabs = TRUE, escape = FALSE,
              caption = "Lead-time sensitivity of the augmented-observations precision gain, hurricane-force realizations. \\label{tab:appB_leadtime}",
              align = c("c","c","c","c","c","c"), linesep = "") |>
  footnote(general = "Each row reports the controls-table specification estimated on the indicated lead-time subset of hurricane-force observations (wind > 33 m/s). County-clustered standard errors in parentheses.",
           threeparttable = TRUE, fixed_small_size = TRUE)
writeLines(as.character(B1_tex), here("output/appB_leadtime.tex"))
add_adjust_box(here("output/appB_leadtime.tex"))

# B.2 — intensity threshold grid ----------------------------------------------
B2 <- map_dfr(c(25, 33, 40), function(thr) {
  d <- denial |> filter(lead_days == 3, wind_obs > thr)
  if (nrow(d) < 10) return(NULL)
  m <- delta_spec(d)
  cf <- coef(m)["augmented_obs"]; se <- sqrt(diag(vcov(m)))["augmented_obs"]
  tibble(threshold = thr, n = nrow(d),
         delta = abs(cf), se = se,
         p = 2*(1 - pnorm(abs(cf)/se)),
         sigma_bar = mean(d$wind_fc_sd, na.rm=TRUE))
})

B2_disp <- B2 |>
  transmute(
    `Threshold` = sprintf("$>%d$ m/s", threshold),
    `Obs.` = format(n, big.mark = ","),
    `$\\bar{\\sigma}$ (m/s)` = sprintf("%.2f", sigma_bar),
    `$\\delta$ (m/s)` = sprintf("%.3f", delta),
    `SE` = sprintf("(%.3f)", se),
    `p` = ifelse(p < 0.001, "$<$0.001", sprintf("%.3f", p))
  )

B2_tex <- kbl(B2_disp, format = "latex", booktabs = TRUE, escape = FALSE,
              caption = "Intensity-threshold sensitivity of $\\delta$, 3-day lead. \\label{tab:appB_intensity}",
              align = c("l","c","c","c","c","c"), linesep = "") |>
  footnote(general = "County-clustered standard errors in parentheses.",
           threeparttable = TRUE, fixed_small_size = TRUE, escape = FALSE)
writeLines(as.character(B2_tex), here("output/appB_intensity.tex"))
add_adjust_box(here("output/appB_intensity.tex"))

# B.3 — one-storm-out jackknife on delta --------------------------------------
hf3_storms <- denial |>
  filter(lead_days == 3, wind_obs > 33) |>
  distinct(name) |> pull(name)

B3 <- map_dfr(c("(none)", hf3_storms), function(s) {
  d <- if (s == "(none)") denial else denial |> filter(name != s)
  d <- d |> filter(lead_days == 3, wind_obs > 33)
  if (nrow(d) < 10) return(NULL)
  m <- delta_spec(d)
  cf <- coef(m)["augmented_obs"]; se <- sqrt(diag(vcov(m)))["augmented_obs"]
  tibble(dropped = s, n = nrow(d),
         delta = abs(cf), se = se,
         p = 2*(1 - pnorm(abs(cf)/se)))
}) |> arrange(if_else(dropped == "(none)", -1L, 0L))

B3_disp <- B3 |>
  transmute(
    `Dropped` = str_to_title(dropped),
    `Obs.` = format(n, big.mark = ","),
    `$\\delta$ (m/s)` = sprintf("%.3f", delta),
    `SE` = sprintf("(%.3f)", se),
    `p` = ifelse(p < 0.001, "$<$0.001", sprintf("%.3f", p))
  )

B3_tex <- kbl(B3_disp, format = "latex", booktabs = TRUE, escape = FALSE,
              caption = "One-storm-out jackknife of $\\delta$, hurricane-force / 3-day lead sample. \\label{tab:appB_jackknife}",
              align = c("l","c","c","c","c"), linesep = "") |>
  footnote(general = "County-clustered standard errors in parentheses. The baseline row drops no storm.",
           threeparttable = TRUE, fixed_small_size = TRUE, escape = FALSE)
writeLines(as.character(B3_tex), here("output/appB_jackknife.tex"))
add_adjust_box(here("output/appB_jackknife.tex"))

# B.4 — Maria-in vs Maria-out -------------------------------------------------
mB4_off <- delta_spec(denial             |> filter(lead_days == 3, wind_obs > 33))
mB4_on  <- delta_spec(denial_with_maria  |> filter(lead_days == 3, wind_obs > 33))

B4 <- tibble(
  sample = c("Excluding Maria (baseline)", "Including Maria"),
  n = c(mB4_off$nobs, mB4_on$nobs),
  delta = c(abs(coef(mB4_off)["augmented_obs"]), abs(coef(mB4_on)["augmented_obs"])),
  se = c(sqrt(diag(vcov(mB4_off)))["augmented_obs"], sqrt(diag(vcov(mB4_on)))["augmented_obs"])
) |>
  mutate(p = 2*(1 - pnorm(abs(delta)/se)))

B4_disp <- B4 |>
  transmute(
    `Sample` = sample,
    `Obs.` = format(n, big.mark = ","),
    `$\\delta$ (m/s)` = sprintf("%.3f", delta),
    `SE` = sprintf("(%.3f)", se),
    `p` = ifelse(p < 0.001, "$<$0.001", sprintf("%.3f", p))
  )

B4_tex <- kbl(B4_disp, format = "latex", booktabs = TRUE, escape = FALSE,
              caption = "Inclusion of Hurricane Maria: effect on $\\delta$. \\label{tab:appB_maria}",
              align = c("l","c","c","c","c"), linesep = "") |>
  footnote(general = "Maria is excluded from the main analysis because it did not make landfall in the continental US. County-clustered standard errors in parentheses.",
           threeparttable = TRUE, fixed_small_size = TRUE, escape = FALSE)
writeLines(as.character(B4_tex), here("output/appB_maria.tex"))
add_adjust_box(here("output/appB_maria.tex"))

# ==============================================================================
# Appendix C — per-storm decomposition of total $2.91B
# ==============================================================================
# Re-derive Stage 3 valuation per storm using the same beta and delta the main
# report uses.

# Recompute beta on full panel (Conley HAC)
m_beta <- run_conley(spec_col7("total_cost"), voi_load())
beta_hurr <- unname(coef(m_beta)[grep("categoryHurricane", names(coef(m_beta)))])

# Delta and baseline sigma from main report's Stage 2 sample
reg_data <- denial |> filter(lead_days == 3, wind_obs > 33)
m_delta  <- feols(wind_fc_sd ~ augmented_obs | county + name,
                  data = reg_data, vcov = ~county)
delta <- abs(coef(m_delta)["augmented_obs"])
sigma_bar <- reg_data |> filter(augmented_obs == 1) |>
  summarise(s = mean(wind_fc_sd, na.rm = TRUE)) |> pull(s)
delta_sigma_sq <- (sigma_bar + delta)^2 - sigma_bar^2
value_per_county <- beta_hurr * delta_sigma_sq

per_storm <- reg_data |>
  group_by(name) |>
  summarise(
    affected_counties = n(),
    .groups = "drop"
  ) |>
  mutate(
    value_M = affected_counties * value_per_county,
    share   = value_M / sum(value_M)
  ) |>
  arrange(desc(value_M))

per_storm_disp <- per_storm |>
  transmute(
    Storm = str_to_title(name),
    `HF Counties Affected` = affected_counties,
    `Value (\\$M)` = sprintf("%.1f", value_M),
    `Share of Total` = sprintf("%.1f\\%%", 100 * share)
  ) |>
  bind_rows(
    tibble(
      Storm = "\\textbf{Total}",
      `HF Counties Affected` = sum(per_storm$affected_counties),
      `Value (\\$M)` = sprintf("\\textbf{%.1f}", sum(per_storm$value_M)),
      `Share of Total` = "\\textbf{100.0\\%}"
    )
  )

C_tex <- kbl(
  per_storm_disp,
  format    = "latex",
  booktabs  = TRUE,
  escape    = FALSE,
  caption   = "Per-storm decomposition of the \\$2.91B retrospective social value. \\label{tab:appC_per_storm}",
  align     = c("l", "c", "c", "c"),
  linesep   = ""
) |>
  footnote(
    general = sprintf(
      "Value per affected county-storm equals the marginal cost parameter times the variance reduction (%.2f x %.2f = $%.2fM). Counties counted are those with a 3-day-lead hurricane-force prediction (wind > 33 m/s) in the denial sample, matching the main report's Stage 3.",
      beta_hurr, delta_sigma_sq, value_per_county
    ),
    threeparttable = TRUE,
    fixed_small_size = TRUE
  )
writeLines(as.character(C_tex), here("output/appC_per_storm.tex"))
add_adjust_box(here("output/appC_per_storm.tex"))

# C — bar chart ---------------------------------------------------------------
storm_plot <- per_storm |>
  mutate(name = str_to_title(name),
         name = fct_reorder(name, value_M)) |>
  ggplot(aes(x = name, y = value_M)) +
  geom_col(fill = "#1B9E77", width = 0.7) +
  geom_text(aes(label = sprintf("$%.0fM", value_M)),
            hjust = -0.1, size = 4) +
  coord_flip() +
  scale_y_continuous(
    labels  = scales::dollar_format(prefix = "$", suffix = "M", accuracy = 1),
    expand  = expansion(mult = c(0, 0.18))
  ) +
  labs(
    x = NULL,
    y = "Retrospective social value of augmented observations"
  ) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor   = element_blank())

ggsave(
  plot     = storm_plot,
  filename = here("output/appC_per_storm.png"),
  width    = 10, height = 5, dpi = 300
)

cat("\nAppendix outputs written to output/appA_*.tex, output/appB_*.tex, output/appC_per_storm.{tex,png}.\n")
