# ==============================================================================
# NOAA Report - Appendices A, B, C
# ------------------------------------------------------------------------------
# A. Beta robustness on the VOI panel:
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

# A.1 — three outcomes (col-7 spec) -------------------------------------------
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
  title     = "Outcome robustness across cost specifications (2005--2024 panel). \\label{tab:appA_outcomes}",
  notes     = list("All specifications match column (7) of Table @REF{tab:voi-table} with county and state-by-hurricane fixed effects plus realized wind and precipitation bins. Standard errors are Conley Spatial HAC with a 400 km distance cutoff."),
  align     = "lccc",
  threeparttable = TRUE,
  escape    = FALSE,
  output    = here("output/appA_outcomes.tex")
)
# modelsummary's footnote pass strips backslashes; restore the cross-ref
# via a sentinel-substitution so the resulting LaTeX has a working \ref.
.aoa_path <- here("output/appA_outcomes.tex")
writeLines(gsub("@REF\\{", "\\\\ref{", readLines(.aoa_path)), .aoa_path)
add_adjust_box(.aoa_path)

# A.2 — implied retrospective value under each outcome -----------------------
# Use the three column-(7) coefficients above to compute the dollar value of
# augmented observations under each notion of value, summed over the same
# 2017--2020 hurricane-force / 3-day-lead county-storm panel as the main
# Stage 3 valuation in NOAA_report.R.

denial_full <- as_tibble(data.table::fread(denial_path)) |>
  filter(name != "maria")
hf3 <- denial_full |>
  filter(lead_days == 3, wind_obs > 33, !is.na(augmented_obs))

m_delta_app <- feols(wind_fc_sd ~ augmented_obs | county + name,
                     data = hf3, vcov = ~county)
delta_app      <- abs(coef(m_delta_app)["augmented_obs"])
sigma_bar_app  <- hf3 |>
  filter(augmented_obs == 1) |>
  summarise(s = mean(wind_fc_sd, na.rm = TRUE)) |>
  pull(s)
delta_sigma_sq <- (sigma_bar_app + delta_app)^2 - sigma_bar_app^2

# pop / GDP are constant within (county, year)
county_attr <- voi_load() |>
  distinct(county, year, population, gdp)
val <- hf3 |>
  left_join(county_attr, by = c("county", "year"))
stopifnot(sum(is.na(val$gdp) | is.na(val$population)) == 0)

get_hurr <- function(m) {
  ce <- as.matrix(m)
  unname(ce[grep("categoryHurricane", rownames(ce)), "Estimate"])
}
b_tot <- get_hurr(mA2_tot)   # $M     per (m/s)^2
b_gdp <- get_hurr(mA2_gdp)   # %      per (m/s)^2 (LHS scaled by 100)
b_pc  <- get_hurr(mA2_pc)    # $/pers per (m/s)^2

val <- val |>
  mutate(
    v_tot_M = b_tot * delta_sigma_sq,
    v_gdp_M = (b_gdp / 100) * delta_sigma_sq * gdp / 1e3,
    v_pc_M  = b_pc * delta_sigma_sq * population / 1e6
  )

agg <- tibble(
  Outcome = c("Total cost (\\$M)",
              "Cost / county GDP (\\%)",
              "Cost per capita (\\$)"),
  beta    = c(b_tot, b_gdp, b_pc),
  per_co  = c(mean(val$v_tot_M),
              mean(val$v_gdp_M),
              mean(val$v_pc_M)),
  total_B = c(sum(val$v_tot_M) / 1000,
              sum(val$v_gdp_M) / 1000,
              sum(val$v_pc_M)  / 1000)
)

# Build the LaTeX directly so the math note (\Delta, \sigma, \bar, \ref) is not
# stripped by kableExtra's footnote() escape pass.
val_tex_str <- sprintf(
'\\begin{table}

\\caption{Implied retrospective social value of augmented observations under three notions of value. \\label{tab:appA_valuations}}
\\centering
\\begin{adjustbox}{max width = \\textwidth}
\\begin{threeparttable}
\\begin{tabular}[t]{lrrr}
\\toprule
Outcome variable & $\\hat{\\beta}^w_{Hurr}$ & Mean per-county value (\\$M) & Aggregate value 2017--2020 (\\$B)\\\\
\\midrule
Total cost (\\$M) & %.3f & %.2f & %.2f\\\\
Cost / county GDP (\\%%) & %.3f & %.2f & %.2f\\\\
Cost per capita (\\$) & %.3f & %.2f & %.2f\\\\
\\bottomrule
\\end{tabular}
\\begin{tablenotes}
\\small
\\item Per-county value equals the column-(7) Hurricane coefficient times the variance reduction $\\Delta\\sigma^{2}=(\\bar{\\sigma}+\\delta)^{2}-\\bar{\\sigma}^{2}=%.2f$ (m/s)$^{2}$, evaluated in the 2017--2020 hurricane-force / 3-day-lead sample. For the total-cost specification the per-county value is uniform across counties; for cost/GDP and cost per capita it scales with county GDP and population, respectively, so the table reports the mean per-county value. Aggregate values sum the per-county dollar valuation over the %d county--storm rows in the same denial sample, matching the panel used in Section \\ref{sec:results}.
\\end{tablenotes}
\\end{threeparttable}
\\end{adjustbox}
\\end{table}
',
  b_tot, mean(val$v_tot_M), sum(val$v_tot_M) / 1000,
  b_gdp, mean(val$v_gdp_M), sum(val$v_gdp_M) / 1000,
  b_pc,  mean(val$v_pc_M),  sum(val$v_pc_M)  / 1000,
  delta_sigma_sq, nrow(val)
)
writeLines(val_tex_str, here("output/appA_valuations.tex"))

cat(sprintf("[Appendix A] aggregate value 2017--2020 (B$): total=%.2f gdp=%.2f pc=%.2f\n",
            agg$total_B[1], agg$total_B[2], agg$total_B[3]))

# A.3 — one-storm-out jackknife on Hurricane beta -----------------------------
top_storms <- c("katrina", "harvey", "sandy", "ian", "ida",
                "michael", "laura", "irma", "wilma", "rita")
jack_rows <- list()
for (s in c("none", top_storms)) {
  d <- if (s == "none") voi_load() else voi_load_drop(s)
  m <- run_conley(spec_col7("total_cost"), d)
  # conleyreg returns a `coeftest` matrix: cols are Estimate / Std. Error / t / Pr(>|t|)
  ce <- as.matrix(m)
  hurr_ix <- grep("categoryHurricane", rownames(ce))
  sub_ix  <- grep("categoryTropical Storm", rownames(ce))
  jack_rows[[s]] <- tibble(
    dropped = s,
    n_storms = length(unique(d$hurricane)),
    n_rows = nrow(d),
    beta_hurr    = ce[hurr_ix, "Estimate"],
    se_hurr      = ce[hurr_ix, "Std. Error"],
    beta_subhurr = ce[sub_ix,  "Estimate"],
    se_subhurr   = ce[sub_ix,  "Std. Error"]
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
  align     = c("l", "r", "r", "r")
) |>
  footnote(
    general = "Each row reports column (7) estimates after dropping the indicated storm from the panel. Conley Spatial HAC standard errors (400 km cutoff) in parentheses. The baseline row drops no storm.",
    general_title = "",
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
              align = c("c","r","r","r","r","r"), linesep = "") |>
  footnote(general = "Each row reports the controls-table specification of Table \\\\ref{tab:noaa-controls} estimated on the indicated lead-time subset of hurricane-force observations (wind $\\\\geq$ 33 m/s) in the 2017--2020 continental-US AO denial archive (excluding Hurricane Maria). The coefficient $\\\\delta$ is the average reduction in the ex-ante wind-speed forecast standard deviation attributable to the assimilation of dropsonde profiles (the ALL configuration of \\\\citet{DitchekEA2023}); the precision gain is reported as $|\\\\hat{\\\\delta}|$, so larger positive values indicate a larger noise reduction from AO. County-clustered standard errors in parentheses.",
           general_title = "",
           threeparttable = TRUE, fixed_small_size = TRUE,
           escape = FALSE)
writeLines(as.character(B1_tex), here("output/appB_leadtime.tex"))
add_adjust_box(here("output/appB_leadtime.tex"))

# B.2 — Saffir-Simpson category sensitivity -----------------------------------
# Cumulative cut-offs: Cat 1+ (>=33 m/s), Cat 2+ (>=43), Cat 3+ (>=50),
# Cat 4+ (>=58), Cat 5 (>=70).  Rows with fewer than 10 obs are skipped.
cats <- tibble(
  label = c("Category 1 and above ($\\geq$33 m/s)",
            "Category 2 and above ($\\geq$43 m/s)",
            "Category 3 and above ($\\geq$50 m/s)",
            "Category 4 and above ($\\geq$58 m/s)",
            "Category 5 ($\\geq$70 m/s)"),
  thr   = c(33, 43, 50, 58, 70)
)
B2 <- map2_dfr(cats$label, cats$thr, function(lab, thr) {
  d <- denial |> filter(lead_days == 3, wind_obs >= thr)
  if (nrow(d) < 10) return(NULL)
  m <- delta_spec(d)
  cf <- coef(m)["augmented_obs"]; se <- sqrt(diag(vcov(m)))["augmented_obs"]
  tibble(category = lab, threshold = thr, n = nrow(d),
         delta = abs(cf), se = se,
         p = 2*(1 - pnorm(abs(cf)/se)),
         sigma_bar = mean(d$wind_fc_sd, na.rm=TRUE))
})

# Build LaTeX directly so the math note ($\geq$) survives.
B2_rows <- paste(sapply(seq_len(nrow(B2)), function(i) with(B2[i, ], sprintf(
  "%s & %s & %.2f & %.3f & (%.3f) & %s\\\\",
  category, format(n, big.mark = ","), sigma_bar, delta, se,
  ifelse(p < 0.001, "$<$0.001", sprintf("%.3f", p))
))), collapse = "\n")
B2_tex_str <- paste0(
  "\\begin{table}\n\n",
  "\\caption{Saffir-Simpson category sensitivity, 3-day lead. \\label{tab:appB_intensity}}\n",
  "\\centering\n",
  "\\begin{threeparttable}\n",
  "\\begin{tabular}[t]{lrrrrr}\n",
  "\\toprule\n",
  "Category & Obs. & $\\bar{\\sigma}$ (m/s) & $\\delta$ (m/s) & SE & p\\\\\n",
  "\\midrule\n",
  B2_rows, "\n",
  "\\bottomrule\n",
  "\\end{tabular}\n",
  "\\begin{tablenotes}\n\\small\n",
  "\\item Cumulative cut-offs on realized wind speed. County-clustered standard errors in parentheses. Category 4 and above ($\\geq$58 m/s) is omitted because the denial sample contains fewer than 10 such observations; Category 5 ($\\geq$70 m/s) is absent from the sample.\n",
  "\\end{tablenotes}\n",
  "\\end{threeparttable}\n",
  "\\end{table}\n"
)
writeLines(B2_tex_str, here("output/appB_intensity.tex"))

# B.2b — same Saffir-Simpson category grid with Maria included ----------------
cats_maria <- tibble(
  label = c("Category 1 and above ($\\geq$33 m/s)",
            "Category 2 and above ($\\geq$43 m/s)",
            "Category 3 and above ($\\geq$50 m/s)",
            "Category 4 and above ($\\geq$58 m/s)",
            "Category 5 ($\\geq$70 m/s)"),
  thr   = c(33, 43, 50, 58, 70)
)
B2m <- map2_dfr(cats_maria$label, cats_maria$thr, function(lab, thr) {
  d <- denial_with_maria |> filter(lead_days == 3, wind_obs >= thr)
  if (nrow(d) < 10) return(NULL)
  m <- delta_spec(d)
  cf <- coef(m)["augmented_obs"]; se <- sqrt(diag(vcov(m)))["augmented_obs"]
  tibble(category = lab, threshold = thr, n = nrow(d),
         delta = abs(cf), se = se,
         p = 2*(1 - pnorm(abs(cf)/se)),
         sigma_bar = mean(d$wind_fc_sd, na.rm=TRUE))
})

B2m_rows <- paste(sapply(seq_len(nrow(B2m)), function(i) with(B2m[i, ], sprintf(
  "%s & %s & %.2f & %.3f & (%.3f) & %s\\\\",
  category, format(n, big.mark = ","), sigma_bar, delta, se,
  ifelse(p < 0.001, "$<$0.001", sprintf("%.3f", p))
))), collapse = "\n")
B2m_tex_str <- paste0(
  "\\begin{table}\n\n",
  "\\caption{Saffir-Simpson category sensitivity with Hurricane Maria included, 3-day lead. \\label{tab:appB_intensity_maria}}\n",
  "\\centering\n",
  "\\begin{threeparttable}\n",
  "\\begin{tabular}[t]{lrrrrr}\n",
  "\\toprule\n",
  "Category & Obs. & $\\bar{\\sigma}$ (m/s) & $\\delta$ (m/s) & SE & p\\\\\n",
  "\\midrule\n",
  B2m_rows, "\n",
  "\\bottomrule\n",
  "\\end{tabular}\n",
  "\\begin{tablenotes}\n\\small\n",
  "\\item Sample is the seven 2017--2020 landfalling hurricanes in the denial archive (the six continental-US storms used in Table \\ref{tab:appB_intensity} plus Hurricane Maria). Cumulative cut-offs on realized wind speed. County-clustered standard errors in parentheses. Category 5 ($\\geq$70 m/s) is absent from the sample.\n",
  "\\end{tablenotes}\n",
  "\\end{threeparttable}\n",
  "\\end{table}\n"
)
writeLines(B2m_tex_str, here("output/appB_intensity_maria.tex"))

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
              caption = "One-storm-out jackknife, hurricane-force / 3-day lead sample. \\label{tab:appB_jackknife}",
              align = c("l","r","r","r","r"), linesep = "") |>
  footnote(general = "Each row reports the Stage 2 precision-gain specification of Table \\\\ref{tab:noaa-controls} re-estimated after dropping the indicated storm from the 2017--2020 continental-US AO denial archive (hurricane-force / 3-day lead, Maria excluded). The baseline row drops no storm. The coefficient $\\\\delta$ is the average reduction in the ex-ante wind-speed forecast standard deviation attributable to the assimilation of dropsonde profiles (the ALL configuration of \\\\citet{DitchekEA2023}). County-clustered standard errors in parentheses.",
           general_title = "",
           threeparttable = TRUE, fixed_small_size = TRUE, escape = FALSE)
writeLines(as.character(B3_tex), here("output/appB_jackknife.tex"))

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
              caption = "Inclusion of Hurricane Maria. \\label{tab:appB_maria}",
              align = c("l","r","r","r","r"), linesep = "") |>
  footnote(general = "Both rows estimate the Stage 2 precision-gain specification of Table \\\\ref{tab:noaa-controls} on hurricane-force ($\\\\geq$ 33 m/s) realizations at the 3-day lead. The baseline row uses the six 2017--2020 continental-US landfalls in the AO denial archive; the second row adds Hurricane Maria back into the panel. The coefficient $\\\\delta$ is the average reduction in the ex-ante wind-speed forecast standard deviation attributable to the assimilation of dropsonde profiles (the ALL configuration of \\\\citet{DitchekEA2023}). Maria is excluded from the main analysis because it did not make landfall in the continental US. County-clustered standard errors in parentheses.",
           general_title = "",
           threeparttable = TRUE, fixed_small_size = TRUE, escape = FALSE)
writeLines(as.character(B4_tex), here("output/appB_maria.tex"))
add_adjust_box(here("output/appB_maria.tex"))

# B.5 — one-storm-out jackknife with Maria included --------------------------
# Same design as B.3 but on the broader denial archive including Maria.
hf3_maria <- denial_with_maria |>
  filter(lead_days == 3, wind_obs > 33)
storms_maria <- hf3_maria |> distinct(name) |> pull(name) |> sort()

B5 <- map_dfr(c("(none)", storms_maria), function(s) {
  d <- if (s == "(none)") hf3_maria else hf3_maria |> filter(name != s)
  if (nrow(d) < 10) return(NULL)
  m <- delta_spec(d)
  cf <- coef(m)["augmented_obs"]
  se <- sqrt(diag(vcov(m)))["augmented_obs"]
  tibble(dropped = s, n = nrow(d),
         delta = abs(cf), se = se,
         p = 2*(1 - pnorm(abs(cf)/se)))
}) |> arrange(if_else(dropped == "(none)", -1L, 0L))

# Build LaTeX directly so the \ref{} cross-reference in the footnote survives.
B5_rows <- paste(sapply(seq_len(nrow(B5)), function(i) with(B5[i, ], sprintf(
  "%s & %s & %.3f & (%.3f) & %s\\\\",
  str_to_title(dropped), format(n, big.mark = ","), delta, se,
  ifelse(p < 0.001, "$<$0.001", sprintf("%.3f", p))
))), collapse = "\n")
B5_tex_str <- paste0(
  "\\begin{table}\n\n",
  "\\caption{One-storm-out jackknife with Hurricane Maria included, hurricane-force / 3-day lead sample. \\label{tab:appB_jackknife_maria}}\n",
  "\\centering\n",
  "\\begin{threeparttable}\n",
  "\\begin{tabular}[t]{lrrrr}\n",
  "\\toprule\n",
  "Dropped & Obs. & $\\delta$ (m/s) & SE & p\\\\\n",
  "\\midrule\n",
  B5_rows, "\n",
  "\\bottomrule\n",
  "\\end{tabular}\n",
  "\\begin{tablenotes}\n\\small\n",
  "\\item Sample is the seven 2017--2020 landfalling hurricanes with hurricane-force / 3-day-lead realizations in the denial archive (the six continental-US storms used in Table \\ref{tab:appB_jackknife} plus Hurricane Maria). County-clustered standard errors in parentheses. The baseline row drops no storm.\n",
  "\\end{tablenotes}\n",
  "\\end{threeparttable}\n",
  "\\end{table}\n"
)
writeLines(B5_tex_str, here("output/appB_jackknife_maria.tex"))

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
    `Value (\\$M)` = formatC(value_M, format = "f", digits = 1, big.mark = ","),
    `Share of Total` = sprintf("%.1f\\%%", 100 * share)
  ) |>
  bind_rows(
    tibble(
      Storm = "\\textbf{Total}",
      `HF Counties Affected` = sum(per_storm$affected_counties),
      `Value (\\$M)` = sprintf("\\textbf{%s}",
                                formatC(sum(per_storm$value_M),
                                        format = "f", digits = 1, big.mark = ",")),
      `Share of Total` = "\\textbf{100.0\\%}"
    )
  )

C_tex <- kbl(
  per_storm_disp,
  format    = "latex",
  booktabs  = TRUE,
  escape    = FALSE,
  caption   = "Per-storm decomposition of the \\$2.91B retrospective social value. \\label{tab:appC_per_storm}",
  align     = c("l", "r", "r", "r"),
  linesep   = ""
) |>
  footnote(
    general = sprintf(
      "Value per affected county-storm equals the marginal cost parameter times the variance reduction (%.2f x %.2f = \\\\$%.2fM). Counties counted are those with a 3-day-lead hurricane-force prediction (wind $\\\\geq$ 33 m/s) in the denial sample, matching the main report's Stage 3.",
      beta_hurr, delta_sigma_sq, value_per_county
    ),
    general_title = "",
    threeparttable = TRUE,
    fixed_small_size = TRUE,
    escape = FALSE
  )
writeLines(as.character(C_tex), here("output/appC_per_storm.tex"))
add_adjust_box(here("output/appC_per_storm.tex"))

cat("\nAppendix outputs written to output/appA_*.tex, output/appB_*.tex, output/appC_per_storm.tex.\n")
