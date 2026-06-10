# ==============================================================================
# Stage 3 - Retrospective social value of augmented observations
# Combines the Stage 1 cost curvature (beta) and the Stage 2 precision gain
# (delta) into a dollar value; no re-estimation -- beta and delta are read from
# the stage-number files. Builds the valuation tables and the back-of-the-envelope
# extension beyond the denial archive.
# Inputs:  output/stage1_numbers.txt, output/stage2_numbers.txt,
#          data/clean_denial/forecast_denial_data_with_lead_times.csv,
#          data/clean/final_dataset.rds, data/raw/wind_error/*.csv
# Outputs: output/{implied_value,per_storm}.tex; output/stage3_numbers.txt;
#          output/extended_valuation_numbers.txt
# Run:     Rscript --vanilla code/3.valuation.r
# ==============================================================================
if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, tidyverse, here, fixest, kableExtra)
source(here("code/functions_table.r"))

# read a numeric value from a "label = value [unit]" stage-number line
read_num <- function(file, key) {
  rhs <- sub(".*=", "", grep(key, readLines(here(file)), value = TRUE)[1])
  as.numeric(regmatches(rhs, regexpr("-?[0-9]+(\\.[0-9]+)?", rhs)))
}
beta_hurr <- read_num("output/stage1_numbers.txt", "^beta_hurr")
beta_gdp  <- read_num("output/stage1_numbers.txt", "^beta_gdp_hurr")
beta_pc   <- read_num("output/stage1_numbers.txt", "^beta_pc_hurr")

denial_path <- here("data/clean_denial/forecast_denial_data_with_lead_times.csv")
reg_data <- fread(denial_path)[!is.na(augmented_obs) & lead_days == 3 & wind_obs > 33]

# Stage 2 reports the rounded delta; recompute the point estimate and the baseline
# AO forecast SD here at full precision so the dollar valuation does not drift.
delta     <- abs(coef(feols(wind_fc_sd ~ augmented_obs | county + name, reg_data, vcov = ~name))["augmented_obs"])
sigma_bar <- mean(reg_data[augmented_obs == 1]$wind_fc_sd, na.rm = TRUE)
delta_sigma_sq <- (sigma_bar + delta)^2 - sigma_bar^2
value_per_county <- beta_hurr * delta_sigma_sq

# ------------------------------------------------------------------------------
# Headline valuation (uniform per-county value over the denial county-storms)
# ------------------------------------------------------------------------------
sample_benefits <- reg_data |>
  group_by(name) |>
  summarise(affected_counties = n_distinct(county), .groups = "drop") |>
  mutate(total_benefit_M = affected_counties * value_per_county)
total_value_B <- sum(sample_benefits$total_benefit_M) / 1000

writeLines(c(
  sprintf("beta_hurr (Hurricane, $M per (m/s)^2)  = %.3f", beta_hurr),
  sprintf("delta (m/s)                            = %.3f", delta),
  sprintf("baseline sigma_All (m/s)               = %.2f", sigma_bar),
  sprintf("delta_sigma_sq ((m/s)^2)               = %.2f", delta_sigma_sq),
  sprintf("Value per Affected County ($M)         = %.2f", value_per_county),
  sprintf("Affected hurricane-force counties      = %d",   sum(sample_benefits$affected_counties)),
  sprintf("Total Retrospective Value ($B)         = %.2f", total_value_B)
), here("output/stage3_numbers.txt"))

# ------------------------------------------------------------------------------
# Table: implied value under three cost outcomes (tab:implied_value)
# ------------------------------------------------------------------------------
county_attr <- readRDS(here("data/clean/final_dataset.rds")) |>
  as_tibble() |> distinct(county, year, population, gdp)
val <- reg_data |>
  distinct(county, name, year) |>                       # one row per county-storm
  left_join(county_attr, by = c("county", "year")) |>
  mutate(v_tot_M = beta_hurr * delta_sigma_sq,
         v_gdp_M = (beta_gdp / 100) * delta_sigma_sq * gdp / 1e3,
         v_pc_M  = beta_pc * delta_sigma_sq * population / 1e6)
val_econ <- val |> filter(!is.na(gdp), !is.na(population))
n_all <- nrow(val); n_econ <- nrow(val_econ)

# Maria's PR counties lack GDP/population: the total-cost aggregate spans all
# county-storms, the scaled columns only the continental ones.
writeLines(sprintf(
'\\begin{table}

\\caption{Implied retrospective social value of augmented observations under three notions of value. \\label{tab:implied_value}}
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
\\item The total-cost aggregate sums over all %d hurricane-force county-storms including Maria. The cost/GDP and cost-per-capita columns sum over the %d continental county-storms with observed GDP and population.
\\end{tablenotes}
\\end{threeparttable}
\\end{adjustbox}
\\end{table}
',
  beta_hurr, mean(val$v_tot_M),      sum(val$v_tot_M) / 1000,
  beta_gdp,  mean(val_econ$v_gdp_M), sum(val_econ$v_gdp_M) / 1000,
  beta_pc,   mean(val_econ$v_pc_M),  sum(val_econ$v_pc_M) / 1000,
  n_all, n_econ),
  here("output/implied_value.tex"))

# ------------------------------------------------------------------------------
# Table: per-storm decomposition with storm-specific baseline sigma (tab:per_storm)
# ------------------------------------------------------------------------------
per_storm <- reg_data |>
  group_by(name) |>
  summarise(affected_counties = n_distinct(county),
            sigma_h = mean(wind_fc_sd[augmented_obs == 1], na.rm = TRUE), .groups = "drop") |>
  mutate(value_M = affected_counties * beta_hurr * ((sigma_h + delta)^2 - sigma_h^2),
         share   = value_M / sum(value_M)) |>
  arrange(desc(value_M))
per_storm_disp <- per_storm |>
  transmute(Storm = str_to_title(name), `HF Counties Affected` = affected_counties,
            `$\\bar{\\sigma}_{\\text{ALL},h}$ (m/s)` = formatC(sigma_h, format = "f", digits = 2),
            `Value (\\$M)` = formatC(value_M, format = "f", digits = 1, big.mark = ","),
            `Share of Total` = sprintf("%.1f\\%%", 100 * share)) |>
  bind_rows(tibble(Storm = "\\textbf{Total}",
                   `HF Counties Affected` = sum(per_storm$affected_counties),
                   `$\\bar{\\sigma}_{\\text{ALL},h}$ (m/s)` = "",
                   `Value (\\$M)` = sprintf("\\textbf{%s}", formatC(sum(per_storm$value_M), format = "f", digits = 1, big.mark = ",")),
                   `Share of Total` = "\\textbf{100.0\\%}"))
kbl(per_storm_disp, format = "latex", booktabs = TRUE, escape = FALSE, linesep = "",
    caption = sprintf("Per-storm decomposition of the \\$%.2fB retrospective social value. \\label{tab:per_storm}", sum(per_storm$value_M) / 1000),
    align = c("l", "r", "r", "r", "r")) |>
  footnote(general = sprintf("Each storm's value is its hurricane-force / 3-day-lead county count times $\\\\hat{\\\\beta}^w_{Hurr}\\\\,[(\\\\bar{\\\\sigma}_{\\\\text{ALL},h}+\\\\hat{\\\\delta})^2-\\\\bar{\\\\sigma}_{\\\\text{ALL},h}^2]$, with $\\\\hat{\\\\beta}^w_{Hurr}=%.2f$ and $\\\\hat{\\\\delta}=%.3f$ m/s. The total is invariant to using storm-specific or pooled $\\\\sigma$ because value is linear in $\\\\sigma$.", beta_hurr, delta),
           general_title = "", threeparttable = TRUE, fixed_small_size = TRUE, escape = FALSE) |>
  as.character() |> writeLines(here("output/per_storm.tex"))
add_adjust_box(here("output/per_storm.tex"))

# ------------------------------------------------------------------------------
# Extension beyond the denial archive: price every continental hurricane-force,
# 3-day-lead county-storm at its own baseline sigma_3 (back-of-the-envelope).
# ------------------------------------------------------------------------------
files <- list.files(here("data/raw/wind_error"), pattern = "_errors\\.csv$", full.names = TRUE)
raw <- rbindlist(lapply(files, fread), fill = TRUE)
raw[, year := as.integer(substr(as.character(pred_date), 1, 4))]
cs <- raw[lead_days == 3, .(wind_obs = max(vmax_sust_obs, na.rm = TRUE),
                            sigma3 = mean(vmax_sust_pred_sd, na.rm = TRUE)),
          by = .(year, name, county = gridid)]
hf <- cs[is.finite(wind_obs) & wind_obs > 33 & is.finite(sigma3)]
hf[, value_M := beta_hurr * ((sigma3 + delta)^2 - sigma3^2)]
summ <- function(d) list(storms = uniqueN(d$name), cs = nrow(d), B = sum(d$value_M) / 1000)
hwrf <- summ(hf[year >= 2007]); all10 <- summ(hf[year >= 2010]); allyr <- summ(hf)
den  <- summ(hf[name %in% c("irma", "michael", "florence", "laura", "sally", "isaias")])
rate <- function(s, y0) s$B * 1000 / (2024 - y0 + 1)
writeLines(c(
  sprintf("continental denial-6: %d county-storms, $%.2fB", den$cs, den$B),
  sprintf("since HWRF 2007-2024: %d storms, %d county-storms, $%.2fB, $%.0fM/yr",
          hwrf$storms, hwrf$cs, hwrf$B, rate(hwrf, 2007)),
  sprintf("2010-2024: $%.2fB, $%.0fM/yr", all10$B, rate(all10, 2010)),
  sprintf("2005-2024: $%.2fB, $%.0fM/yr", allyr$B, rate(allyr, 2005))
), here("output/extended_valuation_numbers.txt"))
cat("Wrote Stage 3 tables + output/{stage3_numbers,extended_valuation_numbers}.txt\n")
