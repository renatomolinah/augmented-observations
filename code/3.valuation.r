# ==============================================================================
# Stage 3 - Retrospective social value of augmented observations
# Prices the Stage 1 precision gain (delta) with the Stage 2 cost curvature
# (beta); both are read from the stage-number files, nothing is re-estimated.
# Also runs the back-of-the-envelope extension beyond the denial archive.
# Inputs:  output/stage1_numbers.txt, output/stage2_numbers.txt,
#          data/clean_denial/forecast_denial_data_with_lead_times.csv,
#          data/clean/final_dataset.rds, data/raw/wind_error/*.csv,
#          data/raw/sheldus/named_pr/damages_2017_maria_pr.csv,
#          data/raw/fema/PublicAssistanceFundedProjectsDetails_2024.csv
# Outputs: output/{implied_value,per_storm}.tex; output/stage3_numbers.txt;
#          output/{extended_valuation,denial_cost}_numbers.txt
# Run:     Rscript --vanilla code/3.valuation.r
# ==============================================================================
if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, tidyverse, here, kableExtra)
source(here("code/functions_table.r"))

beta_hurr <- read_num("output/stage2_numbers.txt", "^beta_hurr")
beta_gdp  <- read_num("output/stage2_numbers.txt", "^beta_gdp_hurr")
beta_pc   <- read_num("output/stage2_numbers.txt", "^beta_pc_hurr")

denial_path <- here("data/clean_denial/forecast_denial_data_with_lead_times.csv")
reg_data <- fread(denial_path)[!is.na(augmented_obs) & lead_days == 3 & wind_obs > 33]

delta     <- read_num("output/stage1_numbers.txt", "^delta +=")
sigma_bar <- read_num("output/stage1_numbers.txt", "sigma_bar")
delta_sigma_sq <- (sigma_bar + delta)^2 - sigma_bar^2
value_per_county <- beta_hurr * delta_sigma_sq

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
  # full precision: Stage 4 reads this for the benefit-cost ratios (the paper rounds when citing)
  sprintf("Total Retrospective Value ($B)         = %.6f", total_value_B)
), here("output/stage3_numbers.txt"))

county_attr <- readRDS(here("data/clean/final_dataset.rds")) |>
  as_tibble() |> distinct(county, year, population, gdp)
val <- reg_data |>
  distinct(county, name, year) |>
  left_join(county_attr, by = c("county", "year")) |>
  mutate(v_tot_M = beta_hurr * delta_sigma_sq,
         v_gdp_M = (beta_gdp / 100) * delta_sigma_sq * gdp / 1e3,
         v_pc_M  = beta_pc * delta_sigma_sq * population / 1e6)
val_econ <- val |> filter(!is.na(gdp), !is.na(population))
n_all <- nrow(val); n_econ <- nrow(val_econ)

# Maria's PR counties lack GDP/population: the scaled columns span only the continental county-storms
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

per_storm <- reg_data |>
  group_by(name) |>
  summarise(affected_counties = n_distinct(county),
            sigma_h = mean(wind_fc_sd[augmented_obs == 1], na.rm = TRUE), .groups = "drop") |>
  mutate(value_M = affected_counties * beta_hurr * ((sigma_h + delta)^2 - sigma_h^2),
         share   = value_M / sum(value_M)) |>
  arrange(desc(value_M))
per_storm_disp <- per_storm |>
  transmute(Storm = str_to_title(name), `HF Counties Affected` = affected_counties,
            `$\\sigma_{\\text{ALL},h}$ (m/s)` = formatC(sigma_h, format = "f", digits = 2),
            `Value (\\$M)` = formatC(value_M, format = "f", digits = 1, big.mark = ","),
            `Share of Total` = sprintf("%.1f\\%%", 100 * share)) |>
  bind_rows(tibble(Storm = "\\textbf{Total}",
                   `HF Counties Affected` = sum(per_storm$affected_counties),
                   `$\\sigma_{\\text{ALL},h}$ (m/s)` = "",
                   `Value (\\$M)` = sprintf("\\textbf{%s}", formatC(sum(per_storm$value_M), format = "f", digits = 1, big.mark = ",")),
                   `Share of Total` = "\\textbf{100.0\\%}"))
kbl(per_storm_disp, format = "latex", booktabs = TRUE, escape = FALSE, linesep = "",
    caption = sprintf("Per-storm decomposition of the \\$%.2fB retrospective social value. \\label{tab:per_storm}", sum(per_storm$value_M) / 1000),
    align = c("l", "r", "r", "r", "r")) |>
  footnote(general = sprintf("Each storm's value is its hurricane-force / 3-day-lead county count times $\\\\hat{\\\\beta}^w_{Hurr}\\\\,[\\\\sigma_{\\\\text{NO},h}^2-\\\\sigma_{\\\\text{ALL},h}^2]$, where $\\\\sigma_{\\\\text{NO},h}=\\\\sigma_{\\\\text{ALL},h}+\\\\hat{\\\\delta}_{3,\\\\text{HF}}$, with $\\\\hat{\\\\beta}^w_{Hurr}=%.2f$ and $\\\\hat{\\\\delta}_{3,\\\\text{HF}}=%.3f$ m/s. The total is invariant to using storm-specific or pooled $\\\\sigma$ because value is linear in $\\\\sigma$.", beta_hurr, delta),
           general_title = "", threeparttable = TRUE, fixed_small_size = TRUE, escape = FALSE) |>
  as.character() |> writeLines(here("output/per_storm.tex"))
add_adjust_box(here("output/per_storm.tex"))

# realized cost of the denial-sample storms, the denominator for the avoided-cost
# share. Continental storms come from the cost panel. Maria is built from the raw
# sources instead, since most of its FEMA PA is statewide and the panel keeps only
# county-level projects: SHELDUS named-storm damages plus fatalities at the VSL, and
# FEMA PA for DR-4339 net of Category B, in 2019$
# all ten denial storms, and the seven with hurricane-force 3-day exposures that carry value
denial_storms <- unique(fread(denial_path)$name)
panel_cost <- readRDS(here("data/clean/final_dataset.rds")) |>
  as_tibble() |>
  filter(hurricane %in% denial_storms, hurricane != "maria") |>
  group_by(hurricane) |>
  summarise(cost_B = sum(total_cost, na.rm = TRUE) / 1e3, .groups = "drop") |>
  mutate(valued = hurricane %in% unique(reg_data$name))
maria_sheldus <- fread(here("data/raw/sheldus/named_pr/damages_2017_maria_pr.csv"))
maria_dmg_B <- maria_sheldus[, sum(`PropertyDmg(ADJ 2019)` + `CropDmg(ADJ 2019)` + 9.39e6 * Fatalities)] / 1e9
pa <- fread(here("data/raw/fema/PublicAssistanceFundedProjectsDetails_2024.csv"),
            select = c("disasterNumber", "stateCode", "damageCategoryCode", "projectAmount"))
pa[, projectAmount := as.numeric(gsub("[$,]", "", projectAmount))]
# CPI-U 2017 -> 2019, the landfall-year deflation used for the panel in 1.01.clean_fema_foia.r
maria_pa_B <- pa[disasterNumber == 4339 & stateCode == "PR" & substr(damageCategoryCode, 1, 1) != "B",
                 sum(projectAmount, na.rm = TRUE)] * (255.657 / 245.120) / 1e9
maria_cost_B <- maria_dmg_B + maria_pa_B
denial_cost_B <- sum(panel_cost$cost_B) + maria_cost_B
valued_cost_B <- sum(panel_cost$cost_B[panel_cost$valued]) + maria_cost_B
writeLines(c(
  sprintf("continental denial storms, panel cost ($B)   = %.2f", sum(panel_cost$cost_B)),
  sprintf("  of which storms carrying value ($B)        = %.2f", sum(panel_cost$cost_B[panel_cost$valued])),
  sprintf("Maria SHELDUS damages + VSL deaths ($B)      = %.2f", maria_dmg_B),
  sprintf("Maria FEMA PA ex Cat B, DR-4339 ($B)         = %.2f", maria_pa_B),
  sprintf("Maria total cost ($B)                        = %.2f", maria_cost_B),
  sprintf("all denial storms, total cost ($B)           = %.2f", denial_cost_B),
  sprintf("avoided-cost share, all denial storms (%%)    = %.2f", 100 * total_value_B / denial_cost_B),
  sprintf("storms carrying value, total cost ($B)       = %.2f", valued_cost_B),
  sprintf("avoided-cost share, storms carrying value (%%) = %.2f", 100 * total_value_B / valued_cost_B)
), here("output/denial_cost_numbers.txt"))

# back-of-the-envelope extension beyond the denial archive: every hurricane-force,
# 3-day-lead county-storm, Puerto Rico included, priced at its own baseline sigma_3
files <- c(list.files(here("data/raw/wind_error"), pattern = "_errors\\.csv$", full.names = TRUE),
           list.files(here("data/raw/wind_error/pr"), pattern = "_errors\\.csv$", full.names = TRUE))
raw <- rbindlist(lapply(files, fread), fill = TRUE)
raw[, year := as.integer(substr(as.character(pred_date), 1, 4))]
cs <- raw[lead_days == 3, .(wind_obs = max(vmax_sust_obs, na.rm = TRUE),
                            sigma3 = mean(vmax_sust_pred_sd, na.rm = TRUE)),
          by = .(year, name, county = gridid)]
hf <- cs[is.finite(wind_obs) & wind_obs > 33 & is.finite(sigma3)]
hf[, value_M := beta_hurr * ((sigma3 + delta)^2 - sigma3^2)]
summ <- function(d) list(storms = uniqueN(d$name), cs = nrow(d), B = sum(d$value_M) / 1000)
hwrf <- summ(hf[year >= 2007]); all10 <- summ(hf[year >= 2010]); allyr <- summ(hf)
cont <- summ(hf[substr(sprintf("%05d", as.integer(county)), 1, 2) != "72"])
den  <- summ(hf[name %in% c("irma", "michael", "florence", "laura", "sally", "isaias")])
rate <- function(s, y0) s$B * 1000 / (2024 - y0 + 1)
writeLines(c(
  sprintf("continental denial-6: %d county-storms, $%.2fB", den$cs, den$B),
  sprintf("since HWRF 2007-2024: %d storms, %d county-storms, $%.2fB, $%.0fM/yr",
          hwrf$storms, hwrf$cs, hwrf$B, rate(hwrf, 2007)),
  sprintf("  decomposition: %.1f HF exposures/yr x $%.1fM per exposure = $%.0fM/yr",
          hwrf$cs / (2024 - 2007 + 1), hwrf$B * 1000 / hwrf$cs, rate(hwrf, 2007)),
  sprintf("2010-2024: $%.2fB, $%.0fM/yr", all10$B, rate(all10, 2010)),
  sprintf("2005-2024: %d storms, %d county-storms, $%.2fB, $%.0fM/yr",
          allyr$storms, allyr$cs, allyr$B, rate(allyr, 2005)),
  sprintf("  decomposition: %.1f HF exposures/yr x $%.1fM per exposure = $%.0fM/yr",
          allyr$cs / (2024 - 2005 + 1), allyr$B * 1000 / allyr$cs, rate(allyr, 2005)),
  sprintf("  continental only: %d county-storms, $%.2fB, $%.0fM/yr", cont$cs, cont$B, rate(cont, 2005)),
  sprintf("annual value 2005-2024 ($M/yr) = %.2f", rate(allyr, 2005))
), here("output/extended_valuation_numbers.txt"))
cat("Wrote Stage 3 tables + output/{stage3_numbers,extended_valuation_numbers}.txt\n")
