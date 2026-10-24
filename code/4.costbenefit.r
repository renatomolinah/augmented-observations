# ==============================================================================
# Stage 4 - Cost-benefit benchmark
# Compares the Stage 3 social value against published program costs: the 2016
# ANSER AoA external benefits and life-cycle costs, and the NOAA/GAO fleet
# recapitalization. Only the social value comes from the pipeline; the cost
# figures are cited constants.
# Inputs:  output/stage3_numbers.txt
# Outputs: output/platform_comparison.tex; output/stage4_costbenefit_numbers.txt
# Run:     Rscript --vanilla code/4.costbenefit.r
# ==============================================================================
if (!require("pacman")) install.packages("pacman")
pacman::p_load(here)
source(here("code/functions_table.r"))

our_value_B <- read_num("output/stage3_numbers.txt", "Total Retrospective Value")

# ANSER 2016 External Benefits AoA (BY$2024 NPV @ 7%, 2030-2055): benefit
# channels from Fig. 34, replacement life-cycle costs from Fig. 33
aoa <- list(
  npv_evacuation = 138.84, npv_air_traffic = 134.18, npv_maritime = 0.18,
  npv_chemistry = 227.65, npv_total = 500.86, bcr_base = 0.75, years_horizon = 25,
  lcc = c("Two C-130J (least cost)" = 680.0, "WP-3D life extension" = 723.6, "Two A400M" = 1414.15))
# 2022 NOAA Aircraft Recapitalization Plan + GAO-25-107210 (Jan 2025).
recap <- list(total_acq_B = 1.40, remaining_B = 0.678)
# 2026 Recapitalization Act: $2.5B acquisition + $45M/yr O&M over a 30-year life.
# 7% is the A-94 base case (1992 text, reinstated by M-25-23).
bill <- list(acq_M = 2500, om_M = 45, life = 30, r_base = 0.07)

our_value_M <- our_value_B * 1000
annual_value_M <- read_num("output/extended_valuation_numbers.txt", "annual value 2005-2024")
crf <- function(r, n) r / (1 - (1 + r)^-n)
annual_cost <- function(r) bill$acq_M * crf(r, bill$life) + bill$om_M
bcr <- function(r, benefit = annual_value_M) benefit / annual_cost(r)
aoa_annual_M <- aoa$npv_total * crf(bill$r_base, aoa$years_horizon)
r_breakeven <- uniroot(function(r) bcr(r) - 1, c(1e-4, 0.5))$root
r_breakeven_aoa <- uniroot(function(r) bcr(r, annual_value_M + aoa_annual_M) - 1, c(1e-4, 0.5))$root
writeLines(c(
  "-- ANSER (2016) External Benefits AoA, NPV BY$2024 @ 7%, 2030-2055 --",
  sprintf("  Evacuation avoided NPV ($M)             = %.2f", aoa$npv_evacuation),
  sprintf("  Air traffic disruption NPV ($M)         = %.2f", aoa$npv_air_traffic),
  sprintf("  Maritime traffic disruption NPV ($M)    = %.2f", aoa$npv_maritime),
  sprintf("  Air chemistry research NPV ($M)         = %.2f", aoa$npv_chemistry),
  sprintf("  TOTAL benefit NPV ($M)                  = %.2f", aoa$npv_total),
  sprintf("  LCC Alt 2 (C-130J) NPV ($M)             = %.2f", aoa$lcc[[1]]),
  sprintf("  Base-case BCR                           = %.2f", aoa$bcr_base),
  "",
  "-- Our retrospective valuation (2017-2020) --",
  sprintf("  Total retrospective value ($M)          = %.2f", our_value_M),
  sprintf("  Our value / ANSER 25-yr benefit NPV     = %.2fx", our_value_M / aoa$npv_total),
  sprintf("  Our value / ANSER Alt 2 LCC             = %.2fx", our_value_M / aoa$lcc[[1]]),
  sprintf("  Our value / NOAA total acquisition      = %.2fx", our_value_B / recap$total_acq_B),
  "",
  "-- NOAA Aircraft Recapitalization (NOAA 2022 / GAO 2025) --",
  sprintf("  Total acquisition, 4x C-130J ($B)       = %.2f", recap$total_acq_B),
  sprintf("  Remaining to fund (Jan 2025, $B)        = %.2f", recap$remaining_B),
  "",
  "-- 2026 Recapitalization Act: annualized over 30 years --",
  sprintf("  Annual avoided costs, 2005-2024 ($M/yr) = %.2f", annual_value_M),
  sprintf("  Capital charge @ 7%% ($M/yr)             = %.2f", annual_cost(bill$r_base) - bill$om_M),
  sprintf("  Annual cost incl. O&M @ 7%% ($M/yr)      = %.2f", annual_cost(bill$r_base)),
  sprintf("  BCR @ 7%% (A-94 base case)               = %.2f", bcr(bill$r_base)),
  sprintf("  Break-even real rate (%%)                = %.2f", 100 * r_breakeven),
  sprintf("  ANSER benefits annualized @ 7%% ($M/yr)  = %.2f", aoa_annual_M),
  sprintf("  BCR @ 7%% incl. ANSER channels           = %.2f", bcr(bill$r_base, annual_value_M + aoa_annual_M)),
  sprintf("  Break-even real rate incl. ANSER (%%)    = %.2f", 100 * r_breakeven_aoa)
), here("output/stage4_costbenefit_numbers.txt"))

alts <- data.frame(name = names(aoa$lcc), lcc = unname(aoa$lcc))
alts$aoa_bcr <- aoa$npv_total / alts$lcc
alts$our_bcr <- (our_value_B * 1000) / alts$lcc
alts$combined_bcr <- (aoa$npv_total + our_value_B * 1000) / alts$lcc
body <- paste0(alts$name, " & ", formatC(alts$lcc, format = "f", digits = 0, big.mark = ","),
               " & ", sprintf("%.2f", alts$aoa_bcr), " & ", sprintf("%.1f", alts$our_bcr),
               " & ", sprintf("%.1f", alts$combined_bcr), "\\\\")
note <- sprintf(paste0(
  "\\item Life-cycle costs are the net present values (base-year \\$2024) of the three replacement ",
  "alternatives costed in the 2016 Analysis of Alternatives \\citep{ANSER2016ExternalBenefits}. The ",
  "AoA column divides that study's \\$%d million external-benefit estimate by each cost, the this-paper ",
  "column instead uses the \\$%.2f billion denial-sample social value of the dropsonde precision channel, ",
  "and the combined column their \\$%.2f billion sum."),
  round(aoa$npv_total), our_value_B, aoa$npv_total / 1000 + our_value_B)
writeLines(c(
  "\\begin{table}", "",
  "\\caption{Social value of augmented observations against the replacement-platform alternatives. \\label{tab:platform_comparison}}",
  "\\centering", "\\begin{adjustbox}{max width = \\textwidth}", "\\begin{threeparttable}",
  "\\begin{tabular}[t]{lrrrr}", "\\toprule",
  "Replacement alternative & Life-cycle cost & \\multicolumn{3}{c}{Benefit-cost ratio}\\\\",
  "\\cmidrule(l{3pt}r{3pt}){3-5}",
  " & (NPV, \\$M) & AoA & This paper & Combined\\\\",
  "\\midrule", body, "\\bottomrule", "\\end{tabular}",
  "\\begin{tablenotes}", "\\small", note, "\\end{tablenotes}",
  "\\end{threeparttable}", "\\end{adjustbox}", "\\end{table}"
), here("output/platform_comparison.tex"))
cat("Wrote output/platform_comparison.tex + stage4_costbenefit_numbers.txt\n")
