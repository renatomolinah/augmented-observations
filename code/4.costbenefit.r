# ==============================================================================
# Stage 4 - Cost-benefit benchmark
# Compares the Stage 3 social value against published program costs: the 2016 AoA
# external-benefit estimate and replacement-platform life-cycle costs (ANSER2016),
# and the NOAA/GAO fleet recapitalization. All cost/benefit figures are external,
# cited constants; only the social value is read from the pipeline (Stage 3).
# Inputs:  output/stage3_numbers.txt
# Outputs: output/platform_comparison.tex; output/stage4_costbenefit_numbers.txt
# Run:     Rscript --vanilla code/4.costbenefit.r
# ==============================================================================
if (!require("pacman")) install.packages("pacman")
pacman::p_load(here)

read_num <- function(file, key) {
  rhs <- sub(".*=", "", grep(key, readLines(here(file)), value = TRUE)[1])
  as.numeric(regmatches(rhs, regexpr("-?[0-9]+(\\.[0-9]+)?", rhs)))
}
our_value_B <- read_num("output/stage3_numbers.txt", "Total Retrospective Value")

# --- External constants -------------------------------------------------------
# 2016 ANSER External Benefits AoA (BY$2024 NPV @ 7%, 2030-2055): benefit channels
# (Fig. 34), the three costed replacement alternatives' life-cycle costs (Fig. 33).
aoa <- list(
  npv_evacuation = 138.84, npv_air_traffic = 134.18, npv_maritime = 0.18,
  npv_chemistry = 227.65, npv_total = 500.86, bcr_base = 0.75, years_horizon = 25,
  lcc = c("Two C-130J (least cost)" = 680.0, "WP-3D life extension" = 723.6, "Two A400M" = 1414.15))
# 2022 NOAA Aircraft Recapitalization Plan + GAO-25-107210 (Jan 2025).
recap <- list(total_acq_B = 1.40, remaining_B = 0.678)

# --- Stage 4 numbers ----------------------------------------------------------
our_value_M <- our_value_B * 1000
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
  sprintf("  Remaining to fund (Jan 2025, $B)        = %.2f", recap$remaining_B)
), here("output/stage4_costbenefit_numbers.txt"))

# --- Table: social value vs replacement-platform alternatives (tab:platform_comparison)
alts <- data.frame(name = names(aoa$lcc), lcc = unname(aoa$lcc))
alts$aoa_bcr <- aoa$npv_total / alts$lcc
alts$our_bcr <- (our_value_B * 1000) / alts$lcc
body <- paste0(alts$name, " & ", formatC(alts$lcc, format = "f", digits = 0, big.mark = ","),
               " & ", sprintf("%.2f", alts$aoa_bcr), " & ", sprintf("%.1f", alts$our_bcr), "\\\\")
note <- sprintf(paste0(
  "\\item Life-cycle costs are the net present values (base-year \\$2024) of the three replacement ",
  "alternatives costed in the 2016 Analysis of Alternatives \\citep{ANSER2016ExternalBenefits}. The ",
  "AoA column divides that study's \\$%d million external-benefit estimate by each cost, and the final ",
  "column instead uses the \\$%.2f billion denial-sample social value of the dropsonde precision channel."),
  round(aoa$npv_total), our_value_B)
writeLines(c(
  "\\begin{table}", "",
  "\\caption{Social value of augmented observations against the replacement-platform alternatives. \\label{tab:platform_comparison}}",
  "\\centering", "\\begin{adjustbox}{max width = \\textwidth}", "\\begin{threeparttable}",
  "\\begin{tabular}[t]{lrrr}", "\\toprule",
  "Replacement alternative & Life-cycle cost (NPV, \\$M) & Benefit-cost ratio, AoA & Benefit-cost ratio, this paper\\\\",
  "\\midrule", body, "\\bottomrule", "\\end{tabular}",
  "\\begin{tablenotes}", "\\small", note, "\\end{tablenotes}",
  "\\end{threeparttable}", "\\end{adjustbox}", "\\end{table}"
), here("output/platform_comparison.tex"))
cat("Wrote output/platform_comparison.tex + stage4_costbenefit_numbers.txt\n")
