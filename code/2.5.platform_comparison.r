# ==============================================================================
# 2.5 Appendix C Table C2: AO social value vs the AoA replacement-platform
# alternatives. Costs and benefits are from the 2016 AoA report (ANSER2016,
# Fig. 33); the value is the Stage 3 headline. Writes platform_comparison.tex.
# ==============================================================================
if (!require("pacman")) install.packages("pacman")
pacman::p_load(here)

our_value_B    <- 5.05    # denial-sample headline social value ($B), Stage 3
aoa_benefits_M <- 501     # AoA external benefits, NPV BY$2024 ($M)

# AoA Fig. 33: life-cycle cost NPV (BY$2024, $M) for the three costed alternatives
alts <- data.frame(
  name = c("Two C-130J (least cost)", "WP-3D life extension", "Two A400M"),
  lcc  = c(680.0, 723.6, 1414.15)
)
alts$aoa_bcr <- aoa_benefits_M / alts$lcc
alts$our_bcr <- (our_value_B * 1000) / alts$lcc

body <- paste0(
  alts$name, " & ",
  formatC(alts$lcc, format = "f", digits = 0, big.mark = ","), " & ",
  sprintf("%.2f", alts$aoa_bcr), " & ",
  sprintf("%.1f", alts$our_bcr), "\\\\"
)

note <- sprintf(paste0(
  "\\item Life-cycle costs are the net present values (base-year \\$2024) of the three replacement ",
  "alternatives costed in the 2016 Analysis of Alternatives \\citep{ANSER2016ExternalBenefits}. The ",
  "AoA column divides that study's \\$%d million external-benefit estimate by each cost, and the final ",
  "column instead uses the \\$%.2f billion denial-sample social value of the dropsonde precision ",
  "channel."),
  aoa_benefits_M, our_value_B)

tex <- c(
  "\\begin{table}", "",
  "\\caption{Social value of augmented observations against the replacement-platform alternatives. \\label{tab:platform_comparison}}",
  "\\centering",
  "\\begin{adjustbox}{max width = \\textwidth}",
  "\\begin{threeparttable}",
  "\\begin{tabular}[t]{lrrr}",
  "\\toprule",
  "Replacement alternative & Life-cycle cost (NPV, \\$M) & Benefit-cost ratio, AoA & Benefit-cost ratio, this paper\\\\",
  "\\midrule", body, "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}", "\\small", note, "\\end{tablenotes}",
  "\\end{threeparttable}", "\\end{adjustbox}", "\\end{table}"
)
writeLines(tex, here("augmented_observations/figures and tables/platform_comparison.tex"))
cat("Wrote platform_comparison.tex\n")
