# ==============================================================================
# 2.1.extended_valuation.r
# PURPOSE: Extend the retrospective AO valuation beyond the 8-storm denial archive
#          to every hurricane-force, 3-day-lead county-storm in the 2005-2024
#          forecast panel. Augmented observations are flown for essentially every
#          hurricane threatening the continental US, so the continental denial
#          subsample (6 storms, 62 county-storms) captures only a fraction of the
#          realized value.
#
# METHOD:  Each county-storm is priced at its OWN 3-day baseline forecast
#          uncertainty sigma_3 (NHC operational forecast SD, from the raw
#          wind_error files), holding the AO precision gain delta fixed at its
#          Stage 2 estimate:   V = beta_Hurr * [ (sigma_3 + delta)^2 - sigma_3^2 ].
#          beta_Hurr (Stage 1) and delta (Stage 2) are read from the headline
#          pipeline output so this script stays in sync with the main results.
#
# CRITERION: lead_days == 3 & realized wind > 33 m/s. This automatically excludes
#          rapidly forming storms with no 3-day-ahead hurricane-force forecast
#          (e.g., Harvey, Hanna), exactly as in the denial sample.
#
# OUTPUT:  output/extended_valuation_numbers.txt  (citable figures; not a table).
# DEPENDS: data/raw/wind_error/*_errors.csv (Box mount) and output/stage3_numbers.txt.
# ==============================================================================
suppressMessages({library(data.table); library(here)})

# --- beta and delta from the headline pipeline (Stage 1 / Stage 2) ------------
s3_path <- here("output/stage3_numbers.txt")
if (!file.exists(s3_path))
  s3_path <- here("augmented_observations/figures and tables/stage3_numbers.txt")
s3 <- readLines(s3_path)
getnum <- function(key) as.numeric(sub(".*=\\s*", "", grep(key, s3, value = TRUE)[1]))
beta  <- getnum("^beta_hurr")
delta <- getnum("^delta \\(m/s\\)")
stopifnot(is.finite(beta), is.finite(delta))

# --- raw 3-day forecast-error panel, all 37 storms ----------------------------
files <- list.files(here("data/raw/wind_error"), pattern = "_errors\\.csv$", full.names = TRUE)
raw <- rbindlist(lapply(files, fread), fill = TRUE)
raw[, year := as.integer(substr(as.character(pred_date), 1, 4))]

# county-storm at the 3-day lead: peak realized wind, mean forecast SD over cycles
cs <- raw[lead_days == 3,
          .(wind_obs = max(vmax_sust_obs, na.rm = TRUE),
            sigma3   = mean(vmax_sust_pred_sd, na.rm = TRUE)),
          by = .(year, name, county = gridid)]
hf <- cs[is.finite(wind_obs) & wind_obs > 33 & is.finite(sigma3)]
hf[, value_M := beta * ((sigma3 + delta)^2 - sigma3^2)]

# uniform per-county value at the denial-sample baseline (robustness)
sigma_bar <- 10.13
vbar <- beta * ((sigma_bar + delta)^2 - sigma_bar^2)

summ <- function(d) list(storms = uniqueN(d$name), cs = nrow(d),
                         B = sum(d$value_M) / 1000, Bunif = nrow(d) * vbar / 1000)
all10 <- summ(hf[year >= 2010]); allyr <- summ(hf)
hwrf  <- summ(hf[year >= 2007])   # back-of-envelope: since HWRF went operational (2007)
yrs10 <- 2024 - 2010 + 1; yrsall <- 2024 - 2005 + 1; yrshwrf <- 2024 - 2007 + 1

# validation: the six continental denial storms reproduce 62 county-storms
den <- hf[name %in% c("irma", "michael", "florence", "laura", "sally", "isaias")]

out <- c(
  sprintf("beta_Hurr ($M per (m/s)^2)            = %.3f", beta),
  sprintf("delta (m/s)                           = %.3f", delta),
  sprintf("VALIDATION: continental denial-6 storms = %d county-storms, $%.3fB (own-sigma)",
          nrow(den), sum(den$value_M) / 1000),
  "",
  sprintf("HEADLINE -- since 2010: %d storms, %d hurricane-force county-storms", all10$storms, all10$cs),
  sprintf("  storm-specific (own sigma_3) total  = $%.2fB  ($%.0fM/yr over %d yr)",
          all10$B, all10$B * 1000 / yrs10, yrs10),
  sprintf("  uniform (sigma_bar=%.2f, $%.2fM/cs) = $%.2fB  (robustness)", sigma_bar, vbar, all10$Bunif),
  "",
  sprintf("BOTE since HWRF (2007-2024): %d storms, %d county-storms, $%.2fB ($%.0fM/yr over %d yr)",
          hwrf$storms, hwrf$cs, hwrf$B, hwrf$B * 1000 / yrshwrf, yrshwrf),
  sprintf("ALL 2005-2024: %d storms, %d county-storms, $%.2fB ($%.0fM/yr over %d yr)",
          allyr$storms, allyr$cs, allyr$B, allyr$B * 1000 / yrsall, yrsall)
)
dir.create(here("output"), showWarnings = FALSE)
writeLines(out, here("output/extended_valuation_numbers.txt"))
cat(paste(out, collapse = "\n"), "\n")

# --- LaTeX table (matches the appendix threeparttable/booktabs style) ---------
fmtrow <- function(label, s) sprintf("%s & %d & %d & %.2f & %s\\\\",
                                     label, s$storms, s$cs, s$B, s$ann)
denrow <- list(storms = uniqueN(den$name), cs = nrow(den),
               B = sum(den$value_M) / 1000, ann = "--")
all10$ann  <- sprintf("%.0f", all10$B  * 1000 / yrs10)
allyr$ann  <- sprintf("%.0f", allyr$B  * 1000 / yrsall)
hwrf$ann   <- sprintf("%.0f", hwrf$B   * 1000 / yrshwrf)
tex <- c(
  "\\begin{table}", "",
  "\\caption{Extended retrospective social value of augmented observations, full forecast panel. \\label{tab:extended_valuation}}",
  "\\centering",
  "\\begin{adjustbox}{max width = \\textwidth}",
  "\\begin{threeparttable}",
  "\\begin{tabular}[t]{lrrrr}",
  "\\toprule",
  "Sample & Storms & HF County-Storms & Value (\\$B) & Annualized (\\$M/yr)\\\\",
  "\\midrule",
  fmtrow("Denial sample, 2017--2020 (validation)", denrow),
  fmtrow("Since HWRF, 2007--2024", hwrf),
  fmtrow("Full panel, 2010--2024", all10),
  fmtrow("Full panel, 2005--2024", allyr),
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}",
  "\\small",
  sprintf("\\item Each hurricane-force ($\\geq$ 33 m/s), 3-day-lead county-storm is priced at its own baseline forecast standard deviation $\\sigma_3$, holding the marginal cost parameter $\\hat{\\beta}^{w}_{Hurr} = %.2f$ and the precision gain $\\delta = %.2f$ m/s fixed at their Stage 1 and Stage 2 estimates: $V = \\hat{\\beta}^{w}_{Hurr}\\,[(\\sigma_3+\\delta)^2 - \\sigma_3^2]$. Under storm-specific $\\sigma_3$ the six continental 2017--2020 denial storms reproduce 62 county-storms at \\$%.2fB; applying the pooled baseline $\\bar{\\sigma} = %.2f$ m/s uniformly across the same 62 county-storms gives the continental-subsample figure. The main-text headline additionally includes Hurricane Maria. Rapidly forming storms with no 3-day-ahead hurricane-force forecast (e.g., Harvey, Hanna) are excluded by construction.", beta, delta, denrow$B, sigma_bar),
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{adjustbox}",
  "\\end{table}"
)
writeLines(tex, here("output/extended_valuation.tex"))
