# ==============================================================================
# 2.1 Extend the retrospective AO valuation to every hurricane-force, 3-day-lead
# county-storm in the 2005-2024 panel, each priced at its own baseline sigma_3:
# V = beta_Hurr * [(sigma_3+delta)^2 - sigma_3^2], beta/delta read from the pipeline.
# Output: output/extended_valuation_numbers.txt (figures only). Needs the Box mount.
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
# Numbers only; the standalone extended-valuation table was retired in favor of
# the platform comparison (code/2.5.platform_comparison.r). The $6.2B BOTE figure
# is cited from this output in the main text and Appendix C.
