# ==============================================================================
# 2.1 Extend the retrospective AO valuation to every hurricane-force, 3-day-lead
# county-storm in the 2005-2024 panel, each priced at its own baseline sigma_3:
# V = beta_Hurr * [(sigma_3+delta)^2 - sigma_3^2], beta/delta read from the pipeline.
# Output: output/extended_valuation_numbers.txt (figures only). Needs the Box mount.
# ==============================================================================
suppressMessages({library(data.table); library(here)})

# --- beta and delta from the headline pipeline (Stage 1 / Stage 2) ------------
s3 <- readLines(here("output/stage3_numbers.txt"))
getnum <- function(key) as.numeric(sub(".*=\\s*", "", grep(key, s3, value = TRUE)[1]))
beta  <- getnum("^beta_hurr")
delta <- getnum("^delta \\(m/s\\)")

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

summ <- function(d) list(storms = uniqueN(d$name), cs = nrow(d), B = sum(d$value_M) / 1000)
hwrf  <- summ(hf[year >= 2007])   # since HWRF went operational (2007)
all10 <- summ(hf[year >= 2010])
allyr <- summ(hf)                 # full 2005-2024 panel
den   <- summ(hf[name %in% c("irma", "michael", "florence", "laura", "sally", "isaias")])
rate  <- function(s, y0) s$B * 1000 / (2024 - y0 + 1)   # $M per year

out <- c(
  sprintf("beta_Hurr ($M/(m/s)^2) = %.3f", beta),
  sprintf("delta (m/s) = %.3f", delta),
  sprintf("continental denial-6: %d county-storms, $%.2fB", den$cs, den$B),
  sprintf("since HWRF 2007-2024: %d storms, %d county-storms, $%.2fB, $%.0fM/yr",
          hwrf$storms, hwrf$cs, hwrf$B, rate(hwrf, 2007)),
  sprintf("2010-2024: $%.2fB, $%.0fM/yr", all10$B, rate(all10, 2010)),
  sprintf("2005-2024: $%.2fB, $%.0fM/yr", allyr$B, rate(allyr, 2005))
)
writeLines(out, here("output/extended_valuation_numbers.txt"))
cat(out, sep = "\n")
