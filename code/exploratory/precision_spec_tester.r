# ============================================================================
# precision_spec_tester.r
# Scratch tool to test Stage-1 (delta = AO precision gain) specifications.
# The estimand is a PAIRED difference: for the same county-storm-date-lead we
# observe both configs, so delta = sigma_NO - sigma_ALL (>0 means AO helps).
# Differencing removes county and storm fixed effects for free; all that is
# left is to choose how to slice by intensity and lead.
#
# Interactive:  source("code/exploratory/precision_spec_tester.r"); then call
#               delta_by_cell(...) with your own arguments.
# Batch:        Rscript --vanilla code/exploratory/precision_spec_tester.r
#               (prints the recipe block at the bottom)
# ============================================================================
suppressMessages({library(data.table); library(fixest)})
setFixest_nthreads(1)

DATA <- "/Users/renatomolina/Desktop/Research/GitHub/augmented-observations/data/clean_denial/forecast_denial_data_with_lead_times.csv"

# ---- build the paired dataset (one row per county-storm-date-lead) ----------
raw <- fread(DATA)[!is.na(augmented_obs) & lead_days %in% 1:3]
key <- c("county", "name", "date", "lead_days")
agg <- raw[, .(sigma  = mean(wind_fc_sd, na.rm = TRUE),   # ex-ante forecast SD (outcome)
               w_real = mean(wind_obs,   na.rm = TRUE),   # realized (ex post) wind
               w_fore = mean(wind_fc,    na.rm = TRUE)),   # forecast (ex ante) wind
           by = c(key, "augmented_obs")]
w <- dcast(agg, county + name + date + lead_days ~ augmented_obs,
           value.var = c("sigma", "w_real", "w_fore"))
setnames(w, c("sigma_1", "sigma_0"), c("sigma_all", "sigma_no"))
w[, `:=`(dsigma = sigma_no - sigma_all,   # AO precision gain
         w_real = w_real_1,               # realized wind (config-invariant)
         w_fore = w_fore_1)]              # operational (ALL-run) forecast wind
w <- w[!is.na(dsigma)]

# ============================================================================
# delta_by_cell(): mean AO precision gain per intensity bin (optionally x lead)
#   intensity : "real" (ex post, realized) or "fore" (ex ante, operational forecast)
#   breaks    : intensity bin edges in m/s, e.g. c(-Inf, 33, Inf) for sub-HF / HF
#   leads     : which lead days to keep, e.g. 3 or 1:3
#   by_lead   : TRUE  -> a separate cell per (lead, bin)
#               FALSE -> pool the chosen leads into one cell per bin
#   Reports, per cell: n pairs, distinct storms (the cluster count),
#   mean sigma_ALL (the valuation baseline), delta, cluster(storm) SE, p.
# ============================================================================
delta_by_cell <- function(intensity = "real", breaks = c(-Inf, 33, Inf),
                          leads = 3, by_lead = FALSE, data = w) {
  dt <- copy(data[lead_days %in% leads])
  xv <- if (intensity == "real") dt$w_real else dt$w_fore
  dt[, bin := cut(xv, breaks = breaks, right = FALSE, dig.lab = 3)]
  dt <- dt[!is.na(bin)]
  grp <- if (by_lead) c("lead_days", "bin") else "bin"
  dt[, cell := do.call(paste, c(.SD, sep = " | ")), .SDcols = grp]

  m  <- feols(dsigma ~ 0 + cell, dt, cluster = ~name)
  ct <- as.data.table(coeftable(m), keep.rownames = "cell")
  setnames(ct, c("cell", "delta", "se", "t", "p"))
  ct[, cell := sub("^cell", "", cell)]
  info <- dt[, .(pairs = .N, storms = uniqueN(name),
                 sigma_all = mean(sigma_all, na.rm = TRUE)), by = cell]
  out <- merge(info, ct[, .(cell, delta, se, p)], by = "cell")[order(cell)]
  out[, `:=`(delta = round(delta, 3), se = round(se, 3), p = round(p, 4),
             sigma_all = round(sigma_all, 2))]
  out[]
}

# ---- spec_model(): the same cell regression, returned as a feols object -----
# so you can drop it straight into etable() for a formatted RStudio table.
spec_model <- function(intensity = "real", breaks = c(-Inf, 33, Inf),
                       leads = 3, by_lead = FALSE, data = w) {
  dt <- copy(data[lead_days %in% leads])
  xv <- if (intensity == "real") dt$w_real else dt$w_fore
  dt[, bin := cut(xv, breaks = breaks, right = FALSE, dig.lab = 3)]
  dt <- dt[!is.na(bin)]
  grp <- if (by_lead) c("lead_days", "bin") else "bin"
  dt[, cell := do.call(paste, c(.SD, sep = " | ")), .SDcols = grp]
  feols(dsigma ~ 0 + cell, dt, cluster = ~name)
}

# etable_specs(): side-by-side etable of several named specs. Renders as a
# clean aligned text table in the RStudio console. Pass file="foo.tex" to
# instead write a LaTeX table to disk.
etable_specs <- function(..., file = NULL) {
  models <- list(...)
  if (!is.null(file)) return(etable(models, file = file, replace = TRUE,
                                    digits = 3, fitstat = ~ n))
  etable(models, digits = 3, fitstat = ~ n)   # text table; print() to view
}

# ---- optional: restricted wild cluster bootstrap p for one cell -------------
# Use when a cell has few storm clusters (asymptotic p is unreliable there).
wcb_p <- function(intensity = "real", breaks = c(-Inf, 33, Inf), leads = 3,
                  B = 1999, seed = 2024, data = w) {
  dt <- copy(data[lead_days %in% leads])
  xv <- if (intensity == "real") dt$w_real else dt$w_fore
  dt <- dt[cut(xv, breaks, right = FALSE) == tail(levels(cut(xv, breaks, right = FALSE)), 1)]
  if (uniqueN(dt$name) < 2) return(NA_real_)
  webb <- c(-sqrt(1.5), -1, -sqrt(.5), sqrt(.5), 1, sqrt(1.5))
  storms <- unique(dt$name); G <- length(storms); idx <- match(dt$name, storms)
  m <- feols(dsigma ~ 1, dt, cluster = ~name); t_obs <- coef(m)[[1]] / se(m)[[1]]
  set.seed(seed); ts <- numeric(B)
  for (b in seq_len(B)) {                      # H0: mean gain = 0, so resample raw dsigma
    dt[, ystar := dsigma * webb[sample.int(6, G, replace = TRUE)][idx]]
    mb <- feols(ystar ~ 1, dt, cluster = ~name); ts[b] <- coef(mb)[[1]] / se(mb)[[1]]
  }
  mean(abs(ts) >= abs(t_obs))
}

# ============================================================================
# RECIPES — copy, tweak, rerun. (These print when run via Rscript.)
# ============================================================================
if (sys.nframe() == 0) {
  show <- function(lbl, x) { cat("\n== ", lbl, " ==\n", sep = ""); print(x) }

  show("A. current headline: REALIZED, HF split at 33, 3-day lead",
       delta_by_cell("real", c(-Inf, 33, Inf), leads = 3))

  show("B. REALIZED, finer bins (sub / 25-33 / 33-42 / 42+), 3-day  [monotonicity]",
       delta_by_cell("real", c(-Inf, 25, 33, 42, Inf), leads = 3))

  show("C. FORECAST intensity, HF split at 33, 3-day  [note the empty top bin]",
       delta_by_cell("fore", c(-Inf, 33, Inf), leads = 3))

  show("D. REALIZED HF, all leads shown separately  [localization at 3-day]",
       delta_by_cell("real", c(-Inf, 33, Inf), leads = 1:3, by_lead = TRUE))

  show("E. REALIZED HF, leads 1-3 POOLED into one cell  [pooling washes it out]",
       delta_by_cell("real", c(-Inf, 33, Inf), leads = 1:3, by_lead = FALSE))

  cat("\nwild cluster bootstrap p for the 3-day realized-HF cell: ",
      round(wcb_p("real", c(-Inf, 33, Inf), leads = 3), 4), "\n")

  # F. formatted etable — opens readable in the RStudio console / Viewer
  cat("\n== F. etable: realized HF vs finer realized bins vs forecast HF (3-day) ==\n")
  print(etable_specs(
    "Realized HF"    = spec_model("real", c(-Inf, 33, Inf),        leads = 3),
    "Realized bins"  = spec_model("real", c(-Inf, 25, 33, 42, Inf), leads = 3),
    "Forecast HF"    = spec_model("fore", c(-Inf, 33, Inf),        leads = 3)))
}
