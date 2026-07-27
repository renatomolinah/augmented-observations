# ==============================================================================
# Stage 1 - Precision gain from augmented observations (delta)
# Estimates how much AO reduce the ex-ante wind-speed forecast SD, with inference
# from a restricted wild cluster bootstrap (Webb weights, clustered on storm,
# since there are few storm clusters). Builds the Stage 1 tables. The lead and
# intensity cell identified here (3-day, hurricane-force) is the one Stage 2
# prices, so Stage 2 estimates its cost curvature at the same 3-day lead.
# Inputs:  data/clean_denial/forecast_denial_data_with_lead_times.csv
# Outputs: output/{paper_interaction_grid,paper_precision_full,
#          paper_interaction_grid_precip,precision_jackknife_maria}.tex;
#          output/interaction_wcb_pvalues{,_precip}.csv; output/stage1_numbers.txt
# Run:     Rscript --vanilla code/1.precision.r
# ==============================================================================
if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, fixest, here, modelsummary, parallel)
options(modelsummary_format_numeric_latex = "plain")
source(here("code/functions_table.r"))
setFixest_nthreads(1)               # parallelism is across bootstrap tasks instead

B       <- 1999
N_CORES <- 6
webb    <- c(-sqrt(1.5), -1, -sqrt(.5), sqrt(.5), 1, sqrt(1.5))
RNGkind("L'Ecuyer-CMRG"); set.seed(2024)
STARS   <- c("*" = 0.1, "**" = 0.05, "***" = 0.01)
STAT    <- "[{p.value}]"            # show the wild bootstrap p-value (SE uninformative)

# explicit dummies so a single interaction column can be dropped for the bootstrap
d <- fread(here("data/clean_denial/forecast_denial_data_with_lead_times.csv"))
d <- d[!is.na(augmented_obs) & lead_days %in% c(1, 2, 3)]
d[, `:=`(hf = as.integer(wind_obs > 33),
         l2 = as.integer(lead_days == 2), l3 = as.integer(lead_days == 3))]
d[, `:=`(AO = augmented_obs, AO_l2 = augmented_obs * l2, AO_l3 = augmented_obs * l3,
         AO_hf = augmented_obs * hf, AO_l2_hf = augmented_obs * l2 * hf,
         AO_l3_hf = augmented_obs * l3 * hf, l2_hf = l2 * hf, l3_hf = l3 * hf)]

# Intensity control: 20 equal-count bins of the AVERAGE forecast wind, taken from
# the operational (ALL) run so it is config-invariant. This replaces the realized
# wind_obs control: wind_obs is exogenous to AO but the forecast is the ex-ante
# object; the lead-specific forecast wind is post-treatment (AO shifts it) so we
# use the storm-county average, which leaves the AO estimates unchanged. Twenty
# bins to match the wind_cut = ntile(wind_obs, 20) partition used in Stage 2; the
# estimate is invariant to the count anyway (quantile edges, not ntile, so a
# county-storm never splits across bins).
d[, wind_fc_avg := mean(wind_fc[augmented_obs == 1], na.rm = TRUE), by = .(county, name)]
d[, fc_cut := as.factor(cut(wind_fc_avg,
             breaks = unique(quantile(wind_fc_avg, 0:20 / 20, na.rm = TRUE)),
             include.lowest = TRUE, labels = FALSE))]

full_triple <- c("AO", "l2", "l3", "hf", "AO_l2", "AO_l3", "AO_hf",
                 "l2_hf", "l3_hf", "AO_l2_hf", "AO_l3_hf")

# ------------------------------------------------------------------------------
# Restricted wild cluster bootstrap p-value for one coefficient. Seeded from the
# test content so each p is reproducible and order-invariant; resample on storm.
# ------------------------------------------------------------------------------
mkform <- function(lhs, regs, fe) {
  rhs <- if (length(regs) == 0) "1" else paste(regs, collapse = "+")
  as.formula(paste(lhs, "~", rhs, "|", fe))
}
wcb_p <- function(dat, idx_, G_, regs, fe, term, lhs = "wind_fc_sd") {
  iv <- utf8ToInt(paste(c(sort(regs), fe, term, lhs), collapse = "|"))
  set.seed(2024L + as.integer(sum(iv * seq_along(iv)) %% 1000000L))
  m  <- feols(mkform(lhs, regs, fe), dat, cluster = ~name)
  t_obs <- coef(m)[term] / se(m)[term]
  mr <- feols(mkform(lhs, setdiff(regs, term), fe), dat, cluster = ~name)
  fit_r <- predict(mr); res_r <- resid(mr); dt <- copy(dat); tstar <- numeric(B)
  for (b in seq_len(B)) {
    w <- webb[sample.int(6, G_, replace = TRUE)][idx_]
    dt[, ystar := fit_r + res_r * w]
    mb <- feols(mkform("ystar", regs, fe), dt, cluster = ~name)
    tstar[b] <- coef(mb)[term] / se(mb)[term]
  }
  mean(abs(tstar) >= abs(t_obs))
}
run_family <- function(coldefs, dat, lhs = "wind_fc_sd") {
  storms <- unique(dat$name); G_ <- length(storms); idx_ <- match(dat$name, storms)
  tasks <- rbindlist(lapply(names(coldefs),
    function(cc) data.table(col = cc, term = coldefs[[cc]]$regs)))
  pv <- rbindlist(mclapply(seq_len(nrow(tasks)), function(i) {
    data.table(col = tasks$col[i], term = tasks$term[i],
               p.value = wcb_p(dat, idx_, G_, coldefs[[tasks$col[i]]]$regs,
                               coldefs[[tasks$col[i]]]$fe, tasks$term[i], lhs))
  }, mc.cores = N_CORES, mc.set.seed = TRUE))
  est <- rbindlist(lapply(names(coldefs), function(cc) {
    m <- feols(mkform(lhs, coldefs[[cc]]$regs, coldefs[[cc]]$fe), dat, cluster = ~name)
    rg <- coldefs[[cc]]$regs
    data.table(col = cc, term = rg, estimate = unname(coef(m)[rg]),
               std.error = unname(se(m)[rg]), nobs = nobs(m),
               r2 = unname(r2(m, "r2")), ar2 = unname(r2(m, "ar2")))
  }))
  merge(est, pv, by = c("col", "term"))
}
make_mods <- function(res, coldefs) {
  setNames(lapply(names(coldefs), function(cc) {
    rr <- res[col == cc][match(coldefs[[cc]]$regs, term)]
    structure(list(
      tidy   = data.frame(term = rr$term, estimate = rr$estimate,
                          std.error = rr$std.error, p.value = rr$p.value),
      glance = data.frame(nobs = rr$nobs[1], r.squared = rr$r2[1], adj.r.squared = rr$ar2[1])),
      class = "modelsummary_list")
  }), names(coldefs))
}
gm <- list(
  list(raw = "nobs",          clean = "Num.Obs.", fmt = function(x) formatC(x, format = "d", big.mark = ",")))
boot_note <- " Square brackets report wild cluster bootstrap $p$-values with Webb weights, on which the stars are based."

# ------------------------------------------------------------------------------
# Tables: interaction grid (tab:interaction-grid) and full vector (tab:precision_full)
# ------------------------------------------------------------------------------
cols <- list(
  "(1)" = list(regs = c("AO"),                              fe = "county"),
  "(2)" = list(regs = c("AO"),                              fe = "county+name"),
  "(3)" = list(regs = c("AO"),                              fe = "county+name+fc_cut"),
  "(4)" = list(regs = c("AO", "l2", "l3", "AO_l2", "AO_l3"), fe = "county+name+fc_cut"),
  "(5)" = list(regs = full_triple,                          fe = "county+name"),
  "(6)" = list(regs = full_triple,                          fe = "county+name+fc_cut"))
cat(sprintf("[Stage 1] interaction grid: bootstrap (B=%d, %d cores)\n", B, N_CORES))
res <- run_family(cols, d)
fwrite(res, here("output/interaction_wcb_pvalues.csv"))
mods <- make_mods(res, cols)

intr_cm <- c(
  "AO" = "$\\delta$: Augmented Observations (AO)", "AO_l2" = "$\\delta \\times$ 2-day lead",
  "AO_l3" = "$\\delta \\times$ 3-day lead", "AO_hf" = "$\\delta \\times$ Hurricane-force",
  "AO_l2_hf" = "$\\delta \\times$ 2-day $\\times$ HF", "AO_l3_hf" = "$\\delta \\times$ 3-day $\\times$ HF")
intr_rows <- data.frame(
  term = c("County FE", "Storm FE", "Lead-time main effects", "Intensity main effect", "Forecast-wind bins"),
  "(1)" = c("\\checkmark", "", "", "", ""), "(2)" = c("\\checkmark", "\\checkmark", "", "", ""),
  "(3)" = c("\\checkmark", "\\checkmark", "", "", "\\checkmark"),
  "(4)" = c("\\checkmark", "\\checkmark", "\\checkmark", "", "\\checkmark"),
  "(5)" = c("\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", ""),
  "(6)" = c("\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark"), check.names = FALSE)
modelsummary(
  mods, coef_map = intr_cm, gof_map = gm, add_rows = intr_rows,
  fmt = "%.3f", statistic = STAT, stars = STARS, escape = FALSE, threeparttable = TRUE,
  title = "Precision Gained due to Augmented Observations. \\label{tab:interaction-grid}",
  notes = list(paste0("HF denotes hurricane-force realizations, wind $\\\\geq$ 33 m/s; the reference category is 1-day-lead, sub-hurricane-force forecasts. ``Lead-time main effects'' are the 2- and 3-day lead indicators and ``Intensity main effect'' the hurricane-force indicator. ``Forecast-wind bins'' are twenty equal-count bins of the storm-county average forecast wind speed.", boot_note)),
  output = here("output/paper_interaction_grid.tex"))
add_adjust_box(here("output/paper_interaction_grid.tex"))

intr_cm_full <- c(
  "AO" = "$\\delta$: Augmented Observations (AO)", "l2" = "2-day lead", "l3" = "3-day lead",
  "hf" = "Hurricane-force (HF)",
  "AO_l2" = "$\\delta \\times$ 2-day lead", "AO_l3" = "$\\delta \\times$ 3-day lead",
  "AO_hf" = "$\\delta \\times$ Hurricane-force", "l2_hf" = "2-day lead $\\times$ HF",
  "l3_hf" = "3-day lead $\\times$ HF", "AO_l2_hf" = "$\\delta \\times$ 2-day $\\times$ HF",
  "AO_l3_hf" = "$\\delta \\times$ 3-day $\\times$ HF")
intr_rows_full <- data.frame(
  term = c("County FE", "Storm FE", "Forecast-wind bins"),
  "(1)" = c("\\checkmark", "", ""), "(2)" = c("\\checkmark", "\\checkmark", ""),
  "(3)" = c("\\checkmark", "\\checkmark", "\\checkmark"), "(4)" = c("\\checkmark", "\\checkmark", "\\checkmark"),
  "(5)" = c("\\checkmark", "\\checkmark", ""), "(6)" = c("\\checkmark", "\\checkmark", "\\checkmark"),
  check.names = FALSE)
modelsummary(
  mods, coef_map = intr_cm_full, gof_map = gm, add_rows = intr_rows_full,
  fmt = "%.3f", statistic = STAT, stars = STARS, escape = FALSE, threeparttable = TRUE,
  title = "Full coefficient estimates for the augmented-observations precision regression. \\label{tab:precision_full}",
  notes = list(paste0("Full coefficient vector of the precision regression, of which the main text reports only the augmented-observation terms. HF denotes hurricane-force realizations ($\\\\geq$ 33 m/s), with 1-day-lead sub-hurricane forecasts as the reference.", boot_note)),
  output = here("output/paper_precision_full.tex"))
add_adjust_box(here("output/paper_precision_full.tex"))

# ------------------------------------------------------------------------------
# Appendix table: precipitation forecast precision (tab:interaction-grid-precip).
# Mirror of the wind grid with the ex ante precipitation forecast SD as the
# outcome. Reported for symmetry with the null precipitation cost curvature in
# Stage 2: augmented observations sharpen the wind forecast, not precipitation.
# ------------------------------------------------------------------------------
dp <- d[!is.na(precip_fc_sd)]
cat(sprintf("[Stage 1] precip interaction grid: bootstrap (B=%d, %d cores)\n", B, N_CORES))
res_p  <- run_family(cols, dp, lhs = "precip_fc_sd")
fwrite(res_p, here("output/interaction_wcb_pvalues_precip.csv"))
mods_p <- make_mods(res_p, cols)
intr_cm_p <- c(
  "AO" = "$\\delta^p$: Augmented Observations (AO)", "AO_l2" = "$\\delta^p \\times$ 2-day lead",
  "AO_l3" = "$\\delta^p \\times$ 3-day lead", "AO_hf" = "$\\delta^p \\times$ Hurricane-force",
  "AO_l2_hf" = "$\\delta^p \\times$ 2-day $\\times$ HF", "AO_l3_hf" = "$\\delta^p \\times$ 3-day $\\times$ HF")
modelsummary(
  mods_p, coef_map = intr_cm_p, gof_map = gm, add_rows = intr_rows,
  fmt = "%.3f", statistic = STAT, stars = STARS, escape = FALSE, threeparttable = TRUE,
  title = "Precipitation Forecast Precision and Augmented Observations. \\label{tab:interaction-grid-precip}",
  notes = list(paste0("Outcome is the ex ante precipitation forecast standard deviation. HF denotes hurricane-force realizations, wind $\\\\geq$ 33 m/s; the reference category is 1-day-lead, sub-hurricane-force forecasts. ``Lead-time main effects'' are the 2- and 3-day lead indicators and ``Intensity main effect'' the hurricane-force indicator. ``Forecast-wind bins'' are twenty equal-count bins of the storm-county average forecast wind speed.", boot_note)),
  output = here("output/paper_interaction_grid_precip.tex"))
add_adjust_box(here("output/paper_interaction_grid_precip.tex"))

# ------------------------------------------------------------------------------
# Table: one-storm-out jackknife of the AO x 3-day x HF triple (tab:precision_jackknife_maria)
# ------------------------------------------------------------------------------
TERM <- "AO_l3_hf"
mkf  <- function(lhs, regs) as.formula(paste(lhs, "~", paste(regs, collapse = "+"), "| county + name + fc_cut"))
wcb  <- function(dat, seed) {
  storms <- unique(dat$name); G_ <- length(storms); idx_ <- match(dat$name, storms)
  set.seed(seed)
  m  <- feols(mkf("wind_fc_sd", full_triple), dat, cluster = ~name)
  t_obs <- coef(m)[[TERM]] / se(m)[[TERM]]
  mr <- feols(mkf("wind_fc_sd", setdiff(full_triple, TERM)), dat, cluster = ~name)
  fit_r <- predict(mr); res_r <- resid(mr); dt <- copy(dat); ts <- numeric(B)
  for (b in seq_len(B)) {
    w <- webb[sample.int(6, G_, replace = TRUE)][idx_]
    dt[, ystar := fit_r + res_r * w]
    mb <- feols(mkf("ystar", full_triple), dt, cluster = ~name)
    ts[b] <- coef(mb)[[TERM]] / se(mb)[[TERM]]
  }
  list(est = coef(m)[[TERM]], p = mean(abs(ts) >= abs(t_obs)), n = nobs(m))
}
seed_of <- function(s) 2024L + sum(utf8ToInt(s))
star    <- function(p) ifelse(p < 0.01, "***", ifelse(p < 0.05, "**", ifelse(p < 0.1, "*", "")))
storms  <- sort(unique(d$name))
# (None) reuses the grid's col-6 estimate/p so the baseline matches the main table exactly;
# only the dropped-storm rows are re-bootstrapped here.
base <- res[col == "(6)" & term == "AO_l3_hf"]
jk <- rbind(
  data.table(dropped = "(none)", n = base$nobs, est = base$estimate, p = base$p.value),
  rbindlist(mclapply(storms, function(s) {
    r <- wcb(d[name != s], seed_of(paste0("jk_", s)))
    data.table(dropped = s, n = r$n, est = r$est, p = r$p)
  }, mc.cores = N_CORES, mc.set.seed = TRUE)))

tc <- function(s) ifelse(s == "(none)", "(None)", paste0(toupper(substr(s,1,1)), substr(s,2,nchar(s))))
jkbody <- paste0(tc(jk$dropped), " & ", format(jk$n, big.mark = ","), " & ",
                 sprintf("%.3f%s", jk$est, star(jk$p)), " & ", sprintf("%.3f", jk$p), "\\\\")
jktex <- c("\\begin{table}", "",
  "\\caption{One-storm-out jackknife of the augmented-observation $\\times$ 3-day $\\times$ hurricane-force interaction. \\label{tab:precision_jackknife_maria}}",
  "\\centering", "\\begin{threeparttable}", "\\begin{tabular}[t]{lrrr}", "\\toprule",
  "Dropped & Obs. & AO $\\times$ 3-day $\\times$ HF (m/s) & Bootstrap $p$\\\\", "\\midrule", jkbody, "\\bottomrule",
  "\\end{tabular}", "\\begin{tablenotes}", "\\small",
  "\\item * p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01 The augmented-observation $\\times$ 3-day $\\times$ hurricane-force interaction from the full lead $\\times$ intensity model, re-estimated after dropping each storm; the (None) row reproduces the main-text estimate. Significance from a restricted wild cluster bootstrap with Webb weights (B = 1999, clustered on storm).",
  "\\end{tablenotes}", "\\end{threeparttable}", "\\end{table}")
writeLines(jktex, here("output/precision_jackknife_maria.tex"))
add_adjust_box(here("output/precision_jackknife_maria.tex"))

# ------------------------------------------------------------------------------
# Stage 1 numbers: the headline delta and the baseline forecast SD it improves
# (HF + 3-day lead, AO-assimilated). Consumed by Stage 3.
# ------------------------------------------------------------------------------
reg3   <- d[lead_days == 3 & wind_obs > 33]
mod    <- feols(wind_fc_sd ~ augmented_obs | county + name + fc_cut, reg3, vcov = ~name)
delta  <- abs(coef(mod)["augmented_obs"])
dse    <- sqrt(diag(vcov(mod)))["augmented_obs"]
dp     <- 2 * (1 - pnorm(abs(coef(mod)["augmented_obs"]) / dse))
ao3    <- reg3[augmented_obs == 1]
# full precision: Stage 3 reads delta and sigma_bar for the dollar valuation (the paper rounds when citing)
writeLines(c(
  sprintf("delta                              = %.6f m/s", delta),
  sprintf("delta SE                           = %.6f",     dse),
  sprintf("delta p-value                      = %.4f",     dp),
  "",
  "-- HF + 3-day lead, AO-only (Stage 3 baseline) --",
  sprintf("  N                                = %d",       nrow(ao3)),
  sprintf("  sigma_bar                        = %.6f m/s", mean(ao3$wind_fc_sd, na.rm = TRUE)),
  sprintf("  sd(sigma)                        = %.6f m/s", sd(ao3$wind_fc_sd,   na.rm = TRUE))
), here("output/stage1_numbers.txt"))
cat("Wrote Stage 1 tables + output/stage1_numbers.txt\n")
