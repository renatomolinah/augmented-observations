# ==============================================================================
# 2.3 Stage-2 precision tables with restricted wild cluster bootstrap inference
# (Webb weights, clustered on storm), since few storm clusters make analytical
# p-values over-reject; stars come from the bootstrap p. Outputs the interaction
# grid (tab:interaction-grid) and the full coefficient table (tab:precision_full).
# Run: Rscript --vanilla code/2.3.interaction_bootstrap.r
# ==============================================================================
if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, fixest, here, modelsummary, parallel)
options(modelsummary_format_numeric_latex = "plain")
source(here("code/functions_table.r"))
setFixest_nthreads(1)               # we parallelize across bootstrap tasks instead

B       <- 1999
N_CORES <- 6
webb    <- c(-sqrt(1.5), -1, -sqrt(.5), sqrt(.5), 1, sqrt(1.5))
RNGkind("L'Ecuyer-CMRG"); set.seed(2024)
STARS   <- c("*" = 0.1, "**" = 0.05, "***" = 0.01)
STAT    <- "[{p.value}]"   # show only the wild bootstrap p-value (SE uninformative)

# ---- data: explicit dummies so a single interaction column can be dropped -----
d <- fread(here("data/clean_denial/forecast_denial_data_with_lead_times.csv"))
d <- d[!is.na(augmented_obs) & lead_days %in% c(1, 2, 3)]
d[, `:=`(hf = as.integer(wind_obs > 33),
         l2 = as.integer(lead_days == 2),
         l3 = as.integer(lead_days == 3))]
d[, `:=`(AO = augmented_obs,
         AO_l2 = augmented_obs * l2, AO_l3 = augmented_obs * l3,
         AO_hf = augmented_obs * hf,
         AO_l2_hf = augmented_obs * l2 * hf, AO_l3_hf = augmented_obs * l3 * hf,
         l2_hf = l2 * hf, l3_hf = l3 * hf)]

mkform <- function(lhs, regs, fe) {
  rhs <- if (length(regs) == 0) "1" else paste(regs, collapse = "+")  # FE-only restricted fit
  as.formula(paste(lhs, "~", rhs, "|", fe))
}

# ---- restricted wild cluster bootstrap p-value for one coefficient ------------
# Seed from the test CONTENT (regressors + FE + term) so each p is reproducible
# and invariant to the task list. Cluster (and resample Webb weights) on storm.
wcb_p <- function(dat, idx_, G_, regs, fe, term) {
  iv <- utf8ToInt(paste(c(sort(regs), fe, term), collapse = "|"))
  set.seed(2024L + as.integer(sum(iv * seq_along(iv)) %% 1000000L))
  m  <- feols(mkform("wind_fc_sd", regs, fe), dat, cluster = ~name)
  t_obs <- coef(m)[term] / se(m)[term]
  mr <- feols(mkform("wind_fc_sd", setdiff(regs, term), fe), dat, cluster = ~name)
  fit_r <- predict(mr); res_r <- resid(mr)
  ffb <- mkform("ystar", regs, fe)
  dt <- copy(dat)
  tstar <- numeric(B)
  for (b in seq_len(B)) {
    w <- webb[sample.int(6, G_, replace = TRUE)][idx_]
    dt[, ystar := fit_r + res_r * w]
    mb <- feols(ffb, dt, cluster = ~name)
    tstar[b] <- coef(mb)[term] / se(mb)[term]
  }
  mean(abs(tstar) >= abs(t_obs))
}

# ---- run a whole table family: estimate, clustered SE, bootstrap p per coef ---
run_family <- function(coldefs, dat) {
  storms <- unique(dat$name); G_ <- length(storms); idx_ <- match(dat$name, storms)
  tasks <- rbindlist(lapply(names(coldefs),
    function(cc) data.table(col = cc, term = coldefs[[cc]]$regs)))
  pv <- rbindlist(mclapply(seq_len(nrow(tasks)), function(i) {
    cc <- tasks$col[i]; tm <- tasks$term[i]
    data.table(col = cc, term = tm,
               p.value = wcb_p(dat, idx_, G_, coldefs[[cc]]$regs, coldefs[[cc]]$fe, tm))
  }, mc.cores = N_CORES, mc.set.seed = TRUE))
  est <- rbindlist(lapply(names(coldefs), function(cc) {
    m <- feols(mkform("wind_fc_sd", coldefs[[cc]]$regs, coldefs[[cc]]$fe), dat, cluster = ~name)
    rg <- coldefs[[cc]]$regs
    data.table(col = cc, term = rg, estimate = unname(coef(m)[rg]),
               std.error = unname(se(m)[rg]), nobs = nobs(m),
               r2 = unname(r2(m, "r2")), ar2 = unname(r2(m, "ar2")))
  }))
  list(res = merge(est, pv, by = c("col", "term")), G = G_)
}

# ---- modelsummary_list objects from a family result ---------------------------
make_mods <- function(res, coldefs) {
  mods <- lapply(names(coldefs), function(cc) {
    rr <- res[col == cc][match(coldefs[[cc]]$regs, term)]   # preserve regressor order
    ti <- data.frame(term = rr$term, estimate = rr$estimate,
                     std.error = rr$std.error, p.value = rr$p.value)
    gi <- data.frame(nobs = rr$nobs[1], r.squared = rr$r2[1], adj.r.squared = rr$ar2[1])
    structure(list(tidy = ti, glance = gi), class = "modelsummary_list")
  })
  setNames(mods, names(coldefs))
}

gm <- list(
  list(raw = "nobs",          clean = "Num.Obs.", fmt = function(x) formatC(x, format = "d", big.mark = ",")),
  list(raw = "r.squared",     clean = "R2",       fmt = 3),
  list(raw = "adj.r.squared", clean = "R2 Adj.",  fmt = 3))

# ==============================================================================
# Interaction grid (full denial panel, leads 1-3)
# ==============================================================================
# Columns (5) and (6): full triple interaction without and with the realized-wind control.
full_triple <- c("AO", "l2", "l3", "hf", "AO_l2", "AO_l3", "AO_hf",
                 "l2_hf", "l3_hf", "AO_l2_hf", "AO_l3_hf")
cols <- list(
  "(1)" = list(regs = c("AO"),                                           fe = "county"),
  "(2)" = list(regs = c("AO"),                                           fe = "county+name"),
  "(3)" = list(regs = c("AO", "wind_obs"),                               fe = "county+name"),
  "(4)" = list(regs = c("AO", "l2", "l3", "AO_l2", "AO_l3", "wind_obs"), fe = "county+name"),
  "(5)" = list(regs = full_triple,                                       fe = "county+name"),
  "(6)" = list(regs = c(full_triple, "wind_obs"),                        fe = "county+name")
)
cat(sprintf("[2.3] interaction grid: bootstrap (B=%d, %d cores)\n", B, N_CORES))
fam <- run_family(cols, d)
res <- fam$res
fwrite(res, here("output/interaction_wcb_pvalues.csv"))
mods <- make_mods(res, cols)

intr_cm <- c(
  "AO"       = "$\\delta$: Augmented Observations (AO)",
  "AO_l2"    = "$\\delta \\times$ 2-day lead",
  "AO_l3"    = "$\\delta \\times$ 3-day lead",
  "AO_hf"    = "$\\delta \\times$ Hurricane-force",
  "AO_l2_hf" = "$\\delta \\times$ 2-day $\\times$ HF",
  "AO_l3_hf" = "$\\delta \\times$ 3-day $\\times$ HF",
  "wind_obs" = "Realized wind speed (m/s)")
intr_rows <- data.frame(
  term = c("County FE", "Storm FE", "Lead-time main effects", "Intensity main effect"),
  "(1)" = c("\\checkmark", "", "", ""),
  "(2)" = c("\\checkmark", "\\checkmark", "", ""),
  "(3)" = c("\\checkmark", "\\checkmark", "", ""),
  "(4)" = c("\\checkmark", "\\checkmark", "\\checkmark", ""),
  "(5)" = c("\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark"),
  "(6)" = c("\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark"),
  check.names = FALSE)

# NOTE: modelsummary strips one backslash level in the notes pass, so LaTeX
# macros need FOUR backslashes here to survive as \geq \delta \times.
boot_note <- "Square brackets report wild cluster bootstrap $p$-values with Webb weights, on which the stars are based."

modelsummary(
  mods, coef_map = intr_cm, gof_map = gm, add_rows = intr_rows,
  fmt = "%.3f", statistic = STAT, stars = STARS,
  escape = FALSE, threeparttable = TRUE,
  title = "Precision Gained due to Augmented Observations. \\label{tab:interaction-grid}",
  notes = list(paste0("HF denotes hurricane-force realizations, wind $\\\\geq$ 33 m/s; the reference category is 1-day-lead, sub-hurricane-force forecasts. ``Lead-time main effects'' are the 2- and 3-day lead indicators and ``Intensity main effect'' the hurricane-force indicator. ", boot_note)),
  output = here("output/paper_interaction_grid.tex"))
add_adjust_box(here("output/paper_interaction_grid.tex"))
cat("[2.3] wrote output/paper_interaction_grid.tex\n")

# Order: main effects, then two-way interactions, then the full triple last.
intr_cm_full <- c(
  "AO"       = "$\\delta$: Augmented Observations (AO)",
  "l2"       = "2-day lead",
  "l3"       = "3-day lead",
  "hf"       = "Hurricane-force (HF)",
  "wind_obs" = "Realized wind speed (m/s)",
  "AO_l2"    = "$\\delta \\times$ 2-day lead",
  "AO_l3"    = "$\\delta \\times$ 3-day lead",
  "AO_hf"    = "$\\delta \\times$ Hurricane-force",
  "l2_hf"    = "2-day lead $\\times$ HF",
  "l3_hf"    = "3-day lead $\\times$ HF",
  "AO_l2_hf" = "$\\delta \\times$ 2-day $\\times$ HF",
  "AO_l3_hf" = "$\\delta \\times$ 3-day $\\times$ HF")
intr_rows_full <- data.frame(
  term = c("County FE", "Storm FE", "Realized wind control"),
  "(1)" = c("\\checkmark", "", ""), "(2)" = c("\\checkmark", "\\checkmark", ""),
  "(3)" = c("\\checkmark", "\\checkmark", "\\checkmark"), "(4)" = c("\\checkmark", "\\checkmark", "\\checkmark"),
  "(5)" = c("\\checkmark", "\\checkmark", ""), "(6)" = c("\\checkmark", "\\checkmark", "\\checkmark"),
  check.names = FALSE)
modelsummary(
  mods, coef_map = intr_cm_full, gof_map = gm, add_rows = intr_rows_full,
  fmt = "%.3f", statistic = STAT, stars = STARS,
  escape = FALSE, threeparttable = TRUE,
  title = "Full coefficient estimates for the augmented-observations precision regression. \\label{tab:precision_full}",
  notes = list(paste0("Full coefficient vector of the precision regression, of which the main text reports only the augmented-observation terms. HF denotes hurricane-force realizations ($\\\\geq$ 33 m/s), with 1-day-lead sub-hurricane forecasts as the reference. ", boot_note)),
  output = here("output/paper_precision_full.tex"))
add_adjust_box(here("output/paper_precision_full.tex"))
cat("[2.3] wrote output/paper_precision_full.tex\n")

print(res[, .(col, term, estimate = round(estimate, 3), se = round(std.error, 3), p = round(p.value, 3))])
