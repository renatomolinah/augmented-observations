# ==============================================================================
# Stage 1 - Precision gain from augmented observations (delta)
# Estimates how much AO reduce the ex-ante wind-speed forecast SD, with inference
# from a restricted wild cluster bootstrap (Webb weights, clustered on storm,
# since there are few storm clusters). Builds the Stage 1 tables. The lead and
# intensity cell identified here (3-day, hurricane-force) is the one Stage 2
# prices, so Stage 2 estimates its cost curvature at the same 3-day lead.
# Inputs:  data/clean_denial/forecast_denial_data_with_lead_times.csv
# Outputs: output/{paper_interaction_grid,paper_precision_full,
#          precision_jackknife_maria}.tex; output/interaction_wcb_pvalues.csv;
#          output/stage1_numbers.txt
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
wcb_p <- function(dat, idx_, G_, regs, fe, term) {
  iv <- utf8ToInt(paste(c(sort(regs), fe, term), collapse = "|"))
  set.seed(2024L + as.integer(sum(iv * seq_along(iv)) %% 1000000L))
  m  <- feols(mkform("wind_fc_sd", regs, fe), dat, cluster = ~name)
  t_obs <- coef(m)[term] / se(m)[term]
  mr <- feols(mkform("wind_fc_sd", setdiff(regs, term), fe), dat, cluster = ~name)
  fit_r <- predict(mr); res_r <- resid(mr); dt <- copy(dat); tstar <- numeric(B)
  for (b in seq_len(B)) {
    w <- webb[sample.int(6, G_, replace = TRUE)][idx_]
    dt[, ystar := fit_r + res_r * w]
    mb <- feols(mkform("ystar", regs, fe), dt, cluster = ~name)
    tstar[b] <- coef(mb)[term] / se(mb)[term]
  }
  mean(abs(tstar) >= abs(t_obs))
}
run_family <- function(coldefs, dat) {
  storms <- unique(dat$name); G_ <- length(storms); idx_ <- match(dat$name, storms)
  tasks <- rbindlist(lapply(names(coldefs),
    function(cc) data.table(col = cc, term = coldefs[[cc]]$regs)))
  pv <- rbindlist(mclapply(seq_len(nrow(tasks)), function(i) {
    data.table(col = tasks$col[i], term = tasks$term[i],
               p.value = wcb_p(dat, idx_, G_, coldefs[[tasks$col[i]]]$regs,
                               coldefs[[tasks$col[i]]]$fe, tasks$term[i]))
  }, mc.cores = N_CORES, mc.set.seed = TRUE))
  est <- rbindlist(lapply(names(coldefs), function(cc) {
    m <- feols(mkform("wind_fc_sd", coldefs[[cc]]$regs, coldefs[[cc]]$fe), dat, cluster = ~name)
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
  list(raw = "nobs",          clean = "Num.Obs.", fmt = function(x) formatC(x, format = "d", big.mark = ",")),
  list(raw = "r.squared",     clean = "R2",       fmt = 3),
  list(raw = "adj.r.squared", clean = "R2 Adj.",  fmt = 3))
boot_note <- "Square brackets report wild cluster bootstrap $p$-values with Webb weights, on which the stars are based."

# ------------------------------------------------------------------------------
# Tables: interaction grid (tab:interaction-grid) and full vector (tab:precision_full)
# ------------------------------------------------------------------------------
cols <- list(
  "(1)" = list(regs = c("AO"),                                           fe = "county"),
  "(2)" = list(regs = c("AO"),                                           fe = "county+name"),
  "(3)" = list(regs = c("AO", "wind_obs"),                               fe = "county+name"),
  "(4)" = list(regs = c("AO", "l2", "l3", "AO_l2", "AO_l3", "wind_obs"), fe = "county+name"),
  "(5)" = list(regs = full_triple,                                       fe = "county+name"),
  "(6)" = list(regs = c(full_triple, "wind_obs"),                        fe = "county+name"))
cat(sprintf("[Stage 1] interaction grid: bootstrap (B=%d, %d cores)\n", B, N_CORES))
res <- run_family(cols, d)
fwrite(res, here("output/interaction_wcb_pvalues.csv"))
mods <- make_mods(res, cols)

intr_cm <- c(
  "AO" = "$\\delta$: Augmented Observations (AO)", "AO_l2" = "$\\delta \\times$ 2-day lead",
  "AO_l3" = "$\\delta \\times$ 3-day lead", "AO_hf" = "$\\delta \\times$ Hurricane-force",
  "AO_l2_hf" = "$\\delta \\times$ 2-day $\\times$ HF", "AO_l3_hf" = "$\\delta \\times$ 3-day $\\times$ HF",
  "wind_obs" = "Realized wind speed (m/s)")
intr_rows <- data.frame(
  term = c("County FE", "Storm FE", "Lead-time main effects", "Intensity main effect"),
  "(1)" = c("\\checkmark", "", "", ""), "(2)" = c("\\checkmark", "\\checkmark", "", ""),
  "(3)" = c("\\checkmark", "\\checkmark", "", ""), "(4)" = c("\\checkmark", "\\checkmark", "\\checkmark", ""),
  "(5)" = c("\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark"),
  "(6)" = c("\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark"), check.names = FALSE)
modelsummary(
  mods, coef_map = intr_cm, gof_map = gm, add_rows = intr_rows,
  fmt = "%.3f", statistic = STAT, stars = STARS, escape = FALSE, threeparttable = TRUE,
  title = "Precision Gained due to Augmented Observations. \\label{tab:interaction-grid}",
  notes = list(paste0("HF denotes hurricane-force realizations, wind $\\\\geq$ 33 m/s; the reference category is 1-day-lead, sub-hurricane-force forecasts. ``Lead-time main effects'' are the 2- and 3-day lead indicators and ``Intensity main effect'' the hurricane-force indicator. ", boot_note)),
  output = here("output/paper_interaction_grid.tex"))
add_adjust_box(here("output/paper_interaction_grid.tex"))

intr_cm_full <- c(
  "AO" = "$\\delta$: Augmented Observations (AO)", "l2" = "2-day lead", "l3" = "3-day lead",
  "hf" = "Hurricane-force (HF)", "wind_obs" = "Realized wind speed (m/s)",
  "AO_l2" = "$\\delta \\times$ 2-day lead", "AO_l3" = "$\\delta \\times$ 3-day lead",
  "AO_hf" = "$\\delta \\times$ Hurricane-force", "l2_hf" = "2-day lead $\\times$ HF",
  "l3_hf" = "3-day lead $\\times$ HF", "AO_l2_hf" = "$\\delta \\times$ 2-day $\\times$ HF",
  "AO_l3_hf" = "$\\delta \\times$ 3-day $\\times$ HF")
intr_rows_full <- data.frame(
  term = c("County FE", "Storm FE", "Realized wind control"),
  "(1)" = c("\\checkmark", "", ""), "(2)" = c("\\checkmark", "\\checkmark", ""),
  "(3)" = c("\\checkmark", "\\checkmark", "\\checkmark"), "(4)" = c("\\checkmark", "\\checkmark", "\\checkmark"),
  "(5)" = c("\\checkmark", "\\checkmark", ""), "(6)" = c("\\checkmark", "\\checkmark", "\\checkmark"),
  check.names = FALSE)
modelsummary(
  mods, coef_map = intr_cm_full, gof_map = gm, add_rows = intr_rows_full,
  fmt = "%.3f", statistic = STAT, stars = STARS, escape = FALSE, threeparttable = TRUE,
  title = "Full coefficient estimates for the augmented-observations precision regression. \\label{tab:precision_full}",
  notes = list(paste0("Full coefficient vector of the precision regression, of which the main text reports only the augmented-observation terms. HF denotes hurricane-force realizations ($\\\\geq$ 33 m/s), with 1-day-lead sub-hurricane forecasts as the reference. ", boot_note)),
  output = here("output/paper_precision_full.tex"))
add_adjust_box(here("output/paper_precision_full.tex"))

# ------------------------------------------------------------------------------
# Table: one-storm-out jackknife of the AO x 3-day x HF triple (tab:precision_jackknife_maria)
# ------------------------------------------------------------------------------
TERM <- "AO_l3_hf"
mkf  <- function(lhs, regs) as.formula(paste(lhs, "~", paste(regs, collapse = "+"), "| county + name"))
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
jk <- rbindlist(mclapply(c("(none)", storms), function(s) {
  sub <- if (s == "(none)") d else d[name != s]
  r <- wcb(sub, seed_of(paste0("jk_", s)))
  data.table(dropped = s, n = r$n, est = r$est, p = r$p)
}, mc.cores = N_CORES, mc.set.seed = TRUE))

tc <- function(s) ifelse(s == "(none)", "(None)", paste0(toupper(substr(s,1,1)), substr(s,2,nchar(s))))
jkbody <- paste0(tc(jk$dropped), " & ", format(jk$n, big.mark = ","), " & ",
                 sprintf("%.3f%s", jk$est, star(jk$p)), " & ", sprintf("%.3f", jk$p), "\\\\")
jktex <- c("\\begin{table}", "",
  "\\caption{One-storm-out jackknife of the augmented-observation $\\times$ 3-day $\\times$ hurricane-force interaction. \\label{tab:precision_jackknife_maria}}",
  "\\centering", "\\begin{threeparttable}", "\\begin{tabular}[t]{lrrr}", "\\toprule",
  "Dropped & Obs. & $\\delta_{3,\\text{HF}}$ (m/s) & Bootstrap $p$\\\\", "\\midrule", jkbody, "\\bottomrule",
  "\\end{tabular}", "\\begin{tablenotes}", "\\small",
  "\\item Triple-interaction coefficient $\\delta_{3,\\text{HF}}$ from the full interaction (Table \\ref{tab:precision_full}), re-estimated after dropping each storm. Significance from a restricted wild cluster bootstrap with Webb weights (B = 1999, clustered on storm).",
  "\\end{tablenotes}", "\\end{threeparttable}", "\\end{table}")
writeLines(jktex, here("output/precision_jackknife_maria.tex"))
add_adjust_box(here("output/precision_jackknife_maria.tex"))

# ------------------------------------------------------------------------------
# Stage 1 numbers: the headline delta and the baseline forecast SD it improves
# (HF + 3-day lead, AO-assimilated). Consumed by Stage 3.
# ------------------------------------------------------------------------------
reg3   <- d[lead_days == 3 & wind_obs > 33]
mod    <- feols(wind_fc_sd ~ augmented_obs | county + name, reg3, vcov = ~name)
delta  <- abs(coef(mod)["augmented_obs"])
dse    <- sqrt(diag(vcov(mod)))["augmented_obs"]
dp     <- 2 * (1 - pnorm(abs(coef(mod)["augmented_obs"]) / dse))
ao3    <- reg3[augmented_obs == 1]
writeLines(c(
  sprintf("delta                              = %.3f m/s", delta),
  sprintf("delta SE                           = %.3f",     dse),
  sprintf("delta p-value                      = %.4f",     dp),
  "",
  "-- HF + 3-day lead, AO-only (Stage 3 baseline) --",
  sprintf("  N                                = %d",       nrow(ao3)),
  sprintf("  sigma_bar                        = %.2f m/s", mean(ao3$wind_fc_sd, na.rm = TRUE)),
  sprintf("  sd(sigma)                        = %.2f m/s", sd(ao3$wind_fc_sd,   na.rm = TRUE))
), here("output/stage1_numbers.txt"))
cat("Wrote Stage 1 tables + output/stage1_numbers.txt\n")
