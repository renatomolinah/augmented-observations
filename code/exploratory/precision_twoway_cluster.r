# ==============================================================================
# Stage 2 robustness - precision regression with TWO-WAY clustering (storm + county)
# Reproduces the interaction grid (tab:interaction-grid) but reports two-way
# cluster-robust (Cameron-Gelbach-Miller) p-values on storm and county, instead
# of the storm-only wild cluster bootstrap. Point estimates are identical; only
# the bracketed p-values change. The t small-sample correction uses the smaller
# cluster count (10 storms), so the few-storm conservatism is retained while
# within-county correlation is also accounted for.
# Inputs:  data/clean_denial/forecast_denial_data_with_lead_times.csv
# Outputs: output/paper_interaction_grid_twoway.tex;
#          output/interaction_twoway_pvalues.csv;
#          output/stage2_twoway_cluster_compare.txt
# Run:     Rscript --vanilla code/exploratory/precision_twoway_cluster.r
# ==============================================================================
if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, fixest, here, modelsummary)
options(modelsummary_format_numeric_latex = "plain")
source(here("code/functions_table.r"))
setFixest_nthreads(1)

STARS <- c("*" = 0.1, "**" = 0.05, "***" = 0.01)
STAT  <- "({std.error})"            # round parentheses = standard error (stars from p-value)
CL    <- ~name + county             # <-- two-way clustering: storm and county

d <- fread(here("data/clean_denial/forecast_denial_data_with_lead_times.csv"))
d <- d[!is.na(augmented_obs) & lead_days %in% c(1, 2, 3)]
d[, `:=`(hf = as.integer(wind_obs > 33),
         l2 = as.integer(lead_days == 2), l3 = as.integer(lead_days == 3))]
d[, `:=`(AO = augmented_obs, AO_l2 = augmented_obs * l2, AO_l3 = augmented_obs * l3,
         AO_hf = augmented_obs * hf, AO_l2_hf = augmented_obs * l2 * hf,
         AO_l3_hf = augmented_obs * l3 * hf, l2_hf = l2 * hf, l3_hf = l3 * hf)]

full_triple <- c("AO", "l2", "l3", "hf", "AO_l2", "AO_l3", "AO_hf",
                 "l2_hf", "l3_hf", "AO_l2_hf", "AO_l3_hf")

mkform <- function(lhs, regs, fe) {
  rhs <- if (length(regs) == 0) "1" else paste(regs, collapse = "+")
  as.formula(paste(lhs, "~", rhs, "|", fe))
}
# Analytical two-way cluster-robust estimates + p-values for one column.
fit_col <- function(regs, fe) {
  m  <- feols(mkform("wind_fc_sd", regs, fe), d, cluster = CL)
  data.table(term = regs, estimate = unname(coef(m)[regs]),
             std.error = unname(se(m)[regs]), p.value = unname(pvalue(m)[regs]),
             nobs = nobs(m), r2 = unname(r2(m, "r2")), ar2 = unname(r2(m, "ar2")))
}
run_family <- function(coldefs) rbindlist(lapply(names(coldefs), function(cc) {
  cbind(col = cc, fit_col(coldefs[[cc]]$regs, coldefs[[cc]]$fe))
}))
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
  list(raw = "r.squared",     clean = "R2",       fmt = 3))
twoway_note <- "Round parentheses report two-way cluster-robust standard errors on storm and county, with the small-sample $t$ correction based on the smaller cluster count (ten storms)."

cols <- list(
  "(1)" = list(regs = c("AO"),                                           fe = "county"),
  "(2)" = list(regs = c("AO"),                                           fe = "county+name"),
  "(3)" = list(regs = c("AO", "wind_obs"),                               fe = "county+name"),
  "(4)" = list(regs = c("AO", "l2", "l3", "AO_l2", "AO_l3", "wind_obs"), fe = "county+name"),
  "(5)" = list(regs = full_triple,                                       fe = "county+name"),
  "(6)" = list(regs = c(full_triple, "wind_obs"),                        fe = "county+name"))
cat("[Stage 2 / two-way cluster] interaction grid\n")
res <- run_family(cols)
fwrite(res, here("output/interaction_twoway_pvalues.csv"))
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
  title = "Precision Gained due to Augmented Observations, two-way clustered inference. \\label{tab:interaction-grid-twoway}",
  notes = list(paste0("HF denotes hurricane-force realizations, wind $\\\\geq$ 33 m/s; the reference category is 1-day-lead, sub-hurricane-force forecasts. ``Lead-time main effects'' are the 2- and 3-day lead indicators and ``Intensity main effect'' the hurricane-force indicator. ", twoway_note)),
  output = here("output/paper_interaction_grid_twoway.tex"))
add_adjust_box(here("output/paper_interaction_grid_twoway.tex"))

cat("Wrote two-way clustered Stage 2 table + p-value CSV\n")
