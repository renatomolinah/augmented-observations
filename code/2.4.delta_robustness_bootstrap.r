# 2.4 Stage-2 robustness for the AO x 3-day x hurricane-force triple interaction:
# one-storm-out jackknife and Maria in/out, wild cluster bootstrap p (Webb
# weights, B=1999, storm cluster); mirrors 2.3. Writes precision_jackknife_maria
# and precision_maria.
if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, fixest, here, parallel)
source(here("code/functions_table.r"))
setFixest_nthreads(1)
B <- 1999; N_CORES <- 6
webb <- c(-sqrt(1.5), -1, -sqrt(.5), sqrt(.5), 1, sqrt(1.5))
RNGkind("L'Ecuyer-CMRG")

d <- fread(here("data/clean_denial/forecast_denial_data_with_lead_times.csv"))
d <- d[!is.na(augmented_obs) & lead_days %in% c(1, 2, 3)]
d[, `:=`(hf = as.integer(wind_obs > 33), l2 = as.integer(lead_days == 2), l3 = as.integer(lead_days == 3))]
d[, `:=`(AO = augmented_obs, AO_l2 = augmented_obs*l2, AO_l3 = augmented_obs*l3, AO_hf = augmented_obs*hf,
         AO_l2_hf = augmented_obs*l2*hf, AO_l3_hf = augmented_obs*l3*hf, l2_hf = l2*hf, l3_hf = l3*hf)]

REGS <- c("AO","l2","l3","hf","AO_l2","AO_l3","AO_hf","l2_hf","l3_hf","AO_l2_hf","AO_l3_hf")
TERM <- "AO_l3_hf"
mkf  <- function(lhs, regs) as.formula(paste(lhs, "~", paste(regs, collapse = "+"), "| county + name"))

wcb <- function(dat, seed) {
  storms <- unique(dat$name); G_ <- length(storms); idx_ <- match(dat$name, storms)
  set.seed(seed)
  m  <- feols(mkf("wind_fc_sd", REGS), dat, cluster = ~name)
  t_obs <- coef(m)[[TERM]] / se(m)[[TERM]]
  mr <- feols(mkf("wind_fc_sd", setdiff(REGS, TERM)), dat, cluster = ~name)
  fit_r <- predict(mr); res_r <- resid(mr); dt <- copy(dat); ts <- numeric(B)
  for (b in seq_len(B)) {
    w <- webb[sample.int(6, G_, replace = TRUE)][idx_]
    dt[, ystar := fit_r + res_r * w]
    mb <- feols(mkf("ystar", REGS), dt, cluster = ~name)
    ts[b] <- coef(mb)[[TERM]] / se(mb)[[TERM]]
  }
  list(est = coef(m)[[TERM]], p = mean(abs(ts) >= abs(t_obs)), n = nobs(m))
}
seed_of <- function(s) 2024L + sum(utf8ToInt(s))
star    <- function(p) ifelse(p < 0.01, "***", ifelse(p < 0.05, "**", ifelse(p < 0.1, "*", "")))

storms <- sort(unique(d$name))
jk <- rbindlist(mclapply(c("(none)", storms), function(s) {
  sub <- if (s == "(none)") d else d[name != s]
  r <- wcb(sub, seed_of(paste0("jk_", s)))
  data.table(dropped = s, n = r$n, est = r$est, p = r$p)
}, mc.cores = N_CORES, mc.set.seed = TRUE))
maria <- rbindlist(list(
  { r <- wcb(d,                seed_of("m_in"));  data.table(sample = "Including Maria (main sample)", n = r$n, est = r$est, p = r$p) },
  { r <- wcb(d[name != "maria"], seed_of("m_out")); data.table(sample = "Excluding Maria (robustness)",  n = r$n, est = r$est, p = r$p) }
))
cat("== triple-interaction jackknife ==\n"); print(jk)
cat("== triple-interaction, Maria in/out ==\n"); print(maria)

tc <- function(s) ifelse(s == "(none)", "(None)", paste0(toupper(substr(s,1,1)), substr(s,2,nchar(s))))

jkbody <- paste0(tc(jk$dropped), " & ", format(jk$n, big.mark = ","), " & ",
                 sprintf("%.3f%s", jk$est, star(jk$p)), " & ", sprintf("%.3f", jk$p), "\\\\")
jktex <- c("\\begin{table}", "",
  "\\caption{One-storm-out jackknife of the augmented-observation $\\times$ 3-day $\\times$ hurricane-force interaction. \\label{tab:precision_jackknife_maria}}",
  "\\centering", "\\begin{threeparttable}", "\\begin{tabular}[t]{lrrr}", "\\toprule",
  "Dropped & Obs. & $\\delta_{3,\\text{HF}}$ (m/s) & Bootstrap $p$\\\\", "\\midrule", jkbody, "\\bottomrule",
  "\\end{tabular}", "\\begin{tablenotes}", "\\small",
  "\\item Each row re-estimates the full lead $\\times$ intensity interaction (Table \\ref{tab:precision_full}) on the 2017--2020 denial panel after dropping the indicated storm, and reports the triple-interaction coefficient $\\delta_{3,\\text{HF}}$, the additional precision gain in the 3-day hurricane-force cell. Significance is from a restricted wild cluster bootstrap with Webb weights (B = 1999, clustered on storm). The baseline row drops no storm.",
  "\\end{tablenotes}", "\\end{threeparttable}", "\\end{table}")
writeLines(jktex, here("augmented_observations/figures and tables/precision_jackknife_maria.tex"))
add_adjust_box(here("augmented_observations/figures and tables/precision_jackknife_maria.tex"))

mbody <- paste0(maria$sample, " & ", format(maria$n, big.mark = ","), " & ",
                sprintf("%.3f%s", maria$est, star(maria$p)), " & ", sprintf("%.3f", maria$p), "\\\\")
mtex <- c("\\begin{table}", "",
  "\\caption{Sensitivity of the triple interaction to Hurricane Maria. \\label{tab:precision_maria}}",
  "\\centering", "\\begin{adjustbox}{max width = \\textwidth}", "\\begin{threeparttable}",
  "\\begin{tabular}[t]{lrrr}", "\\toprule",
  "Sample & Obs. & $\\delta_{3,\\text{HF}}$ (m/s) & Bootstrap $p$\\\\", "\\midrule", mbody, "\\bottomrule",
  "\\end{tabular}", "\\begin{tablenotes}", "\\small",
  "\\item The triple-interaction coefficient $\\delta_{3,\\text{HF}}$ from the full lead $\\times$ intensity interaction estimated on all ten 2017--2020 landfalls versus the six continental landfalls (Maria dropped). Significance is from a restricted wild cluster bootstrap with Webb weights (B = 1999, clustered on storm).",
  "\\end{tablenotes}", "\\end{threeparttable}", "\\end{adjustbox}", "\\end{table}")
writeLines(mtex, here("augmented_observations/figures and tables/precision_maria.tex"))
cat("\nWrote precision_jackknife_maria.tex and precision_maria.tex\n")
