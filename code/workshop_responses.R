# ==============================================================================
# Appendix D - workshop response tabulation
# ------------------------------------------------------------------------------
# Reads the two Qualtrics exports (Part 1.csv, Part 2.csv), decodes value
# labels, and writes the appendix tables that summarise the workshop:
#   output/appD_sample.tex          - sample composition (role x years)
#   output/appD_likert_part1.tex    - Likert distribution: methodology + beta
#   output/appD_likert_part2.tex    - Likert distribution: valuation + CB
#   output/appD_quotes_part1.tex    - open-ended responses, methodology + beta
#   output/appD_quotes_part2.tex    - open-ended responses, valuation + CB
# ==============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, here, kableExtra)

p1_path <- here("data/workshop/Part 1.csv")
p2_path <- here("data/workshop/Part 2.csv")
out_dir <- here("output")
stopifnot(file.exists(p1_path), file.exists(p2_path))
dir.create(out_dir, showWarnings = FALSE)

# ---- value labels ------------------------------------------------------------
# Role codes follow the order used in the Appendix D narrative
# (forecasting/atm science; operational systems; economics/policy).
# Adjust if Qualtrics export uses a different coding.
role_lab <- c(
  "1" = "Forecasting / atmospheric science",
  "2" = "Operational observation systems",
  "3" = "Economics / policy analysis"
)
years_lab <- c(
  "1" = "$<$5 yrs",
  "2" = "5--10 yrs",
  "3" = "11--20 yrs",
  "4" = "$>$20 yrs"
)

# ---- I/O helpers -------------------------------------------------------------
read_qualtrics <- function(path) {
  # Row 1 = machine column codes, row 2 = question text, row 3 = ImportId JSON.
  hdr  <- read.csv(path, header = FALSE, nrows = 1, stringsAsFactors = FALSE)
  dat  <- read.csv(path, header = FALSE, skip = 3, stringsAsFactors = FALSE,
                   na.strings = c("", "NA"))
  names(dat) <- as.character(hdr[1, ])
  as_tibble(dat)
}

is_blank <- function(x) {
  x <- as.character(x)
  is.na(x) | grepl("^\\s*$", x) |
    tolower(trimws(x)) %in% c("n/a", "na", ".", "?")
}

latex_escape <- function(x) {
  x <- as.character(x)
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("([&%#$_{}])", "\\\\\\1", x)
  x <- gsub("\\^", "\\\\^{}", x)
  x <- gsub("~", "\\\\~{}", x)
  x <- gsub("“|”", "\"", x)
  x <- gsub("‘|’", "'", x)
  x <- gsub("–|—", "--", x)
  x <- gsub("…", "...", x)
  x
}

# ---- read both parts ---------------------------------------------------------
p1 <- read_qualtrics(p1_path)
p2 <- read_qualtrics(p2_path)

# ==============================================================================
# Sample composition
# ==============================================================================
mk_comp <- function(df, role_col, year_col, part_label) {
  df |>
    transmute(
      Role  = role_lab[as.character(.data[[role_col]])],
      Years = years_lab[as.character(.data[[year_col]])]
    ) |>
    count(Role, Years, name = "Respondents") |>
    mutate(Part = part_label, .before = 1)
}
p1_comp <- mk_comp(p1, "Q35", "Q34", "Part 1") |> arrange(Role, Years) |> select(-Part)
p2_comp <- mk_comp(p2, "Q23", "Q24", "Part 2") |> arrange(Role, Years) |> select(-Part)

comp_tex <- bind_rows(p1_comp, p2_comp) |>
  kbl(format = "latex", booktabs = TRUE, escape = FALSE,
      align = c("l", "r", "r"),
      caption = "Workshop respondents by professional role and experience.",
      label = "appD_sample") |>
  kable_styling(latex_options = c("hold_position")) |>
  pack_rows("Part 1 (Methodology / Cost curvature)",
            1, nrow(p1_comp), escape = FALSE) |>
  pack_rows("Part 2 (Valuation / Cost--benefit)",
            nrow(p1_comp) + 1, nrow(p1_comp) + nrow(p2_comp), escape = FALSE) |>
  footnote(
    general = paste(
      "Counts are completed survey responses (one row per respondent).",
      "Some respondents in Part 1 did not return for Part 2; the two parts are reported separately."
    ),
    general_title = "",
    threeparttable = TRUE, footnote_as_chunk = TRUE,
    escape = FALSE
  )
comp_str <- as.character(comp_tex)
writeLines(comp_str, file.path(out_dir, "appD_sample.tex"))

# ==============================================================================
# Likert distributions
# ==============================================================================
items_p1 <- tribble(
  ~col,    ~section,       ~short_label,
  "S1-1",  "Methodology",  "Data denial credibly isolates the contribution of augmented observations",
  "S1-2",  "Methodology",  "The 0.26 m/s ex-ante reduction is statistically and practically meaningful",
  "S1-3",  "Methodology",  "The 3-day lead time is appropriate for assessing the value of augmented observations",
  "S2-1",  "Cost curvature",      "Estimation strategy for the marginal cost is clear and sound",
  "S2-2",  "Cost curvature",      "Forecast errors significantly increase costs only at hurricane-force winds",
  "S2-3",  "Cost curvature",      "$\\hat{\\beta}$ is interpretable for NOAA's operational and informational purposes",
  "S2-4",  "Cost curvature",      "NOAA can feasibly replicate the analysis independently"
)
items_p2 <- tribble(
  ~col,    ~section,        ~short_label,
  "S3-1",  "Valuation",     "Translation from $\\Delta\\sigma^2$ to monetary value is conceptually sound",
  "S3-2",  "Valuation",     "The estimated \\$25.77M per county-storm is credible",
  "S3-3",  "Valuation",     "The projection of $\\sim$\\$3.2B in total retrospective value is reasonable",
  "CB1",   "Cost--benefit", "Operational cost assumptions are realistic",
  "CB2",   "Cost--benefit", "The 50:1 benefit-to-cost ratio is credible and policy-relevant",
  "CB3",   "Cost--benefit", "The framework provides sufficient evidence to inform investment decisions"
)

likert_dist <- function(df, items) {
  items |>
    rowwise() |>
    mutate(
      vals = list({
        v <- suppressWarnings(as.integer(df[[col]]))
        v[!is.na(v)]
      })
    ) |>
    mutate(
      N    = length(vals),
      `1`  = sum(vals == 1),
      `2`  = sum(vals == 2),
      `3`  = sum(vals == 3),
      `4`  = sum(vals == 4),
      `5`  = sum(vals == 5),
      Mean = if (N > 0) sprintf("%.2f", mean(vals)) else "",
      `\\% Agree` = if (N > 0) sprintf("%.0f", 100 * mean(vals >= 4)) else ""
    ) |>
    ungroup() |>
    select(section, short_label, N, `1`, `2`, `3`, `4`, `5`, Mean, `\\% Agree`)
}

write_likert <- function(df, items, caption_text, file_name, label) {
  dist <- likert_dist(df, items)
  # Section breaks via pack_rows; drop the Section column itself
  sec_runs <- rle(dist$section)
  sec_ends <- cumsum(sec_runs$lengths)
  sec_starts <- c(1L, head(sec_ends, -1) + 1L)
  tbl <- dist |>
    rename(Statement = short_label) |>
    select(-section) |>
    kbl(format = "latex", booktabs = TRUE, escape = FALSE,
        align = c("l", "c", "c", "c", "c", "c", "c", "c", "c"),
        caption = caption_text, label = label) |>
    kable_styling(latex_options = c("hold_position")) |>
    footnote(
      general = paste(
        "Likert scale: 1 = Strongly disagree, 2 = Disagree, 3 = Neutral,",
        "4 = Agree, 5 = Strongly agree.",
        "N is the count of non-missing responses.",
        "Agree pools responses of 4 and 5."
      ),
      general_title = "",
      threeparttable = TRUE, footnote_as_chunk = TRUE,
      escape = FALSE
    )
  for (i in seq_along(sec_runs$values)) {
    tbl <- pack_rows(tbl, sec_runs$values[i],
                      sec_starts[i], sec_ends[i],
                      escape = FALSE)
  }
  out_str <- as.character(tbl)
  writeLines(out_str, file.path(out_dir, file_name))
}

write_likert(
  p1, items_p1,
  paste("Responses: methodology and marginal cost.",
        "Response distribution and summary statistics."),
  "appD_likert_part1.tex", "appD_likert_part1"
)
write_likert(
  p2, items_p2,
  paste("Responses: valuation and cost--benefit.",
        "Response distribution and summary statistics."),
  "appD_likert_part2.tex", "appD_likert_part2"
)

# ==============================================================================
# Open-ended responses
# ==============================================================================
quotes_p1 <- tribble(
  ~col,    ~section,       ~prompt,
  "S1-4",  "Methodology",  "On whether the 0.26 m/s reduction is operationally meaningful",
  "S1-5",  "Methodology",  "Additional dimensions of forecast quality that should be included",
  "S1-6",  "Methodology",  "Methodological limitations linking augmented observations to forecast precision",
  "S2-5",  "Cost curvature", "Additional robustness checks or alternative specifications",
  "S2-6",  "Cost curvature", "Conceptual or empirical gaps in how forecast error is monetised"
)
quotes_p2 <- tribble(
  ~col,    ~section,        ~prompt,
  "S3-4",  "Valuation",     "Whether the valuation captures the full social value of forecasts",
  "S3-5",  "Valuation",     "Sensitivity of monetary projections to key assumptions",
  "S3-6",  "Valuation",     "Categories of benefits or costs that may not be fully incorporated",
  "CB4",   "Cost--benefit", "Whether the benefit--cost ratio justifies fleet recapitalisation",
  "CB5",   "Cost--benefit", "Additional cost considerations to incorporate",
  "CB6",   "Cost--benefit", "Further analysis to increase confidence in the policy implication",
  "CS1",   "Overall",       "Stage of the analysis presenting the greatest vulnerability",
  "CS2",   "Overall",       "Single additional analysis that would most strengthen the paper",
  "CS3",   "Overall",       "Innovative extension that could substantially increase the policy impact"
)

build_quotes_long <- function(df, q_index) {
  q_index |>
    rowwise() |>
    mutate(responses = list({
      v <- df[[col]]
      v[!is_blank(v)]
    })) |>
    ungroup() |>
    unnest_longer(responses) |>
    filter(!is.na(responses)) |>
    mutate(Response = latex_escape(responses))
}

write_quotes <- function(df, q_index, caption_text, file_name, label) {
  long <- build_quotes_long(df, q_index)
  if (nrow(long) == 0L) {
    writeLines("% no non-empty responses", file.path(out_dir, file_name))
    return(invisible(NULL))
  }
  long_tbl <- long |>
    arrange(section, prompt) |>
    group_by(section, prompt) |>
    mutate(
      Section = if_else(row_number() == 1L, section, ""),
      Prompt  = if_else(row_number() == 1L, prompt,  "")
    ) |>
    ungroup() |>
    select(Section, Prompt, Response)
  tbl <- long_tbl |>
    kbl(format = "latex", booktabs = TRUE, longtable = TRUE, escape = FALSE,
        align = c("l", "p{4.2cm}", "p{8.2cm}"),
        caption = caption_text, label = label) |>
    kable_styling(latex_options = c("repeat_header"),
                  font_size = 8)
  writeLines(as.character(tbl), file.path(out_dir, file_name))
}

write_quotes(
  p1, quotes_p1,
  paste("Open-ended responses: methodology and marginal cost.",
        "Verbatim quotes; empty entries and pure `N/A' answers are omitted."),
  "appD_quotes_part1.tex", "appD_quotes_part1"
)
write_quotes(
  p2, quotes_p2,
  paste("Open-ended responses: valuation, cost--benefit, and overall.",
        "Verbatim quotes; empty entries and pure `N/A' answers are omitted."),
  "appD_quotes_part2.tex", "appD_quotes_part2"
)

cat("Wrote: output/appD_sample.tex,",
    "appD_likert_part1.tex, appD_likert_part2.tex,",
    "appD_quotes_part1.tex, appD_quotes_part2.tex\n")
