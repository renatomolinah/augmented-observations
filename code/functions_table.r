# Read a numeric value from a "label = value [unit]" line in a stage-number file.
# The key must select exactly one line: a partial key that also matched a
# neighbouring row would otherwise return that row's number without a warning.
read_num <- function(file, key) {
  hits <- grep(key, readLines(here(file)), value = TRUE)
  if (length(hits) != 1) {
    stop(sprintf("read_num('%s', '%s') matched %d lines, expected 1%s",
                 file, key, length(hits),
                 if (length(hits)) paste0(":\n  ", paste(hits, collapse = "\n  ")) else ""))
  }
  rhs <- sub(".*=", "", hits)
  as.numeric(regmatches(rhs, regexpr("-?[0-9]+(\\.[0-9]+)?", rhs)))
}

# Wrap a generated threeparttable in an adjustbox so wide tables fit the text width.
add_adjust_box <- function(file,
                           line_before = "\\begin{adjustbox}{max width = \\textwidth}",
                           line_after  = "\\end{adjustbox}",
                           before = "\\begin{threeparttable}",
                           after  = "\\end{threeparttable}") {
  lines       <- readLines(file)
  after_line  <- grep(after,  lines, fixed = TRUE)
  before_line <- grep(before, lines, fixed = TRUE)
  lines <- c(lines[1:(before_line - 1)], line_before,
             lines[before_line:after_line], line_after,
             lines[(after_line + 1):length(lines)])
  writeLines(lines, file)
}
