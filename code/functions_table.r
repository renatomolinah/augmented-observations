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
