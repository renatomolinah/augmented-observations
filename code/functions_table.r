add_adjust_box <- function(file, line_before = "\\begin{adjustbox}{max width = \\textwidth}", line_after = "\\end{adjustbox}", before = "\\begin{threeparttable}", after = "\\end{threeparttable}") {
    # read in the file
    lines <- readLines(file)

    # find the line number of the line beginning with "\begin{threeparttable}"
    after_line <- grep(after, lines, fixed = TRUE)

    # find the line number of the line beginning with "\end{threeparttable}"
    before_line <- grep(before, lines, fixed = TRUE)

    # add the line after the line beginning with "\begin{threeparttable}" and before the line beginning with "\end{threeparttable}"
    lines <- c(lines[1:(before_line-1)], line_before, lines[(before_line):(after_line)], line_after, lines[(after_line+1):length(lines)])

    # write the file
    writeLines(lines, file)
}

load_stack <- function() {
  df = readRDS("data/clean/final_dataset.rds") |>
  drop_na(voi, lat, lon, total_cost_pc, total_cost_per_gdp) |>
  mutate(obs_id = row_number())

df_stack =
  bind_rows(
    df |> mutate(samp2 = 0),
    df |> mutate(samp2 = 1)
  ) |> 
  mutate(
    voi = voi * samp2,
    squared_error = squared_error * (1 - samp2),
    voi_precip = voi_precip * samp2,
    squared_error_precip = squared_error_precip * (1 - samp2)
  )  |> 
  group_by(county, samp2) |>
  mutate(county_id = cur_group_id())
  
  return(df_stack)
}

load_voi <- function() {
  df = readRDS("data/clean/final_dataset.rds") |>
    drop_na(voi, lat, lon, total_cost_pc, total_cost_per_gdp) |>
    mutate(obs_id = row_number())
  return(df)
}

load_pdd_stack <- function() {
  
df <- load_pdd()

df_stack =
  bind_rows(
    df |> mutate(samp2 = 0),
    df |> mutate(samp2 = 1)
  ) |> 
  mutate(
    voi = voi * samp2,
    squared_error = squared_error * (1 - samp2),
    voi_precip = voi_precip * samp2,
    squared_error_precip = squared_error_precip * (1 - samp2)
  )  |> 
  group_by(county, samp2) |>
  mutate(county_id = cur_group_id())

  return(df_stack)
  
}