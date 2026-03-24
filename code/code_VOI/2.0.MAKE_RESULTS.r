# Directories are created by Makefile as dependencies
files_to_run <- c(
  "code/2.00.prepare_packages_results.r",
  "code/2.01.main_results.r",
  "code/2.02.app_fema.r",
  "code/2.03.app_ex_post.r",
  "code/2.04.app_voi.r",
  "code/2.05.app_misc.r",
  "code/2.06.text_numbers.r"
)

invisible(lapply(files_to_run, source))
