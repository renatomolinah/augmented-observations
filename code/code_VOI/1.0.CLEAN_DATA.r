# Directories are created by Makefile as dependencies
files_to_run = list.files(path = "code", pattern = "^[1]", full.names = TRUE)
lapply(files_to_run[-1], source)
