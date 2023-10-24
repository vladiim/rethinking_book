dependencies <- c("rethinking", "dplyr")
lapply(dependencies, require, character.only = TRUE)

files <- dir(".")
lapply(files, source)
print("Loaded")
