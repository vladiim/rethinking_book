dependencies <- c("rethinking", "dplyr")
lapply(dependencies, require, character.only = TRUE)

source("ch1_4.R")
source("ch5_gaussian_model.R")
