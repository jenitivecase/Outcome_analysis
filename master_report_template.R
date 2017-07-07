source("K:/AscendKC/Corp/R_and_D/1-USERS/Jennifer Brussow/options.R")

library(rmarkdown)

results_folder <- "./Results_20170630/"
model_fnames <- grep("result", list.files(results_folder), value = TRUE)
factor_items_fnames <- grep("items", list.files(results_folder), value = TRUE)

