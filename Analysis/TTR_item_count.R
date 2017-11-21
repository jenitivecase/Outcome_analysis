#setup
source("K:/AscendKC/Corp/R_and_D/1-USERS/Jennifer Brussow/options.R")

setwd("K:/AscendKC/Corp/R_and_D/1-USERS/Jennifer Brussow/Outcome Modeling/Outcome_analysis")

needed_packages <- c("mirt", "dplyr", "tidyr", "ggplot2")
load_packages <- function(x) {
  if (!(x %in% rownames(installed.packages()))) {
    install.packages(x)
  }
  suppressPackageStartupMessages(require(x, character.only = TRUE))
}
sapply(needed_packages, load_packages)

data <- readRDS("item-level_RN-2016CP_TTR-scores.rds")
data$TTR_Categ <- as.character(data$TTR_Categ)

test <- unique(data[, c("TTR_Categ", "qbtbQuestionID")])

item_count <- data.frame(table(test$TTR_Categ))
table(item_count$Freq)
