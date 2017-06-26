#setup
needed_packages <- c("mirt", "dplyr", "tidyr", "ggplot2")
load_packages <- function(x) {
  if (!(x %in% rownames(installed.packages()))) {
    install.packages(x)
  }
  suppressPackageStartupMessages(require(x, character.only = TRUE))
}
sapply(needed_packages, load_packages)


options(scipen = 999)
options(stringsAsFactors = FALSE)

date <- format.Date(Sys.Date(), "%Y%m%d")

# outcomes_data <- read.table("item-level_RN-CP_outcome-scores.txt",
#                             sep = ",", header = TRUE, as.is = TRUE,
#                             strip.white = TRUE, fill = TRUE,
#                             blank.lines.skip = TRUE)
# 
# saveRDS(outcomes_data, "item-level_RN-CP_outcome-scores.rds")

outcomes_data <- readRDS("item-level_RN-CP_outcome-scores.rds")

names(outcomes_data)[1] <- gsub("ï..", "", names(outcomes_data)[1])

outcome_code_key <- read.csv("Outcome_codes.csv")
outcome_type_key <- unique(outcome_code_key[, c("OutcomeTypeID", "OutcomeTypeName")])

outcome_type_codes <- unique(outcomes_data$OutcomeTypeID)
outcome_type_codes <- outcome_type_codes[which(!is.na(as.numeric(outcome_type_codes)))]


outcome_type_names <- outcome_type_key[which(outcome_type_key$OutcomeTypeID %in% outcome_type_codes), 
                                       "OutcomeTypeName"]
