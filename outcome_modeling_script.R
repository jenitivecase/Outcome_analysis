#setup
options(scipen = 999)
options(stringsAsFactors = FALSE)

date <- format.Date(Sys.Date(), "%Y%m%d")

setwd("K:/AscendKC/Corp/R_and_D/1-USERS/Jennifer Brussow/Outcome Modeling/Outcome_analysis")

needed_packages <- c("mirt", "dplyr", "tidyr", "ggplot2")
load_packages <- function(x) {
  if (!(x %in% rownames(installed.packages()))) {
    install.packages(x)
  }
  suppressPackageStartupMessages(require(x, character.only = TRUE))
}
sapply(needed_packages, load_packages)

outcomes_data <- readRDS("item-level_RN-CP_outcome-scores.rds")

names(outcomes_data)[1] <- gsub("ï..", "", names(outcomes_data)[1])

outcome_code_key <- read.csv("Outcome_codes.csv")
outcome_type_key <- unique(outcome_code_key[, c("OutcomeTypeID", "OutcomeTypeName")])

outcome_type_codes <- unique(outcomes_data$OutcomeTypeID)
outcome_type_codes <- outcome_type_codes[which(!is.na(as.numeric(outcome_type_codes)))]


outcome_type_names <- outcome_type_key[which(outcome_type_key$OutcomeTypeID %in% outcome_type_codes), 
                                       "OutcomeTypeName"]

for(name in seq_along(outcome_type_names)){
  code <- outcome_type_codes[name]
  outcome <- outcome_type_names[name]
  
  data <- outcomes_data[which(outcomes_data$OutcomeTypeID == code), 
                c("BookletID", "qbtbQuestionID",  
                  "IsCorrect", "OutcomeDescription")]
  
  outcome_map <- unique(data[, c("qbtbQuestionID", "OutcomeDescription")])
  
  #eliminating extraneous columns before spreading data
  data <- data[,c("BookletID", "qbtbQuestionID",  
                  "IsCorrect")]
  
  #moving data into wide format for use with mirt
  data <- data %>%
    spread(qbtbQuestionID, IsCorrect)
  
  #identifying items with 0 variance in responses
  bad_items <- NULL
  
  for(j in 2:ncol(data)){
    if(sum(!is.na(unique(data[, j]))) < 2){
      bad_items <- c(bad_items, colnames(data[j]))
    }
  }
  
  #removing those items from the item map
  outcome_map <- outcome_map[which(!(outcome_map$qbtbQuestionID %in% bad_items)),]
  
  #preparing lists of the items loading onto each factor
  factors <- unique(outcome_map$OutcomeDescription)
  factor_items <- vector("list", length(factors))
  
  for(i in 1:length(factors)){
    factor_items[[i]] <- c(outcome_map[which(outcome_map[, "OutcomeDescription"] == factors[i]), "qbtbQuestionID"])
  }
  
  #formatting lists of items loading onto each factor for mirt model syntax
  factor_syn <- vector("list", length(factors))
  for(i in 1:length(factors)){
    factor_syn[[i]] <- paste0("F",i," = ", paste(factor_items[[i]], collapse = ", "))
  }
  
  syntax_fname <- paste0(outcome, "_mirt_model.txt")
  
  #output the resulting string
  sink(file = syntax_fname)
  cat(paste0(unlist(factor_syn), sep = "\n"))
  sink()
  
  model_syn <- readLines(syntax_fname)
  
  #run analysis
  model <- mirt(data=data[, as.character(unlist(factor_items))], 
                model=model_syn, itemtype = "2PL", method = "QMCEM",
                technical = list(removeEmptyRows = TRUE, NCYCLES = 2000))
  
}
