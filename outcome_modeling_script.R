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

outcome_type_codes <- outcome_type_key[which(outcome_type_key$OutcomeTypeID %in% 
                                               unique(outcomes_data$OutcomeTypeID)), "OutcomeTypeID"]

outcome_type_names <- outcome_type_key[which(outcome_type_key$OutcomeTypeID %in% outcome_type_codes), 
                                       "OutcomeTypeName"]

for(name in 1:length(outcome_type_names)){
  code <- outcome_type_codes[name]
  outcome <- outcome_type_names[name]
  
  subscore_codes <- outcome_code_key[which(outcome_code_key$OutcomeTypeID == code), 
                                     c("OutcomeID_orig", "OutcomeID_map")]
  subscore_names <- outcome_code_key[which(outcome_code_key$OutcomeTypeID == code), 
                                     c("OutcomeDescription_orig", "OutcomeDescription_map")]
  
  data <- outcomes_data[which(outcomes_data$OutcomeTypeID == code), 
                c("BookletID", "qbtbQuestionID",  
                  "IsCorrect", "OutcomeID", "OutcomeDescription")]
  
  outcome_map <- unique(data[, c("qbtbQuestionID", "OutcomeID")])
  
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
  factors <- unique(subscore_codes$OutcomeID_map)
  factor_items <- vector("list", length(factors))
  
  for(i in 1:length(factors)){
    f_subscore_codes <- c(subscore_codes[which(subscore_codes$OutcomeID_map == factors[i]), 
                                         "OutcomeID_orig"])
    
    factor_items[[i]] <- c(outcome_map[which(outcome_map[, "OutcomeID"] %in% f_subscore_codes), 
                                       "qbtbQuestionID"])
  }
  
  saveRDS(factor_items, paste0(outcome, "_factor-items_", date, ".rds"))
  
  #formatting lists of items loading onto each factor for mirt model syntax
  factor_syn <- vector("list", length(factors))
  for(i in 1:length(factors)){
    factor_syn[[i]] <- paste0("F", factors[i], " = ", paste(factor_items[[i]], collapse = ", "))
  }
  
  syntax_fname <- paste0(outcome, "_mirt_model.txt")
  
  #output the resulting string
  model_syn <- NULL
  for(i in 1:length(factor_syn)){
    model_syn <- paste0(model_syn, paste0(as.character(factor_syn[[i]]), "\n"))
  }
  
  # sink(syntax_fname)
  # print(model_syn)
  # sink()
  
  #run analysis
  model <- mirt(data=data[, as.character(unlist(factor_items))], 
                model=model_syn, itemtype = "2PL", method = "QMCEM",
                technical = list(removeEmptyRows = TRUE, NCYCLES = 5000))
  
  saveRDS(model, paste0(outcome, "_analysis_result_", date, ".rds"))
}
