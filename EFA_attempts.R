#setup
options(scipen = 999)
options(stringsAsFactors = FALSE)

date <- format.Date(Sys.Date(), "%Y%m%d")

setwd("K:/AscendKC/Corp/R_and_D/1-USERS/Jennifer Brussow/Outcome Modeling/Outcome_analysis")

needed_packages <- c("mirt", "dplyr", "tidyr", "ggplot2", "nFactors", "psych", "gridExtra")
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


#restrict data to just one form (form with the most responses) to avoid missing data issues
outcomes_data <- outcomes_data[which(outcomes_data$AssessmentID == 133086),]

gc()

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
  
  gc()
  
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
  
  #removing bad items from dataset
  data <- data[, !names(data) %in% bad_items]
  
  #removing those items from the item map
  outcome_map <- outcome_map[which(!(outcome_map$qbtbQuestionID %in% bad_items)),]
  
  #preparing lists of the items loading onto each factor
  n_factors <- length(unique(subscore_codes$OutcomeID_map))
  
  pdf(paste0(getwd(), "/EFA results/", outcome, "_EFA.pdf"))
  
  
  #run analysis
  # #OLS solution
  # model_OLS <- psych::fa(r = data[, 2:ncol(data)], nfactors = n_factors, 
  #                        missing = TRUE, impute = "mean")
  # 
  # #IRT solution
  # model_IRT_FA <- psych::irt.fa(r = data[, 2:ncol(data)], nfactors = n_factors, 
  #                        missing = TRUE, impute = "mean")
  
  #MLE solution
  n_explore <- (n_factors + 3)
  model_ML_fa <- vector("list", n_explore)
  BICs <- c(rep(NA, n_explore))
  
  for(i in 1:n_explore){
    model_ML_fa[[i]] <- psych::fa(r = data[, 2:ncol(data)], nfactors = i, 
                             missing = TRUE, impute = "mean",
                             fm = "mle", cor = "tet")
    
    BICs[i] <- model_ML_fa[[i]]$BIC
  }
  
  # print.psych(model_ML_fa[[n_factors]])
  # model_ML_fa[[n_factors]]$values
  loadings <- as.data.frame(unclass(model_ML_fa[[n_factors]]$loadings))

  # factor_loadings <- data.frame(matrix(NA, ncol = 4, nrow = n_factors, dimnames = list(
  #   c(colnames(loadings)), c("Mean", "SD", "Min", "Max"))))
  # factor_loadings$Mean <- as.matrix(round(colMeans(loadings), 3))
  # factor_loadings$SD <- as.matrix(round(apply(loadings, 2, sd), 3)) 
  # factor_loadings$Min <- as.matrix(round(apply(loadings, 2, min), 3)) 
  # factor_loadings$Max <- as.matrix(round(apply(loadings, 2, max), 3)) 
  # 
  # gridExtra::grid.table(factor_loadings)
  
  loadings_long <- gather(loadings, key = "Factor")
  load_plot <- ggplot(loadings_long, aes(x = value, fill = Factor)) + 
    geom_histogram(binwidth = 0.01, alpha = 0.7, position = "stack") + 
    labs(x = "Loading", y = "Count", 
         title = paste0("Factor Loadings for ", n_factors, "-factor Solution"),
         caption = paste0("Mean = ", round(mean(loadings_long$value), 3), 
                          ", SD = ", round(sd(loadings_long$value), 3)))
  
  print(load_plot)
  
  #plot BICs
  x <- c(1:n_explore)
  plot(x, BICs, col=ifelse(x==n_factors, "blue", "black"),
       pch=ifelse(x==n_factors, 19, 1),
       xlab = "N factors", ylab = "BIC", 
       main = paste0("EFA results: ", outcome))
  
  #Scree plotting
  ev <- eigen(cor(data[, 2:ncol(data)], use = "pairwise.complete.obs")) # get eigenvalues
  ap <- parallel(subject=nrow(data[, 2:ncol(data)]),var=ncol(data[, 2:ncol(data)]),
                 rep=100,cent=.05)
  nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
  plotnScree(nS)
  
  
  dev.off()
  
  gc()
}
