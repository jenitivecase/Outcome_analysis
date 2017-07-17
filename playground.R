needed_packages <- c("mirt", "dplyr", "tidyr", "ggplot2")
load_packages <- function(x) {
  if (!(x %in% rownames(installed.packages()))) {
    install.packages(x)
  }
  library(x, character.only = TRUE)
}

item_outcome_info <- read.table("item_outcome_info.txt",
                       sep = ",", header = TRUE, as.is = TRUE,
                       strip.white = TRUE, fill = TRUE,
                       blank.lines.skip = TRUE)

NLN_data <- read.table("C:/Users/jennifer.brussow/Documents/test_NLN_data.txt",
                            sep = ",", header = TRUE, as.is = TRUE,
                            strip.white = TRUE, fill = TRUE,
                            blank.lines.skip = TRUE)

names(NLN_data)[1] <- gsub("ï..", "", names(NLN_data)[1])

NLN_data <- NLN_data[,c("BookletID", "qbtbQuestionID",  
                "IsCorrect")]

data_test <- NLN_data %>%
  spread(qbtbQuestionID, IsCorrect)


NLN_items <- filter(item_outcome_info, OutcomeTypeName == "NLN Competency")
length(unique(NLN_items$ï..qbtbQuestionID))
table(NLN_items$OutcomeDescription)
