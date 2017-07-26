source("K:/AscendKC/Corp/R_and_D/1-USERS/Jennifer Brussow/options.R")

#### GET ITEM LIST FOR DB ####

results_folder <- "./Results_20170725/"

factor_items_fnames <- grep("items", list.files(results_folder), value = TRUE)

item_lists <- vector("list", length(factor_items_fnames))

for(i in 1:length(item_lists)){
  item_lists[[i]] <- c(unlist(readRDS(paste0(results_folder, factor_items_fnames[i]))))
}

item_list <- unique(unlist(item_lists))
item_list <- paste(item_list, collapse = ",")

sink("2016CP_item_list.txt")
cat(item_list)
sink()

####
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
