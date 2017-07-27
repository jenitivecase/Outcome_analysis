#### SETUP ####

source("K:/AscendKC/Corp/R_and_D/1-USERS/Jennifer Brussow/options.R")

needed_packages <- c("mirt", "ggplot2")
sapply(needed_packages, load_packages)

# 2016 CP info
CP_year <- "2016"
CP_forms <- "A-C"
daterange1_1 <- "11/15/2016"
daterange1_2 <- "12/31/2016"
daterange2_1 <- "04/01/2017"
daterange2_2 <- "05/15/2017"
results_folder <- "./Results_20170725/"

# # 2013 CP info
# CP_year <- "2013"
# CP_forms <- "A-F"
# daterange1_1 <- "11/15/2015"
# daterange1_2 <- "12/31/2015"
# daterange2_1 <- "04/01/2016"
# daterange2_2 <- "05/15/2016"
# results_folder <- "./Results_20170630/"


model_fnames <- grep("result", list.files(results_folder), value = TRUE)
factor_items_fnames <- grep("items", list.files(results_folder), value = TRUE)

if(length(model_fnames) != length(factor_items_fnames)){
  model_fnames_subjs <- simplify2array(strsplit(model_fnames, "_"))[1,]
  factor_items_fnames_subjs <- simplify2array(strsplit(factor_items_fnames, "_"))[1,]
  
  if(length(model_fnames) <= length(factor_items_fnames)){
    factor_items_fnames <- factor_items_fnames[which(factor_items_fnames_subjs %in% model_fnames_subjs)]
  } else if(length(model_fnames) >= length(factor_items_fnames)){
    model_fnames <- model_fnames[which(model_fnames_subjs %in% factor_items_fnames_subjs)]
  }
  
  rm(model_fnames_subjs, factor_items_fnames_subjs)
}

if(CP_year == 2013){
  model_fnames <- grep("BSN", model_fnames, value = TRUE, invert = TRUE)
  model_fnames <- grep("NLN", model_fnames, value = TRUE, invert = TRUE)
  
  factor_items_fnames <- grep("BSN", factor_items_fnames, value = TRUE, invert = TRUE)
  factor_items_fnames <- grep("NLN", factor_items_fnames, value = TRUE, invert = TRUE)
}

outcome_code_key <- read.csv("Outcome_codes.csv")
item_content <- read.xlsx("item_content_from_QBTB-db.xlsx")

item_content[which(item_content$IsCorrect == 1), "responseopt"] <- 
  paste0(item_content[which(item_content$IsCorrect == 1), "responseopt"], " (c)")

item_content <- as.data.frame(apply(item_content, 2, FUN = iconv, from = "", to = "ASCII", sub=''))


worst_items <- vector("list", length(model_fnames))
best_items <- vector("list", length(model_fnames))


#### GET THE VALUES ####
for(file in 1:length(model_fnames)){
  model_fname <- model_fnames[file]
  factor_items_fname <- factor_items_fnames[file]
  results_folder <- results_folder
  
  #load data here
  model <- readRDS(paste0(results_folder, model_fname))
  outcome_name <- unlist(strsplit(model_fname, "_"))[1]
  factor_items <- readRDS(paste0(results_folder, factor_items_fname))
  
  factor_load <- summary(model)$rotF
  
  n_factors <- ncol(factor_load)
  n_items <- nrow(factor_load)
  
  item_f_load <- vector("list", length = length(factor_items))
  
  for(i in 1:length(item_f_load)){
    item_f_load[[i]] <- as.data.frame(factor_load[which(rownames(factor_load) %in% factor_items[[i]]),])
  }
  
  #get itemids
  worst_items[[file]] <- data.frame(Outcome = rep(outcome_name, 10),
                            Factor = rep(NA, 10),
                            Item = rep(NA, 10),
                            Loading = rep(NA, 10))
  
  best_items[[file]] <- data.frame(Outcome = rep(outcome_name, 10),
                            Factor = rep(NA, 10),
                            Item = rep(NA, 10),
                            Loading = rep(NA, 10))
  for(i in 1:n_factors){
    factor_code <- colnames(factor_load)[i]
    factor_code_sub <- gsub("F", "", factor_code)
    factor_name <- unique(outcome_code_key[which(outcome_code_key$OutcomeID_map == factor_code_sub),
                                           "OutcomeDescription_map"])
    

    if(n_factors == 1){
      load_data <- as.data.frame(item_f_load)
    } else {
      load_data <- item_f_load[[i]]
      load_data <- load_data[order(load_data[, factor_code], na.last = NA),]
    }
    
    if(n_factors == 1){
      worst_items[[file]]$Item <- rownames(load_data)[1:10]
      worst_items[[file]]$Loading <- round(load_data[1:10, 1], 3)
      best_items[[file]]$Item <- rownames(load_data)[(NROW(load_data)-9):NROW(load_data)]
      best_items[[file]]$Loading <- round(load_data[(NROW(load_data)-9):NROW(load_data), 1], 3)
    } else if(nrow(load_data) >= 10){
      worst_items[[file]]$Item <- rownames(load_data[1:10,])
      worst_items[[file]]$Loading <- round(load_data[1:10, factor_code], 3)
      best_items[[file]]$Item <- rownames(load_data[(nrow(load_data)-9):nrow(load_data),])
      best_items[[file]]$Loading <- round(load_data[(nrow(load_data)-9):nrow(load_data), factor_code], 3)
    } 
    
  }
  
}