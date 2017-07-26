source("K:/AscendKC/Corp/R_and_D/1-USERS/Jennifer Brussow/options.R")

library(rmarkdown)

out_folder <- paste0("Outcome_FLoad_Reports_", date)

CP_year <- "2016"
CP_forms <- "A-C"
daterange1_1 <- "11/15/2016"
daterange1_2 <- "12/31/2016"
daterange2_1 <- "04/01/2017"
daterange2_2 <- "05/15/2017"

results_folder <- "./Results_20170725/"
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

for(i in 1:length(model_fnames)){
  model_subj <-unlist(strsplit(model_fnames[i], "_"))[1] 
  rmarkdown::render("K:/AscendKC/Corp/R_and_D/1-USERS/Jennifer Brussow/Outcome Modeling/Outcome_analysis/Outcome_Item_Analysis_Results.Rmd",
                    output_file = paste0(model_subj, "_Report_", date, ".pdf"),
                    output_dir = out_folder)
  
}

