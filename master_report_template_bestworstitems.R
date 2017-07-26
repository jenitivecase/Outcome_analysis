source("K:/AscendKC/Corp/R_and_D/1-USERS/Jennifer Brussow/options.R")

library(rmarkdown)

exclude_2016 <- "Y"

results_folder <- "./Results_20170630/"
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

if(exclude_2016 %in% c("Y", "Yes", "y", "yes")){
  model_fnames <- grep("BSN", model_fnames, value = TRUE, invert = TRUE)
  model_fnames <- grep("NLN", model_fnames, value = TRUE, invert = TRUE)
  
  factor_items_fnames <- grep("BSN", factor_items_fnames, value = TRUE, invert = TRUE)
  factor_items_fnames <- grep("NLN", factor_items_fnames, value = TRUE, invert = TRUE)
}

for(file in 1:length(model_fnames)){
  model_subj <- unlist(strsplit(model_fnames[file], "_"))[1] 
  rmarkdown::render("K:/AscendKC/Corp/R_and_D/1-USERS/Jennifer Brussow/Outcome Modeling/Outcome_analysis/Top_Bottom_Items.Rmd",
                    output_file = paste0(model_subj, "_Report_", date, ".pdf"),
                    output_dir = "./BestandWorstItems")
  
}

