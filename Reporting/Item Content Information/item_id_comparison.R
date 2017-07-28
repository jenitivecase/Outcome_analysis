source("K:/AscendKC/Corp/R_and_D/1-USERS/Jennifer Brussow/options.R")

out_folder <- "Misc_Item_Reports"

worst_items_2013 <- read.xlsx("./Reporting/Item Content Information/CP_BestWorstItems_20170728.xlsx", 
                              sheet = "2013worst_items")
worst_items_2016 <- read.xlsx("./Reporting/Item Content Information/CP_BestWorstItems_20170728.xlsx", 
                              sheet = "2016worst_items")

best_items_2013 <- read.xlsx("./Reporting/Item Content Information/CP_BestWorstItems_20170728.xlsx", 
                             sheet = "2013best_items")
best_items_2016 <- read.xlsx("./Reporting/Item Content Information/CP_BestWorstItems_20170728.xlsx", 
                             sheet = "2016best_items")

worst_items_2013 <- worst_items_2013 %>%
  mutate(uniqueid = seq(1:nrow(worst_items_2013))) %>%
  mutate(Year = 2013) %>%
  mutate(Type = "worst")
worst_items_2016 <- worst_items_2016 %>%
  mutate(uniqueid = seq(1:nrow(worst_items_2016)))%>%
  mutate(Year = 2016)%>%
  mutate(Type = "worst")
best_items_2013 <- best_items_2013 %>%
  mutate(uniqueid = seq(1:nrow(best_items_2013))) %>%
  mutate(Year = 2013)%>%
  mutate(Type = "best")
best_items_2016 <- best_items_2016 %>%
  mutate(uniqueid = seq(1:nrow(best_items_2016)))%>%
  mutate(Year = 2016)%>%
  mutate(Type = "best")

all_items <- bind_rows(list(worst_items_2013, worst_items_2016, 
                            best_items_2013, best_items_2016)) %>%
  filter(!is.na(Item))

total_item_num <- nrow(all_items)

#### ALL DUPLICATES ####
all_items_dups <- all_items %>%
  group_by(Item) %>%
  mutate(duplicates = length(unique(uniqueid))) %>%
  filter(duplicates > 1) %>%
  ungroup() %>%
  select(Item) %>%
  left_join(all_items, by = c("Item" = "Item")) %>%
  unique() %>%
  arrange(Item, Loading, Type) %>%
  select(-uniqueid)

item_data <-  all_items_dups
short_descriptor <- "All_Duplicates"
item_list_descriptor <- "Items Appearing on Multiple Subscores"
item_list_text <- paste0("These ", nrow(item_data), " items appeared in the worst-loading 
                         or best-loading items on multiple subscores. These items are
                         a subset of the ", total_item_num, " total items analyzed 
                         from the 2013 and 2016 Comprehensive Predictor.")
rmarkdown::render(paste0("K:/AscendKC/Corp/R_and_D/1-USERS/Jennifer Brussow/Outcome Modeling/", 
                         "Outcome_analysis/Reporting/Item Content Information/Item_list_output.Rmd"),
                  output_file = paste0(short_descriptor, "_Report_", date, ".pdf"),
                  output_dir = paste0("K:/AscendKC/Corp/R_and_D/1-USERS/Jennifer Brussow/Outcome Modeling/", 
                                      "Outcome_analysis/Reporting/Item Content Information/", out_folder))


#### INTERESTING ITEMS ####
most_interesting <- all_items_dups %>%
  group_by(Item) %>%
  filter(length(unique(Type)) > 1) %>%
  ungroup()

item_data <-  most_interesting
short_descriptor <- "Worst_AND_Best_Items"
item_list_descriptor <- "Items Appearing in Worst-Loading Items and Best-Loading Items"
item_list_text <- paste0("These ", nrow(item_data), " items appeared in the worst-loading items 
                         on at least one subscore and in the best-loading items on at
                         least one other subscore. These items are
                         a subset of the ", total_item_num, " total items analyzed 
                         from the 2013 and 2016 Comprehensive Predictor.")
rmarkdown::render(paste0("K:/AscendKC/Corp/R_and_D/1-USERS/Jennifer Brussow/Outcome Modeling/", 
                         "Outcome_analysis/Reporting/Item Content Information/Item_list_output.Rmd"),
                  output_file = paste0(short_descriptor, "_Report_", date, ".pdf"),
                  output_dir = paste0("K:/AscendKC/Corp/R_and_D/1-USERS/Jennifer Brussow/Outcome Modeling/", 
                                      "Outcome_analysis/Reporting/Item Content Information/", out_folder))

#### BAD ITEMS ####
bad_items <- all_items_dups %>%
  group_by(Item) %>%
  mutate(types = length(unique(Type))) %>%
  filter(types == 1) %>%
  ungroup() %>%
  select(-types) %>%
  filter(Type == "worst")

item_data <- bad_items
short_descriptor <- "Bad_items"
item_list_descriptor <- "Items Only Appearing in Worst-Loading Items"
item_list_text <- paste0("These ", nrow(item_data), " items only appeared in the worst-loading items on their 
                         given subscores. These items are
                         a subset of the ", total_item_num, " total items analyzed 
                         from the 2013 and 2016 Comprehensive Predictor.")
rmarkdown::render(paste0("K:/AscendKC/Corp/R_and_D/1-USERS/Jennifer Brussow/Outcome Modeling/", 
                         "Outcome_analysis/Reporting/Item Content Information/Item_list_output.Rmd"),
                  output_file = paste0(short_descriptor, "_Report_", date, ".pdf"),
                  output_dir = paste0("K:/AscendKC/Corp/R_and_D/1-USERS/Jennifer Brussow/Outcome Modeling/", 
                                      "Outcome_analysis/Reporting/Item Content Information/", out_folder))

#### REALLY BAD ITEMS ####
really_bad_items <- bad_items %>%
  filter(Loading < 0) %>%
  group_by(Item) %>%
  filter(length(Item) > 1) %>%
  ungroup()

item_data <- really_bad_items
short_descriptor <- "Worst_items"
item_list_descriptor <- "Items With Only Negative Loadings & >=2 Negative Loadings"
item_list_text <- paste0("These ", nrow(item_data), " items only appeared in the worst-loading items on their 
                         given subscores, had only negative loadings, and had two or more
                         of those negative loadings. These items are
                         a subset of the ", total_item_num, " total items analyzed 
                         from the 2013 and 2016 Comprehensive Predictor.")
rmarkdown::render(paste0("K:/AscendKC/Corp/R_and_D/1-USERS/Jennifer Brussow/Outcome Modeling/", 
                        "Outcome_analysis/Reporting/Item Content Information/Item_list_output.Rmd"),
                  output_file = paste0(short_descriptor, "_Report_", date, ".pdf"),
                  output_dir = paste0("K:/AscendKC/Corp/R_and_D/1-USERS/Jennifer Brussow/Outcome Modeling/", 
                                      "Outcome_analysis/Reporting/Item Content Information/", out_folder))


