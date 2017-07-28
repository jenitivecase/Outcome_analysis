source("K:/AscendKC/Corp/R_and_D/1-USERS/Jennifer Brussow/options.R")


worst_items_2013 <- read.xlsx("CP_BestWorstItems_20170727.xlsx", sheet = "2013worst_items")
worst_items_2016 <- read.xlsx("CP_BestWorstItems_20170727.xlsx", sheet = "2016worst_items")

best_items_2013 <- read.xlsx("CP_BestWorstItems_20170727.xlsx", sheet = "2013best_items")
best_items_2016 <- read.xlsx("CP_BestWorstItems_20170727.xlsx", sheet = "2016best_items")

worst_items_2013 <- worst_items_2013 %>%
  mutate(uniqueid = seq(1:nrow(worst_items_2013))) 
worst_items_2016 <- worst_items_2016 %>%
  mutate(uniqueid = seq(1:nrow(worst_items_2016)))
best_items_2013 <- best_items_2013 %>%
  mutate(uniqueid = seq(1:nrow(best_items_2013))) 
best_items_2016 <- best_items_2016 %>%
  mutate(uniqueid = seq(1:nrow(best_items_2016)))



# worst_items_2013$in2016 <- NA
# for(i in 1:nrow(worst_items_2013)){
#   if(!is.na(worst_items_2013[i, "Item"])){
#       overlap <- worst_items_2016[which(worst_items_2016$Item == worst_items_2013[i, "Item"]), "uniqueid"]
#     if(length(overlap) > 0){
#       worst_items_2013[i, "in2016"] <- overlap
#     }
#   }
# }
# 
# worst_items_2016$in2013 <- NA
# for(i in 1:nrow(worst_items_2016)){
#   if(!is.na(worst_items_2016[i, "Item"])){
#     overlap <- worst_items_2013[which(worst_items_2013$Item == worst_items_2016[i, "Item"]), "uniqueid"]
#     if(length(overlap) > 0){
#       worst_items_2016[i, "in2013"] <- overlap
#     }
#   }
# }


item_dups_2013 <- worst_items_2013 %>% 
  group_by(Item) %>%
  mutate(duplicates = length(unique(uniqueid))) %>%
  filter(duplicates > 1) %>%
  ungroup() %>%
  select(Item) %>%
  left_join(worst_items_2013, by = c("Item" = "Item"))

tibble(ids = test %>% map("uniqueid"))



worst_items_2013$duplicates <- NA
for(i in 1:nrow(worst_items_2013)){
  if(!is.na(worst_items_2013[i, "Item"])){
      overlap <- worst_items_2013[which(worst_items_2013$Item == worst_items_2013[i, "Item"]), "uniqueid"]
      overlap <- overlap[which(overlap != i)]
    if(length(overlap) > 0){
      overlap <- paste0(overlap, collapse = ",")
      worst_items_2013[i, "duplicates"] <- overlap
    }
  }
}

