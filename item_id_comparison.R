source("K:/AscendKC/Corp/R_and_D/1-USERS/Jennifer Brussow/options.R")


worst_items_2013 <- read.xlsx("CP_BestWorstItems_20170727.xlsx", sheet = "2013worst_items")
worst_items_2016 <- read.xlsx("CP_BestWorstItems_20170727.xlsx", sheet = "2016worst_items")

best_items_2013 <- read.xlsx("CP_BestWorstItems_20170727.xlsx", sheet = "2013best_items")
best_items_2016 <- read.xlsx("CP_BestWorstItems_20170727.xlsx", sheet = "2016best_items")

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
                            best_items_2013, best_items_2016))

worst_item_dups_2013 <- worst_items_2013 %>% 
  group_by(Item) %>%
  mutate(duplicates = length(unique(uniqueid))) %>%
  filter(duplicates > 1) %>%
  ungroup() %>%
  select(Item) %>%
  left_join(worst_items_2013, by = c("Item" = "Item"))


worst_item_dups_2016 <- worst_items_2016 %>% 
  group_by(Item) %>%
  mutate(duplicates = length(unique(uniqueid))) %>%
  filter(duplicates > 1) %>%
  ungroup() %>%
  select(Item) %>%
  left_join(worst_items_2016, by = c("Item" = "Item"))

best_item_dups_2013 <- best_items_2013 %>% 
  group_by(Item) %>%
  mutate(duplicates = length(unique(uniqueid))) %>%
  filter(duplicates > 1) %>%
  ungroup() %>%
  select(Item) %>%
  left_join(best_items_2013, by = c("Item" = "Item"))


best_item_dups_2016 <- best_items_2016 %>% 
  group_by(Item) %>%
  mutate(duplicates = length(unique(uniqueid))) %>%
  filter(duplicates > 1) %>%
  ungroup() %>%
  select(Item) %>%
  left_join(best_items_2016, by = c("Item" = "Item"))
