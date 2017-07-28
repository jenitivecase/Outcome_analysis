source("K:/AscendKC/Corp/R_and_D/1-USERS/Jennifer Brussow/options.R")


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

most_interesting <- all_items_dups %>%
  group_by(Item) %>%
  filter(length(unique(Type)) > 1) 

bad_items <- all_items_dups %>%
  group_by(Item) %>%
  mutate(types = length(unique(Type))) %>%
  filter(types == 1) %>%
  ungroup() %>%
  select(-types) %>%
  filter(Type == "worst")

really_bad_items <- bad_items %>%
  filter(Loading < 0) %>%
  group_by(Item) %>%
  filter(length(Item) > 1)