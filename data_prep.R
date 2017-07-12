outcomes_data <- read.table("item-level_RN-CP_outcome-scores.txt",
                            sep = ",", header = TRUE, as.is = TRUE,
                            strip.white = TRUE, fill = TRUE,
                            blank.lines.skip = TRUE)

saveRDS(outcomes_data, "item-level_RN-CP_outcome-scores.rds")

item_outcome_info <- read.table("item_outcome_info.txt",
                                sep = ",", header = TRUE, as.is = TRUE,
                                strip.white = TRUE, fill = TRUE,
                                blank.lines.skip = TRUE)
