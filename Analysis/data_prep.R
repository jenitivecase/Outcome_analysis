outcomes_data <- read.table("item-level_RN-2016CP_outcome-scores.txt",
                            sep = ",", header = TRUE, as.is = TRUE,
                            strip.white = TRUE, fill = TRUE,
                            blank.lines.skip = TRUE)

saveRDS(outcomes_data, "item-level_RN-2016CP_outcome-scores.rds")
gc()


item_outcome_info <- read.table("item_outcome_info.txt",
                                sep = ",", header = TRUE, as.is = TRUE,
                                strip.white = TRUE, fill = TRUE,
                                blank.lines.skip = TRUE)


# TTR_data <- read.table("item-level_RN-2016CP_TTR-scores.txt",
#                             sep = ",", header = TRUE, as.is = TRUE,
#                             strip.white = TRUE, fill = TRUE,
#                             blank.lines.skip = TRUE)

TTR_data <- read.csv("item-level_RN-2016CP_TTR-scores.csv")
names(TTR_data)[1] <- gsub("ï..", "", names(TTR_data)[1])

saveRDS(TTR_data, "item-level_RN-2016CP_TTR-scores.rds")
gc()