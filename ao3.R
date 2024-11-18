
#Clear Workspace
cat("\014")
rm(list=ls())
set.seed(18552)

#Load in Libraries
library(tidyverse)
library(ggthemes)
library(patchwork)

#Load in the data
setwd("/Users/elaineye/Desktop/DSCI ")

tagData<-read.csv("data/ao3data/tags-20210226.csv") 
View(tagData)
filtered_data <- tagData %>% filter(type == "Fandom")

merged_counts <- filtered_data %>%
  filter(!is.na(merger_id)) %>%
  group_by(merger_id) %>%
  summarize(sum_count = sum(cached_count, na.rm = TRUE))

View(merged_counts)

merged_data <- filtered_data %>%
  left_join(merged_counts, by = c("id" = "merger_id")) %>%
  mutate(
    cached_count = ifelse(!is.na(sum_count), cached_count + sum_count, cached_count)
  ) %>%
  select(-sum_count)

merged_data <- merged_data %>% filter(is.na(merger_id))


View(merged_data)
View(filtered_data)

if ("Black Channel" %in% filtered_data$name) {
  "Yes"
} else {
  "No"
}

library(dplyr)
library(purrr)

# List all CSV files in the directory
file_list <- list.files(path = "data/ao3data/labeled_fandoms/", pattern = "*.csv", full.names = TRUE)

# Combine all files using purrr::map and dplyr::bind_rows
fandoms_data <- file_list %>%
  map_dfr(read.csv)

fandoms_data <- fandoms_data[ , !(names(fandoms_data) %in% "X")]
names(fandoms_data)[names(fandoms_data) == "X0"] <- "name"
names(fandoms_data)[names(fandoms_data) == "X1"] <- "category"


fandoms_processed_data <- fandoms_data %>%
  # Apply changes only for anime_manga_fandoms
  mutate(name = ifelse(category == "anime_manga_fandoms", 
                       gsub(" \\([^)]*\\)$", "", name),  # Remove parentheses at the end
                       name)) 
View(fandoms_data)
View(fandoms_processed_data)

merge <- merged_data %>% full_join(fandoms_processed_data, by = "name")
View(merge)

merge_sum_process <- merge %>%
  # Apply changes only for specific categories
  mutate(name = gsub(" \\([^)]*\\)$", "", name)) %>%
  # Group by name to merge rows
  group_by(name) %>%
  summarize(
    category = first(na.omit(category)),  # Retain the first non-NA category
    cached_count = sum(cached_count, na.rm = TRUE), # Sum cached_count
    .groups = "drop"
  )



View(merge_sum_process)
na_category <- merge_sum_process %>%
  filter(is.na(category))

View(na_category)

merge_sum_test <- merge %>%
  # Apply changes to remove parentheses and everything after "|"
  mutate(name = gsub(" \\([^)]*\\)$", "", name)) %>%     # Remove everything after "|"
  # Group by name to merge rows
  group_by(name) %>%
  summarize(
    id = first(id),
    category = first(na.omit(category)),  # Retain the first non-NA category
    cached_count = sum(cached_count, na.rm = TRUE), # Sum cached_count
    .groups = "drop"
  )
View(merge_sum_test)

na_category <- merge_sum_test %>%
  filter(is.na(category))

View(na_category)

works_data<-read.csv("data/ao3data/works-20210226.csv") 
works_data$creation.date <- as.Date(works_data$creation.date)
works_within_range <- works_data %>%
  filter(creation.date >= as.Date("2019-02-26") & creation.date <= as.Date("2021-02-26"))

View(works_within_range)

library(dplyr)
library(tidyr)

works_within_range <- works_within_range %>%
  mutate(work_id = row_number())
works_within_range$X <- NULL


works_tags_split <- works_within_range %>%
  # Split the `tags` column into multiple rows
  separate_rows(tags, sep = "\\+") %>%
  # Convert `tags` to numeric for matching
  mutate(tags = as.numeric(tags)) %>%
  # Join with `fandom_data` (assuming `merge_sum_test` is the processed fandom data)
  left_join(merge_sum_test, by = c("tags" = "id"), relationship = "many-to-many") %>%
  # Group back by `work_id` to collapse into single rows
  group_by(work_id) %>%
  summarize(
    creation.date = first(creation.date),      # Take the first date (adjust as needed)
    tags = paste(tags, collapse = "+"),       # Recombine tags
    category = first(na.omit(category))  # Select category with the highest count
  )
View(works_tags_split)

na_category <- works_tags_split %>%
  filter(is.na(category))

View(na_category)


