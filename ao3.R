
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
  filter(creation.date >= as.Date("2019-01-01") & creation.date <= as.Date("2021-01-31"))

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

no_na_category<-works_tags_split %>%
  filter(!is.na(category))

View(na_category)
write.csv(no_na_category, "filename2.csv", row.names = FALSE)


no_na_category <- read_csv("filename2.csv")
# Count frequencies by category and date
# Extract year and month, and aggregate data
monthly_data <- no_na_category %>%
  mutate(year_month = format(creation.date, "%Y-%m")) %>%  # Extract year-month
  group_by(year_month, category) %>%
  summarize(frequency = n(), .groups = "drop")  # Count occurrences

monthly_data <- monthly_data %>%
  filter(creation.date >= as.Date("2019-01-01") & creation.date <= as.Date("2021-01-31"))

# Plot trends
ggplot(monthly_data, aes(x = year_month, y = frequency, color = category, group = category)) +
  geom_line() +
  labs(
    title = "Trends by Category Over Time",
    x = "Month",
    y = "Frequency",
    color = "Category"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability


#extar
library(ggplot2)
library(dplyr)

# Filter data between January 2019 and January 2021
filtered_data <- no_na_category %>%
  filter(creation.date >= as.Date("2019-01-01") & creation.date <= as.Date("2021-01-31"))

# Summarize frequencies by exact date and category
daily_data <- filtered_data %>%
  group_by(creation.date, category) %>%
  summarize(frequency = n(), .groups = "drop")  # Count occurrences

# Calculate total frequency per category
category_totals <- daily_data %>%
  group_by(category) %>%
  summarize(total_frequency = sum(frequency), .groups = "drop")

# Reorder categories by total frequency (largest to smallest)
daily_data <- daily_data %>%
  mutate(category = factor(category, levels = category_totals %>%
                             arrange(desc(total_frequency)) %>%
                             pull(category)))

# Plot stacked area chart with sorted areas
ggplot(daily_data, aes(x = creation.date, y = frequency,fill = category)) +
  geom_area(alpha = 0.8, stat = "smooth", method = "loess")+  # Stacked area chart
  labs(
    title = "Trends by Category Over Time (Jan 2019 - Jan 2021)",
    x = "Date",
    y = "Frequency",
    fill = "Category"
  ) +
  scale_x_date(
    date_breaks = "3 months",  # Adjust tick marks to every 3 months
    date_labels = "%b %Y"     # Format labels as "Jan 2020"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    legend.position = "right"  # Adjust legend position
  ) +
  theme_minimal() + theme(
    axis.text.x = element_text(color = "darkred", angle = 45, hjust = 1, size = 12),  # X-axis text in dark red
    axis.text.y = element_text(color = "darkred", size = 12),  # Y-axis text in dark red
    legend.position = "right"  # Keep legend position
  )




ggplot(monthly_data, aes(x = year_month, y = frequency, color = category, group = category)) +
  geom_line() +
  labs(
    title = "Trends by Category Over Time",
    x = "Month",
    y = "Frequency",
    color = "Category"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_minimal() + theme(
    axis.text.x = element_text(color = "darkred", angle = 45, hjust = 1, size = 12),  # X-axis text in dark red
    axis.text.y = element_text(color = "darkred", size = 12),  # Y-axis text in dark red
    legend.position = "right"  # Keep legend position
  )
# Rotate x-axis labels for readability
