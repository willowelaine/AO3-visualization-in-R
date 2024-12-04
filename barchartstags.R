cat("\014")
rm(list=ls())
set.seed(18552)
setwd("/Users/elaineye/Desktop/DSCI ")

tagData<-read.csv("data/ao3data/tags-20210226.csv") 
works_data<-read.csv("data/ao3data/works-20210226.csv") 
View(works_data)

works_data$creation.date <- as.Date(works_data$creation.date)

# Convert the creation date to Date type
works_data$creation.date <- as.Date(works_data$creation.date)

library(tidyverse)

# Split works_data tags into rows and ensure numeric format
works_data <- works_data %>%
  separate_rows(tags, sep = "\\+") %>%
  mutate(tags = as.integer(tags))  # Convert to integer



# Filter for Freeform tags in tagData
freeform_tags <- tagData %>%
  filter(type == "Freeform") %>%
  select(id, name)

View(freeform_tags)

# Join the works_data with freeform_tags
works_data_with_names <- works_data %>%
  left_join(freeform_tags, by = c("tags" = "id"))

View(works_data_with_names)

# Split data into pre-pandemic, early pandemic, and late pandemic
prePandemicData <- works_data_with_names %>%
  filter(creation.date >= as.Date("2019-04-01") & creation.date <= as.Date("2020-02-29"))

earlyPandemicData <- works_data_with_names %>%
  filter(creation.date >= as.Date("2020-03-01") & creation.date <= as.Date("2020-11-30"))

laterPandemicData <- works_data_with_names %>%
  filter(creation.date >= as.Date("2020-12-01") & creation.date <= as.Date("2021-02-28"))


process_tags <- function(data) {
  data %>%
    filter(!is.na(name)) %>%  # Filter rows where tag name exists
    group_by(name) %>%
    summarise(count = n(), .groups = "drop") %>%
    mutate(percentage = count / sum(count) * 100) %>%  # Calculate percentage
    arrange(desc(percentage))
}

# Apply process_tags function to each period
prePandemic_tags <- process_tags(prePandemicData)
earlyPandemic_tags <- process_tags(earlyPandemicData)
laterPandemic_tags <- process_tags(laterPandemicData)

# Function to filter out "Redacted" and get top tags
filter_and_top_tags <- function(tags, n = 10) {
  tags %>%
    filter(name != "Redacted") %>%  # Exclude "Redacted"
    slice_max(order_by = percentage, n = n)  # Get top n by percentage
}

# Get top 10 tags for each period
prePandemic_top_tags <- filter_and_top_tags(prePandemic_tags)
earlyPandemic_top_tags <- filter_and_top_tags(earlyPandemic_tags)
laterPandemic_top_tags <- filter_and_top_tags(laterPandemic_tags)

# Combine data for visualization
combined_tags <- bind_rows(
  prePandemic_top_tags %>% mutate(period = "Pre-Pandemic (April 2019 – February 2020)"),
  earlyPandemic_top_tags %>% mutate(period = "Early Pandemic (March 2020 – November 2020)"),
  laterPandemic_top_tags %>% mutate(period = "Later Pandemic (December 2020 – February 2021)")
)

# Write the combined data to a CSV (optional)
write.csv(combined_tags, "tagsPercentages.csv", row.names = FALSE)

# Create a faceted bar chart with percentages
ggplot(combined_tags, aes(x = reorder(name, percentage), y = percentage, fill = period)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ period, scales = "free_y") +  # Create one graph for each period
  labs(title = "Top 10 Tags by Percentage for Each Period", x = "Tags", y = "Percentage (%)") +
  theme_minimal()

ggplot(combined_tags, aes(x = reorder(name, percentage), y = percentage, fill = period)) +
  geom_bar(stat = "identity") +
  geom_text(
    aes(label = sprintf("%.2f%%", percentage)),  # Format percentage labels
    position = position_stack(vjust = 0.5),  # Position text at the center of bars
    size = 3, color = "white"  # Adjust text size and color
  ) +
  coord_flip() +
  facet_wrap(~ period, scales = "free_y") +  # Create one graph for each period
  labs(title = "Top 10 Tags by Percentage for Each Period (Relative Proportions within Each Period)", x = "Tags", y = "Percentage (%)") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Angle x-axis labels
    axis.text.y = element_text(size = 10),  # Adjust y-axis text size
    strip.text = element_text(size = 12, face = "bold")  # Adjust facet title size
  )


combined_tags <- combined_tags %>%
  mutate(period = case_when(
    period == "Pre-Pandemic (April 2019 – February 2020)" ~ "Pre-Pandemic",
    period == "Early Pandemic (March 2020 – November 2020)" ~ "Early Pandemic",
    period == "Later Pandemic (December 2020 – February 2021)" ~ "Later Pandemic",
   
    TRUE ~ period
  ))


combined_tags <- combined_tags %>%
  mutate(period = factor(period, levels = c(
    "Pre-Pandemic",  # Move this to the first position
    "Early Pandemic",
    "Later Pandemic"
  )))
# Create custom legend labels with parentheses
legend_labels <- c(
  "Pre-Pandemic (April 2019 – February 2020)",
  "Early Pandemic (March 2020 – November 2020)",
  "Later Pandemic (December 2020 – February 2021)"

)



# Plot with updated facet labels and custom legend
ggplot(combined_tags, aes(x = reorder(name, percentage), y = percentage, fill = period)) +
  geom_bar(stat = "identity") +
  geom_text(
    aes(label = sprintf("%.2f", percentage)),  # Format percentage with 3 decimal places
    position = position_stack(vjust = 0.5),
    size = 3, color = "white"
  ) +
  coord_flip() +
  facet_wrap(~ period, scales = "free_y") +
  labs(
    title = "Top 10 Tags by Percentage for Each Period (Normalized By Total Tags in Period)",
    x = "Tags",
    y = "Percentage (%)",
    fill = "Period"  # Legend title
  ) +
  scale_fill_manual(
    values = c("#88E0EF", "#FF5151", "#FF9B6A"),  # Customize colors if needed
    labels = legend_labels  # Use custom legend labels with parentheses
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10),
    plot.margin = margin(t = 30, r = 20, b = 20, l = 20)
  )


