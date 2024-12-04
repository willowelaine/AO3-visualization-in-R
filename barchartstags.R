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


View(works_data_with_names)

# Split data into pre-pandemic, early pandemic, and late pandemic
prePandemicData <- works_data %>%
  filter(creation.date >= as.Date("2019-03-01") & creation.date <= as.Date("2019-12-31"))

pandemicData <- works_data %>%
  filter(creation.date >= as.Date("2020-03-01") & creation.date <= as.Date("2020-12-31"))


# Join the works_data with freeform_tags
prePandemicData <- prePandemicData %>%
  left_join(freeform_tags, by = c("tags" = "id"))
pandemicData <- pandemicData %>%
  left_join(freeform_tags, by = c("tags" = "id"))


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
pandemic_tags <- process_tags(pandemicData)

# Function to filter out "Redacted" and get top tags
filter_and_top_tags <- function(tags, n = 10) {
  tags %>%
    filter(name != "Redacted") %>%  # Exclude "Redacted"
    slice_max(order_by = percentage, n = n)  # Get top n by percentage
}

# Get top 10 tags for each period
prePandemic_top_tags <- filter_and_top_tags(prePandemic_tags)
pandemic_top_tags <- filter_and_top_tags(pandemic_tags)

# Combine data for visualization
combined_tags <- bind_rows(
  prePandemic_top_tags %>% mutate(period = "Pre-Pandemic (March 2019 – December 2019)"),
  pandemic_top_tags %>% mutate(period = "Pandemic (March 2020 – December 2020)")
)

# Write the combined data to a CSV (optional)
write.csv(combined_tags, "percentageCombined.csv", row.names = FALSE)

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
    period == "Pre-Pandemic (March 2019 – December 2019)" ~ "Pre-Pandemic",
    period == "Pandemic (March 2020 – December 2020)" ~ "Pandemic",
   
    TRUE ~ period
  ))


combined_tags <- combined_tags %>%
  mutate(period = factor(period, levels = c(
    "Pre-Pandemic",  # Move this to the first position
    "Pandemic"
  )))
# Create custom legend labels with parentheses
legend_labels <- c(
  "Pre-Pandemic (March 2019 – December 2019)",
  "Pandemic (March 2020 – December 2020)"
)



# Plot with updated facet labels and custom legend
ggplot(combined_tags, aes(x = reorder(name, percentage), y = percentage, fill = period)) +
  geom_bar(stat = "identity") +
  geom_text(
    aes(label = sprintf("%.2f", percentage)),  # Format percentage with 3 decimal places
    position = position_stack(vjust = 0.5),
    size = 3.5, color = "white"
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
    values = c("#88E0EF", "#FF5151"),  # Customize colors if needed
    labels = legend_labels  # Use custom legend labels with parentheses
  ) +
  theme_minimal() + 
  theme(
    plot.title = element_text(color = "darkred", size = 14, face = "bold", hjust = 0.5),  # Center and style title
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    legend.position = "right",  # Legend on the right
    panel.grid.major = element_line(color = "#999999", size = 0.3),  # Major grid lines
    panel.grid.minor = element_line(color = "#999999", size = 0.2,linetype = "dotted"),  # Minor grid lines
    panel.background = element_rect(fill = "#f4efde", color = NA),  # Background of the panel
    plot.background = element_rect(fill = "#f4efde", color = NA),  # Background of the entire plot
    legend.background = element_rect(fill = "#f4efde", color = NA),  # Background for the legend
    plot.margin = margin(20, 20, 20, 20)  # Add padding around the plot
  ) 


