#Clear Workspace
cat("\014")
rm(list=ls())
set.seed(18552)

#Load in Libraries
library(tidyverse)
library(ggthemes)
library(patchwork)
library(dplyr)
library(lubridate)
setwd("/Users/elaineye/Desktop/DSCI ")

library(ggplot2)
library(ggplot2)

# Example data
data <- read.csv("filename2.csv")
View(data)
data$creation.date <- as.Date(data$creation.date, format = "%Y-%m-%d")

# Filter data between January 2019 and January 2021
filtered_data <- data %>%
  filter(creation.date >= as.Date("2019-01-01") & creation.date <= as.Date("2021-01-31"))

# Summarize frequencies by creation.date and category (if needed)
filtered_data <- filtered_data %>%
  group_by(creation.date, category) %>%
  summarize(Frequency = n(), .groups = "drop")




# Create the line graph with monthly ticks
ggplot(filtered_data, aes(x = creation.date, y = Frequency, group = category, color = category)) +
  geom_line(linewidth = .2) +
  geom_point(size = .2) +
  facet_wrap(~ category, scales = "free_y") +  # Separate graphs for each category
  scale_x_date(
    date_breaks = "1 month",   # Monthly tick marks
    date_labels = "%b %Y"     # Format labels as "Jan 2019", "Feb 2019", etc.
  ) +
  labs(
    title = "Fandom Popularity Over Time (Jan 2019 - Jan 2021)",
    x = "Month",
    y = "Frequency",
    color = "Category"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12),  # Category labels
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    legend.position = "none"  # Remove legend
  )
+
  theme_minimal() + theme(
    axis.text.x = element_text(color = "darkred", angle = 45, hjust = 1, size = 12),  # X-axis text in dark red
    axis.text.y = element_text(color = "darkred", size = 12),  # Y-axis text in dark red
    legend.position = "right"  # Keep legend position
  )