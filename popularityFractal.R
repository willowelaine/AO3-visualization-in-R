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


filtered_data <- filtered_data %>%
  mutate(category = case_when(
    category == "Anime/ Manga" ~ "Anime/Manga",
    category == "Books and Literature" ~ "Books/Literature",
    category == "Cartoons/Comics/Graphicnovels" ~ "Cartoons/Comics/Graphic Novels",
    category == "Celebrities/Real/People" ~ "Celebrities/Real People",
    category == "Music and Bands" ~ "Music/Bands",
    category == "Other/Media" ~ "Other Media",
    category == "Tv shows" ~ "TV Shows",
    category == "Videogames" ~ "Video Games",
    TRUE ~ category  # Keep the original name if no match
  ))

# Create the line graph with monthly ticks
ggplot(filtered_data, aes(x = creation.date, y = Frequency, group = category, color = category)) +
  geom_line(linewidth = .2) +
  geom_point(size = .2) +
  facet_wrap(~ category, scales = "fixed") +  # Separate graphs for each category
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
    plot.title = element_text(color = "darkred", size = 14, face = "bold", hjust = 0.5),  # Center and style title
    axis.text.x = element_text(angle = 45, hjust = 1, size=5),  # Rotate x-axis labels
    legend.position = "right",  # Legend on the right
    panel.grid.major = element_line(color = "#999999", size = 0.3),  # Major grid lines
    panel.grid.minor = element_line(color = "#999999", size = 0.2,linetype = "dotted"),  # Minor grid lines
    panel.background = element_rect(fill = "#f4efde", color = NA),  # Background of the panel
    plot.background = element_rect(fill = "#f4efde", color = NA),  # Background of the entire plot
    legend.background = element_rect(fill = "#f4efde", color = NA),  # Background for the legend
    plot.margin = margin(20, 20, 20, 20)  # Add padding around the plot
  ) 