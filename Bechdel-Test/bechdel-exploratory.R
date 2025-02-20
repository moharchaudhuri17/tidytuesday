# Bechdel Test Exploratory Analysis
# Date: February 2025
#
# This script performs exploratory analysis on the Bechdel test dataset
# from TidyTuesday (March 9, 2021).

# Load required libraries
library(tidyverse)
library(tidytuesdayR)

# ---- Data Loading ----
# Load TidyTuesday data from March 9, 2021
data <- tt_load("2021-03-09")
bechdel <- data$raw_bechdel
movies <- data$movies

# ---- Basic Counts ----
# Count movies by clean_test and binary outcome
test_counts <- movies %>%
  count(clean_test, binary)
print(test_counts)

# ---- Year-by-Year Analysis ----
# Count movies by year and binary outcome
yearly_counts <- movies %>%
  count(year, binary)

# Visualize counts by year with stacked bars
yearly_plot <- yearly_counts %>%
  ggplot(aes(year, n, fill = binary)) +
  geom_col() +
  labs(
    title = "Number of Movies by Year and Bechdel Test Outcome",
    x = "Year",
    y = "Number of Movies",
    fill = "Bechdel Test"
  ) +
  theme_minimal()

# Display the plot
print(yearly_plot)

# ---- Decade-Level Analysis ----
# Analyze pass rates by decade
decade_analysis <- movies %>%
  group_by(decade = floor(year/10)*10) %>%
  summarise(
    n_movies = n(),
    pct_pass = mean(binary == "PASS")
  )

# Visualize change over decades
decade_plot <- decade_analysis %>%
  ggplot(aes(decade, pct_pass)) +
  geom_line() +
  geom_point(aes(size = n_movies)) +
  expand_limits(y = 0) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Percentage of Movies Passing Bechdel Test by Decade",
    subtitle = "Point size indicates number of movies",
    x = "Decade",
    y = "Percentage Passing",
    size = "Number of Movies"
  ) +
  theme_minimal()

# Display the decade plot
print(decade_plot)

# ---- Rating Analysis ----
# Count movies by rating and year
rating_counts <- bechdel %>%
  count(rating, year)

# Summary statistics
summary_stats <- bechdel %>%
  group_by(year) %>%
  summarise(
    n = n(),
    mean_rating = mean(rating),
    median_rating = median(rating),
    min_rating = min(rating),
    max_rating = max(rating)
  )

# View summary statistics
print(summary_stats)

# Export clean data for visualization
write_csv(decade_analysis, "bechdel_by_decade.csv")
write_csv(summary_stats, "bechdel_yearly_stats.csv")
