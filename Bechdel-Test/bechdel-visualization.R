# Bechdel Test Visualization
# Date: Updated February 2025
# 
# This script creates a visualization of Bechdel test ratings by year,
# using a novel approach to display the median test score through 
# character formatting in year digits.

# Inspired by Georgios Karamanis

# Load required libraries
library(tidyverse)
library(tidytuesdayR)
library(colorspace)  # For color manipulation

# Load TidyTuesday data from March 9, 2021
data <- tt_load("2021-03-09")
bechdel <- data$raw_bechdel
movies <- data$movies

# Set font family variables
f1 <- "JetBrains Mono"
f1l <- "Regular"
f1b <- "ExtraBold"

# ---- Data Preparation ----
# Process data to calculate median ratings by year and prepare for visualization
bechdel_med <- bechdel %>%
  # Group by year and calculate median Bechdel rating and count
  group_by(year) %>%
  summarise(median_rating = median(rating),
            # Calculate proportion of movies with rating 3
            n = sum(rating == 3) / n()) %>%
  # Split year into individual characters and create position indices
  mutate(
    char = strsplit(as.character(year), split = ""),
    char_n = list(c(0:3))
  ) %>%
  ungroup() %>%
  # Expand to create one row per character per year
  unnest(c(char, char_n)) %>%
  # Determine which characters to highlight based on median rating
  mutate(
    # Create font family string based on whether character position matches median rating
    fam = paste0(f1, " ", if_else(char_n == floor(median_rating) | 
                                    char_n == ceiling(median_rating), f1b, f1l)),
    # Calculate decade for vertical positioning
    decade = (year - 1) %/% 10 * 10,
    # Calculate horizontal position within decade
    x = year - decade,
    # Add small offset for consecutive bold characters to avoid overlap
    margin_x = if_else(lag(str_detect(fam, "Bold"), default = FALSE), 0.04, 0)
  ) %>%
  # Calculate cumulative margin within each year
  group_by(year) %>%
  mutate(
    margin_x = cumsum(margin_x),
    to_plot = x + char_n/10 + margin_x
  ) %>%
  ungroup()

# Create final dataset with safe font face for ggplot
df <- bechdel_med %>%
  mutate(
    # Convert font family to fontface parameter that works in ggplot
    fam_safe = if_else(str_detect(fam, "Bold"), "bold", "plain")
  )

# ---- Visualization ----
ggplot(df) +
  # Main year digits - each digit of each year as text
  geom_text(aes(
    x = x + char_n/10 + margin_x,
    y = decade,
    label = char,
    size = if_else(str_detect(fam, "Bold"), 4.5, 3.5),
    color = if_else(str_detect(fam, "Bold"), n, NA_real_),
    fontface = fam_safe
  ),
  stat = "unique", hjust = 0) +
  
  # ---- Legend and Explanation Elements ----
# Example year with position indicator (1997)
annotate("text",
         x = c(1, 1.15, 1.35, 1.55),
         y = 1850,
         label = c("1", "9", "9", "7"),
         size = c(6, 6, 6, 7.5),
         fontface = c("plain", "plain", "plain", "bold"),
         color = c("grey20", "grey20", "grey20", "#932566")
) +
  
  # Position indicators (0-3)
  annotate("text",
           x = c(1, 1.15, 1.35, 1.55),
           y = 1862,
           label = c("0", "1", "2", "3"),
           size = 3,
           fontface = "plain",
           color = "grey20"
  ) +
  
  # Explanation text
  annotate("text",
           x = 0,
           y = 1866,
           label = "Bold digits show the median Bechdel test score (0-3)\nWhen a score falls between values, both digits are bold\nPlacement of the bold digit = median Bechdel rating for that year\nThus, for 1997, the median bechdel rating is 3",
           size = 3,
           fontface = "plain",
           color = "grey15",
           lineheight = 1.2,
           hjust = 0,
           vjust = 1
  ) +
  
  # Arrows connecting example to explanation
  annotate("segment",
           x = c(1, 1.15, 1.35, 1.55),
           y = 1859,
           xend = c(1, 1.15, 1.35, 1.55),
           yend = 1855,
           color = "#3a86ff",
           linewidth = 0.3,
           arrow = arrow(length = unit(0.007, "npc"), type = "closed")
  ) +
  
  # Background area for legend/explanation (invisible box)
  annotate("rect",
           xmin = -0.25,
           xmax = 3.5,
           ymin = 1845,
           ymax = 1885,
           fill = "white",
           color = "black",
           alpha = 0,
           size = 0.5
  ) +
  
  # ---- Title Elements ----
# Main title
annotate("text",
         x = 10,
         y = 1850,
         label = "Are movies passing the Bechdel Test?",
         size = 9,
         fontface = "bold",
         color = "#1a1a1a",
         hjust = 1
) +
  
  # Subtitle
  annotate("text",
           x = 10,
           y = 1859,
           label = "Analyzing 8,839 films (1888-2021): median Bechdel test ratings by year",
           size = 4,
           fontface = "plain",
           color = "#3a3a3a",
           hjust = 1,
           vjust = 0.75,
           lineheight = 1.1
  ) +
  
  # ---- Scales and Theme ----
# Reverse y-axis to have oldest years at bottom
scale_y_reverse(expand = c(0.02, 0)) +
  
  # Use size values as provided in aesthetic mapping
  scale_size_identity() +
  
  # Color scale for film counts
  scale_color_viridis_c(
    option = "inferno",  # Use inferno color palette
    begin = 0.2,         # Start at 20% of the palette
    end = 0.9,           # End at 90% of the palette  
    na.value = "grey30", # Color for non-highlighted digits
    guide = guide_colorbar(
      title = "Share of movies made that pass the test",
      title.position = "left",
      title.hjust = 0.5,
      barwidth = unit(0.2, "cm"),
      barheight = unit(4, "cm"),
      title.theme = element_text(
        size = 10,
        face = "plain",
        color = "#3a3a3a",
        angle = 90  # Rotate title to vertical
      )
    )
  ) +
  
  # Caption for data source
  labs(
    caption = "Source: Bechdeltest.com · Inspired by Georgios Karamanis"
  ) +
  
  # Base theme and customizations
  theme_void() +
  theme(
    # Legend positioning and styling
    legend.position = c(0.05, 0.4),
    legend.key.height = unit(0.4, "line"),
    legend.key.width = unit(4, "line"),
    legend.title = element_text(size = 9, color = "#3a3a3a", hjust = 0.5),
    legend.text = element_text(size = 8, color = "#3a3a3a"),
    
    # Background styling
    plot.background = element_rect(
      fill = colorRampPalette(c("#f8f9fa", "#e9ecef"))(100)[50],
      color = NA
    ),
    
    # Caption styling
    plot.caption = element_text(
      color = "#6c757d",
      hjust = 0.98,
      size = 8,
      margin = margin(0, 0, 15, 0)
    ),
    
    # Plot margins
    plot.margin = margin(20, 20, 20, 20)
  )


ggsave("bechdel-test.png", dpi = 320, height = 8, width = 13)

