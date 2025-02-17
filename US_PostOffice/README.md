# US Post Offices Historical Visualization (1764-2000)

A data visualization project exploring the evolution of United States Post Offices over two centuries, revealing the fascinating story of American expansion and communication infrastructure development.



## Overview
This project visualizes the historical distribution and evolution of United States Post Offices from 1764 to 2000, creating both static maps and an animated visualization that shows how post offices spread across America like stars at dusk, painting a portrait of a nation stitching itself together one letter at a time.

## Data Source
- Data from TidyTuesday (2021-04-13)
- Historical US Post Office locations and operational dates
- Includes establishment and discontinuation dates, geographical coordinates, and state information

## Visualizations
The project includes several types of visualizations:
1. **Static Maps**: Showing post office distribution at key historical moments
2. **Density Maps**: Highlighting areas of concentrated postal service
3. **Animated Timeline**: Displaying the evolution of post office locations over time
4. **Population-Adjusted Analysis**: Visualizing post office density relative to population



## Dependencies
```r
library(tidytuesdayR)  # For data loading
library(tidyverse)     # Data manipulation
library(dplyr)         # Data manipulation
library(maps)          # For map visualization
library(magick)        # Image processing
library(scales)        # Scale functions
library(ggthemes)      # Additional themes
library(historydata)   # Historical data
library(sf)            # Spatial features
library(gganimate)     # Animation
library(viridis)       # Color scales
```




