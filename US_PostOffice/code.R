# =============================================================================
# US Post Offices Visualization (1764-2000)
# Data source: TidyTuesday (2021-04-13)
# Description: Analysis and visualization of historical US Post Office locations
# =============================================================================

# -------------- 1. Setup and Data Loading --------------
# Load required libraries
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

# Set default theme
theme_set(theme_light())

# Load the TidyTuesday dataset
df <- tidytuesdayR::tt_load('2021-04-13')
data <- df$post_offices

# -------------- 2. Data Exploration --------------
# Check for unique IDs
data %>% 
  count(id, sort = TRUE)

# Examine duplicate names
data %>% 
  count(name, sort = TRUE)

# Explore potential duplicates
data %>% 
  count(name, orig_name, state, county1, sort = TRUE)

# Investigate multiple entries
data %>% 
  add_count(name, orig_name, state, county1, sort = TRUE) %>%
  filter(n >= 2) %>%
  arrange(name, orig_name, state, county1) %>%
  select(id, name, orig_name, state, county1,
         established, discontinued, continuous, coordinates)

# -------------- 3. Data Cleaning and Processing --------------
# Filter out post-2002 data
data <- data %>%
  filter(discontinued <= 2002)

# Create yearly data
PO_year <- data %>%
  select(name, state, established, discontinued) %>%
  filter(!is.na(established)) %>%
  mutate(year = map2(established, discontinued, seq)) %>%
  unnest(year)

# Process post office data
post_offices <- df$post_offices %>%
  select(name, state, county1, established, discontinued, continuous,
         stamp_index, id, coordinates, latitude, longitude,
         gnis_dist, gnis_county, gnis_state) %>%
  filter(established >= 1639,
         is.na(discontinued) | discontinued >= established)

# Create yearly dataset
post_office_years <- post_offices %>%
  select(name, state, established, discontinued, latitude, longitude) %>%
  replace_na(list(discontinued = 2003)) %>%
  filter(established >= 1750, discontinued <= 2021) %>%
  mutate(year = map2(established, discontinued, seq)) %>%
  unnest(year)

# -------------- 4. Basic Visualizations --------------
# Annual post office count
post_office_years %>%
  count(year, name = "N_PO") %>%
  arrange(desc(year)) %>%
  ggplot(aes(year, N_PO)) +
  geom_area() +
  labs(x = "Year",
       y = "# of PO in US over years")

# Cumulative post offices by state
post_office_cumulative <- post_office_years %>%
  count(year, state = fct_lump(state, 16), name = "N_PO") %>%
  mutate(state = fct_reorder(state, -N_PO, sum)) %>%
  filter(state != "Other") %>%
  ggplot(aes(year, N_PO, fill = state)) +
  geom_area() +
  labs(x = "Year",
       y = "# of PO in US over years") +
  facet_wrap(~state) +
  theme(legend.position = "none")

# -------------- 5. Map Visualizations --------------
# Prepare state map data
states_map <- map_data("state") %>%
  as_tibble() %>%
  mutate(state = state.abb[match(region, str_to_lower(state.name))])

# Basic chloropleth map
post_office_years %>%
  filter(year == 2003) %>%
  count(state, sort = TRUE) %>%
  inner_join(states_map, by = "state") %>%
  ggplot(aes(long, lat, group = group, fill = n)) +
  geom_polygon() +
  scale_fill_gradient2(low = "blue", high = "red", midpoint = 750) +
  theme_map() +
  labs(fill = "# of PO")

# -------------- 6. Population-Adjusted Visualizations --------------
# Prepare population data
state_pop <- us_state_populations %>%
  mutate(state = state.abb[match(state, state.name)]) %>%
  replace_na(list(state = "DC"))

# Create density-adjusted visualization
post_office_years %>%
  filter(year == 2000) %>%
  count(state, sort = TRUE) %>%
  inner_join(state_pop %>% filter(year == 2000), by = "state") %>%
  mutate(po_density = n/(population/1e6)) %>%
  inner_join(states_map, by = "state") %>%
  ggplot(aes(long, lat, group = group, fill = po_density)) +
  geom_polygon() +
  scale_fill_gradient2(low = "blue", high = "red", midpoint = 250) +
  theme_map() +
  labs(fill = "PO per million people")

# -------------- 7. Time Series Analysis --------------
# Calculate cumulative statistics by state and year
post_office_cumulative_all <- post_office_years %>%
  count(year, state, name = "N_PO")

# Historical comparison across key years
post_office_cumulative_all %>%
  inner_join(state_pop, by = c("year", "state")) %>%
  filter(year %in% c(1800, 1850, 1900, 1950)) %>%
  right_join(states_map, by = "state") %>%
  mutate(po_density = N_PO/(population/1e6)) %>%
  filter(!is.na(year)) %>%
  ggplot(aes(long, lat, group = group, fill = po_density)) +
  geom_polygon() +
  scale_fill_gradient2(
    low = "blue",
    high = "red",
    midpoint = 3000
  ) +
  theme_map() +
  labs(fill = "PO per million people") +
  facet_wrap(~year)

# -------------- 8. Density Visualization --------------
# Create density-based visualization for 1900
static_post_office_years %>%
  filter(year == 1900,
         !state %in% c("HI", "AK")) %>%
  ggplot() +
  # Add state borders
  borders("state", colour = "gray80", size = 0.2) +
  # Add density heat map
  stat_density_2d(
    aes(x = longitude, y = latitude, fill = ..level..),
    geom = "polygon",
    alpha = 0.4
  ) +
  # Add points on top
  geom_point(
    aes(longitude, latitude),
    size = 0.2,
    alpha = 0.3,
    color = "white"
  ) +
  scale_fill_viridis() +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "black"),
    plot.background = element_rect(fill = "black"),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(
      size = 16,
      face = "bold",
      hjust = 0.5,
      color = "white"
    ),
    plot.subtitle = element_text(
      size = 12,
      hjust = 0.5,
      color = "gray80"
    ),
    legend.position = "none"
  ) +
  coord_map("albers", lat0 = 39, lat1 = 45) +
  labs(
    title = "Density of US Post Offices in 1900",
    subtitle = "Highlighting areas of concentrated postal service coverage"
  )

# -------------- 9. Animation Creation --------------
# Create base animation
animated_po <- post_office_years %>%
  filter(!state %in% c("HI", "AK")) %>%
  ggplot() +
  # Add state borders
  borders("state", colour = "gray80", size = 0.2) +
  # Add points with styling
  geom_point(
    aes(longitude, latitude),
    size = 0.3,
    alpha = 0.4,
    color = "#2171b5"
  ) +
  coord_map("albers", lat0 = 39, lat1 = 45) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  ) +
  labs(
    title = "US Post Offices Distribution",
    subtitle = "Year: {frame_time}"
  ) +
  # Animation settings
  transition_time(year) +
  enter_fade() +
  exit_fade() +
  ease_aes('linear')

# Render and save animation
animated_po_render <- animate(
  animated_po,
  nframes = 100,  # Number of frames
  fps = 5,        # Frames per second
  width = 800,    # Output width
  height = 500,   # Output height
  renderer = gifski_renderer(loop = TRUE)
)

# Save final outputs
anim_save("post_office_evolution.gif", animated_po_render)


