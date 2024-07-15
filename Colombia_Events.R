#Relative data could easily be found online. Acled data could be obtained from Acled.com for students.
library(tidyverse)
library(sf)
library(ggplot2)

# Set working directory
setwd("/Users/.../data")

# Load ACLED data
acled_data <- read_csv("LatinAmerica_2018-2024_May10.csv")

# Remove duplicate records
acled_data <- acled_data %>%
  distinct()

# Filter out rows with missing latitude and longitude
acled_data <- acled_data %>%
  filter(!is.na(latitude) & !is.na(longitude))

# Convert data to spatial format
acled_sf <- st_as_sf(acled_data, coords = c("longitude", "latitude"), crs = 4326)
acled_colombia_sf <- acled_sf %>% filter(country == "Colombia")

# Convert event_date to Date format
acled_data <- acled_data %>%
  mutate(EventDate = as.Date(event_date, format = "%Y-%m-%d"))

# Exploratory Data Analysis
# Plot 1: Number of Conflict Events Over Time in Colombia
event_time_series <- acled_data %>%
  filter(country == "Colombia") %>%
  group_by(EventDate) %>%
  summarise(event_count = n())

ggplot(event_time_series, aes(x = EventDate, y = event_count)) +
  geom_line(color = "blue") +
  labs(title = "Number of Conflict Events Over Time in Colombia",
       x = "Date",
       y = "Number of Events") +
  theme_minimal()

# Plot 2: Spatial Distribution of Conflict Events in Colombia
ggplot(acled_colombia_sf) +
  geom_sf(aes(color = event_type), size = 0.5) +
  scale_color_viridis_d() +
  labs(title = "Spatial Distribution of Conflict Events in Colombia",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal()

# Mapping Conflict Dynamics
# Load Colombia shapefile
colombia_shapefile <- st_read("Countries_shape/gadm41_COL_2.shp")

# Plot 3: Conflict Events in Colombia by Type
ggplot() +
  geom_sf(data = colombia_shapefile, fill = "white", color = "black") +
  geom_sf(data = acled_colombia_sf, aes(color = event_type), alpha = 0.5) +
  scale_color_viridis_d() +
  labs(title = "Conflict Events in Colombia by Type",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal()

# Additional Analysis and Visualizations

# Plot 4: Event Types Over Time
event_types_over_time <- acled_data %>%
  filter(country == "Colombia") %>%
  group_by(EventDate, event_type) %>%
  summarise(event_count = n())

ggplot(event_types_over_time, aes(x = EventDate, y = event_count, color = event_type)) +
  geom_line() +
  labs(title = "Event Types Over Time in Colombia",
       x = "Date",
       y = "Number of Events") +
  theme_minimal() +
  scale_color_viridis_d()

# Plot 5: Proportion of Event Types
event_type_proportion <- acled_data %>%
  filter(country == "Colombia") %>%
  group_by(event_type) %>%
  summarise(event_count = n()) %>%
  mutate(proportion = event_count / sum(event_count))

ggplot(event_type_proportion, aes(x = "", y = proportion, fill = event_type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Proportion of Event Types in Colombia") +
  theme_void() +
  scale_fill_viridis_d()

# Save the plots to files
ggsave("conflict_events_over_time.png", plot = last_plot(), width = 10, height = 6)
ggsave("spatial_distribution_of_conflict_events.png", plot = last_plot(), width = 10, height = 6)
ggsave("conflict_events_by_type.png", plot = last_plot(), width = 10, height = 6)
ggsave("event_types_over_time.png", plot = last_plot(), width = 10, height = 6)
ggsave("proportion_of_event_types.png", plot = last_plot(), width = 10, height = 6)

