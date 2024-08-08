# setwd("/Users/.../Desktop/data")


if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  sf, # vector data operations
  dplyr, # data wrangling
  ggplot2, # for map creation
  modelsummary, # regression table generation # patchwork # arrange multiple plots
)
library(sf)
library(dplyr)
library(ggplot2) 
library(modelsummary) 

theme_for_map <- theme(
  axis.ticks = element_blank(),
  axis.text = element_blank(),
  axis.line = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_line(color = "transparent"),
  panel.background = element_blank(),
  plot.background = element_rect(fill = "transparent", color = "transparent")
)

#create a map showing the spatial distribution of the variable NRATE .
trial_design_sf <- st_as_sf(trial_design, wkt = "geometry")

ggplot() +
  geom_sf(data = trial_design_sf, aes(fill = NRATE)) +
  scale_fill_gradient(name = "NRATE", low = "lightblue", high = "darkblue") +
  theme_void()

#“as applied nitrogen” (NH3.gpkg), “electric conductivity” (ec.shp), and “collected yield.” (yield.rds)
nh3 <- st_read("NH3.gpkg")
ec <- st_read("ec.shp")
yield <- readRDS("yield.rds")

library(patchwork)

g_NH3 <- ggplot() +
  geom_sf(data = nh3, aes(color = aa_NH3)) +
  scale_color_gradient(name = "NH3", low = "lightblue", high = "darkblue") +
  theme_void()
g_NH3
g_ec <- ggplot() +
  geom_sf(data = ec, aes(color = ec)) +
  scale_color_gradient(name = "Electric Conductivity", low = "lightblue", high = "darkblue") +
  theme_void()
g_ec
g_yield <- ggplot() +
  geom_sf(data = yield, aes(color = yield)) +
  scale_color_gradient(name = "Collected Yield", low = "lightblue", high = "darkblue") +
  theme_void()
g_yield
g_NH3 <- g_NH3 + theme_for_map
g_ec <- g_ec + theme_for_map
g_yield <- g_yield + theme_for_map

combined_figure <- (g_yield / g_NH3 / g_ec) +
  plot_layout(guides = "collect")
combined_figure

#divide each plot into six subplots.
# Add an ID variable for each subplot within a plot
nh3$subplot_id <- rep(1:6, length.out = nrow(nh3))
ec$subplot_id <- rep(1:6, length.out = nrow(ec))
yield$subplot_id <- rep(1:6, length.out = nrow(yield))

# Modify the individual maps to include subplots
g_NH3 <- ggplot() +
  geom_sf(data = nh3, aes(color = aa_NH3)) +
  scale_color_gradient(name = "NH3", low = "lightblue", high = "darkblue") +
  theme_void() +
  facet_wrap(~subplot_id)
g_NH3
g_ec <- ggplot() +
  geom_sf(data = ec, aes(color = ec)) +
  scale_color_gradient(name = "Electric Conductivity", low = "lightblue", high = "darkblue") +
  theme_void() +
  facet_wrap(~subplot_id)
g_ec
g_yield <- ggplot() +
  geom_sf(data = yield, aes(color = yield)) +
  scale_color_gradient(name = "Collected Yield", low = "lightblue", high = "darkblue") +
  theme_void() +
  facet_wrap(~subplot_id)
g_yield
# Apply the theme to each map
g_NH3 <- g_NH3 + theme_for_map
g_ec <- g_ec + theme_for_map
g_yield <- g_yield + theme_for_map

# Combine individual maps into one figure
combined_figure <- (g_yield / g_NH3 / g_ec) +
  plot_layout(guides = "collect")

# Display the combined figure
combined_figure

#mean value of corn yield, nitrogen rate, and EC for each of the subplots
#!!!!!
library(sf)
library(ggplot2)

# Function to calculate mean
calculate_mean <- function(yield_subplots, column_name) {
  print(paste("Before aggregation for", column_name, ":"))
  print(head(yield_subplots))
  
  aggregate_result <- aggregate(yield_subplots[[column_name]], by = list(yield_subplots$subplot_id), FUN = mean, na.rm = TRUE)
  
  print(paste("After aggregation for", column_name, ":"))
  print(head(aggregate_result))
  
  return(aggregate_result)
}

# Initial data frames
print("Initial yield data:")
print(head(yield))
print("Initial nh3 data:")
print(head(nh3))
print("Initial ec data:")
print(head(ec))

# Ensure CRS is the same for all datasets
yield <- st_transform(yield, crs = st_crs(nh3))
ec <- st_transform(ec, crs = st_crs(nh3))

# Perform nearest neighbor join
yield_subplots <- st_join(yield, nh3, join = st_nearest_feature)
print("After joining yield and nh3 using nearest neighbor:")
print(head(yield_subplots))

yield_subplots <- st_join(yield_subplots, ec, join = st_nearest_feature)
print("After joining yield_subplots and ec using nearest neighbor:")
print(head(yield_subplots))

# Check for missing data
print("Checking for missing data:")
print(sum(is.na(yield_subplots$yield)))
print(sum(is.na(yield_subplots$aa_NH3)))
print(sum(is.na(yield_subplots$ec)))

# Ensure subplot_id is correctly used
yield_subplots$subplot_id <- yield_subplots$subplot_id.x

# Calculate means
mean_yield <- calculate_mean(yield_subplots, "yield")
mean_NH3 <- calculate_mean(yield_subplots, "aa_NH3")
mean_EC <- calculate_mean(yield_subplots, "ec")

# Convert to data frames
mean_yield_df <- as.data.frame(mean_yield)
mean_NH3_df <- as.data.frame(mean_NH3)
mean_EC_df <- as.data.frame(mean_EC)

# Create a combined data frame for plotting
subplot_data <- data.frame(
  subplot_id = mean_yield_df$Group.1,
  mean_yield = mean_yield_df$x,
  mean_NH3 = mean_NH3_df$x,
  mean_EC = mean_EC_df$x
)

print("Combined data frame for plotting:")
print(head(subplot_data))

# Plot the data
ggplot(subplot_data, aes(x = mean_NH3, y = mean_yield)) +
  geom_point() +
  labs(x = "Mean Nitrogen Rate", y = "Mean Corn Yield") +
  ggtitle("Scatter Plot of Mean Nitrogen Rate vs. Mean Corn Yield")

ggplot(subplot_data, aes(x = mean_EC, y = mean_yield)) +
  geom_point() +
  labs(x = "Mean Electric Conductivity", y = "Mean Corn Yield") +
  ggtitle("Scatter Plot of Mean Electric Conductivity vs. Mean Corn Yield")

ggplot(subplot_data, aes(x = mean_EC, y = mean_NH3)) +
  geom_point() +
  labs(x = "Mean Electric Conductivity", y = "Mean Nitrogen Rate") +
  ggtitle("Scatter Plot of Mean Electric Conductivity vs. Mean Nitrogen Rate")


#!!!!!!!!!!
# Fit the regression model using the yield_subplots dataset
ols_res <- lm(yield ~ mean_NH3_df + I(mean_NH3_dfˆ2) + I(mean_NH3_df * ec) + I(mean_NH3_dfˆ2 * ec), data = yield)

# Print the regression summary
summary(ols_res)
