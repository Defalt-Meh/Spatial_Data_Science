# Load necessary libraries
library(raster)
library(sf)
library(ggplot2)
library(tmap)
library(leaflet)
library(ncdf4)

# Define file paths for ERA5-Land data (replace with actual file paths)
soil_moisture_file <- "path_to_era5_land_soil_moisture.nc"
vegetation_file <- "path_to_era5_land_vegetation.nc"

# Load data
soil_moisture_nc <- nc_open(soil_moisture_file)
vegetation_nc <- nc_open(vegetation_file)

# Extract variables (adjust the names based on actual dataset)
soil_moisture_data <- ncvar_get(soil_moisture_nc, "soil_moisture_variable_name")
vegetation_data <- ncvar_get(vegetation_nc, "vegetation_variable_name")

# Extract time, latitude, and longitude
time <- ncvar_get(soil_moisture_nc, "time")
lat <- ncvar_get(soil_moisture_nc, "lat")
lon <- ncvar_get(soil_moisture_nc, "lon")

# Close NetCDF files
nc_close(soil_moisture_nc)
nc_close(vegetation_nc)

# Convert data to raster format (assuming monthly data and processing a single month for simplicity)
soil_moisture_raster <- raster(t(soil_moisture_data[, , 1]), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=crs("+proj=longlat +datum=WGS84"))
vegetation_raster <- raster(t(vegetation_data[, , 1]), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=crs("+proj=longlat +datum=WGS84"))

# Plot Soil Moisture
plot(soil_moisture_raster, main="Soil Moisture")
ggsave("soil_moisture_map.png")

# Plot Vegetation Health
plot(vegetation_raster, main="Vegetation Health")
ggsave("vegetation_health_map.png")

# Create interactive maps with Leaflet
leaflet() %>%
  addTiles() %>%
  addRasterImage(soil_moisture_raster, colors = colorRampPalette(c("blue", "green", "yellow", "red"))(100), opacity = 0.8) %>%
  addLegend("bottomright", pal = colorRampPalette(c("blue", "green", "yellow", "red"))(100), values = soil_moisture_raster, title = "Soil Moisture")

leaflet() %>%
  addTiles() %>%
  addRasterImage(vegetation_raster, colors = colorRampPalette(c("brown", "green"))(100), opacity = 0.8) %>%
  addLegend("bottomright", pal = colorRampPalette(c("brown", "green"))(100), values = vegetation_raster, title = "Vegetation Health")

# Perform time series analysis and trend detection (simplified example)
# Example: Plotting average soil moisture over time
avg_soil_moisture <- apply(soil_moisture_data, 3, mean, na.rm = TRUE)
plot(time, avg_soil_moisture, type = "l", main="Average Soil Moisture Over Time", xlab="Time", ylab="Average Soil Moisture")

# Example: Plotting average vegetation health over time
avg_vegetation <- apply(vegetation_data, 3, mean, na.rm = TRUE)
plot(time, avg_vegetation, type = "l", main="Average Vegetation Health Over Time", xlab="Time", ylab="Average Vegetation Health")
