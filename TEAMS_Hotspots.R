# Loading Libraries and Reading Data ####

# Load necessary libraries
library(readr)      # For reading CSV files
library(dplyr)      # For data manipulation
library(lubridate)  # For date-time manipulation
library(ggplot2)    # For data visualization
library(dbscan)     # For DBSCAN clustering
library(ggmap)      # For mapping
library(geosphere)  # For geographic calculations
library(scales)     # For scaling functions in ggplot2
library(tidyr)      # For data tidying 
library(gridExtra)
library(corrplot)
library(openair)


# Set seed for reproducibility
set.seed(123)

# Register Stadia Maps API key
api_key <- "45381642-ef1c-4009-90a4-e54942f742c7"
register_stadiamaps(key = api_key)

# Get the name of working directory in RProject
proj.path <- getwd()

# Read the raw hotspots data from the data folder in the RProject directory
hotspots_raw <- read_csv(file.path(proj.path, 'data', 'hotspots.csv'))

# Convert rep_date to POSIXct format
hotspots_raw$rep_date <- as.POSIXct(hotspots_raw$rep_date, format = "%Y-%m-%d %H:%M:%S")

# Add year and month columns to the data
hotspots_raw$year <- year(hotspots_raw$rep_date)
hotspots_raw$month <- month(hotspots_raw$rep_date, label = TRUE, abbr = TRUE)

# Filter the data to include only records from 2014 to 2023
hotspots <- hotspots_raw %>%
  filter(year >= 2014 & year <= 2023)

# Check the structure of the dataset
str(hotspots)

# Check the variable names
names(hotspots)

# Visualizing and Focusing on Specific Time Periods ####

# Summarize the number of fire events per year and month
# This helps us understand the distribution of fire events over time
fire_events_per_month <- hotspots %>%
  group_by(year, month) %>%
  summarise(n_events = n(), .groups = 'drop') 

# Print the summary table to examine the data
print(fire_events_per_month)

# Create a bar plot to visualize the number of fire events per month
ggplot(fire_events_per_month, aes(x = month, y = n_events)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Number of Fire Events per Month",
       x = "Month",
       y = "Number of Fire Events") +
  scale_y_continuous(labels = scales::comma) + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))

# Reshape the data to a wide format for easier visualization and comparison
fire_events_wide <- fire_events_per_month %>%
  pivot_wider(names_from = month, values_from = n_events, values_fill = 0)

# Print the wide format summary table to inspect the reshaped data
print(fire_events_wide)

# Define the color palette for the heatmap
colors <- c("gray95", "gray80", "yellow", "orange", "darkred", "black")

# Define the values for the color breaks
values <- c(0, 500, 1000, 2000, 10000, 350000)

# Create a heatmap to visualize the number of fire events per month and year
ggplot(fire_events_per_month, aes(x = month, y = factor(year), fill = n_events)) +
  geom_tile(color = "white") +
  scale_fill_gradientn(colors = colors, values = rescale(values), labels = scales::comma) +
  labs(title = "Number of Fire Events per Month and Year",
       x = "Month",
       y = "Year",
       fill = "Number of Events") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10))

# Focus on the peak fire season: May to October
# Filter the data to include only these months
hotspots_peak <- hotspots %>%
  filter(month(rep_date) %in% c(5, 6, 7, 8, 9, 10))

# Summarize the number of fire events per year and month for the peak season
fire_events_per_month_subset <- hotspots_peak %>%
  group_by(year, month) %>%
  summarise(n_events = n(), .groups = 'drop')

# Print the summary table to examine the peak season data
print(fire_events_per_month_subset)

# Reshape the data to a wide format for the peak season
fire_events_subset_wide <- fire_events_per_month_subset %>%
  pivot_wider(names_from = month, values_from = n_events, values_fill = 0)

# Print the wide format summary table for the peak season
print(fire_events_subset_wide)

# Create a heatmap to visualize the number of fire events per month and year for the peak season
ggplot(fire_events_per_month_subset, aes(x = month, y = factor(year), fill = n_events)) +
  geom_tile(color = "white") +
  scale_fill_gradientn(colors = colors, values = rescale(values), labels = scales::comma) +
  labs(title = "Number of Fire Events per Month and Year (Peak)",
       x = "Month",
       y = "Year",
       fill = "Number of Events") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10))

# Iteration Process to Find Best Parameters ####



# Prepare the data for clustering (latitude, longitude, date)
event_data <- hotspots_peak %>%
  select(lat, lon, rep_date) %>%  # Select necessary columns
  mutate(
    lat_km = lat * 111,  # Convert latitude from degrees to kilometers (1 degree ~ 111 km)
    lon_km = lon * 111,  # Convert longitude from degrees to kilometers (1 degree ~ 111 km)
    time_hours = as.numeric(difftime(rep_date, min(rep_date), units = "hours"))  # Calculate time in hours from the first recorded event
    # Note: time_hours is used to include the time component in clustering
    # This ensures events occurring within a certain time frame (20 hours) are considered together
  ) %>%
  select(lat_km, lon_km, time_hours)  # Reassign event_data to include only the necessary columns


# # # # Function to apply DBSCAN and count clusters
# apply_dbscan <- function(data, eps_value, minPts_value) {
#   db <- dbscan(data, eps = eps_value, minPts = minPts_value)  # Apply DBSCAN
#   data$event_cluster <- db$cluster  # Add cluster labels
#   num_clusters <- length(unique(data$event_cluster)) - 1  # Count clusters excluding noise (0)
#   num_noise <- sum(data$event_cluster == 0)  # Count noise points
#   cat("eps =", eps_value, ", minPts =", minPts_value, ": Number of clusters =", num_clusters, ", Number of noise points =", num_noise, "\n")
#   return(num_clusters)
# }
# 
# # Parameters for the loop
# desired_clusters <- 16956  # Official number of fires
# best_eps <- NA  # Placeholder for best eps value
# best_minPts <- NA  # Placeholder for best minPts value
# best_diff <- Inf  # Placeholder for the smallest difference
# 
# # Iterate over eps and minPts values to find the best match
# for (eps_val in seq(5, 15, by = 1)) {  # Adjust range and step size as needed
#   for (minPts_val in seq(1, 5, by = 1)) {  # Adjust range and step size as needed
#     num_clusters <- apply_dbscan(event_data, eps_val, minPts_val)
#     diff <- abs(num_clusters - desired_clusters)
#     if (diff < best_diff) {  # Check if this is the best match so far
#       best_eps <- eps_val
#       best_minPts <- minPts_val
#       best_diff <- diff
#     }
#   }
# }
# 
# # Print out the best parameters found
# cat("Best eps value:", best_eps, "\n")  # Best eps value: 12
# cat("Best minPts value:", best_minPts, "\n")  # Best minPts value: 1

# The best parameters found mean that fire events within a 12 km radius are grouped into clusters,
# and even a single event can form a cluster if it occurs alone within that distance during the given time span.

# Final Clustering and Analysis ####

# Apply DBSCAN with the best parameters found
best_eps <- 12
best_minPts <- 1
best_db <- dbscan(event_data, eps = best_eps, minPts = best_minPts)

# Add cluster labels to the original dataset
hotspots_peak$event_cluster <- best_db$cluster

# Filter out noise (cluster 0)
# Cluster 0 represents noise points, which are outliers that do not belong to any cluster.
hotspots_peak <- hotspots_peak %>%
  filter(event_cluster != 0)

# Number of clusters identified by DBSCAN
cat("Number of clusters identified by DBSCAN:", length(unique(hotspots_peak$event_cluster)), "\n")

# Output: Number of clusters identified by DBSCAN: 17400
# This tells us that there are 17,400 clusters identified in the dataset after removing noise.

# Summary of clusters
cluster_summary <- hotspots_peak %>%
  group_by(event_cluster) %>%
  summarise(
    start_date = min(rep_date),  # Get the earliest date in the cluster
    end_date = max(rep_date),  # Get the latest date in the cluster
    num_events = n(),  # Count the number of events in the cluster
    latitude = mean(lat),  # Calculate the average latitude of events in the cluster
    longitude = mean(lon)  # Calculate the average longitude of events in the cluster
  )

# Print the summary of clusters
print(cluster_summary)


# The 'cluster_summary' table provides a detailed summary of each cluster:
# - event_cluster: The ID of the cluster
# - start_date: The earliest date of an event in the cluster
# - end_date: The latest date of an event in the cluster
# - num_events: The total number of events in the cluster
# - latitude: The average latitude of the events in the cluster
# - longitude: The average longitude of the events in the cluster

# Get the top 5 largest clusters from the cluster_summary
top_clusters <- cluster_summary %>%
  arrange(desc(num_events)) %>%
  slice_head(n = 5)  # Get the top 5 clusters

# Print the top clusters
print(top_clusters)

# Output:
#   event_cluster start_date          end_date            num_events latitude longitude
# 1          4792 2018-08-04 16:56:00 2018-08-25 07:15:00      66933     53.0     -126. # Tweedsmuir Complex Fire (2018)
# 2          3120 2017-07-28 20:57:00 2017-08-13 02:04:00      61666     52.7     -124. # Elephant Hill Fire (2017)
# 3         14487 2023-09-14 03:57:00 2023-09-24 04:08:00      56406     58.7     -121. # Fort Nelson Fire (2023)
# 4          4790 2018-08-05 12:20:00 2018-08-22 20:06:00      34712     54.2     -125. # Shovel Lake Fire (2018)
# 5          3125 2017-07-23 02:47:00 2017-08-13 05:00:00      30241     51.0     -121. # Hanceville-Riske Creek Fire (2017)

# Plot the events in the largest clusters 
ggplot(hotspots_peak %>% filter(event_cluster %in% top_clusters$event_cluster), 
       aes(x = lon, y = lat, color = factor(event_cluster))) +
  geom_point(alpha = 0.5, size = 2) +
  labs(title = "Top 5 Largest Fire Clusters",
       x = "Longitude",
       y = "Latitude",
       color = "Cluster ID") +
  theme_minimal() +
  theme(legend.position = "bottom")



# Function to plot clusters on map
plot_clusters_on_map <- function(cluster_ids, data, zoom_level = 10) {
  # Filter data for the specific clusters
  cluster_data <- data %>% filter(event_cluster %in% cluster_ids)
  
  # Define the bounding box for the map based on the clusters' coordinates
  bbox <- c(left = min(cluster_data$lon) - 0.3, 
            bottom = min(cluster_data$lat) - 0.3, 
            right = max(cluster_data$lon) + 0.3, 
            top = max(cluster_data$lat) + 0.3)
  
  # Get the map from Stadia Maps API
  map <- get_stadiamap(bbox = bbox, zoom = zoom_level, maptype = "stamen_terrain")
  
  
  
  # Plot the clusters data on the map
  print(ggmap(map) +
          geom_point(data = cluster_data, aes(x = lon, y = lat, color = factor(event_cluster)), size = 2, alpha = 0.3, shape = 21) +  
          labs(title = "Matched Fire Clusters", 
               x = "Longitude", 
               y = "Latitude", 
               color = "Cluster ID") +
          theme_minimal() +
          theme(legend.position = "bottom") +
          scale_color_manual(values = c("firebrick1", "orange", "yellow")) # Custom colours
  ) 
}



# Plot the top 5 clusters individually
plot_clusters_on_map(top_clusters$event_cluster[1], hotspots_peak, zoom_level = 10)
plot_clusters_on_map(top_clusters$event_cluster[2], hotspots_peak, zoom_level = 10)
plot_clusters_on_map(top_clusters$event_cluster[3], hotspots_peak, zoom_level = 10)
plot_clusters_on_map(top_clusters$event_cluster[4], hotspots_peak, zoom_level = 10)
plot_clusters_on_map(top_clusters$event_cluster[5], hotspots_peak, zoom_level = 10)

# The plots show large areas affected by fires.
# Each fire cluster covers a big region, indicating major fire events.
# Fire events within clusters are closely packed, showing high fire activity.
# The fires follow the terrain, influenced by vegetation and topography.

# Kelowna Fire Event Analysis ####

# Known fire near Kelowna in 2023
kelowna_fire <- data.frame(
  fire_name = "McDougall Creek Fire",
  start_date = as.POSIXct("2023-08-16"),
  end_date = as.POSIXct("2023-08-30"),
  latitude = 49.8821,
  longitude = -119.4370
)

# Calculate the distance from each cluster to Kelowna fire
cluster_summary <- cluster_summary %>%
  mutate(
    distance_to_kelowna_fire = distHaversine(
      matrix(c(longitude, latitude), ncol = 2),
      matrix(c(kelowna_fire$longitude, kelowna_fire$latitude), ncol = 2)
    ) / 1000  # Convert meters to kilometers
  )

# Filter clusters that are within 50 km and have suitable dates
kelowna_clusters <- cluster_summary %>%
  filter(
    distance_to_kelowna_fire < 50 &  # Check if within 50 km
      start_date <= kelowna_fire$end_date &  # Check if cluster starts before the known fire ends
      end_date >= kelowna_fire$start_date    # Check if cluster ends after the known fire starts
  )

# Print the matching clusters
print(kelowna_clusters)


# Plot the matching clusters on the map
plot_clusters_on_map(kelowna_clusters$event_cluster, hotspots_peak, zoom_level = 10)













# Introduction to Variables ####

# The dataset contains different variables that are important to analyze fire events in British Columbia.
# These variables can be broadly categorized into the following categories:
# Location, Time, Data Collection Methods, Weather Data, Fire Indices, Other.

# 1. Location Variables ####
# These variables provide the geographical coordinates of fire events, 
# which are essential for mapping and spatial analysis.
# - `lat`: Latitude of the fire event.
# - `lon`: Longitude of the fire event.

# Summary of Latitude and Longitude
lat_range <- range(hotspots$lat, na.rm = TRUE)
lon_range <- range(hotspots$lon, na.rm = TRUE)

cat("Latitude Range:", lat_range, "\n")
cat("Longitude Range:", lon_range, "\n")

# The range of latitude and longitude shows that the hotspots dataset is inside BC boundaries.

# 2. Temporal Variables ####
# - `rep_date`: The date and time when the fire event was reported.

# Full Dataset Analysis ####
# Summary table describing each hotspots dataset by year
hotspots_df_summary <- hotspots %>%
  group_by(year) %>%
  summarise(
    start_date = min(rep_date),
    end_date = max(rep_date),
    hotspots_df_day_count = as.numeric(difftime(max(rep_date), min(rep_date), units = "days"))
  )

print(hotspots_df_summary)

# Plotting the number of days reported per year
ggplot(hotspots_df_summary, aes(x = factor(year), y = hotspots_df_day_count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Number of Days Reported per Year",
       x = "Year",
       y = "Number of Days") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# This barplot shows, how many days each year had recorded fire events.
# It helps us see if the reporting period has grown over the years.

# Peak Season Analysis ####
# Summary table for the peak season (May to October) describing each hotspots dataset by year
hotspots_peak_df_summary <- hotspots_peak %>%
  group_by(year) %>%
  summarise(
    start_date = min(rep_date),
    end_date = max(rep_date),
    hotspots_peak_day_count = as.numeric(difftime(max(rep_date), min(rep_date), units = "days")),
    num_clusters = n_distinct(event_cluster)  # Number of clusters in the peak season for each year
  )

print(hotspots_peak_df_summary)

# The table shows the earliest and latest dates of fire event reporting,
# the total number of days with recorded fire events, 
# and the number of unique fire clusters detected during the peak season for each year.

# Plotting the number of clusters per year
ggplot(hotspots_peak_df_summary, aes(x = factor(year), y = num_clusters)) +
  geom_bar(stat = "identity", fill = "darkorange") +
  labs(title = "Number of Clusters per Year (Peak Season)",
       x = "Year",
       y = "Number of Clusters") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# The barplot shows the number of unique fire clusters detected each year during the peak fire season.
# This number fluctuates each year, with noticeable peaks in 2018, 2021, and 2023.

# 3. Data Collection Methods ####
# Different sources, satellites, and sensors are used for data collection. 
# This shows data's reliability and coverage.

# 3.1 Data Sources ####
# Summary and Visualization of Sources
source_summary <- hotspots %>% 
  group_by(source) %>% 
  summarise(num_events = n())

print(source_summary)

ggplot(source_summary, aes(x = reorder(source, -num_events), y = num_events)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Number of Fire Events Reported by Each Source",
       x = "Source",
       y = "Number of Events") +
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 3.2 Satellites Used ####
# Summary and Visualization of Satellites
satellite_summary <- hotspots %>% 
  group_by(satellite) %>% 
  summarise(num_events = n())

print(satellite_summary)

ggplot(satellite_summary, aes(x = reorder(satellite, -num_events), y = num_events)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Number of Fire Events Reported by Each Satellite",
       x = "Satellite",
       y = "Number of Events") +
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 3.3 Sensors Used ####
# Summary and Visualization of Sensors
sensor_summary <- hotspots %>% 
  group_by(sensor) %>% 
  summarise(num_events = n())

print(sensor_summary)

ggplot(sensor_summary, aes(x = reorder(sensor, -num_events), y = num_events)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Number of Fire Events Reported by Each Sensor",
       x = "Sensor",
       y = "Number of Events") +
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 3.4 Specific Event Analysis ####
# Example of a Specific Fire Event: Kelowna Fire Biggest Cluster (Cluster ID: 13928)

# Filter the data for the specific cluster
event_data <- hotspots_peak %>%
  filter(event_cluster == 13928)

# Summarize the sources, satellites, and sensors used for this event
event_sources <- event_data %>%
  group_by(source) %>%
  summarise(num_events = n())

event_satellites <- event_data %>%
  group_by(satellite) %>%
  summarise(num_events = n())

event_sensors <- event_data %>%
  group_by(sensor) %>%
  summarise(num_events = n())

# Print summaries
print(event_sources)
print(event_satellites)
print(event_sensors)

# In the Kelowna fire cluster (ID: 13928), data came from various sources, satellites, and sensors. 
# Most data were provided by NASA sources and the VIIRS-I sensor, mainly using the S-NPP satellite.
# The VIIRS-I sensor is used the most for both this event and the overall data.
# The S-NPP satellite is also a key source in both cases.

# 4. Weather Data Variables ####

# Weather conditions are critical factors in the spread of fires.
# - `temp`: Temperature (°C)
# - `rh`: Relative Humidity (%)
# - `ws`: Wind Speed (km/h)
# - `wd`: Wind Direction (degrees)
# - `pcp`: Precipitation (mm)

# Function to describe each numerical column 
describe_numerical <- function(df, cols) {
  summary_list <- list()
  
  for (col in cols) {
    summary_stats <- data.frame(
      Variable = col,
      Missing_Values = sum(is.na(df[[col]])),
      Min = round(min(df[[col]], na.rm = TRUE), 2),
      Median = round(median(df[[col]], na.rm = TRUE), 2),
      Mean = round(mean(df[[col]], na.rm = TRUE), 2),
      Max = round(max(df[[col]], na.rm = TRUE), 2)
    )
    summary_list[[col]] <- summary_stats
  }
  
  summary_table <- bind_rows(summary_list)
  return(summary_table)
}

# Summary of weather data variables
summary_hotspots <- describe_numerical(hotspots_peak, c('temp', 'rh', 'ws', 'wd', 'pcp'))
print(summary_hotspots)

# Create a table with mean values for plots
monthly_avg <- hotspots_peak %>%
  group_by(year, month) %>%
  summarise(avg_temp = mean(temp, na.rm = TRUE),
            avg_rh = mean(rh, na.rm = TRUE),
            avg_ws = mean(ws, na.rm = TRUE),
            avg_pcp = mean(pcp, na.rm = TRUE),
            avg_ffmc = mean(ffmc, na.rm = TRUE),
            avg_dmc = mean(dmc, na.rm = TRUE),
            avg_dc = mean(dc, na.rm = TRUE),
            avg_isi = mean(isi, na.rm = TRUE),
            avg_bui = mean(bui, na.rm = TRUE),
            avg_fwi = mean(fwi, na.rm = TRUE),
            avg_sfc = mean(sfc, na.rm = TRUE),
            avg_tfc = mean(tfc, na.rm = TRUE),
            avg_bfc = mean(bfc, na.rm = TRUE),
            avg_hfi = mean(hfi, na.rm = TRUE),
            avg_ros = mean(ros, na.rm = TRUE),
            .groups = 'drop') 

print(monthly_avg)

# There are no missing values in the weather data.
# The minimum temperature recorded is -9.66°C, which is unusually low for fire events.
# The maximum temperature recorded is 43.88°C, which is expected during peak fire seasons.
# The relative humidity ranges from 7.00% to 100.00%, there are different weather conditions.
# Wind speeds range from 0.00 km/h to 16.47 km/h.
# There is a full range of possible wind directions.
# The precipitation data has a maximum value of 651.79 mm, needs further investigation.


# Summarize the count of subzero temperature events by year and month
subzero_temps_summary <- hotspots_peak %>%
  filter(temp < 0) %>%
  group_by(year, month) %>%
  summarise(count = n(), .groups = 'drop') %>%
  arrange(desc(year))

# Print the summary table
print(subzero_temps_summary)

# The peak month for subzero temperature events is October,
# even colder conditions do not stop fires from happening.

# Summarize the count of precipitation events > 100 mm by year and month
high_pcp_summary <- hotspots_peak %>%
  filter(pcp > 100) %>%
  group_by(year, month, event_cluster) %>%
  summarise(count = n(), .groups = 'drop') %>%
  arrange(desc(year))

# Print the summary table
print(high_pcp_summary)

# Plot the event on map
plot_clusters_on_map(14510, hotspots_peak, zoom_level = 10)


# Remove the high precipitation outlier from the dataset
hotspots_peak <- hotspots_peak %>%
  filter(!(event_cluster == 14510 & year == 2023 & month == "Jul"))

# Re-run the descriptive statistics without the outlier
summary_hotspots <- describe_numerical(hotspots_peak, c('temp', 'rh', 'ws', 'wd', 'pcp'))
print(summary_hotspots)


# 4.1 Temperature (°C) ####

# Create a boxplot to compare temperature distributions across years
ggplot(hotspots_peak, aes(x = factor(year), y = temp)) +
  geom_boxplot(fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Temperature Distribution Across Years (Peak)",
       x = "Year",
       y = "Temperature (°C)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10))


# Lineplot for temperature over time
ggplot(monthly_avg, aes(x = month, y = avg_temp, color = factor(year), group = year)) +
  geom_line(size = 0.5, alpha = 0.6, linetype = "dotted") +  # raw data
  geom_smooth(se = FALSE, method = "loess", size = 1, linetype = "solid") +  # Smoothed trend line
  labs(title = "Temperature by Month",
       x = "Month",
       y = "Temperature (°C)",
       color = "Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# 4.2 Relative Humidity (%)####

# Create a boxplot to compare relative humidity distributions across years
ggplot(hotspots_peak, aes(x = factor(year), y = rh)) +
  geom_boxplot(fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Relative Humidity Distribution Across Years (Peak)",
       x = "Year",
       y = "Relative Humidity") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10))


# Lineplot for relative humidity over time
ggplot(monthly_avg, aes(x = month, y = avg_rh, color = factor(year), group = year)) +
  geom_line(size = 0.5, alpha = 0.6, linetype = "dotted") +  # raw data
  geom_smooth(se = FALSE, method = "loess", size = 1, linetype = "solid") +  # Smoothed trend line
  labs(title = "Relative Humidity by Month",
       x = "Month",
       y = "Relative Humidity",
       color = "Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# 4.3 Wind Speed (km/h) ####

# Create a boxplot to compare wind speed distributions across years
ggplot(hotspots_peak, aes(x = factor(year), y = ws)) +
  geom_boxplot(fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Wind Speed Distribution Across Years (Peak)",
       x = "Year",
       y = "Wind Speed (km/h)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10))

# Lineplot for wind speed over time
ggplot(monthly_avg, aes(x = month, y = avg_ws, color = factor(year), group = year)) +
  geom_line(size = 0.5, alpha = 0.6, linetype = "dotted") +  # raw data
  geom_smooth(se = FALSE, method = "loess", size = 1, linetype = "solid") +  # Smoothed trend line
  labs(title = "Wind Speed by Month",
       x = "Month",
       y = "Wind Speed (km/h)",
       color = "Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
summary(hotspots_peak$ws)


# 4.4 Wind direction (degrees) 

# Visualize with Wind Rose to see most common wind direction using converted wind speed

# Convert wind speed from km/h to m/s
hotspots_peak <- hotspots_peak %>%
  mutate(ws_m_s = ws / 3.6)

windRose(mydata = hotspots_peak, ws = "ws_m_s", wd = "wd", 
         main = "Wind Rose", paddle = FALSE)




# 4.5 Precipitation (mm) ####

# Create a boxplot to compare precipitation distributions across years
ggplot(hotspots_peak, aes(x = factor(year), y = pcp)) +
  geom_boxplot(fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Precipitation Distribution Across Years (Peak)",
       x = "Year",
       y = "Precipitation (mm)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10))

# Lineplot for precipitation over time
ggplot(monthly_avg, aes(x = month, y = avg_pcp, color = factor(year), group = year)) +
  geom_line(size = 0.5, alpha = 0.6, linetype = "dotted") +  # raw data
  geom_smooth(se = FALSE, method = "loess", size = 1, linetype = "solid") +  # Smoothed trend line
  labs(title = "Precipitation by Month",
       x = "Month",
       y = "Precipitation (mm)",
       color = "Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Even after removing one extreme outlier, there are still many high precipitation values. 
# Since precipitation often has many zero entries, 
# need to handle these outliers carefully to ensure they don't skew our results.


# Define the function to remove outliers
remove_outliers <- function(data, variable) {
  q1 <- quantile(data[[variable]], 0.25, na.rm = TRUE)
  q3 <- quantile(data[[variable]], 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  lower_bound <- q1 - 1.5 * iqr
  upper_bound <- q3 + 1.5 * iqr
  data %>% filter(data[[variable]] >= lower_bound & data[[variable]] <= upper_bound)
}

# Remove outliers for temperature, relative humidity, and wind speed
hotspots_peak_clean <- hotspots_peak %>%
  remove_outliers("temp") %>%
  remove_outliers("rh") %>%
  remove_outliers("ws")

# Check the summary statistics after removing outliers
summary_clean <- describe_numerical(hotspots_peak_clean, c('temp', 'rh', 'ws', 'wd', 'pcp'))
print(summary_clean)


# Create a box plot to compare temperature distributions across years
ggplot(hotspots_peak_clean, aes(x = factor(year), y = temp)) +
  geom_boxplot(fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Temperature Distribution Across Years (Peak, Cleaned)",
       x = "Year",
       y = "Temperature (°C)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10))


# Create a box plot to compare relative humidity distributions across years
ggplot(hotspots_peak_clean, aes(x = factor(year), y = rh)) +
  geom_boxplot(fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Relative Humidity Distribution Across Years (Peak, Cleaned)",
       x = "Year",
       y = "Relative Humidity (%)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10))


# Create a box plot to compare wind speed distributions across years
ggplot(hotspots_peak_clean, aes(x = factor(year), y = ws)) +
  geom_boxplot(fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Wind Speed Distribution Across Years (Peak, Cleaned)",
       x = "Year",
       y = "Wind Speed (km/h)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10))




# 5. Fire Indices ####
# These indices are derived from weather conditions and provide insights into fire behavior and potential risks.
# - `ffmc`: Fine Fuel Moisture Code
# - `dmc`: Duff Moisture Code
# - `dc`: Drought Code
# - `isi`: Initial Spread Index
# - `bui`: Buildup Index
# - `fwi`: Fire Weather Index

# Summary of fire indices
summary_fire_indices <- describe_numerical(hotspots_peak, c('ffmc', 'dmc', 'dc', 'isi', 'bui', 'fwi'))
print(summary_fire_indices)


# 5.1 Fine Fuel Moisture Code ####
# A numeric rating of the moisture content of litter and other cured fine fuels.
# This code is an indicator of the relative ease of ignition and the flammability of fine fuel.

# The FFMC scale ranges from 0 to 101.
# 0-30: Very wet conditions; ignition is difficult.
# 30-70: Damp conditions; moderate difficulty for ignition.
# 70-85: Dry conditions; fuels are easily ignitable.
# 85-101: Very dry conditions; fuels are highly ignitable and fires can spread rapidly.


# Plot histogram for FFMC
ggplot(hotspots_peak, aes(x = ffmc)) +
  geom_histogram(binwidth = 2, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of FFMC Values", x = "FFMC", y = "Frequency") +
  scale_y_continuous(labels = comma) + 
  theme_minimal()

# Plot boxplot for FFMC
ggplot(hotspots_peak, aes(x = factor(year), y = ffmc)) +
  geom_boxplot(fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "FFMC Distribution Across Years",
       x = "Year",
       y = "FFMC") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10))

# Line plot for FFMC over time
ggplot(monthly_avg, aes(x = month, y = avg_ffmc, color = factor(year), group = year)) +
  geom_line(size = 0.5, alpha = 0.6, linetype = "dotted") +  # raw data
  geom_smooth(se = FALSE, method = "loess", size = 1, linetype = "solid") +  # Dashed line for trend
  labs(title = "FFMC by Month",
       x = "Month",
       y = "FFMC",
       color = "Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Remove outliers
hotspots_peak_clean <- hotspots_peak %>%
  remove_outliers("ffmc")

# Plot boxplot for FFMC
ggplot(hotspots_peak_clean, aes(x = factor(year), y = ffmc)) +
  geom_boxplot(fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "FFMC Distribution Across Years",
       x = "Year",
       y = "FFMC") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10))




# 5.2 Duff Moisture Code ####
# A numeric rating of the average moisture content
# of loosely compacted organic layers of moderate depth. 
# This code gives an indication of fuel consumption in moderate duff layers 
# and medium-size woody material.

# It ranges typically from 0 to over 200,
# higher values indicate drier conditions and a higher fire risk.

# Values below 20: Low fire danger, duff layers are wet, and ignition is unlikely.
# Values between 21 and 40: Moderate fire danger, duff layers start drying.
# Values between 41 and 100: High fire danger, the duff layer is dry, prone to ignition.
# Values above 100: Extreme fire danger, the duff layer is very dry, and ignition is very likely with the potential for intense fires.



# Plot histogram for DMC
ggplot(hotspots_peak, aes(x = dmc)) +
  geom_histogram(binwidth = 2, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of DMC Values", x = "DMC", y = "Frequency") +
  scale_y_continuous(labels = comma) + 
  theme_minimal()

# Plot boxplot for DMC
ggplot(hotspots_peak, aes(x = factor(year), y = dmc)) +
  geom_boxplot(fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "DMC Distribution Across Years",
       x = "Year",
       y = "DMC") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10))

# Line plot for DMC over time
ggplot(monthly_avg, aes(x = month, y = avg_dmc, color = factor(year), group = year)) +
  geom_line(size = 0.5, alpha = 0.6, linetype = "dotted") +  # raw data
  geom_smooth(se = FALSE, method = "loess", size = 1, linetype = "solid") +  # Dashed line for trend
  labs(title = "DMC by Month",
       x = "Month",
       y = "DMC",
       color = "Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))






# Remove outliers
hotspots_peak_clean <- hotspots_peak %>%
  remove_outliers("dmc")

# Plot boxplot for DMC
ggplot(hotspots_peak_clean, aes(x = factor(year), y = dmc)) +
  geom_boxplot(fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "DMC Distribution Across Years",
       x = "Year",
       y = "DMC") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10))




# 5.3 Drought Code ####
# A numeric rating of the average moisture content of deep, compact organic layers.
# This code is a useful indicator of seasonal drought effects on forest fuels
# and the amount of smoldering in deep duff layers and large logs.


# 0-100: Indicates wet conditions with low fire potential.
# 100-300: Moderate drought conditions with increasing fire potential.
# 300-500: High drought conditions, leading to high fire risk.
# 500+: Indicates extreme drought, posing a very high fire risk and potential for intense, prolonged burning.



# Plot histogram for DC
ggplot(hotspots_peak, aes(x = dc)) +
  geom_histogram(binwidth = 20, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of DC Values", x = "DC", y = "Frequency") +
  scale_y_continuous(labels = comma) + 
  theme_minimal()

# Plot boxplot for DC
ggplot(hotspots_peak, aes(x = factor(year), y = dc)) +
  geom_boxplot(fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "DC Distribution Across Years",
       x = "Year",
       y = "DC") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10))

# Line plot for DC over time
ggplot(monthly_avg, aes(x = month, y = avg_dc, color = factor(year), group = year)) +
  geom_line(size = 0.5, alpha = 0.6, linetype = "dotted") +  # raw data
  geom_smooth(se = FALSE, method = "loess", size = 1, linetype = "solid") +  # Dashed line for trend
  labs(title = "DC by Month",
       x = "Month",
       y = "DC",
       color = "Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# DC is important because it helps understand how dry the forest floor is
# and the chance for deep-burning fires.
# Higher DC values usually mean longer dry periods and can show how severe fire seasons might be.





# Remove outliers
hotspots_peak_clean <- hotspots_peak %>%
  remove_outliers("dc")

# Plot boxplot for DC
ggplot(hotspots_peak_clean, aes(x = factor(year), y = dc)) +
  geom_boxplot(fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "DC Distribution Across Years",
       x = "Year",
       y = "DC") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10))



# 5.4 Compare indices trends to timeline of fire events####

# Analyze the trends of key indices (FFMC DMC DC) and the number of fire events.
# This can show how environmental conditions, indicated by these indices, correlate with the frequency of fires.

# Merge the dataframes
monthly_data <- merge(monthly_avg, fire_events_per_month, by = c("year", "month"))

# Convert 'month' column from ordered factor to numeric
monthly_data$month <- as.numeric(monthly_data$month)

# Create the 'Date' column using 'year' and 'month'
monthly_data$Date <- as.Date(paste(monthly_data$year, monthly_data$month, "01", sep = "-"))

# Normalize the fire numbers data 
max_events <- max(monthly_data$n_events, na.rm = TRUE)

monthly_data <- monthly_data %>%
  mutate(norm_n_events = n_events / max_events)

# Maximum values for scaling
max_ffmc <- max(monthly_data$avg_ffmc, na.rm = TRUE)
max_dmc <- max(monthly_data$avg_dmc, na.rm = TRUE)
max_dc <- max(monthly_data$avg_dc, na.rm = TRUE)



# Plot FFMC and Fire Counts
ggplot(monthly_data, aes(x = Date)) +
  geom_line(aes(y = avg_ffmc, color = "FFMC"), size = 1) +
  geom_bar(aes(y = norm_n_events * max_ffmc, fill = "Fire Occurrences"), stat = "identity", color = "black", alpha = 0.6) +
  scale_y_continuous(
    name = "FFMC",
    sec.axis = sec_axis(~ . * 1000 / max_ffmc * max(monthly_data$n_events) / 1000, 
                        name = "Number of Fire Occurrences", labels = comma)) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  labs(title = "Monthly Trends of FFMC and Fire Occurrences",
       x = "Year",
       color = "Index",
       fill = "Index") +
  scale_color_manual(values = c("FFMC" = "lightblue")) +
  scale_fill_manual(values = c("Fire Occurrences" = "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot DMC and Fire Counts
ggplot(monthly_data, aes(x = Date)) +
  geom_line(aes(y = avg_dmc, color = "DMC"), size = 1) +
  geom_bar(aes(y = norm_n_events * max_dmc, fill = "Fire Occurrences"), stat = "identity", color = "black", alpha = 0.6) +
  scale_y_continuous(
    name = "DMC",
    sec.axis = sec_axis(~ . * 1000 / max_dmc * max(monthly_data$n_events) / 1000, 
                        name = "Number of Fire Occurrences", labels = comma)) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  labs(title = "Monthly Trends of DMC and Fire Occurrences",
       x = "Year",
       color = "Index",
       fill = "Index") +
  scale_color_manual(values = c("DMC" = "lightblue")) +
  scale_fill_manual(values = c("Fire Occurrences" = "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot DC and Fire Counts
ggplot(monthly_data, aes(x = Date)) +
  geom_line(aes(y = avg_dc, color = "DC"), size = 1) +
  geom_bar(aes(y = norm_n_events * max_dc, fill = "Fire Occurrences"), stat = "identity", color = "black", alpha = 0.6) +
  scale_y_continuous(
    name = "DC",
    sec.axis = sec_axis(~ . * 1000 / max_dc * max(monthly_data$n_events) / 1000, 
                        name = "Number of Fire Occurrences", labels = comma)) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  labs(title = "Monthly Trends of DC and Fire Occurrences",
       x = "Year",
       color = "Index",
       fill = "Index") +
  scale_color_manual(values = c("DC" = "lightblue")) +
  scale_fill_manual(values = c("Fire Occurrences" = "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




# 5.5 Initial Spread Index ####
# A numerical rating of the expected rate of fire spread
# based on wind speed, temperature, and fine fuel moisture content. 
# ISI is crucial for understanding how quickly a fire can spread once it has ignited.

# 0-3: Low spread potential. Fires will spread slowly and are relatively easy to control.
# 4-7: Moderate spread potential. Fires spread more quickly and may require more effort to control.
# 8-12: High spread potential. Fires spread rapidly and can be difficult to control.
# 13-19: Very high spread potential. Fires spread very rapidly and are challenging to control.
# 20+: Extreme spread potential. Fires spread uncontrollably and can be extremely dangerous.



# Plot histogram for ISI
ggplot(hotspots_peak, aes(x = isi)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of ISI Values", x = "ISI", y = "Frequency") +
  scale_y_continuous(labels = scales::comma) + 
  theme_minimal()

# Plot boxplot for ISI
ggplot(hotspots_peak, aes(x = factor(year), y = isi)) +
  geom_boxplot(fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "ISI Distribution Across Years",
       x = "Year",
       y = "ISI") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10))

# Line plot for ISI over time
ggplot(monthly_avg, aes(x = month, y = avg_isi, color = factor(year), group = year)) +
  geom_line(size = 0.5, alpha = 0.6, linetype = "dotted") +  # raw data
  geom_smooth(se = FALSE, method = "loess", size = 1, linetype = "solid") +  # Smoothed trend line
  labs(title = "ISI by Month",
       x = "Month",
       y = "ISI",
       color = "Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Analyses of the extreme outliers of 2014, values greater than 40


# Filter out entries with ISI values greater than 40
isi_outliers <- hotspots_peak %>%
  filter(isi >= 40)

# Check the number of outliers
dim(isi_outliers)
isi_outliers %>% 
  count(event_cluster) %>% 
  arrange(desc(n))
# Most isi outliers come from clusters 401 and 130


plot_clusters_on_map(401, hotspots_peak, zoom_level = 10)
plot_clusters_on_map(130, hotspots_peak, zoom_level = 10)


# The data from July 2014, shows very high ISI values at a specific location in British Columbia.
# This matches the date of the Mount McAllister fire, a large wildfire that burned over 20,000 hectares.
# The weather that day included high temperatures, low humidity, and strong winds, which made the fire spread quickly.
# These conditions explain the high ISI values, indicating a high potential for severe fire behavior.


# Remove outliers
hotspots_peak_clean <- hotspots_peak %>%
  remove_outliers("isi")

# Plot boxplot for ISI
ggplot(hotspots_peak_clean, aes(x = factor(year), y = isi)) +
  geom_boxplot(fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "ISI Distribution Across Years",
       x = "Year",
       y = "ISI") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10))

# After removing outliers, the cleaned ISI boxplots show more consistent distributions across the years. 
# The extreme ISI values in 2014 were linked to the Mount McAllister fire.
# The cleaned data provides a clearer view of typical ISI values,
# better representing fire spread potential across different years.


# 5.6 Buildup Index ####
# A numerical rating of the total amount of fuel available for combustion.
# It is derived from the Duff Moisture Code (DMC) and the Drought Code (DC)

# Low: 0-40
# Moderate: 41-80
# High: 81-120
# Extreme: 121 and above

ggplot(hotspots_peak, aes(x = bui)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of BUI Values", x = "BUI", y = "Frequency") +
  scale_y_continuous(labels = scales::comma) + 
  theme_minimal()

ggplot(hotspots_peak, aes(x = factor(year), y = bui)) +
  geom_boxplot(fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "BUI Distribution Across Years",
       x = "Year",
       y = "BUI") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10))


ggplot(monthly_avg, aes(x = month, y = avg_bui, color = factor(year), group = year)) +
  geom_line(size = 0.5, alpha = 0.6, linetype = "dotted") +  # raw data
  geom_smooth(se = FALSE, method = "loess", size = 1, linetype = "solid") +  # Smoothed trend line
  labs(title = "BUI by Month",
       x = "Month",
       y = "BUI",
       color = "Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




# Remove outliers
hotspots_peak_clean <- hotspots_peak %>%
  remove_outliers("bui")

# Plot boxplot for BUI
ggplot(hotspots_peak_clean, aes(x = factor(year), y = bui)) +
  geom_boxplot(fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "BUI Distribution Across Years",
       x = "Year",
       y = "BUI") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10))


# 5.7 Fire Weather Index ####
# A numeric rating of fire intensity. It is based on the ISI and the BUI, 
# and is used as a general index of fire danger throughout the forested areas of Canada.

# Low (0-5): Minimal fire danger.
# Moderate (6-12): Fires can start from most accidental causes, but the spread is slow.
# High (13-20): Fires can start easily and spread rapidly.
# Very High (21-30): Fires will start very easily, spread rapidly, and burn intensely.
# Extreme (31+): Fires start and spread quickly, and are intense and challenging to control.

ggplot(hotspots_peak, aes(x = fwi)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of FWI Values", x = "FWI", y = "Frequency") +
  scale_y_continuous(labels = scales::comma) + 
  theme_minimal()

ggplot(hotspots_peak, aes(x = factor(year), y = fwi)) +
  geom_boxplot(fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "FWI Distribution Across Years",
       x = "Year",
       y = "FWI") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10))


ggplot(monthly_avg, aes(x = month, y = avg_fwi, color = factor(year), group = year)) +
  geom_line(size = 0.5, alpha = 0.6, linetype = "dotted") +  # raw data
  geom_smooth(se = FALSE, method = "loess", size = 1, linetype = "solid") +  # Smoothed trend line
  labs(title = "FWI by Month",
       x = "Month",
       y = "FWI",
       color = "Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




# Analyses of the extreme outliers of 2014, values greater than 75


# Filter out entries with fwi values greater than 75
fwi_outliers <- hotspots_peak %>%
  filter(fwi >= 75)

# Check the number of outliers
dim(fwi_outliers)
fwi_outliers %>% 
  count(event_cluster) %>% 
  arrange(desc(n))
# Most fwi outliers come from clusters 401 and 130, same as isi outliers


plot_clusters_on_map(401, hotspots_peak, zoom_level = 10)
plot_clusters_on_map(130, hotspots_peak, zoom_level = 10)




# Remove outliers
hotspots_peak_clean <- hotspots_peak %>%
  remove_outliers("fwi")

# Plot boxplot for FWI
ggplot(hotspots_peak_clean, aes(x = factor(year), y = fwi)) +
  geom_boxplot(fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "FWI Distribution Across Years",
       x = "Year",
       y = "FWI") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10))
# 
# After removing outliers, the FWI boxplots show more consistent distributions across the years. 
# Median FWI values generally fall between 20 and 40. Reducing outliers, especially from the 2014 Mount McAllister fire, 
# provides a clearer view of typical fire weather conditions and trends over the years.

# 5.8 Compare indices trends to timeline of fire events####

# Analyze the trends of key indices (ISI BUI FWI) and the number of fire events.
# This can show how environmental conditions, indicated by these indices, correlate with the frequency of fires.

# Use the previously normalized monthly_data table

# Maximum values for scaling
max_isi <- max(monthly_data$avg_isi, na.rm = TRUE)
max_bui <- max(monthly_data$avg_bui, na.rm = TRUE)
max_fwi <- max(monthly_data$avg_fwi, na.rm = TRUE)



# Plot ISI and Fire Counts
ggplot(monthly_data, aes(x = Date)) +
  geom_line(aes(y = avg_isi, color = "ISI"), size = 1) +
  geom_bar(aes(y = norm_n_events * max_isi, fill = "Fire Occurrences"), stat = "identity", color = "black", alpha = 0.6) +
  scale_y_continuous(
    name = "ISI",
    sec.axis = sec_axis(~ . * 1000 / max_isi * max(monthly_data$n_events) / 1000, 
                        name = "Number of Fire Occurrences", labels = comma)) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  labs(title = "Monthly Trends of ISI and Fire Occurrences",
       x = "Year",
       color = "Index",
       fill = "Index") +
  scale_color_manual(values = c("ISI" = "skyblue")) +
  scale_fill_manual(values = c("Fire Occurrences" = "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot BUI and Fire Counts
ggplot(monthly_data, aes(x = Date)) +
  geom_line(aes(y = avg_bui, color = "BUI"), size = 1) +
  geom_bar(aes(y = norm_n_events * max_bui, fill = "Fire Occurrences"), stat = "identity", color = "black", alpha = 0.6) +
  scale_y_continuous(
    name = "BUI",
    sec.axis = sec_axis(~ . * 1000 / max_bui * max(monthly_data$n_events) / 1000, 
                        name = "Number of Fire Occurrences", labels = comma)) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  labs(title = "Monthly Trends of BUI and Fire Occurrences",
       x = "Year",
       color = "Index",
       fill = "Index") +
  scale_color_manual(values = c("BUI" = "skyblue")) +
  scale_fill_manual(values = c("Fire Occurrences" = "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot FWI and Fire Counts
ggplot(monthly_data, aes(x = Date)) +
  geom_line(aes(y = avg_fwi, color = "FWI"), size = 1) +
  geom_bar(aes(y = norm_n_events * max_fwi, fill = "Fire Occurrences"), stat = "identity", color = "black", alpha = 0.6) +
  scale_y_continuous(
    name = "FWI",
    sec.axis = sec_axis(~ . * 1000 / max_fwi * max(monthly_data$n_events) / 1000, 
                        name = "Number of Fire Occurrences", labels = comma)) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  labs(title = "Monthly Trends of FWI and Fire Occurrences",
       x = "Year",
       color = "Index",
       fill = "Index") +
  scale_color_manual(values = c("FWI" = "skyblue")) +
  scale_fill_manual(values = c("Fire Occurrences" = "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# The plots show a clear link between indices and the number of fires.
# When there are more fires, the FWI is also higher.

# It's important to remember that while high FWI values mean conditions are good for fires,
# they don't indicate fire presence.
# Other factors like human activities or lightning strikes are needed to start fires in these conditions.

# Also, while the FWI helps predict how severe fires might be,
# it can't tell us exactly how many fires will happen in a season.
# For example, the FWI was lower in 2018 than in 2017, but 2018 still had some major fires. 
# This shows that indices can indicate fire conditions but don't predict the exact number of fires.





# 6. Other Variables ####
# These variables include additional factors that can influence fire events, such as fuel type and rate of spread.
# - `fuel`: Fuel Type
# - `ros`: Rate of Spread (modelled)
# - `hfi`: Head Fire Intensity (modelled)



# 6.1 Fuel Type  ####

# D1: Deciduous trees (leafless, early spring to fall)
# C2, C3, C4, C5, C7: Various types of coniferous trees
# O1, O1a, O1b: Grass or herbaceous vegetation
# M1, M1M2, M2, M2_25, M2_35, M2_50, M2_65: Mixedwood or transitional vegetation
# bog: Wetland areas with peatland vegetation
# water: Water bodies
# urban: Urban areas
# non_fuel: Areas with no significant vegetation (rock, gravel)
# low_veg: Areas with low vegetation (possibly recently burned or cleared)
# farm: Agricultural areas
# D2: Deadfall or downed wood

# Group by fuel and summarize
fuel_counts <- hotspots_peak %>%
  group_by(fuel) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Create a bar plot for the fuel column
ggplot(fuel_counts, aes(x = reorder(fuel, -count), y = count)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Fuel Types", x = "Fuel Type", y = "Count") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




# Summary of other variables
summary_other_variables <- describe_numerical(hotspots_peak, c('ros', 'hfi'))
print(summary_other_variables)




# 6.2 Rate of Spread ####
# The predicted speed of the fire at the front or head of the fire (where the fire moves fastest),
# and takes into account both crowning and spotting. 
# It is measured in metres per minute and is based on the Fuel Type, Initial Spread Index, Buildup Index, 
# and several fuel-specific parameters such as phenological state (leafless or green) in deciduous trees, 
# crown base height in coniferous trees, and percent curing in grasses.
# The scale ranges from 0 to over 20 m/min in the dataset.



ggplot(hotspots_peak, aes(x = ros)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution Rate of Spread at Fire Hotspots",
       x = "ROS (m/min)",
       y = "Frequency") +
  scale_y_continuous(labels = comma) + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))


ggplot(hotspots_peak, aes(x = factor(year), y = ros)) +
  geom_boxplot(fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Rate of Spread Distribution Across Years",
       x = "Year",
       y = "ROS (m/min)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10))




# Remove outliers
hotspots_peak_clean <- hotspots_peak %>%
  remove_outliers("ros")

# Plot boxplot for ROS
ggplot(hotspots_peak_clean, aes(x = factor(year), y = ros)) +
  geom_boxplot(fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "ROS Distribution Across Years",
       x = "Year",
       y = "ROS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10))





# 6.3 Head Fire Intencity (modelled) ####
# Measures the intensity or energy output of a fire at its front (head).
# HFI is measured in kilowatts per metre (kW/m) of the fire front and is calculated based on the Rate of Spread (ROS)
# and the Total Fuel Consumption (TFC).

# Low (0-500 kW/m): Fires are relatively easy to control and generally cause limited damage.
# Moderate (500-2000 kW/m): Fires can be challenging to control, requiring more resources and effort.
# High (2000-4000 kW/m): Fires are intense and difficult to manage, often requiring aerial firefighting resources.
# Very High (4000-10,000 kW/m): Fires are extremely intense and nearly impossible to control, posing significant risk to life and property.
# Extreme (10,000+ kW/m): Fires exhibit explosive behavior and can cause catastrophic damage.

# Helps predict fire destructiveness, allocate resources, warn or evacuate public.

# Plot histogram for HFI
ggplot(hotspots_peak, aes(x = hfi)) +
  geom_histogram(binwidth = 1000, fill = "skyblue", color = "black", alpha = 0.7) + # Each bin represent a range of 1000 HFI units.
  labs(title = "Distribution of HFI Values",
       x = "HFI (kW/m)",
       y = "Frequency") +
  scale_y_continuous(labels = scales::comma) + 
  scale_x_continuous(breaks = seq(0, 75000, 10000)) +
  theme_minimal()

ggplot(hotspots_peak, aes(x = factor(year), y = hfi)) +
  geom_boxplot(fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "HFI Distribution Across Years",
       x = "Year",
       y = "HFI (kW/m)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10))






ggplot(monthly_avg, aes(x = month, y = avg_hfi, color = factor(year), group = year)) +
  geom_line(size = 0.5, alpha = 0.6, linetype = "dotted") +  # raw data
  geom_smooth(se = FALSE, method = "loess", size = 1, linetype = "solid") +  # Smoothed trend line
  labs(title = "HFI by Month",
       x = "Month",
       y = "HFI (kW/m)",
       color = "Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Different years show a lot of differences in HFI values. 
# For example, 2014 and 2021 had higher peaks, meaning there was more intense fire activity in those years. 
# 2020 had lower HFI values,  there were milder fire conditions or better fire control efforts.
# 
# The plots also show how HFI can change from month to month within a single fire season.
# Weather conditions, temperature, humidity, and wind speed,  can affect how fires behave.


# Analyze the trends of HFI and the number of fire events.

# Use the previously normalized monthly_data table

# Maximum values for scaling
max_hfi <- max(monthly_data$avg_hfi, na.rm = TRUE)


# Plot HFI and Fire Counts
ggplot(monthly_data, aes(x = Date)) +
  geom_line(aes(y = avg_hfi, color = "HFI"), size = 1) +
  geom_bar(aes(y = norm_n_events * max_hfi, fill = "Fire Occurrences"), stat = "identity", color = "black", alpha = 0.6) +
  scale_y_continuous(
    name = "HFI",
    sec.axis = sec_axis(~ . * 1000 / max_hfi * max(monthly_data$n_events) / 1000, 
                        name = "Number of Fire Occurrences", labels = comma)) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  labs(title = "Monthly Trends of HFI and Fire Occurrences",
       x = "Year",
       color = "Index",
       fill = "Index") +
  scale_color_manual(values = c("HFI" = "lightblue")) +
  scale_fill_manual(values = c("Fire Occurrences" = "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




# Analyses of the extreme outliers of 2014, 2018, 2021  and 2023, values greater than 75000


# Filter out entries with HFI values greater than 75000
hfi_outliers <- hotspots_peak %>%
  filter(hfi >= 75000)

# Check the number of outliers
dim(hfi_outliers)
hfi_outliers %>% 
  count(event_cluster) %>% 
  arrange(desc(n))
# Most hfi outliers come from clusters 4956, 4844, 13837 and 4887

# See the details of the events with high hfi
cluster_summary %>%
  filter(event_cluster %in% c(4956, 4844, 13837, 4887))


plot_clusters_on_map(4956, hotspots_peak, zoom_level = 10) # 19 Jul 2018, Snowy Mountain Fire and other fires in the Okanagan region.
plot_clusters_on_map(4844, hotspots_peak, zoom_level = 10) # 12 Aug 2018
plot_clusters_on_map(4887, hotspots_peak, zoom_level = 10) # 11 Aug 2018,  Placer Mountain Fire area, a notable fire from 2018.
plot_clusters_on_map(13837, hotspots_peak, zoom_level = 10) # 19 Aug 2023, area close to cluster 4887.

# the coordinates of these clusters match with known historic fire data in 2018 and 2023.

# Remove outliers
hotspots_peak_clean <- hotspots_peak %>%
  remove_outliers("hfi")

# Plot boxplot for HFI
ggplot(hotspots_peak_clean, aes(x = factor(year), y = hfi)) +
  geom_boxplot(fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "HFI Distribution Across Years",
       x = "Year",
       y = "HFI") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10))






# SFC TFC and BFC are a part of BORFIRE model.
# The Boreal Fire Effects (BORFIRE) model estimates carbon emissions from wildfires in Canada. 
# It calculates preburn fuel loads for various forest components using the Canadian Forest Fire Weather Index (FWI) System.
# This model considers surface fuel consumption, 
# which includes organic soil, surface litter, and dead wood, to estimate fire emissions accurately.




"sfc" # Surface Fuel Consumption (modelled)
# SFC represents the amount of fuel consumed by surface fires, 
# including organic soil (duff), surface litter, dead and downed woody debris.
# Calculated using indices of the FWI System (BUI and DC).

# Range typically varies significantly depending on the forest type and weather conditions.

print(monthly_avg)

ggplot(hotspots_peak, aes(x = sfc)) +
  geom_histogram(binwidth = 0.5, fill = "steelblue", color = "black") +
  labs(title = "Distribution of Surface Fuel Consumption (SFC)",
       x = "SFC (kg/m²)",
       y = "Frequency") +
  theme_minimal() + 
  scale_y_continuous(labels = comma)






"tfc" # Total Fuel Consumption (modelled)
# TFC is the total amount of fuel consumed by both surface and crown fires. 
# It includes all components in SFC plus the consumption of overstory fuels (canopy).
# Estimates the overall carbon emissions from a fire event.
# Range varies widely based on the intensity and spread of the fire, 
# as well as the initial fuel load and moisture content.



ggplot(hotspots_peak, aes(x = tfc)) +
  geom_histogram(binwidth = 0.5, fill = "steelblue", color = "black") +
  labs(title = "Distribution of Total Fuel Consumption (TFC)",
       x = "TFC (kg/m²)",
       y = "Frequency") +
  theme_minimal() +
  scale_y_continuous(labels = comma)
  





"bfc" # Borfire Fuel Consumption (modelled)      
# BFC refers to a specific component of the BORFIRE model focused on estimating carbon emissions from boreal forest fires.

summary(hotspots_peak$bfc)

# Explore huge bfc values
huge_bfc <- hotspots_peak %>%
  filter(bfc > 1000)

# Print the resulting data frame to inspect the entries with huge bfc values

huge_bfc %>%
  group_by(year, month) %>%
  summarise(n_entries = n()) %>%
  arrange(year, month)

summary(huge_bfc$bfc)

# Possibly the information here is incorrectly recorded

# The Max  value in the dataset appeart to be out of valid range

# Remove NA values from BFC data
hotspots_peak_BFC <- hotspots_peak %>%
  filter(!is.na(bfc))



# Calculate IQR for BFC
IQR_bfc <- IQR(hotspots_peak_BFC$bfc, na.rm = TRUE)
Q1_bfc <- quantile(hotspots_peak_BFC$bfc, 0.25, na.rm = TRUE)
Q3_bfc <- quantile(hotspots_peak_BFC$bfc, 0.75, na.rm = TRUE)

# Define lower and upper bounds for outliers
upper_bound_bfc <- Q3_bfc + 1.5 * IQR_bfc

# Filter out the outliers
hotspots_peak_BFC <- hotspots_peak_BFC %>%
  filter(bfc <= upper_bound_bfc)

# Summary of filtered data
summary(hotspots_peak_BFC$bfc)


# Plot Histogram of BFC Data Without Handling Outliers
ggplot(hotspots_peak_BFC, aes(x = bfc)) +
  geom_histogram(binwidth = 0.5, fill = "steelblue", color = "black") +
  labs(title = "Distribution of Boreal Fire Consumption (BFC)",
       x = "BFC",
       y = "Frequency") +
  theme_minimal() + 
  scale_y_continuous(labels = scales::comma)














"cfb" #  Crown Fraction Burned (%) at hotspot location (modelled)
# The CFB index measures the proportion of tree crowns that are consumed by a wildfire. 
# It ranges from 0 to 100%, 
# where 0% means no crown fire activity and 100% indicates 
# that the entire tree crowns in the area have been burned.

ggplot(hotspots_peak, aes(x = cfb)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of CFB at Fire Hotspots",
       x = "CFB",
       y = "Frequency") +
  scale_y_continuous(labels = comma) + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))

# This histogram is dominated by zero values, making it difficult to identify clear trends.


# Count missing values in the CFB column
sum(is.na(hotspots_peak$cfb))


# Identify years with the most missing values
missing_cfb <- hotspots_peak %>%
  group_by(year) %>%
  summarise(
    total_count = n(),
    missing_count = sum(is.na(cfb)),
    missing_percentage = (missing_count / total_count) * 100
  ) %>%
  arrange(desc(missing_percentage))

missing_cfb

# 2015 has the highest percentage of missing values at 56.1%.
# 2017 has 54.9%.
# 2016 has 46.8%.
# 2018-2024 and 2014 have no missing values

zero_cfb <- hotspots_peak %>%
  group_by(year) %>%
  summarise(
    total_count = n(),
    zero_count = sum(cfb == 0, na.rm = TRUE),
    zero_percentage = (zero_count / total_count) * 100
  ) %>%
  arrange(desc(zero_percentage))

zero_cfb

# 2019 has the highest percentage of zero values at 92.7%.
# 2020, 2021, 2022, and 2023 also have high values ranging from 65.4% to 77.8%.
# 2015-2018, and 2024 have almost no zero values.

# Data collection for this variable lacks quality.

# Filter out zero and na values to check the CFB values when it was properly recorded.

hotspots_peak_CFB <- hotspots_peak %>%
  filter(cfb > 0 & !is.na(cfb))

# Create the histogram for CFB
ggplot(hotspots_peak_CFB, aes(x = cfb)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
  labs(title = "Distribution of Crown Fraction Burned (CFB) at Fire Hotspots",
       x = "CFB",
       y = "Frequency") +
  theme_minimal() + 
  scale_y_continuous(labels = scales::comma)

# The histogram shows a significant peak at the value of 100%,
# indicating that in many fire events with non zero recorded CFB values,
# the entire tree crown is completely burned.
# This suggests that when fires do occur,
# they tend to be quite severe, affecting the full canopy of trees.






"age" # Age of Trees (in days)


# Count missing values in the Age column
sum(is.na(hotspots_peak$age))


# Identify years with the most missing values
missing_age <- hotspots_peak %>%
  group_by(year) %>%
  summarise(
    total_count = n(),
    missing_count = sum(is.na(age)),
    missing_percentage = (missing_count / total_count) * 100
  ) %>%
  arrange(desc(missing_percentage))

missing_age


zero_age <- hotspots_peak %>%
  group_by(year) %>%
  summarise(
    total_count = n(),
    zero_count = sum(age == 0, na.rm = TRUE),
    zero_percentage = (zero_count / total_count) * 100
  ) %>%
  arrange(desc(zero_percentage))

zero_age

# # The years with valid amount of observations are 2014 2015 2016. 
# The variable almost doesnt have meaningfull values in 2017 and 2018. 
# The more resent hotspots have stopped recording this variable altogether.

hotspots_peak_age <- hotspots_peak %>%
  filter(age > 0 & !is.na(age))

# Create the histogram for Age
ggplot(hotspots_peak_age, aes(x = age)) +
  geom_histogram(binwidth = 100, fill = "steelblue", color = "black") +
  labs(title = "Distribution of Tree Age at Fire Hotspots",
       x = "Age",
       y = "Frequency") +
  theme_minimal() + 
  scale_y_continuous(labels = scales::comma)






"estarea" # approximate burned area based on historical average area burned per hotspot by
# agency and fuel type


# Count missing values in the estarea
sum(is.na(hotspots_peak$estarea))


# Identify years with the most missing values
missing_estarea <- hotspots_peak %>%
  group_by(year) %>%
  summarise(
    total_count = n(),
    missing_count = sum(is.na(estarea)),
    missing_percentage = (missing_count / total_count) * 100
  ) %>%
  arrange(desc(year))

missing_estarea

# 2023 2022 and 2021 do not have this variable.
# 2015 2016 2018 2019 has around 50% missing values
# 2020 85 % missing values


zero_estarea <- hotspots_peak %>%
  group_by(year) %>%
  summarise(
    total_count = n(),
    zero_count = sum(estarea == 0, na.rm = TRUE),
    zero_percentage = (zero_count / total_count) * 100
  ) %>%
  arrange(desc(year))

zero_estarea

# Additionally to the missing values already described, 2017 has almost 50% zero values
# The only year with values present is 2014
# The more resent hotspots have stopped recording this variable altogether.
# There has probably been a change in reporting teckniques with this variable,
# it may be not suitable for meaningful analyses for the 10 year period


"polyid"   


# Count missing values in the estarea
sum(is.na(hotspots_peak$polyid))


# Identify years with the most missing values
missing_polyid <- hotspots_peak %>%
  group_by(year) %>%
  summarise(
    total_count = n(),
    missing_count = sum(is.na(polyid)),
    missing_percentage = (missing_count / total_count) * 100
  ) %>%
  arrange(desc(year))

missing_polyid

# 2018-2023 do not have this variable.



zero_polyid <- hotspots_peak %>%
  group_by(year) %>%
  summarise(
    total_count = n(),
    zero_count = sum(polyid == 0, na.rm = TRUE),
    zero_percentage = (zero_count / total_count) * 100
  ) %>%
  arrange(desc(year))

zero_polyid

# Additionally 2017 has almost all zero values

# Initially the variable was used to identify specific event,
# but recently the reporting process has changed
# Only meaningful information in 2014 2015 and 2016, 
# it may be not suitable for meaningful analyses for the 10 year period





"pcuring" # Percent Curing
# This indicates the proportion of grass and other fuels (non-woody) that are in a cured (dried) state, ready to burn.
# For example, if pcuring is 80%, it means that 80% of the vegetation is dead or dry enough to ignite and sustain fire.

# The FWI System incorporates pcuring as part of its calculations to determine indices 
# like the Fine Fuel Moisture Code (FFMC) and the Initial Spread Index (ISI).
# These indices are used to estimate the ease of ignition and 
# the rate of spread for fires in grasslands and similar fuel types.

# Count missing values in the pcuring
sum(is.na(hotspots_peak$pcuring))


# Identify years with the most missing values
missing_pcuring <- hotspots_peak %>%
  group_by(year) %>%
  summarise(
    total_count = n(),
    missing_count = sum(is.na(pcuring)),
    missing_percentage = (missing_count / total_count) * 100
  ) %>%
  arrange(desc(year))

missing_pcuring

# 2023 does not have this variable.


# Exclude the year 2023 where pcuring is missing
hotspots_peak_pcuring <- hotspots_peak %>%
  filter(!(year == 2023)) %>%
  filter(!is.na(pcuring))

# Check the summary of the filtered data
summary(hotspots_peak_pcuring$pcuring)

# Checking for values greater than 100%
pcuring_above_100 <- hotspots_peak %>% filter(pcuring > 100)
summary(pcuring_above_100)

# 4 entries with values above 100% - out of range values, possible mistake in reporting

zero_pcuring <- hotspots_peak %>%
  group_by(year) %>%
  summarise(
    total_count = n(),
    zero_count = sum(pcuring == 0, na.rm = TRUE),
    zero_percentage = (zero_count / total_count) * 100
  ) %>%
  arrange(desc(year))

zero_pcuring
sum(zero_pcuring$zero_count)
# 62239 zero values

# Create the histogram for pcuring
ggplot(hotspots_peak_pcuring, aes(x = pcuring)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Curing Percentage (pcuring) Values", 
       x = "Curing Percentage (pcuring)", 
       y = "Frequency") +
  scale_y_continuous(labels = scales::comma) + 
  theme_minimal()

# Create the boxplot for pcuring across years
ggplot(hotspots_peak_pcuring, aes(x = factor(year), y = pcuring)) +
  geom_boxplot(fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Curing Percentage (pcuring) Across Years",
       x = "Year",
       y = "Curing Percentage (pcuring)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10))


# The median pcuring values for these years range between 30% and 50%.
# There are many entries with pcuring at 0%, 
# possibly indicating periods with no drying or data recording issues.


"cfactor" # Curing Factor
# The amount of curing, or drying, of vegetation.

# Count missing values in the cfactor
sum(is.na(hotspots_peak$cfactor))


# Identify years with the most missing values
missing_cfactor <- hotspots_peak %>%
  group_by(year) %>%
  summarise(
    total_count = n(),
    missing_count = sum(is.na(cfactor)),
    missing_percentage = (missing_count / total_count) * 100
  ) %>%
  arrange(desc(year))

missing_cfactor

# 2019 - 2023 does not have this variable.


# Exclude the years where cfactor is missing
hotspots_peak_cfactor <- hotspots_peak %>%
  filter((year < 2019)) %>%
  filter(!is.na(cfactor))

# Check the summary of the filtered data
summary(hotspots_peak_cfactor$cfactor)

# range from 0 to 1
# Represents the curing factor as a proportion rather than a percentage.


# Create the histogram for cfactor
ggplot(hotspots_peak_cfactor, aes(x = cfactor)) +
  geom_histogram(binwidth = 0.1, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Curing Factor (cfactor) Values", 
       x = "Curing Factor (cfactor)", 
       y = "Frequency") +
  scale_y_continuous(labels = scales::comma) + 
  theme_minimal()

# Create the boxplot for cfactor across years
ggplot(hotspots_peak_cfactor, aes(x = factor(year), y = cfactor)) +
  geom_boxplot(fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Curing Factor (cfactor) Across Years",
       x = "Year",
       y = "Curing Factor (cfactor)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10))



# The mean cfactor is 0.13
# The variable has stopped being recorded recently.
# It may be not suitable for meaningful analyses for the 10 year period



"greenup" # – phenological state of deciduous trees (0=leafless, 1=green)
sum(is.na(hotspots_peak$greenup))

# Identify years with the most missing values
missing_greenup <- hotspots_peak %>%
  group_by(year) %>%
  summarise(
    total_count = n(),
    missing_count = sum(is.na(greenup)),
    missing_percentage = (missing_count / total_count) * 100
  ) %>%
  arrange(desc(year))

missing_greenup

# 2023 does not have this variable.

# Count the number of observations in each greenup state
greenup_count <- hotspots_peak %>%
  group_by(greenup) %>%
  summarise(count = n())
greenup_count

# There are invalid values present in the dataset. This variable is only 0 or 1

# Filter out invalid values in the greenup variable
hotspots_peak_greenup <- hotspots_peak %>%
  filter(greenup %in% c(0, 1))

# Check the summary of the filtered data
summary(hotspots_peak_greenup$greenup)

greenup_by_year <- hotspots_peak_greenup %>%
  group_by(year, greenup) %>%
  summarise(count = n(), .groups = 'drop') %>%
  arrange(desc(year), greenup)

# Visualize the count of observations in each greenup state
ggplot(greenup_by_year, aes(x = factor(greenup), y = count, fill = factor(greenup))) +
  geom_bar(stat = "identity") +
  labs(title = "Count of Observations by Greenup State",
       x = "Greenup State",
       y = "Count",
       fill = "Greenup State") +
  scale_fill_manual(values = c("0" = "bisque3", "1" = "lightgreen")) +
  theme_minimal()+
  scale_y_continuous(labels = scales::comma) 

# There are significantly more observations for greenup state 1 (trees with green leaves) compared to state 0 (leafless trees).
# The greenup state alone may not provide significant insights into fire behavior or risks.



"elev" # elevation above sea level (meters)
# BC's elevation ranges from sea level to approximately 4000 meters,
# with the highest peak being Mount Fairweather at 4663 meters.

# Check for missing values
sum(is.na(hotspots_peak$elev))

# A low percentage of missing values

range(hotspots_peak$elev, na.rm = TRUE)
# -1 3129 While there is one instance of a below sea level elevation, the range is valid for BC
# These particular entries may have been reported incorrectly due to technical issues.

# Identify and analyze outliers
elev_outliers <- hotspots_peak %>%
  filter(elev < 0)
print(elev_outliers)



# Histogram of elevation values
ggplot(hotspots_peak, aes(x = elev)) +
  geom_histogram(binwidth = 100, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Elevation Values",
       x = "Elevation (meters)",
       y = "Frequency") +
  theme_minimal()

# Overall the distribution of elevation levels is normal for British Columbia.

"cfl"     

"tfc0" 

"sfl"  

"ecozone"# – Ecozone in which hotspot is located

# Identify years with the most missing values
missing_ecozone <- hotspots_peak %>%
  group_by(year) %>%
  summarise(
    total_count = n(),
    missing_count = sum(is.na(ecozone)),
    missing_percentage = (missing_count / total_count) * 100
  ) %>%
  arrange(desc(year))

missing_ecozone

# For the years 2019-2023 there are only 24+4 missing entries.


# Count the number of hotspots in each ecozone
ecozone_counts <- hotspots_peak %>% filter(year >= 2019) %>% 
  group_by(ecozone) %>%
  summarise(count = n())

# Print the ecozone counts
print(ecozone_counts)

# Convert ecozone to a factor
ecozone_counts$ecozone <- factor(ecozone_counts$ecozone, levels = c(14, 4, 9, 12, 13, "NA"))

# Define custom colors and labels for the selected ecozones (Statistics Canada site)
# https://www.statcan.gc.ca/en/subjects/standard/environment/elc/2017-map
custom_colors <- c("14" = "#B5D79F", "4" = "#989898", "9" = "#36C48E", 
                   "12" = "#ACC32D", "13" = "#05734D", "NA" = "#000000")

custom_labels <- c("14" = "Montane Cordillera", "4" = "Taiga Plains", "9" = "Boreal Plains",
                   "12" = "Boreal Cordillera", "13" = "Pacific Maritime")

# Visualize the distribution of hotspots by ecozone with custom legend
ggplot(ecozone_counts, aes(x = reorder(ecozone, -count), y = count, fill = ecozone)) +
  geom_bar(stat = "identity") +
  labs(title = "Distribution of Hotspots by Ecozone (2019 onwards)",
       x = "Ecozone",
       y = "Number of Hotspots",
       fill = "Ecozone") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = custom_colors, labels = custom_labels) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "right", 
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))


# The plot shows distribution of ecozones in the hotspot dataset.
# The plot shows that the Montane Cordillera ecozone has the highest number of hotspots, 
# indicating it is the most fire-prone ecozone in the dataset from 2019 onwards.

"sfc0"     

"cbh"   








# Tests and models ####

# After EDA on the hotspots dataset subset it to focus 
# on specific numerical variables.

# Define the numeric columns
numeric_columns <- c("temp", "rh", "ws", "pcp", "ffmc", "dmc", "dc", "isi", "bui", "fwi", "ros", "hfi")

# Subset the dataframe
hotspots_test <- hotspots_peak_clean %>%
  select(all_of(numeric_columns), year, month, event_cluster)



# List of fire indices to summarize
fire_indices <- c('ffmc', 'dmc', 'dc', 'isi', 'bui', 'fwi')

# Apply the describe_numerical function to the fire indices in the hotspots_test dataset
summary_fire_indices <- describe_numerical(hotspots_test, fire_indices)

# Print the summary statistics for the fire indices
print(summary_fire_indices)

# The table shows summary statistics, 
# showing the number of missing values, minimum, median, mean, and maximum values for each index.

# During EDA we looked into distributions of six key Fire Indices across different years.
# The distributions of these indices for a particular year, 2018, 
# appeared to be close to the overall mean values of these indices.
# We also found out that 2018 had some significant fire events.

# Average number of observations per year
nrow(hotspots_peak) / length(unique(hotspots_peak$year))
# 173461.2

# Average number of unique event_clusters per year
length(unique(hotspots_peak$event_cluster)) / length(unique(hotspots_peak$year))
# 1739.9

# Number of observations in 2018
nrow(filter(hotspots_peak_clean, year == 2018))
# 339685

# Number of unique event clusters in 2018
length(unique(filter(hotspots_peak_clean, year == 2018)$event_cluster))
# 2051

# From these calculations, we found that 2018 had a significantly higher number of observations
# and event clusters compared to the average over the ten-year period.
# Despite the higher number of fire events, the main indices (FFMC, DMC, DC, ISI, BUI, FWI) 
# remained close to their mean values across the dataset.

# Therefore we wanted to test if the increased number of fires in 2018 was due to random chance
# or if other factors were at play. 
# We need to perform statistical tests to compare the indices for 2018 against the overall dataset.
# A p-value greater than 0.05 would suggest that the higher number of fires could be due to a chance, 
# and a lower p-value would mean the presence of other contributing factors.





# Summary statistics for the overall dataset
summary_overall <- describe_numerical(hotspots_peak, fire_indices)
summary_overall

# Filter data for the year 2018
hotspots_test_2018 <- hotspots_test %>% filter(year == 2018)

# Summary statistics for the year 2018
summary_2018 <- describe_numerical(hotspots_test_2018, fire_indices)
summary_2018

# Print summary statistics
print(summary_overall)
print(summary_2018)

# Differences
difference <- summary_2018$Mean - summary_overall$Mean

# Percentage difference
percentage_difference <- (summary_2018$Mean - summary_overall$Mean) / summary_overall$Mean * 100

# Create summary table
test_summary <- data.frame(
  Index = summary_overall$Variable,
  Overall_Mean = summary_overall$Mean,
  Year_2018_Mean = summary_2018$Mean,
  Difference = round(difference, 2),
  Percentage_Difference = round(percentage_difference, 2)

)

# Print the summary table
print(test_summary)





# Calculate standard deviation for the overall dataset
sd_overall <- sapply(fire_indices, function(index) sd(hotspots_peak_clean[[index]], na.rm = TRUE))

# Calculate standard deviation for the 2018 dataset
sd_2018 <- sapply(fire_indices, function(index) sd(hotspots_test_2018[[index]], na.rm = TRUE))

# Standard deviations
sd_summary <- data.frame(
  Index = fire_indices,
  Overall_SD = sd_overall,
  Year_2018_SD = sd_2018
)

# Print the summary of standard deviations
print(sd_summary)

# Overall, the lower standard deviations for the year 2018 across all six indices 
# compared to the overall dataset indicate 
# that the conditions related to fire behavior and potential were more consistent in 2018. 



# t-tests for each index
t_test_results <- lapply(fire_indices, function(index) {
  t.test(hotspots_peak_clean[[index]], hotspots_test_2018[[index]], alternative = "two.sided")
})

# p-values and other summary
t_test_summary <- data.frame(
  Index = fire_indices,
  P_Value = sapply(t_test_results, function(result) result$p.value),
  Mean_Difference = sapply(t_test_results, function(result) result$estimate[1] - result$estimate[2]),
  Overall_Mean = sapply(t_test_results, function(result) result$estimate[1]),
  Year_2018_Mean = sapply(t_test_results, function(result) result$estimate[2])
)

t_test_summary$Significant <- t_test_summary$P_Value < 0.05

# Print the t-test summary

print(t_test_summary)

# All the p-values are extremely small, indicating that the differences in means for all six indices 
# between the overall dataset and the year 2018 are statistically significant.
# This suggests that the conditions in 2018 were indeed different from the overall conditions,
# which could explain the increased number of fire events that year.





# Wilcoxon tests for each fire index
wilcox_results <- sapply(fire_indices, function(index) {
  test_result <- wilcox.test(hotspots_peak[[index]], hotspots_test_2018[[index]], alternative = "two.sided")
  return(test_result$p.value)
})

# Display the results
test_summary <- data.frame(
  Index = fire_indices,
  Overall_Mean = summary_overall$Mean,
  Year_2018_Mean = summary_2018$Mean,
  P_Value = wilcox_results,
  Significant = wilcox_results < 0.05
)

# Print the summary of test results
print(test_summary)

# All six fire indices show significant differences between the values in 2018 and the overall values across all years. 
# This suggests that the conditions in 2018 were statistically different from the overall conditions in the dataset, 
# meaning that there were likely specific factors in 2018 that contributed to these differences.

# While the t-tests show that there are significant differences in the fire indices, 
# they don't prove that higher values of these indices cause more fires.
# However, the higher values of indices like FWI, DMC, and others in 2018 suggest there is a link,
# meaning higher values of these indices likely mean conditions for more fires.
# To find the relationship between these indices and the number of fires, need to make
# more analysis, like regression modeling.




# FFMC Histogram in 2018 with Normal Curve
ggplot(hotspots_test_2018, aes(x = ffmc)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
  stat_function(fun = dnorm, args = list(mean = mean(hotspots_test_2018$ffmc, na.rm = TRUE), 
                                         sd = sd(hotspots_test_2018$ffmc, na.rm = TRUE)), 
                color = "red", size = 1) +
  labs(title = "FFMC Histogram in 2018 with Normal Curve",
       x = "FFMC",
       y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))



# DMC Histogram in 2018 with Normal Curve
ggplot(hotspots_test_2018, aes(x = dmc)) +
  geom_histogram(aes(y = ..density..), binwidth = 5, fill = "skyblue", color = "black", alpha = 0.7) +
  stat_function(fun = dnorm, args = list(mean = mean(hotspots_test_2018$dmc, na.rm = TRUE), 
                                         sd = sd(hotspots_test_2018$dmc, na.rm = TRUE)), 
                color = "red", size = 1) +
  labs(title = "DMC Histogram in 2018 with Normal Curve",
       x = "DMC",
       y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))



# DC Histogram in 2018 with Normal Curve
ggplot(hotspots_test_2018, aes(x = dc)) +
  geom_histogram(aes(y = ..density..), binwidth = 50, fill = "skyblue", color = "black", alpha = 0.7) +
  stat_function(fun = dnorm, args = list(mean = mean(hotspots_test_2018$dc, na.rm = TRUE), 
                                         sd = sd(hotspots_test_2018$dc, na.rm = TRUE)), 
                color = "red", size = 1) +
  labs(title = "DC Histogram in 2018 with Normal Curve",
       x = "DC",
       y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))



# ISI Histogram in 2018 with Normal Curve
ggplot(hotspots_test_2018, aes(x = isi)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
  stat_function(fun = dnorm, args = list(mean = mean(hotspots_test_2018$isi, na.rm = TRUE), 
                                         sd = sd(hotspots_test_2018$isi, na.rm = TRUE)), 
                color = "red", size = 1) +
  labs(title = "ISI Histogram in 2018 with Normal Curve",
       x = "ISI",
       y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))


# BUI Histogram in 2018 with Normal Curve
ggplot(hotspots_test_2018, aes(x = bui)) +
  geom_histogram(aes(y = ..density..), binwidth = 10, fill = "skyblue", color = "black", alpha = 0.7) +
  stat_function(fun = dnorm, args = list(mean = mean(hotspots_test_2018$bui, na.rm = TRUE), 
                                         sd = sd(hotspots_test_2018$bui, na.rm = TRUE)), 
                color = "red", size = 1) +
  labs(title = "BUI Histogram in 2018 with Normal Curve",
       x = "BUI",
       y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))


# FWI Histogram in 2018 with Normal Curve
ggplot(hotspots_test_2018, aes(x = fwi)) +
  geom_histogram(aes(y = ..density..), binwidth = 5, fill = "skyblue", color = "black", alpha = 0.7) +
  stat_function(fun = dnorm, args = list(mean = mean(hotspots_test_2018$fwi, na.rm = TRUE), 
                                         sd = sd(hotspots_test_2018$fwi, na.rm = TRUE)), 
                color = "red", size = 1) +
  labs(title = "FWI Histogram in 2018 with Normal Curve",
       x = "FWI",
       y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))





# FFMC QQ Plot with a subset of data
sample_data_ffmc <- hotspots_test_2018 %>% sample_n(5000)

ggplot(sample_data_ffmc, aes(sample = ffmc)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "FFMC QQ Plot",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))

# DMC QQ Plot with a subset of data
sample_data_dmc <- hotspots_test_2018 %>% sample_n(5000)

ggplot(sample_data_dmc, aes(sample = dmc)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "DMC QQ Plot",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))


# DC QQ Plot with a subset of data
sample_data_dc <- hotspots_test_2018 %>% sample_n(5000)

ggplot(sample_data_dc, aes(sample = dc)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "DC QQ Plot",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))

# ISI QQ Plot with a subset of data
sample_data_isi <- hotspots_test_2018 %>% sample_n(5000)

ggplot(sample_data_isi, aes(sample = isi)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "ISI QQ Plot",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))

# BUI QQ Plot with a subset of data
sample_data_bui <- hotspots_test_2018 %>% sample_n(5000)

ggplot(sample_data_bui, aes(sample = bui)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "BUI QQ Plot",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))

# FWI QQ Plot with a subset of data
sample_data_fwi <- hotspots_test_2018 %>% sample_n(5000)

ggplot(sample_data_fwi, aes(sample = fwi)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "FWI QQ Plot",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))




# T Test with similar clusters DRAFT ####







# Number of events, mean FWI, and mean DC for each cluster (test data)
cluster_summary_indices <- hotspots_test %>%
  group_by(event_cluster) %>%
  summarise(
    num_events = n(),          
    mean_fwi = mean(fwi),  # Mean FWI value for each cluster
    mean_dc = mean(dc)     # Mean DC value for each cluster
  )

# Filter clusters that have 1000+ events
chosen_clusters <- cluster_summary_indices %>%
  filter(num_events >= 1000 & num_events <= 6000)

# Function to calculate how similar mean values are
calculate_similar <- function(df) {
  df %>%
    mutate(
      fwi_diff = abs(mean_fwi - mean(target_clusters$mean_fwi)),
      dc_diff = abs(mean_dc - mean(target_clusters$mean_dc)),
      similar = fwi_diff + dc_diff
    ) %>%
    arrange(similar)
}

# Apply the function and choose two clusters with the closest mean FWI and DC
chosen_clusters <- chosen_clusters %>%
  calculate_similar() %>%
  slice(1:2)

# Chosen clusters
print(chosen_clusters)

# Chosen cluster IDs
chosen_ids <- c(4811, 13095)

# Summary table for the chosen clusters
hotspots_test %>%
  filter(event_cluster %in% chosen_ids) %>%
  group_by(event_cluster, year, month) %>%
  summarize(
    count = n(),
    mean_ffmc = mean(ffmc, na.rm = TRUE),
    mean_dmc = mean(dmc, na.rm = TRUE),
    mean_dc = mean(dc, na.rm = TRUE),
    mean_isi = mean(isi, na.rm = TRUE),
    mean_bui = mean(bui, na.rm = TRUE),
    mean_fwi = mean(fwi, na.rm = TRUE)
  )


# Plot the chosen clusters on the map
plot_clusters_on_map(chosen_ids, hotspots_peak, zoom_level = 10)




# Filter data for the two chosen clusters
cluster_13095 <- hotspots_test %>% filter(event_cluster == 13095)
cluster_4811 <- hotspots_test %>% filter(event_cluster == 4811)



# DC Histogram cluster_13095
ggplot(cluster_13095, aes(x = dc)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
  stat_function(fun = dnorm, args = list(mean = mean(cluster_13095$dc, na.rm = TRUE), 
                                         sd = sd(cluster_13095$dc, na.rm = TRUE)), 
                color = "red", size = 1) +
  labs(title = "DC Histogram cluster_13095",
       x = "DC",
       y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))

# DC Histogram cluster_4811
ggplot(cluster_4811, aes(x = dc)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
  stat_function(fun = dnorm, args = list(mean = mean(cluster_4811$dc, na.rm = TRUE), 
                                         sd = sd(cluster_4811$dc, na.rm = TRUE)), 
                color = "red", size = 1) +
  labs(title = "DC Histogram cluster_4811",
       x = "DC",
       y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))



# DC QQ Plot cluster_13095
ggplot(cluster_13095, aes(sample = dc)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "DC QQ Plot",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))

# DC QQ Plot cluster_4811
ggplot(cluster_4811, aes(sample = dc)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "DC QQ Plot",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))



# Perform t-test on DC
t.test(cluster_13095$dc, cluster_4811$dc, alternative = "two.sided")


# The t-test shows no significant difference in the DC values between the two clusters (13095 and 4811). 
# The p-value bigger than 0.05 and the confidence interval [-1.63, 1.44] 
# show that the observed differences could be due to random chance rather than a true difference in means. 
# The DC values for these clusters are statistically similar.







# template boxplot & hist####




# HISTOGRAM - DISTRIBUTION OF THE VALUE IN ALL 10 YEARS
ggplot(hotspots_peak, aes(x = pcp)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Windspeed at Fire Hotspots",
       x = "Windspeed (m/s)",
       y = "Frequency") +
  scale_y_continuous(labels = comma) + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))


# BOXPLOTS 10 YEARS SIDE BY SIDE
ggplot(hotspots_peak, aes(x = factor(year), y = pcp)) +
  geom_boxplot(fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "Windspeed Distribution Across Years",
       x = "Year",
       y = "Windspeed (m/s)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10))





# LINE PLOT WITH FILTER FOR SPECIFIC YEAR  (trend line added)
ggplot(hotspots_peak %>% filter(year == 2018), aes(x = rep_date, y = pcp)) +
  geom_line(color = "steelblue") +
  geom_smooth(se = FALSE, method = "loess", size = 1, linetype = "solid") +  # Dashed line for trend
  labs(title = "pcp Over the Peak Season 2014",
       x = "Date",
       y = "Relative Humidity (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = comma)





# 10 YEAR AVERAGE LINEPLOT - USES AVG TABLE
# TRENDLINE
ggplot(monthly_avg, aes(x = month, y = avg_pcp, color = factor(year), group = year)) +
  geom_line(size = 0.5, alpha = 0.6, linetype = "dotted") +  # raw data
  geom_smooth(se = FALSE, method = "loess", size = 1, linetype = "solid") +  # Dashed line for trend
  labs(title = "Average Precipitation by Month",
       x = "Month",
       y = "Average Precipitation (mm)",
       color = "Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))







############## draft####

