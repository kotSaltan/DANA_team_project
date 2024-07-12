# info about presentation####
#EDA: ! 
# data from 2013 - 2024
# many graphs in submissions
# BC
# 6 gb to start and code to narrow it down
# size of the data - now many locations, coordinates
# valid range
# outliers
# fix NA 
# see initial season pattern in the eda
# patters of the years
# highest and lowest fires
# weather - graphs to compare weather and hotspots

# 2nd part
# compare 2 years - analyses, mean comparison, ranking comparison
# 

# presentation for teamwork
# q&a in the end
# crosscheck analyses of the others
# 20-30 mins to present
# 1 question for the other team

# read article, analyze it , do t test and correlation
# compare the dataset with the article

# load packages and file####

# Load the necessary packages
# TEAM (presenting second to last) 30th July


library(readr)
library(readxl)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(corrplot)
library(gridExtra)
library(reshape2)
library(GGally)
library(caret)
library(dbscan)
library(scales)
set.seed(123)

# Create a new RProject, create data folder with the datasets, set working directory.

# Get the name of working directory in RProject
proj.path <- getwd()

# Read the weather datasets

hotspots_raw <- read_csv(file.path(proj.path,'data', 'hotspots.csv'))







# Check the date format
hotspots_raw$rep_date <- as.POSIXct(hotspots_raw$rep_date, format = "%Y-%m-%d %H:%M:%S")
str(hotspots_raw$rep_date)
# Add year and month columns
hotspots_raw$year <- year(hotspots_raw$rep_date)
hotspots_raw$month <- month(hotspots_raw$rep_date, label = TRUE, abbr = TRUE)  # Months as factor (and named)

# Subset the dataframe to include only data from 2014 to the end of 2023
hotspots <- hotspots_raw %>%
  filter(year >= 2014 & year <= 2023)

# Check the structure of the dataset
str(hotspots)

# Check the variable names
names(hotspots)



write.csv(hotspots, file = "hotspots.csv", row.names = FALSE)

# dbcluster####

# TO IDENTIFY SPECIFIC EVENTS FOR FURTHER ANALYSES 
# TO BE ABLE ON TO DESCRIBE NUMERICAL VARIABLES PRIO AND DURING A SPECIFIC FIRE EVENT
# THE NUMBER OF CLUSTERS HAS TO BE CLOSE TO THE OFFICIAL INFORMATION FROM WILDFIRE SERVICE


# Using DBSCAN clustering to identify fire events

# Prepare the data for clustering (latitude, longitude, date)
event_data <- hotspots %>%
  select(lat, lon, rep_date)

# Convert date to numeric for clustering
event_data$date_numeric <- as.numeric(as.POSIXct(event_data$rep_date))


# Scale the date_numeric to have a similar range as lat/lon
# For simplicity, assuming 1 degree ~ 111 km and 20 hours ~ 72000 seconds
# Scale factor: 1 second ~ 0.001 degrees (approximately)
event_data$date_scaled <- event_data$date_numeric * 0.001


# Apply DBSCAN clustering
db <- dbscan(event_data[, c("lat", "lon", "date_scaled")], eps = 0.6, minPts = 5)
# eps value of 0.6 degrees is about 66 km

# Add cluster labels to the original dataset
hotspots$event_cluster <- db$cluster

# Filter for a specific cluster (event of fire)
event_500 <- hotspots %>%
  filter(event_cluster == 500) # Replace with the specific cluster number
# OUTPUT 14 observations lat 53 lon -124 on 16 July 2014


event_20000 <- hotspots %>%
  filter(event_cluster == 20000) 
# OUTPUT 15 observations lat 52 lon -126 on 1 August 2021


event_2000 <- hotspots %>%
  filter(event_cluster == 2000) 
# OUTPUT 14 observations lat 52 lon -124 on 9 July 2015


# Count the number of unique clusters
num_clusters <- length(unique(hotspots$event_cluster)) - 1 # Subtract 1 to exclude the noise cluster (label 0)

# Print the number of clusters
cat("Number of clusters identified by DBSCAN:", num_clusters, "\n")
# OUTPUT 32309

# Total number of fires in BC (official table)
2293+1801+1647+670+825+2117+1353+1050+1858+1481+1861
# 16956




# Detailed summary of the clusters
event_details <- hotspots %>%
  filter(event_cluster != 0) %>%
  group_by(year) %>%
  summarise(
    first_cluster = first(event_cluster),
    start_date_hotspot = min(rep_date),
    end_date_hotspot = max(rep_date),
    events_count = length(unique(event_cluster))
    
  )


# THE TABLE SHOWS WHAT HOTSPOTS DATASETS HAVE WHAT CLUSTERS AND THEIR NUMBER
print(event_details)




# ############### UNCOMMENT TO CHECK IF NEEDED
# # Here i looked into which values to choose for eps
# # I needed to minimize noise (event_cluster = 0) and have reasonable amount of clusters
# apply_dbscan <- function(data, eps_value) {
#   db <- dbscan(data[, c("lat", "lon", "date_scaled")], eps = eps_value, minPts = 5)
#   
#   # Add cluster labels to the original dataset
#   hotspots$event_cluster <- db$cluster
#   
#   # Count the number of unique clusters
#   num_clusters <- length(unique(hotspots$event_cluster)) - 1  # Excluding noise (cluster 0)
#   
#   # Count the number of noise points
#   num_noise <- sum(hotspots$event_cluster == 0)
#   
#   # Print the number of clusters and noise points
#   cat("eps =", eps_value, ": Number of clusters =", num_clusters, ", Number of noise points =", num_noise, "\n")
#   
#   return(hotspots)
# }
# 
# 
# # Experiment with different eps values
# for (eps_val in seq(0.1, 1, by = 0.1)) {
#   db <- dbscan(event_data[, c("lat", "lon", "date_scaled")], eps = eps_val, minPts = 5)
#   cat("eps =", eps_val, ": Number of clusters =", length(unique(db$cluster)) - 1, "\n")
# }
# 
# # Visualize the final clustering results with the chosen eps value 
# chosen_eps <- 0.6  
# hotspots <- apply_dbscan(event_data, chosen_eps)



# cluster explained####



# CHECK CLUSTERING ON SAMLLER DATASET TO SEE NUMBERS PROVIDED BY OFFICIAL TABLE
# # To compare the number of clusters to the data from BC official
# # Filter out events that have fire
# # ASSUME THAT EVENTS OF REAL FIRE WILL HAVE NON-ZERO VALUES - THIS LIMITS THE ORIGINAL HOTSPOTS DATASET
# # Set thresholds
# ffmc_threshold <- 85
# dmc_threshold <- 40
# dc_threshold <- 100
# fwi_threshold <- 20
# 
# # Filter data based on thresholds
# active_fires <- hotspots %>%
#   filter(ffmc > ffmc_threshold & dmc > dmc_threshold & dc > dc_threshold & fwi > fwi_threshold)
# 
# # Check the number of remaining entries
# nrow(active_fires)
# # OUTPUT 1407950 
# 
# # Prepare the data for clustering (latitude, longitude, date)
# event_data <- active_fires %>%
#   select(lat, lon, rep_date)
# 
# # Convert date to numeric (Unix timestamp)
# event_data$date_numeric <- as.numeric(as.POSIXct(event_data$rep_date))
# 
# # Adjust temporal scaling
# event_data$date_scaled <- event_data$date_numeric * 0.0001  # Adjusted scaling factor
# 
# # Apply DBSCAN clustering
# 
# db <- dbscan(event_data[, c("lat", "lon", "date_scaled")], eps = 0.3, minPts = 5)
# # I CHOSE TO GROUP SMALLER RADIUS - ABOUT 33 KM
# 
# # Add cluster labels to the original dataset
# active_fires$event_cluster <- db$cluster
# 
# # Count the number of unique clusters
# num_clusters <- length(unique(active_fires$event_cluster)) - 1  # Excluding noise (cluster 0)
# 
# # Count the number of noise points
# num_noise <- sum(active_fires$event_cluster == 0)
# 
# # Print the number of clusters and noise points
# cat("eps =", eps_value, ": Number of clusters =", num_clusters, ", Number of noise points =", num_noise, "\n")
# 
# ############### OUTPUT eps = 0.3 : Number of clusters = 18804 , Number of noise points = 29620 
# # THIS OUTPUT IS CLOSE TO THE DATA WE HAVE FROM OFFICIAL TABLE, CLUSTERING WORKS TO GROUP FIRE EVENTS



# heatmap of events####


# HEATMAP OF EVENTS


# To see on what time period to focus on to be able to compare year to year


# THREE WAYS TO SUM UP EVENTS PER MONTH
# NUMBER OF CLUSTERS/ NUMBER OF ENTRIES/ NUMBER OF CLUSTERS WITH LABEL FOR EACH YEAR


# Summarize the number of CLUSTERS PER MONTH
fire_events_per_month <- hotspots %>%
  filter(event_cluster != 0) %>% 
  group_by(month) %>%
  summarise(n_events = n())

# Print the summary table
print(fire_events_per_month)



# Summarize ENTRIES PER MONTH
fire_events_per_month <- hotspots %>%
  group_by(month) %>%
  summarise(n_events = n())

# Print the summary table
print(fire_events_per_month)



# Summarize the number of fire events per year and month
fire_events_per_month <- hotspots %>%
  filter(event_cluster != 0) %>%  # Exclude noise clusters
  group_by(year, month) %>%
  summarise(n_events = n()) %>%
  ungroup()

# Print the summary table
print(fire_events_per_month)



# Create a bar plot to visualize the number of fire events per month
ggplot(fire_events_per_month, aes(x = month, y = n_events)) +
  geom_bar(stat = "identity", fill = "steelblue", color = "black") +
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



# Reshape the data to a wide format
fire_events_wide <- fire_events_per_month %>%
  pivot_wider(names_from = month, values_from = n_events, values_fill = 0)

# Print the wide format summary table
print(fire_events_wide)



# Define the color palette
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


# subset for May-Oct####

# FROM THE PLOT AND SUMMARY TABLE WE CAN CHOOSE TO FOCUS ON EVENTS FROM MAY TO OCTOBER

# Subset the data for the months from May to October
hotspots_peak <- hotspots %>%
  filter(month(rep_date) %in% c(5, 6, 7, 8, 9, 10))

write.csv(hotspots_peak, "hotspots_May_Oct.csv", row.names = FALSE)


# MAKE A HEATMAP
# Summarize the number of fire events per year and month
fire_events_per_month_subset <- hotspots_peak %>%
  filter(event_cluster != 0) %>%
  group_by(year, month) %>%
  summarise(n_events = n()) %>%
  ungroup()

# Print the summary table
print(fire_events_per_month_subset)


# Reshape the data to a wide format
fire_events_subset_wide <- fire_events_per_month_subset %>%
  pivot_wider(names_from = month, values_from = n_events, values_fill = 0)

# Print the wide format summary table
print(fire_events_subset_wide)


# Define the color palette
colors <- c("gray95", "gray80", "yellow", "orange", "darkred", "black")

# Define the values for the color breaks
values <- c(0, 500, 1000, 2000, 10000, 350000)

# Create a heatmap to visualize the number of fire events per month and year
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



# variables####

names(hotspots)
names(hotspots_peak)

"lat"
"lon" 

# Latitude: Between 48.309789°N and 60.00065°N
# Longitude: Between -139.058200 °W and -114.05423°W

range(hotspots$lat, na.rm = TRUE)
range(hotspots$lon, na.rm = TRUE)

# The range of latitude and longitude the hotspots dataset is inside BC boundries


"rep_date"  

print(event_details)

# Extract year and number of points from event_details
events_count <- event_details %>%
  select(year, events_count)

# Check the extracted table
print(events_count)

# Create summary table describing each hotspots dataset by year
hotspots_df_summary <- hotspots %>%
  group_by(year) %>%
  summarise(
    start_date = min(rep_date),
    end_date = max(rep_date),
    hotspots_df_day_count = as.numeric(difftime(max(rep_date), min(rep_date), units = "days"))
  )
hotspots_df_summary

# Add number of event clusters to the table

hotspots_df_summary <- hotspots_df_summary %>%
  left_join(events_count, by = "year") %>%
  mutate(
    start_month_day = format(as.Date(start_date), "%m-%d"),
    end_month_day = format(as.Date(end_date), "%m-%d")
  )

print(hotspots_df_summary)


# NOTABLE DIFFERENCES IN THE DATA -  2021-2023 HAVE ALMOST FULL YEAR ROUND RECORD
# This variable shows the beginning and end day the data was collected for the perticular year.
# The table shows how many days each hotspots dataset has and how many clustered events there were each year.



"uid"  

# The uid variable is present in historical datasets up to 2019.
# However, it is absent in more recent datasets.
# Data collection and reporting teckniques of CWFIS have hanged

# Display a few rows to show examples of uid
hotspots %>%
  select(uid) %>%
  head(10)  # Display the first 10 rows
range(hotspots$uid, na.rm = TRUE)


"source" 
hotspots %>% 
  select(source) %>% 
  count(source)

sourse_cluster_count <- hotspots %>%
  group_by(source) %>%
  summarise(num_clusters = n_distinct(event_cluster))

# Create a bar plot of the number of clusters per satellite
ggplot(sourse_cluster_count, aes(x = source, y = num_clusters)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Number of Events Reported by Each Source",
       x = "Source",
       y = "Number of Events (clusters)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



"sensor"  
hotspots %>% 
  select(sensor) %>% 
  count(sensor)

sensor_cluster_count <- hotspots %>%
  group_by(sensor) %>%
  summarise(num_clusters = n_distinct(event_cluster))

# Create a bar plot of the number of clusters per satellite
ggplot(sensor_cluster_count, aes(x = sensor, y = num_clusters)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Number of Events Reported by Each Sensor",
       x = "Sensor",
       y = "Number of Events (clusters)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



"satellite" 
hotspots %>% 
  select(satellite) %>% 
  count(satellite)

satellite_cluster_count <- hotspots %>%
  group_by(satellite) %>%
  summarise(num_clusters = n_distinct(event_cluster))

# Create a bar plot of the number of clusters per satellite
ggplot(satellite_cluster_count, aes(x = satellite, y = num_clusters)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Number of Clusters Produced by Each Satellite",
       x = "Satellite",
       y = "Number of Clusters") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



"agency"  
hotspots %>% 
  select(agency) %>% 
  count(agency)
# only output is BC







# Identify numerical columns to work with
numerical_columns <- c('temp',
                       'rh',
                       'ws',
                       'wd',
                       'pcp',
                       'ffmc',
                       'dmc',
                       'dc',
                       'isi',
                       'bui',
                       'fwi',
                       'ros',
                       'sfc',
                       'tfc',
                       'bfc',
                       'hfi',
                       'cfb',
                       'age',
                       'estarea',
                       'polyid',
                       'pcuring',
                       'cfactor',
                       'greenup',
                       'elev',
                       'cfl',
                       'tfc0',
                       'sfl',
                       'ecozone',
                       'sfc0',
                       'cbh')


# Function to describe each numerical column
describe_numerical <- function(df, cols) {
  for (col in cols) {
    cat("Variable:", col, "\n")
    cat("Type:", class(df[[col]]), "\n")
    cat("Number of Missing Values:", sum(is.na(df[[col]])), "\n")
    cat("Summary Statistics:\n")
    print(summary(df[[col]]))
    cat("\n")
  }
}

# Describe numerical columns
describe_numerical(hotspots, numerical_columns)



# Describe numerical columns SUMMER DATA
describe_numerical(hotspots_peak, numerical_columns)








"temp" # Temperature (°C)

# This variable shows temperature in Celsius at the specific location, at the fire event
# range -21 to 43 with a mean 21, ok as the set includes information from winter and places with high elevation

summary(hotspots_peak$temp)

# For the peak season range is -9 to 43 with mean 21
# Filter the dataset for subzero temperatures, check these entries 
subzero_temps <- hotspots_peak %>%
  filter(temp < 0)

# Print the resulting data frame to inspect the entries with subzero temperatures

subzero_temps %>%
  group_by(year, month) %>%
  summarise(n_entries = n()) %>%
  arrange(year, month)

# Most are in October of 2015, 2019 and 2023, a usual month to have sub-zero temp


# Histogram to show distribution of temperatures
ggplot(hotspots, aes(x = temp)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Temperature at Fire Hotspots",
       x = "Temperature (°C)",
       y = "Frequency") +
  scale_y_continuous(labels = comma) +  # Adjust y-axis labels to numeric format (scales package)
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))


# Distribution of temp for hotspots_peak

# Histogram to show distribution of temperatures
ggplot(hotspots_peak, aes(x = temp)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Temperature at Fire Hotspots (Peak)",
       x = "Temperature (°C)",
       y = "Frequency") +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))


# Create a box plot to compare temperature distributions across years
ggplot(hotspots_peak, aes(x = factor(year), y = temp)) +
  geom_boxplot(fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "Temperature Distribution Across Years (Peak)",
       x = "Year",
       y = "Temperature (°C)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10))


# Create a summary table with mean temperature by year
mean_temp_by_year <- hotspots_peak %>%
  group_by(year) %>%
  summarise(mean_temp = mean(temp, na.rm = TRUE))

# Create a line plot to show temperature trends over years
ggplot(mean_temp_by_year, aes(x = year, y = mean_temp)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "black", size = 2) +
  labs(title = "Average Temperature Over Years (Peak)",
       x = "Year",
       y = "Average Temperature (°C)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))


hotspots_peak %>% 
  filter(year == 2019) %>% 
  summarise(
    mean_temp = mean(temp, na.rm = TRUE),
    min_temp = min(temp, na.rm = TRUE),
    max_temp = max(temp, na.rm = TRUE),
    median_temp = median(temp, na.rm = TRUE)
  )
# Summary table for the year 2019 shows lower temperatures than usual


# Create a line plot for temperature in 2019
ggplot(hotspots_peak %>% filter(year == 2019), aes(x = rep_date, y = temp)) +
  geom_line(color = "steelblue") +
  labs(title = "Temperature Over the Year 2019",
       x = "Date",
       y = "Temperature (°C)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Temperature drops in October






# Create Line Plots for 4 different years to compare temp

# Define common axis limits
y_limits <- range(hotspots_peak$temp[hotspots_peak$year %in% c(2014, 2018, 2020, 2023)])

# Create plots with common axis limits
plot_2014 <- ggplot(hotspots_peak %>% filter(year == 2014), aes(x = rep_date, y = temp)) +
  geom_line(color = "steelblue") +
  labs(title = "Temperature Over the Peak Season 2014",
       x = "Date",
       y = "Temperature (°C)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(limits = y_limits)

plot_2018 <- ggplot(hotspots_peak %>% filter(year == 2018), aes(x = rep_date, y = temp)) +
  geom_line(color = "steelblue") +
  labs(title = "Temperature Over the Peak Season 2018",
       x = "Date",
       y = "Temperature (°C)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(limits = y_limits)

plot_2020 <- ggplot(hotspots_peak %>% filter(year == 2020), aes(x = rep_date, y = temp)) +
  geom_line(color = "steelblue") +
  labs(title = "Temperature Over the Peak Season 2020",
       x = "Date",
       y = "Temperature (°C)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(limits = y_limits)

plot_2023 <- ggplot(hotspots_peak %>% filter(year == 2023), aes(x = rep_date, y = temp)) +
  geom_line(color = "steelblue") +
  labs(title = "Temperature Over the Peak Season 2023",
       x = "Date",
       y = "Temperature (°C)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(limits = y_limits)

# Arrange the plots side by side
grid.arrange(plot_2014, plot_2018, plot_2020, plot_2023, ncol = 2)






"rh" # Relative Humidity (%)

# The amount of moisture in the air as a percentage
# 0 to 100 with a mean 36

"ws"     

"wd"  

"pcp" 
ggplot(hotspots, aes(x = pcp)) +
  geom_histogram(binwidth = 10, fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Percipitation at Fire Hotspots",
       x = "Percipitation (mm)",
       y = "Frequency") +
  scale_y_continuous(labels = comma) +  # Adjust y-axis labels to numeric format (scales package)
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))

# Set a threshold for precipitation
precip_threshold <- 50

# Filter the data to exclude outliers
hotspots_filtered <- hotspots %>%
  filter(pcp <= precip_threshold)

ggplot(hotspots_filtered, aes(x = factor(year), y = pcp)) +
  geom_boxplot(fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "Percipitation Distribution Across Years",
       x = "Year",
       y = "Percipitation (mm)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10))














"ffmc" 
ggplot(hotspots, aes(x = rep_date, y = ffmc)) +
  geom_line() +
  labs(title = "Trend Over Time", x = "Date", y = "ffmc")



"dmc"     
"dc"      
"isi"     
"bui"     
"fwi"      
"fuel"     
"ros"      
"sfc"      
"tfc"      
"bfc"      
"hfi"    
"cfb"    
"age"     
"estarea"  
"polyid"   
"pcuring"  
"cfactor"  
"greenup"  
"elev"     
"cfl"      
"tfc0"     
"sfl"      
"ecozone"  
"sfc0"     
"cbh"   





















# weather var plots####



# TEMP



# RH

# Histogram to show distribution of humidity
ggplot(hotspots_peak, aes(x = rh)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Humidity at Fire Hotspots",
       x = "Humidity (%)",
       y = "Frequency") +
  scale_y_continuous(labels = comma) +  # Adjust y-axis labels to numeric format (scales package)
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))


# Create a box plot to compare humidity distributions across years
ggplot(hotspots_peak, aes(x = factor(year), y = rh)) +
  geom_boxplot(fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "Humidity Distribution Across Years",
       x = "Year",
       y = "Humidity (%)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10))


# Create a summary table with mean humidity by year
mean_rh_by_year <- hotspots_peak %>%
  group_by(year) %>%
  summarise(mean_rh = mean(rh, na.rm = TRUE))

# Create a line plot to show humidity trends over years
ggplot(mean_rh_by_year, aes(x = year, y = mean_rh)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "black", size = 2) +
  labs(title = "Average Humidity Over Years",
       x = "Year",
       y = "Average Humidity (%)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))



# WS

# Histogram to show distribution of windspeed
ggplot(hotspots_peak, aes(x = ws)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Windspeed at Fire Hotspots",
       x = "Windspeed ()",
       y = "Frequency") +
  scale_y_continuous(labels = comma) +  # Adjust y-axis labels to numeric format (scales package)
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))


# Create a box plot to compare windspeed distributions across years
ggplot(hotspots_peak, aes(x = factor(year), y = ws)) +
  geom_boxplot(fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "Windspeed Distribution Across Years",
       x = "Year",
       y = "Windspeed ()") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10))


# Create a summary table with mean windspeed by year
mean_ws_by_year <- hotspots_peak %>%
  group_by(year) %>%
  summarise(mean_ws = mean(ws, na.rm = TRUE))

# Create a line plot to show windspeed trends over years
ggplot(mean_ws_by_year, aes(x = year, y = mean_ws)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "black", size = 2) +
  labs(title = "Average Windspeed Over Years",
       x = "Year",
       y = "Average Windspeed ()") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))





# PCP

# Histogram to show distribution of precipitation
ggplot(hotspots_peak, aes(x = pcp)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Precipitation at Fire Hotspots",
       x = "Precipitation ()",
       y = "Frequency") +
  scale_y_continuous(labels = comma) +  # Adjust y-axis labels to numeric format (scales package)
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))


# Create a box plot to compare precipitation distributions across years
ggplot(hotspots_peak, aes(x = factor(year), y = pcp)) +
  geom_boxplot(fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "Precipitation Distribution Across Years",
       x = "Year",
       y = "Precipitation ()") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10))


# Create a summary table with mean precipitation by year
mean_pcp_by_year <- hotspots_peak %>%
  group_by(year) %>%
  summarise(mean_pcp = mean(ws, na.rm = TRUE))

# Create a line plot to show precipitation trends over years
ggplot(mean_pcp_by_year, aes(x = year, y = mean_pcp)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "black", size = 2) +
  labs(title = "Average Precipitation Over Years",
       x = "Year",
       y = "Average Precipitation ()") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))


# Set a threshold for precipitation
precip_threshold <- 50

# Filter the data to exclude outliers
hotspots_filtered <- hotspots_peak %>%
  filter(pcp <= precip_threshold)

# Histogram to show distribution of precipitation
ggplot(hotspots_filtered, aes(x = pcp)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Precipitation at Fire Hotspots",
       x = "Precipitation ()",
       y = "Frequency") +
  scale_y_continuous(labels = comma) +  # Adjust y-axis labels to numeric format (scales package)
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))


# Create a box plot to compare precipitation distributions across years
ggplot(hotspots_filtered, aes(x = factor(year), y = pcp)) +
  geom_boxplot(fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "Precipitation Distribution Across Years",
       x = "Year",
       y = "Precipitation ()") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10))


# Create a summary table with mean precipitation by year
mean_pcp_by_year <- hotspots_filtered %>%
  group_by(year) %>%
  summarise(mean_pcp = mean(ws, na.rm = TRUE))

# Create a line plot to show precipitation trends over years
ggplot(mean_pcp_by_year, aes(x = year, y = mean_pcp)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "black", size = 2) +
  labs(title = "Average Precipitation Over Years",
       x = "Year",
       y = "Average Precipitation ()") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))


# more variables####





# ws: Wind Speed (km/h)

# Speed of wind in km/h
# 0 to 59 with mean of 9


# wd: Wind Direction (degrees)

# Direction from which the wind is blowing in degrees
# 0 to 360 with mean of 201
# NEED TO FIND THE DIRECTION NWSE


# pcp: Precipitation (mm)

# Amount of precipitation in mm
# 0 to 651 with mean of 0.2
# So the amount is very low generally
# CHECK WHAT EVENTS HAVE HIGH PCP AND CHECK OTHER VAR

# Filter the dataset for high pcp
high_pcp <- hotspots_peak %>%
  filter(pcp > 50)

# Print the resulting data frame to inspect the entries with high pcp
print(high_pcp)

summary(high_pcp$lat)
summary(high_pcp$lon)
summary(high_pcp$month)
# 


# ffmc: Fine Fuel Moisture Code

# An index that represents the moisture content of surface litter and other cured fine fuels
# 0 to 459 with mean of  88
# CHECK EVENTS WITH 0 VALUE _ WHAT THEY MEAN
# WHAT IS THE RANGE FOR THIS INDEX
# WHAT IS THE USUAL SCALE

# dmc: Duff Moisture Code

# The average moisture content of loosely compacted organic layers of moderate depth
# 0 to 459 with mean of 90
# SAME CONCERNS
# WHY ARE THESE VALUES CLOSE _ LOOK INTO SPECIFIC EVENTS


# dc: Drought Code

# Represents the moisture content of deep, compact organic layers
# 0 to 1122 with mean of 512
# HOW IS THIS DIFFERENT FROM INDICES ABOVE
# SAME CONCERNS WITH THE VALUES


# isi: Initial Spread Index

# Combines wind speed and FFMC to estimate the rate of spread immediately following ignition
# 0 to 137 with mean of 8
# HOW THIS INDEX CORRELATES WITH ffmc and ws
# SAME CONCERNS WITH THE VALUES


# bui: Buildup Index

# Combines DMC and DC to estimate the total amount of fuel available for combustion
# 0 to 458 with mean of 120
# HOW THIS INDEX CORRELATES WITH dmc and dc
# SAME CONCERNS WITH THE VALUES


# fwi: Fire Weather Index

# A comprehensive index that rates the potential fire intensity
# 0 to 183 with mean of 28
# WHAT DOES THIS INDEX CONSISTS OF
# SAME CONCERNS WITH THE VALUES


# ros: Rate of Spread (m/min)

# Measures how fast the fire spreads
# -429 to 96 with mean of 6
# WHY THE NEGATIVE INDEX
# HOW IS THE INDEX COMPUTED


# sfc: Surface Fuel Consumption (kg/m²)

# Amount of surface fuel consumed during the fire
# 0 to 4992 with mean of 2689
# WHY THE 0 VALUES


# tfc: Total Fuel Consumption (kg/m²)

# Total fuel consumed including surface, ground, and crown fuel
# 0 to 9 with mean of 3
# CHECK THE SCALE


# bfc: Burnable Fuel Consumption (kg/m²)

# Amount of biomass fuel consumed
# 0 to 45488500 with mean of 297
# LARGE OUTLIERS IN THIS VARIABLE
# THERE ARE A LOT OF MISSING VALUES FOR THIS VARIABLE 42%
# CHECK THE SIGNIFICANCE


# hfi: Head Fire Intensity (kW/m)

# Intensity of the fire at the head
# -91845 to 93142 with mean of 8544
# CHECK THE NEGATIVE VALUES


# cfb: Crown Fraction Burned (%)

#???
# 0 to 100 with a mean of 33
# MISSING VALUES 3%


# age: Age of the Fire (days)

# 0 to 13569 with mean of 604
# CHECK THE SCALE _ CANNOT BE DAYS MAYBE HOURS
# A LOT OF MISSING VALUES 77 %
t <- 1781266
1378215/t
13569/24
# STILL THE MAX IS 565 DAYS DOESNT SEEM PLAUSABLE


# estarea: Estimated Area Burned (ha)

# Estimated area burned by the fire in hectares
# 0 to 39 with mean of 6
# A LOT OF MISSING VALUES 70%
1250009/t


# polyid: Polygon ID (unique identifier for the fire polygon)
# ??????????
# A LOT OF MISSING VALUES 79%
1424268/t


# pcuring: Percentage of Curing (%)

# ?????????????????
# -1 to 125 with mean of 38
598030/t # 33%


# cfactor: Curing Factor

# ???????????????
# -1 to 1 with mean of 0.1
1037089/t # 58%


# greenup: Green-up factor

# ?????????????
# -1 to 1527 with mean of 0.8
698045/t # 39%


# elev: Elevation (m)

# -1 to 3129 mean of 1007
# CHECK WITH THE DATA FOR BC _ MAYBE THE AREAS AFFECTED BY FIRE ARE HIGHER UP


# cfl: Crown Fire Load (kg/m²)

# The fraction of the crown layer involved in the fire
# -1 to 8.2 with mean of 1.1
# ????????????????????????
379340/t # 21%


# tfc0: Initial Total Fuel Consumption (kg/m²)

# ????????
# 0 to 6 with mean of 3
381683/t # 21%


# sfl: Surface Fire Load (kg/m²)

# -1 to 37 with mean of 9
750818/t # 42 %


# ecozone: Ecozone classification

# BC HAS SPECIFIC ECOZONES 
# THIS VARIABLE CAN BE TURNED INTO CATEGORICAL
# DEPENDS ON FLORA/FOUNA OF THE AREA
# CAN IMPUTE MISSING VALUES BASED ON COORDINATES

hotspots %>% 
  select(ecozone) %>% 
  count(ecozone)
# Ecozones are 4 9 12 13 14


# sfc0: Initial Surface Fuel Consumption (kg/m²)

# ?????????????????
# 0 to 4.9 with mean of 2.5
747535/t # 41%


# cbh: Canopy Base Height (m)
# ???????????????/
# -1 to 17 with mean of 7
1447512/t # 81%

# Essential Variables:

# temp: Influences fire behavior
# rh: Affects moisture content in fuels
# ws: Influences fire spread
# wd: Determines fire spread direction
# pcp: Can help in estimating fire suppression needs
# ffmc, dmc, dc: Measure fuel moisture content
# isi, bui, fwi: Indices for predicting fire behavior
# ros, hfi: Rate of fire spread and intensity


# Specialized Variables:

#   cfb, cfl, tfc, bfc: Advanced metrics for detailed fire behavior analysis


