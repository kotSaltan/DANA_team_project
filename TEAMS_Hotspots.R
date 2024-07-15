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
  summarise(n_events = n(), .groups = 'drop') 

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

# write.csv(hotspots_peak, "hotspots_May_Oct.csv", row.names = FALSE)


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






# numerical####
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

# Describe numerical columns for hotspots 
summary_hotspots <- describe_numerical(hotspots, numerical_columns)

# Print the summary table
print(summary_hotspots)


# Describe numerical columns for hotspots_peak
summary_hotspots_peak <- describe_numerical(hotspots_peak, numerical_columns)

# Print the summary table
print(summary_hotspots_peak)




# Create a table with mean values for plots

# Monthly averages for temp, rh, ws and pcp
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
            .groups = 'drop') 

print(monthly_avg)





"temp" # Temperature (°C)####

# This variable shows temperature in Celsius at the specific location, at the fire event
# range -21 to 43 with a mean 21, ok as the set includes information from winter and places with high elevation

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




# Line plot for temperature over time
ggplot(monthly_avg, aes(x = month, y = avg_temp, color = factor(year), group = year)) +
  geom_line(size = 0.5, alpha = 0.6, linetype = "dotted") +  # raw data
  geom_smooth(se = FALSE, method = "loess", size = 1, linetype = "solid") +  # Smoothed trend line
  labs(title = "Temperature by Month",
       x = "Month",
       y = "Temperature (°C)",
       color = "Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# The boxplots and lineplots show  the temperature patterns
# across different years and months in the hotspots dataset.
# 
# he median temperatures for each year generally are around 20-25°C. 
# Some years, like 2016 and 2020, have slightly lower median temperatures.
# There are significant outliers, particularly in 2014, 2015, and 2018,
# showing extremely high or low temperatures on specific days at the event locations.
# 
# 
# Line plots show how temperature flactuates over time.
# Certain years, like 2018 and 2023, show higher peaks, suggesting hotter summer months compared to other years.
# he year 2019 stands out with noticeably lower temperatures during the peak months.
#   

# Explore the the temperature in 2019 as it shows lower values than average
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
  geom_line(color = "darkred") +
  labs(title = "Temperature Over the Peak Season 2014",
       x = "Date",
       y = "Temperature (°C)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(limits = y_limits)

plot_2018 <- ggplot(hotspots_peak %>% filter(year == 2018), aes(x = rep_date, y = temp)) +
  geom_line(color = "darkred") +
  labs(title = "Temperature Over the Peak Season 2018",
       x = "Date",
       y = "Temperature (°C)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(limits = y_limits)

plot_2020 <- ggplot(hotspots_peak %>% filter(year == 2020), aes(x = rep_date, y = temp)) +
  geom_line(color = "darkred") +
  labs(title = "Temperature Over the Peak Season 2020",
       x = "Date",
       y = "Temperature (°C)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(limits = y_limits)

plot_2023 <- ggplot(hotspots_peak %>% filter(year == 2023), aes(x = rep_date, y = temp)) +
  geom_line(color = "darkred") +
  labs(title = "Temperature Over the Peak Season 2023",
       x = "Date",
       y = "Temperature (°C)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(limits = y_limits)

# Arrange the plots side by side
grid.arrange(plot_2014, plot_2018, plot_2020, plot_2023, ncol = 2)


# 2018 and 2023 have on average higher temp during peak fire months



"rh" # Relative Humidity (%)####

# The amount of moisture in the air as a percentage
# 0 to 100 with a mean 36


# Create Line Plots for 4 different years to compare rh

# Define common axis limits
y_limits <- range(hotspots_peak$rh[hotspots_peak$year %in% c(2014, 2018, 2020, 2023)])

# Create plots with common axis limits
plot_2014 <- ggplot(hotspots_peak %>% filter(year == 2014), aes(x = rep_date, y = rh)) +
  geom_line(color = "steelblue") +
  labs(title = "Humidity Over the Peak Season 2014",
       x = "Date",
       y = "Relative Humidity (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(limits = y_limits)

plot_2018 <- ggplot(hotspots_peak %>% filter(year == 2018), aes(x = rep_date, y = rh)) +
  geom_line(color = "steelblue") +
  labs(title = "Humidity Over the Peak Season 2018",
       x = "Date",
       y = "Relative Humidity (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(limits = y_limits)

plot_2020 <- ggplot(hotspots_peak %>% filter(year == 2020), aes(x = rep_date, y = rh)) +
  geom_line(color = "steelblue") +
  labs(title = "Humidity Over the Peak Season 2020",
       x = "Date",
       y = "Relative Humidity (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(limits = y_limits)

plot_2023 <- ggplot(hotspots_peak %>% filter(year == 2023), aes(x = rep_date, y = rh)) +
  geom_line(color = "steelblue") +
  labs(title = "Humidity Over the Peak Season 2023",
       x = "Date",
       y = "Relative Humidity (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(limits = y_limits)

# Arrange the plots side by side
grid.arrange(plot_2014, plot_2018, plot_2020, plot_2023, ncol = 2)

# The variability in relative humidity makes it difficult to establish a consistent trend. 



# PLOTS TO COMPARE TEMP AND RH


# Plot monthly temperature averages
temp_plot <- ggplot(monthly_avg, aes(x = month, y = avg_temp, color = factor(year), group = year)) +
  geom_line() +
  labs(title = "Average Temperature by Month",
       x = "Month",
       y = "Average Temperature (°C)",
       color = "Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Plot monthly humidity averages
humidity_plot <- ggplot(monthly_avg, aes(x = month, y = avg_rh, color = factor(year), group = year)) +
  geom_line() +
  labs(title = "Average Humidity by Month",
       x = "Month",
       y = "Average Humidity (%)",
       color = "Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



grid.arrange(temp_plot, humidity_plot, ncol = 1)




# PLOTS FOR 4 YEARS

# Filter for specific years
monthly_avg_filtered <- monthly_avg %>% 
  filter(year %in% c(2014, 2018, 2020, 2023))



# Plot monthly temperature averages
temp_plot_filtered <- ggplot(monthly_avg_filtered, aes(x = month, y = avg_temp, color = factor(year), group = year)) +
  geom_line(size = 1) +
  labs(title = "Average Temperature by Month",
       x = "Month",
       y = "Average Temperature (°C)",
       color = "Year") +
  scale_color_manual(values = c("2014" = "#F8766D", "2018" = "#00C19F", "2020" = "#619CFF", "2023" = "#FF61C3")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot monthly humidity averages
humidity_plot_filtered <- ggplot(monthly_avg_filtered, aes(x = month, y = avg_rh, color = factor(year), group = year)) +
  geom_line(size = 1) +
  labs(title = "Average Humidity by Month",
       x = "Month",
       y = "Average Humidity (%)",
       color = "Year") +
  scale_color_manual(values = c("2014" = "#F8766D", "2018" = "#00C19F", "2020" = "#619CFF", "2023" = "#FF61C3")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Arrange the plots side by side
grid.arrange(temp_plot_filtered, humidity_plot_filtered, ncol = 1)

# There is a combination of high temperatures and low humidity during the peak fire months (June to September).
# 2018 and 2023 had higher temperatures and lower humidity, meaning more fires.
# When humidity rises sharply in October, fire activity goes down,
# indicating the end of the peak fire season.


"ws" # Wind Speed (km/h)####

# 0 to 59 with mean of 9
# Higher wind speeds in peak fire months can make fire more intense and make it spread faster.

# Common wind speed scale used is the Beaufort scale (in m/s)
# Convert wind speed from km/h to m/s (t) - to plot wd later
hotspots_peak$ws <- hotspots_peak$ws * 0.27778

# Plot monthly wind speed averages with custom colors
ws_plot_filtered <- ggplot(monthly_avg_filtered, aes(x = month, y = avg_ws, color = factor(year), group = year)) +
  geom_line(size = 1) +
  labs(title = "Average Wind Speed by Month",
       x = "Month",
       y = "Average Wind Speed (m/s)",
       color = "Year") +
  scale_color_manual(values = c("2014" = "#F8766D", "2018" = "#00C19F", "2020" = "#619CFF", "2023" = "#FF61C3")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Arrange the plots side by side
grid.arrange(temp_plot_filtered, humidity_plot_filtered, ws_plot_filtered, ncol = 1)

# Wind speed shows significant variability.



"wd" # Wind direction (degrees)

# Visualise with Wind Rose to see most common wind direction
library(openair)

windRose(mydata = hotspots_peak, ws = "ws", wd = "wd", 
         main = "Wind Rose", paddle = FALSE)

# Most of the wind comes from the west (W) and southwest (SW). 
# Regions east and north of areas with strong western, southern and southwestern winds
# should be careful, as these winds can quickly spread fires.
# Wind patterns have are important for wildfire management.



"pcp" # Precipitation (mm)####
# 0 to 651 with mean of 0.2
# So the amount is very low generally


# Plot monthly precipitation averages
ggplot(monthly_avg, aes(x = month, y = avg_pcp, color = factor(year), group = year)) +
  geom_line(size = 0.5, alpha = 0.6, linetype = "dotted") +  # raw data
  geom_smooth(se = FALSE, method = "loess", size = 1, linetype = "solid") +  # Dashed line for trend
  labs(title = "Average Precipitation by Month",
       x = "Month",
       y = "Average Precipitation (mm)",
       color = "Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# The plot indicates a clear seasonal variation in precipitation, 
# with a noticeable peak in July for most years. 


# indices####



"ffmc" # Fine Fuel Moisture Code####
# A numeric rating of the moisture content of litter and other cured fine fuels.
# This code is an indicator of the relative ease of ignition and the flammability of fine fuel.

# The FFMC scale ranges from 0 to 101.
# 0-30: Very wet conditions; ignition is difficult.
# 30-70: Damp conditions; moderate difficulty for ignition.
# 70-85: Dry conditions; fuels are easily ignitable.
# 85-101: Very dry conditions; fuels are highly ignitable and fires can spread rapidly.

# The values in the dataset indicate that during the peak season,
# the FFMC values are generally high, with a median around 91.50 and a maximum of 99.00.
# This suggests that the fine fuels are often in a dry state,
# making them highly ignitable and prone to rapid fire spread.

# ADD NEW MEAN VALUES
# Monthly averages for temp, rh, ws and pcp, ffmc
monthly_avg <- hotspots_peak %>%
  group_by(year, month) %>%
  summarise(avg_temp = mean(temp, na.rm = TRUE),
            avg_rh = mean(rh, na.rm = TRUE),
            avg_ws = mean(ws, na.rm = TRUE),
            avg_pcp = mean(pcp, na.rm = TRUE),
            avg_ffmc = mean(ffmc, na.rm = TRUE),
            .groups = 'drop') 

print(monthly_avg)



# Plot histogram for FFMC
ggplot(hotspots_peak, aes(x = ffmc)) +
  geom_histogram(binwidth = 2, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of FFMC Values", x = "FFMC", y = "Frequency") +
  scale_y_continuous(labels = comma) + 
  theme_minimal()

# Plot boxplot for FFMC
ggplot(hotspots_peak, aes(x = factor(year), y = ffmc)) +
  geom_boxplot(fill = "steelblue", color = "black", alpha = 0.7) +
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

# The line plot shows that FFMC values peak every year during the fire season,
# indicating drier conditions that make it easier for fires to start.

# The boxplots show that most years have high FFMC values above 90,
# meaning conditions are very dry and prone to fires. Only 2020 shows slightly lower dryness.

# The histogram reveals that most FFMC values from 2014 to 2023 are between 85 and 99,
# confirming consistently dry conditions.


"dmc" # Duff Moisture Code####
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


# ADD NEW MEAN VALUES
# Monthly averages for temp, rh, ws and pcp, ffmc, dmc
monthly_avg <- hotspots_peak %>%
  group_by(year, month) %>%
  summarise(avg_temp = mean(temp, na.rm = TRUE),
            avg_rh = mean(rh, na.rm = TRUE),
            avg_ws = mean(ws, na.rm = TRUE),
            avg_pcp = mean(pcp, na.rm = TRUE),
            avg_ffmc = mean(ffmc, na.rm = TRUE),
            avg_dmc = mean(dmc, na.rm = TRUE),
            .groups = 'drop') 

print(monthly_avg)



# Plot histogram for DMC
ggplot(hotspots_peak, aes(x = dmc)) +
  geom_histogram(binwidth = 2, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of DMC Values", x = "DMC", y = "Frequency") +
  scale_y_continuous(labels = comma) + 
  theme_minimal()

# Plot boxplot for DMC
ggplot(hotspots_peak, aes(x = factor(year), y = dmc)) +
  geom_boxplot(fill = "steelblue", color = "black", alpha = 0.7) +
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


# The histogram shows that most DMC values range between 0 and 100,
# with a peak around 50-100. Values above 200 are less common but indicate long dry periods.
# 
# The boxplot for each year reveals that 2017 and 2018 had particularly high outliers,
# indicating extremely dry conditions that could lead to fires.
# On the other hand, 2016 and 2019 had lower DMC values, showing less severe drought.
# 
# The line plot indicates that DMC values peak in late summer (July-August)
# and decrease in autumn.
# This shows that mid to late summer is typically drier, 
# the risk of fire ignition and spread is high during these months.



"dc" # Drought Code ####
# A numeric rating of the average moisture content of deep, compact organic layers.
# This code is a useful indicator of seasonal drought effects on forest fuels
# and the amount of smoldering in deep duff layers and large logs.


# 0-100: Indicates wet conditions with low fire potential.
# 100-300: Moderate drought conditions with increasing fire potential.
# 300-500: High drought conditions, leading to high fire risk.
# 500+: Indicates extreme drought, posing a very high fire risk and potential for intense, prolonged burning.

# NEED ADVICE - MAYBE DO THIS AVERAGE TABLE IN THE BEGINNING FOR ALL. 
# FOR NOW WILL UPDATE IT EVERYTIME

# ADD NEW MEAN VALUES
# Monthly averages for temp, rh, ws and pcp, ffmc, dmc, dc
monthly_avg <- hotspots_peak %>%
  group_by(year, month) %>%
  summarise(avg_temp = mean(temp, na.rm = TRUE),
            avg_rh = mean(rh, na.rm = TRUE),
            avg_ws = mean(ws, na.rm = TRUE),
            avg_pcp = mean(pcp, na.rm = TRUE),
            avg_ffmc = mean(ffmc, na.rm = TRUE),
            avg_dmc = mean(dmc, na.rm = TRUE),
            avg_dc = mean(dc, na.rm = TRUE),
            .groups = 'drop') 

print(monthly_avg)



# Plot histogram for DC
ggplot(hotspots_peak, aes(x = dc)) +
  geom_histogram(binwidth = 20, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of DC Values", x = "DC", y = "Frequency") +
  scale_y_continuous(labels = comma) + 
  theme_minimal()

# Plot boxplot for DC
ggplot(hotspots_peak, aes(x = factor(year), y = dc)) +
  geom_boxplot(fill = "steelblue", color = "black", alpha = 0.7) +
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

# The histogram of DC values from 2014 to 2023 shows most values are between 300 and 600,
# peaking around 500. This means BC often has conditions that can lead to deep-burning fires.

# The boxplot of DC values across years show the variability and trends in drought conditions over time.
# 2017 and 2018: These years have higher median and upper whisker values,
# meaning very dry conditions, likely leading to severe fire seasons.
# 2016 and 2019: These years show lower DC values, 
# suggesting wetter conditions and potentially less severe fire activity.
  
# The line plot of DC values by month shows a clear seasonal trend,
# DC values increase during the summer months (July to August) and decrease in the autumn.
# This occurs at the time of the peak fire season.
# There are a lot of high DC values, more than 500, it shows conditions for deep-burning fires.


# Compare indices trends to timeline of fire events####

# Analyze the trends of key indices (FFMC DMC DC) and the number of fire events.
# This can show how environmental conditions, indicated by these indices, correlate with the frequency of fires.

# Normalize the number of fire events to allow for a meaningful comparison with the indices. 
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

# The plots show a clear relationship between the fire indices (FFMC, DMC, and DC) and the number of fires. 
# When the number of fires is high, the indices also show higher values.

# It's important to note that while high indices indicate conditions favorable for fires, they alone do not cause fires. 
# Additional factors, such as human activities or lightning strikes, are necessary to ignite fires under these conditions.



"isi" # Initial Spread Index ####
# A numerical rating of the expected rate of fire spread
# based on wind speed, temperature, and fine fuel moisture content. 
# ISI is crucial for understanding how quickly a fire can spread once it has ignited.

# 0-3: Low spread potential. Fires will spread slowly and are relatively easy to control.
# 4-7: Moderate spread potential. Fires spread more quickly and may require more effort to control.
# 8-12: High spread potential. Fires spread rapidly and can be difficult to control.
# 13-19: Very high spread potential. Fires spread very rapidly and are challenging to control.
# 20+: Extreme spread potential. Fires spread uncontrollably and can be extremely dangerous.

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
            .groups = 'drop') 

print(monthly_avg)

# Plot histogram for ISI
ggplot(hotspots_peak, aes(x = isi)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of ISI Values", x = "ISI", y = "Frequency") +
  scale_y_continuous(labels = scales::comma) + 
  theme_minimal()

# Plot boxplot for ISI
ggplot(hotspots_peak, aes(x = factor(year), y = isi)) +
  geom_boxplot(fill = "steelblue", color = "black", alpha = 0.7) +
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

# The graphs illustrate that ISI values tend to peak during the summer months,
# indicating higher fire spread potential during this period.
# The boxplot shows variability in ISI values across years, with notable outliers in 2014 and 2020.
# The histogram indicates that most ISI values are low to moderate.

# Analyses of the extreme outliers


# Remove extreme outliers
hotspots_peak_filtered <- hotspots_peak%>%
  filter(isi < 60)
dim(hotspots_peak_filtered)

# Filter out entries with ISI values greater than 60
event_outliers <- hotspots_peak %>%
  filter(isi >= 60)

# Check single event
dim(event_outliers)
view(event_outliers)



# The data from July 19, 2014, shows very high ISI values at a specific location in British Columbia.
# This matches the date of the Mount McAllister fire, a large wildfire that burned over 20,000 hectares.
# The weather that day included high temperatures, low humidity, and strong winds, which made the fire spread quickly.
# These conditions explain the high ISI values, indicating a high potential for severe fire behavior.
# The data points from July 19, 2014, are all in the same cluster, showing that the clustering algorithm grouped them correctly.

# Filter the specific event cluster and near events
event_McAllister <- hotspots %>%
  filter(event_cluster %in% 370:372) 

# Calculate monthly averages for July 2014
monthly_avg_july <- monthly_avg %>%
  filter(year == 2014 & month == "Jul")

# Plot the weather conditions with average lines
ggplot(event_McAllister, aes(x = rep_date)) +
  geom_point(aes(y = temp, color = "Temperature"), size = 3) +
  geom_point(aes(y = rh, color = "Relative Humidity"), size = 3) +
  geom_point(aes(y = ws, color = "Wind Speed"), size = 3) +
  geom_hline(aes(yintercept = monthly_avg_july$avg_temp, color = "Avg Temperature"), linetype = "dashed") +
  geom_hline(aes(yintercept = monthly_avg_july$avg_rh, color = "Avg Relative Humidity"), linetype = "dashed") +
  geom_hline(aes(yintercept = monthly_avg_july$avg_ws, color = "Avg Wind Speed"), linetype = "dashed") +
  labs(title = "Weather Conditions on July 19, 2014 with Monthly Averages",
       x = "Date",
       y = "Value",
       color = "Variable") +
  scale_color_manual(values = c("Temperature" = "red3", "Relative Humidity" = "steelblue2", "Wind Speed" = "green",
                                "Avg Temperature" = "red3", "Avg Relative Humidity" = "steelblue2", "Avg Wind Speed" = "green")) +
  theme_minimal()


# Create a summary table of the weather conditions
weather_summary <- event_McAllister %>%
  select(lat, lon, rep_date, temp, rh, ws, wd, pcp, ffmc, dmc, dc, isi, bui, fwi, ros)

# Print the summary table
print(weather_summary)


# This graph shows the weather conditions at McAllister Creek on July 19, 2014,
# including temperature, relative humidity, and wind speed.
# It compares these values to the average conditions for July 2014.
# The wind speed during this event is notably higher than the monthly average,
# suggesting stronger winds that could help spread the fire.



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
            .groups = 'drop') 

print(monthly_avg)

# Plot histogram for ISI
ggplot(hotspots_peak, aes(x = isi)) +
  geom_histogram(binwidth = 2, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of ISI Values", x = "ISI", y = "Frequency") +
  scale_y_continuous(labels = scales::comma) + 
  theme_minimal()

# Plot boxplot for ISI
ggplot(hotspots_peak_filtered, aes(x = factor(year), y = isi)) +
  geom_boxplot(fill = "steelblue", color = "black", alpha = 0.7) +
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



# The histogram now shows a more typical range of ISI values, removing the extreme values that distorted the original plot.
# 
# The boxplot shows a clearer, more consistent ISI distribution across the years after removing high outliers.
# 
# The seasonal pattern plot still peaks in mid-summer but appears smoother without the extreme values.
# 
# Wind speed greatly affects ISI, as seen on July 19, 2014, at McAllister Creek, where high wind speeds caused high ISI values.


# SHOW CORRELATION OF ISI AND WEATHER INDICES
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
            avg_ros = mean(ros, na.rm = TRUE),
            avg_hfi = mean(hfi, na.rm = TRUE),
            .groups = 'drop') 

print(monthly_avg)

# Using monthly averages instead of individual observations for quicker plotting

ggplot(monthly_avg, aes(x = avg_ws, y = avg_isi)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Average ISI vs Average Wind Speed", x = "Average Wind Speed (km/h)", y = "Average ISI") +
  theme_minimal()


ggplot(monthly_avg, aes(x = avg_temp, y = avg_isi)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Average ISI vs Average Temperature", x = "Average Temperature (°C)", y = "Average ISI") +
  theme_minimal()


ggplot(monthly_avg, aes(x = avg_rh, y = avg_isi)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "green") +
  labs(title = "Average ISI vs Average Relative Humidity", x = "Average Relative Humidity (%)", y = "Average ISI") +
  theme_minimal()

# Average ISI vs. Average Relative Humidity: Shows that as relative humidity increases, ISI decreases.
# Average ISI vs. Average Wind Speed: Shows that higher wind speeds are linked to higher ISI values.
# Average ISI vs. Average Temperature: Shows that higher temperatures are associated with higher ISI values.


# Select columns 
data_corr <- hotspots_peak_filtered %>%
  select(isi, ws, temp, rh)

# Calculate the correlation matrix
corr_matrix <- cor(data_corr)

# Plot the correlation matrix
corrplot(corr_matrix, method = "circle", type = "lower",
         tl.col = "black", tl.srt = 45, title = "Correlation Matrix of ISI and Weather Variables",
         mar = c(0, 0, 1, 0))

# Strong negative correlation between ISI and relative humidity.
# Strong positive correlation between ISI and temperature.
# Moderate positive correlation between ISI and wind speed.




"bui" # Buildup Index ####
# A numerical rating of the total amount of fuel available for combustion.
# It is derived from the Duff Moisture Code (DMC) and the Drought Code (DC)

# Low: 0-40
# Moderate: 41-80
# High: 81-120
# Extreme: 121 and above

ggplot(hotspots_peak_filtered, aes(x = bui)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of BUI Values", x = "BUI", y = "Frequency") +
  scale_y_continuous(labels = scales::comma) + 
  theme_minimal()

ggplot(hotspots_peak_filtered, aes(x = factor(year), y = bui)) +
  geom_boxplot(fill = "steelblue", color = "black", alpha = 0.7) +
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

# The histogram shows that BUI values mostly range from 60 to 100. This means the fire potential is moderate to high for most of the dataset.
# Boxplots show BUI values vary year to year. Some years have higher values, which means drier conditions and a higher chance of intense fires.
# BUI peaks in mid-summer, which matches the time when fire activity is usually the highest.


# Scatter plots to visualize relationships using average values
ggplot(monthly_avg, aes(x = avg_dmc, y = avg_bui)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Average BUI vs Average DMC", x = "Average DMC", y = "Average BUI") +
  theme_minimal()

ggplot(monthly_avg, aes(x = avg_dc, y = avg_bui)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Average BUI vs Average DC", x = "Average DC", y = "Average BUI") +
  theme_minimal()



# Select columns
data_corr <- hotspots_peak_filtered %>%
  select(bui, dmc, dc)

# Calculate the correlation matrix
corr_matrix <- cor(data_corr)

# Plot the correlation matrix
corrplot(corr_matrix, method = "circle", type = "lower",
         tl.col = "black", tl.srt = 45, title = "Correlation Matrix of BUI, DMC, and DC",
         mar = c(0, 0, 1, 0))

# BUI and DMC have a very strong positive correlation, 
# indicating that they are closely related (fire potential and moisture of layer).
# BUI and DC also show a positive correlation, 
# though it is slightly weaker.

# BUI, DMC, and DC are all indicators of fire potential. 
# High values in these indices suggest drier conditions, which can lead to higher fire. 


# ISI vs BUI

# ISI is more directly related to the rate of fire spread. 
# It can how how quickly a fire can move,
# particularly in higher wind conditions.

# BUI shows more total fire potential.
# It accounts for deeper fuel moisture and long-term drought conditions,
# showing amount of fuel available for a fire.


"fwi" # Fire Weather Index ####
# A numeric rating of fire intensity. It is based on the ISI and the BUI, 
# and is used as a general index of fire danger throughout the forested areas of Canada.

# Low (0-5): Minimal fire danger.
# Moderate (6-12): Fires can start from most accidental causes, but the spread is slow.
# High (13-20): Fires can start easily and spread rapidly.
# Very High (21-30): Fires will start very easily, spread rapidly, and burn intensely.
# Extreme (31+): Fires start and spread quickly, and are intense and challenging to control.

ggplot(hotspots_peak_filtered, aes(x = fwi)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of FWI Values", x = "FWI", y = "Frequency") +
  scale_y_continuous(labels = scales::comma) + 
  theme_minimal()

ggplot(hotspots_peak_filtered, aes(x = factor(year), y = fwi)) +
  geom_boxplot(fill = "steelblue", color = "black", alpha = 0.7) +
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

# The histogram shows that most FWI values are between 10 and 30, 
# fire danger is from moderate to very high. 
# Many values are above 30, severe fire weather conditions are frequent.

# The boxplot shows the distribution of FWI values for different years. 
# The line plot shows that FWI peaks in mid-summer, peak fire activity period. 
# The data shows significant variability in fire danger, 
# summer months and certain years  have extreme outliers.

# FWI varies significantly year to year due to a combination of climatic factors, 
# such as temperature, precipitation, and wind speed. 
# For deeper analyses and trends weather has to be closely inspected.

# The variation is influenced by both short-term weather patterns 
# and long-term changes.

# The FWI is calculated using the Initial Spread Index and the Buildup Index,
# both of which take into account wind speed,
# temperature, humidity, and fuel moisture. 
# This is why FWI a reliable indicator of overall fire danger.

# Fire agencies use the FWI to inform the public, issue fire bans, and respond to fires. 
# It helps prioritize resources and actions to mitigate fire risks effectively.



# Compare indices trends to timeline of fire events####

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
  scale_color_manual(values = c("ISI" = "steelblue")) +
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
  scale_color_manual(values = c("BUI" = "steelblue")) +
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
  scale_color_manual(values = c("FWI" = "steelblue")) +
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





"fuel" # Fuel Type  ####

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

# Print the fuel counts
print(fuel_counts)

# Create a bar plot for the fuel column
ggplot(hotspots_peak_filtered, aes(x = fuel)) +
  geom_bar(fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Fuel Types", x = "Fuel Type", y = "Count") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# The dataset shows a variety of fuel types, with "C2" (566,651 records)
# and "C3" (383,781 records) being the most common. 
# Other significant fuel types include "D1" (187,803 records) 
# and "C7" (149,801 records).



"ros" # Rate of Spread ####
# The predicted speed of the fire at the front or head of the fire (where the fire moves fastest),
# and takes into account both crowning and spotting. 
# It is measured in metres per minute and is based on the Fuel Type, Initial Spread Index, Buildup Index, 
# and several fuel-specific parameters such as phenological state (leafless or green) in deciduous trees, 
# crown base height in coniferous trees, and percent curing in grasses.
# The scale ranges from 0 to over 20 m/min in the dataset.



ggplot(hotspots_peak_filtered, aes(x = ros)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black", alpha = 0.7) +
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


ggplot(hotspots_peak_filtered, aes(x = factor(year), y = ros)) +
  geom_boxplot(fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "Rate of Spread Distribution Across Years",
       x = "Year",
       y = "ROS (m/min)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10))


# The plot shows that the ROS can vary significantly year to year, 
# with some years experiencing more extreme fire spread conditions.

# Scatter plot for average ROS vs average ISI
ggplot(monthly_avg, aes(x = avg_isi, y = avg_ros)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Average ROS vs Average ISI",
       x = "Average ISI",
       y = "Average ROS") +
  theme_minimal()

# Scatter plot for average ROS vs average BUI
ggplot(monthly_avg, aes(x = avg_bui, y = avg_ros)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Average ROS vs Average BUI",
       x = "Average BUI",
       y = "Average ROS") +
  theme_minimal()

# Scatter plot for average ROS vs average FWI
ggplot(monthly_avg, aes(x = avg_fwi, y = avg_ros)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "green") +
  labs(title = "Average ROS vs Average FWI",
       x = "Average FWI",
       y = "Average ROS") +
  theme_minimal()

# Calculate correlations between ROS, ISI, BUI, and FWI
correlation_ros_indices <- hotspots_peak_filtered %>%
  select(ros, isi, bui, fwi) %>%
  cor(use = "complete.obs")

# Print the correlation matrix
print(correlation_ros_indices)


# Plot the correlation matrix
corrplot(correlation_ros_indices, method = "circle", type = "lower",
         tl.col = "black", tl.srt = 45, title = "Correlation Matrix of ROS and Other Indices",
         mar = c(0, 0, 1, 0))

# The scatter plots for ROS vs FWI, ROS vs BUI, and ROS vs ISI all show a clear upward trend.
# This means that as the values of FWI, BUI, and ISI increase,
# the Rate of Spread also increases. Higher values indicate more severe fire conditions, 
# more available fuel, and faster initial fire spread.
# This means the fire spreads faster.

# The matrix shows strong positive correlations between ROS and ISI, ROS and FWI,
# and a moderate positive correlation between ROS and BUI.



"sfc" # Surface Fuel Consumption 
# SFC measures the amount of surface fuel (e.g., grasses, leaves, small branches) consumed by a fire, in kg/m²,
# indicating fire intensity. It isrelevant to initial spread of fire as easily ignitible material burns first.
# Ranges from 0 kg/m² (no fuel) to several kg/m² (dense vegetation).


print(monthly_avg)

ggplot(hotspots_peak, aes(x = sfc)) +
  geom_histogram(binwidth = 0.1, fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution Surface Fuel Consumption at Fire Hotspots",
       x = "SFC (kg/m²)",
       y = "Frequency") +
  scale_y_continuous(labels = comma) + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))

# Most SFC values are between 0 to 1.5 kg/m².
# Peaks around 0 and 1.5 kg/m² suggest many fires had low to moderate fuel consumption.
# Fewer high-intensity fires with higher SFC values.


ggplot(hotspots_peak, aes(x = factor(year), y = sfc)) +
  geom_boxplot(fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "Surface Fuel Consumption Distribution Across Years",
       x = "Year",
       y = "SFC (kg/m²)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10))

# SFC varies throughout the years. 
# The SFC can also depend on the type of fuel available.



"tfc" # Total Fuel Consumption
# TFC measures the total amount of all types of fuels (including surface fuels, larger woody fuels, and subsurface organic matter)
# consumed by the fire in kg/m².
# It shows full impact of the fire on the ecosystem.


print(monthly_avg)

ggplot(hotspots_peak, aes(x = tfc)) +
  geom_histogram(binwidth = 0.1, fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution Total Fuel Consumption at Fire Hotspots",
       x = "TFC (kg/m²)",
       y = "Frequency") +
  scale_y_continuous(labels = comma) + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))

# The histogram shows a wide range of TFC values,
# with a notable amount of lower values around 0-3 kg/m² and a few extreme outliers reaching up to 10 kg/m².


ggplot(hotspots_peak, aes(x = factor(year), y = tfc)) +
  geom_boxplot(fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "Total Fuel Consumption Distribution Across Years",
       x = "Year",
       y = "TFC (kg/m²)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10))

# 2017, 2018 and 2021 had relatively high median TFC values, indicating significant fuel consumption in those years.
# 2020 shows a significantly lower median TFC compared to other years, less fuel consumption on average.


"bfc"      

"hfi" # Head Fire Intencity ####
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
ggplot(hotspots_peak_filtered, aes(x = hfi)) +
  geom_histogram(binwidth = 1000, fill = "skyblue", color = "black", alpha = 0.7) + # Each bin represent a range of 1000 HFI units.
  labs(title = "Distribution of HFI Values",
       x = "HFI (kW/m)",
       y = "Frequency") +
  scale_y_continuous(labels = scales::comma) + 
  scale_x_continuous(breaks = seq(0, 75000, 10000)) +
  theme_minimal()

ggplot(hotspots_peak_filtered, aes(x = factor(year), y = hfi)) +
  geom_boxplot(fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "HFI Distribution Across Years",
       x = "Year",
       y = "HFI (kW/m)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10))


# The histogram shows the frequency distribution of HFI values.
# Most HFI values are concentrated at the lower end of the scale, as values increase there are fewer.
# Very high HFI values are outliers they show occasional extremely intense fires.
# While most fires are less intense, the few high-intensity fires can be significant and impactful.



# The boxplots show the distribution of HFI across different years.
# The median value varies yearly, there are fluctuations in fire intensity.
# There are significant outliers in almost all years, there are some fires with extremely high intensities.



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



# more variables####





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

############## draft####






# Compare indices trends to timeline of fire events####

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
  scale_color_manual(values = c("ISI" = "steelblue")) +
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
  scale_color_manual(values = c("BUI" = "steelblue")) +
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
  scale_color_manual(values = c("FWI" = "steelblue")) +
  scale_fill_manual(values = c("Fire Occurrences" = "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# The plots show a clear link between indices and the number of fires.
# When there are more fires, the FWI is also higher.

# It's important to remember that while high FWI values mean conditions are good for fires,
# they don't cause fires by themselves.
# Other factors like human activities or lightning strikes are needed to start fires in these conditions.

# Also, while the FWI helps predict how severe fires might be,
# it can't tell us exactly how many fires will happen in a season.
# For example, the FWI was lower in 2018 than in 2017, but 2018 still had some major fires. 
# This shows that indices can indicate fire conditions but don't predict the exact number of fires.









# quick load####

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
proj.path <- getwd()
hotspots_raw <- read_csv(file.path(proj.path,'data', 'hotspots.csv'))
hotspots_raw$rep_date <- as.POSIXct(hotspots_raw$rep_date, format = "%Y-%m-%d %H:%M:%S")
hotspots_raw$year <- year(hotspots_raw$rep_date)
hotspots_raw$month <- month(hotspots_raw$rep_date, label = TRUE, abbr = TRUE) 
hotspots <- hotspots_raw %>%filter(year >= 2014 & year <= 2023)

event_data <- hotspots %>%select(lat, lon, rep_date)
event_data$date_numeric <- as.numeric(as.POSIXct(event_data$rep_date))
event_data$date_scaled <- event_data$date_numeric * 0.001
db <- dbscan(event_data[, c("lat", "lon", "date_scaled")], eps = 0.6, minPts = 5)
hotspots$event_cluster <- db$cluster

event_details <- hotspots %>%  filter(event_cluster != 0) %>%  group_by(year) %>%  summarise(    first_cluster = first(event_cluster),    start_date_hotspot = min(rep_date),    end_date_hotspot = max(rep_date),    events_count = length(unique(event_cluster))  )
fire_events_per_month <- hotspots %>%  group_by(month) %>%  summarise(n_events = n())
fire_events_wide <- fire_events_per_month %>%  pivot_wider(names_from = month, values_from = n_events, values_fill = 0)


hotspots_peak <- hotspots %>%  filter(month(rep_date) %in% c(5, 6, 7, 8, 9, 10))

fire_events_per_month_subset <- hotspots_peak %>%  filter(event_cluster != 0) %>%  group_by(year, month) %>%  summarise(n_events = n()) %>%  ungroup()
fire_events_subset_wide <- fire_events_per_month_subset %>%  pivot_wider(names_from = month, values_from = n_events, values_fill = 0)

events_count <- event_details %>%  select(year, events_count)
hotspots_df_summary <- hotspots %>%  group_by(year) %>%  summarise(    start_date = min(rep_date),    end_date = max(rep_date),    hotspots_df_day_count = as.numeric(difftime(max(rep_date), min(rep_date), units = "days"))  )
hotspots_df_summary <- hotspots_df_summary %>%  left_join(events_count, by = "year") %>%  mutate(    start_month_day = format(as.Date(start_date), "%m-%d"),    end_month_day = format(as.Date(end_date), "%m-%d")  )


numerical_columns <- c('temp',                       'rh',                       'ws',                       'wd',                       'pcp',                       'ffmc',                       'dmc',                       'dc',                       'isi',                       'bui',                       'fwi',                       'ros',                       'sfc',                       'tfc',                       'bfc',                       'hfi',                       'cfb',                       'age',                       'estarea',                       'pcuring',                       'cfactor',                       'greenup',                       'elev',                       'cfl',                       'tfc0',                       'sfl',                       'ecozone',                       'sfc0',                       'cbh')

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

summary_hotspots <- describe_numerical(hotspots, numerical_columns)
summary_hotspots_peak <- describe_numerical(hotspots, numerical_columns)


monthly_avg <- hotspots_peak %>%  group_by(year, month) %>%
  summarise(avg_temp = mean(temp, na.rm = TRUE),
            avg_rh = mean(rh, na.rm = TRUE),
            avg_ws = mean(ws, na.rm = TRUE),
            avg_pcp = mean(pcp, na.rm = TRUE),
            avg_ffmc = mean(ffmc, na.rm = TRUE),
            .groups = 'drop') 



#####
#