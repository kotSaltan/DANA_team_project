# Load the necessary packages ####

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

# Create a new RProject, create data folder with the datasets, set working directory.

# Get the name of working directory in RProject
proj.path <- getwd()

# Read the weather datasets

weather <- read_csv(file.path(proj.path,'data', 'weather.csv'))
names(weather)
range(weather$date)
str(weather)



# Subset for peak period ####
 
# Ensure the date column is in Date format
weather <- weather %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"),
         month = format(date, "%m"))

# Set range of dates to analyze
start_date <- as.Date('2014-01-01')
end_date <- as.Date('2023-12-31')

# Subset the data by date range
weather_peak <- weather %>%
  filter(date >= start_date & date <= end_date)

# Subset the data for May to October
weather_peak <- weather_peak %>%
  filter(month %in% c("05", "06", "07", "08", "09", "10"))

# Select only the specified variables
weather_peak <- weather_peak %>%
  select(station_name, station_id, lat, lon, date, year, month, day, 
         mean_temp, spd_max_gust, total_precip, total_rain)



# Describe numerical ####

# Function to describe each numerical column
describe_numerical <- function(df, cols) {
  summary_list <- list()
  
  for (col in cols) {
    summary_stats <- data.frame(
      Variable = col,
      Min = round(min(df[[col]], na.rm = TRUE), 2),
      Median = round(median(df[[col]], na.rm = TRUE), 2),
      Mean = round(mean(df[[col]], na.rm = TRUE), 2),
      Max = round(max(df[[col]], na.rm = TRUE), 2),
      Missing_Values = sum(is.na(df[[col]])),
      NA_Percentage = round(sum(is.na(df[[col]])) / nrow(df) * 100, 2)
    )
    summary_list[[col]] <- summary_stats
  }
  
  summary_table <- bind_rows(summary_list)
  return(summary_table)
}



# Get the summary for each numerical column
summary_weather_peak <- describe_numerical(weather_peak, c("mean_temp", "spd_max_gust", "total_precip", "total_rain"))

# Print the summary table
print(summary_weather_peak)


# Table shows NA values in spd_max_gust  (80%) and total_rain (54%)




# Monthly averages 
monthly_avg <- weather_peak %>%
  group_by(year, month) %>%
  summarise(avg_temp = mean(mean_temp, na.rm = TRUE),
            avg_ws = mean(spd_max_gust, na.rm = TRUE),
            avg_pcp = mean(total_precip, na.rm = TRUE),
            avg_rain = mean(total_rain, na.rm = TRUE),
            
            .groups = 'drop') 

print(monthly_avg)

# Missing values ####

# Calculate the count of missing values by year and by month
# to see when most data is missing


# See missing values of Mean Temperature by year

missing_temp <- weather_peak %>%
  group_by(year) %>%
  summarise(
    total_count = n(),  # Total number of records for the year
    missing_count = sum(is.na(mean_temp)),  # Count of missing values in mean_temp
    missing_percentage = (missing_count / total_count) * 100  # Percentage of missing values
  ) %>%
  arrange(desc(year))  # Arrange in descending order of year

missing_temp

# See missing values of Mean Temperature by month

missing_temp <- weather_peak %>%
  group_by(month) %>%
  summarise(
    total_count = n(),
    missing_count = sum(is.na(mean_temp)),
    missing_percentage = (missing_count / total_count) * 100
  ) %>%
  arrange(desc(month))

missing_temp

# The missing values range from 16.0% to 23.0% per year.
# The year with the highest percentage of missing values is 2018 with 23.0%.
# The missing values are consistent over the years with a slight increase in some years,
# but there is no drastic fluctuation.

# See missing values of Max Speed Gust by year

missing_ws <- weather_peak %>%
  group_by(year) %>%
  summarise(
    total_count = n(),
    missing_count = sum(is.na(spd_max_gust)),
    missing_percentage = (missing_count / total_count) * 100
  ) %>%
  arrange(desc(year))

missing_ws

# See missing values of Max Speed Gust by month

missing_ws <- weather_peak %>%
  group_by(month) %>%
  summarise(
    total_count = n(),
    missing_count = sum(is.na(spd_max_gust)),
    missing_percentage = (missing_count / total_count) * 100
  ) %>%
  arrange(desc(month))

missing_ws

# The missing values range from 76.3% to 84.2% per year.
# The year with the highest percentage of missing values is 2014 with 84.2%.
# The percentage of missing values is quite high, there is an trend over the years.


# See missing values of Total Precipitation by year

missing_pcp <- weather_peak %>%
  group_by(year) %>%
  summarise(
    total_count = n(),
    missing_count = sum(is.na(total_precip)),
    missing_percentage = (missing_count / total_count) * 100
  ) %>%
  arrange(desc(year))

missing_pcp

# See missing values of Total Precipitation by month

missing_pcp <- weather_peak %>%
  group_by(month) %>%
  summarise(
    total_count = n(),
    missing_count = sum(is.na(total_precip)),
    missing_percentage = (missing_count / total_count) * 100
  ) %>%
  arrange(desc(month))

missing_pcp

# The missing values range from 16.0% to 28.0% per year.
# The year with the highest percentage of missing values is 2019 with 28.0%.
# There is an increase in missing values in more recent years, from 2018 onwards.



# See missing values of Total Rain by year

missing_rain <- weather_peak %>%
  group_by(year) %>%
  summarise(
    total_count = n(),
    missing_count = sum(is.na(total_rain)),
    missing_percentage = (missing_count / total_count) * 100
  ) %>%
  arrange(desc(year))

missing_rain

# See missing values of Total Rain by month

missing_rain <- weather_peak %>%
  group_by(month) %>%
  summarise(
    total_count = n(),
    missing_count = sum(is.na(total_rain)),
    missing_percentage = (missing_count / total_count) * 100
  ) %>%
  arrange(desc(month))

missing_rain

# The missing values range from 44.8% to 61.2% per year.
# The year with the highest percentage of missing values is 2023 with 61.2%.
# There is an increasing trend of missing values over the years.




# Quality check of FLAG columns####
# Include the flag variables to see if it has info about the NA

# Select only the specified variables
weather_peak_flag <- weather %>%
  filter(month %in% c("05", "06", "07", "08", "09", "10"))

weather_peak_flag <- weather_peak_flag %>%
  select(station_name, station_id, lat, lon, date, year, month, day, 
         spd_max_gust, spd_max_gust_flag, total_rain, total_rain_flag)

unique(weather_peak_flag$spd_max_gust_flag)
unique(weather_peak_flag$total_rain_flag)

# Count flag types by month
flag_counts_by_month <- weather_peak_flag %>%
  group_by(month) %>%
  summarize(
    NA_total_rain = sum(total_rain_flag == "M", na.rm = TRUE),
    NA_spd_max_gust = sum(spd_max_gust_flag == "M", na.rm = TRUE),
    T_total_rain = sum(total_rain_flag == "T", na.rm = TRUE),
    L_total_rain = sum(total_rain_flag == "L", na.rm = TRUE),
    E_spd_max_gust = sum(spd_max_gust_flag == "E", na.rm = TRUE)
  ) %>%
  arrange(desc(NA_total_rain), desc(NA_spd_max_gust))

# Count flag types by year
flag_counts_by_year <- weather_peak_flag %>%
  group_by(year) %>%
  summarize(
    M_total_rain = sum(total_rain_flag == "M", na.rm = TRUE),
    M_spd_max_gust = sum(spd_max_gust_flag == "M", na.rm = TRUE),
    T_total_rain = sum(total_rain_flag == "T", na.rm = TRUE),
    L_total_rain = sum(total_rain_flag == "L", na.rm = TRUE),
    E_spd_max_gust = sum(spd_max_gust_flag == "E", na.rm = TRUE)
  ) %>%
  arrange(desc(M_total_rain), desc(M_spd_max_gust))

# Display the summaries
print(flag_counts_by_month)
print(flag_counts_by_year)






#######################




# Weather Variables Plots####
# NEED TO MAKE ALL FOUR VARIABLES AND DESCRIBE)

names(weather_peak)
monthly_avg <- weather_peak %>%
  group_by(year, month) %>%
  summarise(avg_temp = mean(mean_temp, na.rm = TRUE),
            avg_ws = mean(spd_max_gust, na.rm = TRUE),
            avg_pcp = mean(total_precip, na.rm = TRUE),
            avg_rain = mean(total_rain, na.rm = TRUE),
            .groups = 'drop') 

print(monthly_avg)

# Plot histogram for mean_temp
ggplot(weather_peak, aes(x = mean_temp)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of mean_temp Values", x = "mean_temp", y = "Frequency") +
  scale_y_continuous(labels = scales::comma) + 
  theme_minimal()

# Plot boxplot for mean_temp
ggplot(weather_peak, aes(x = factor(year), y = mean_temp)) +
  geom_boxplot(fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "mean_temp Distribution Across Years",
       x = "Year",
       y = "mean_temp") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10))

# Line plot for mean_temp over time
ggplot(monthly_avg, aes(x = month, y = avg_temp, color = factor(year), group = year)) +
  geom_line(size = 0.5, alpha = 0.6, linetype = "dotted") +  # raw data
  geom_smooth(se = FALSE, method = "loess", size = 1, linetype = "solid") +  # Smoothed trend line
  labs(title = "mean_temp by Month",
       x = "Month",
       y = "mean_temp",
       color = "Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



########

# WINDSPEED
names(weather_peak)
monthly_avg <- weather_peak %>%
  group_by(year, month) %>%
  summarise(avg_temp = mean(mean_temp, na.rm = TRUE),
            avg_ws = mean(spd_max_gust, na.rm = TRUE),
            avg_pcp = mean(total_precip, na.rm = TRUE),
            avg_rain = mean(total_rain, na.rm = TRUE),
            .groups = 'drop') 

print(monthly_avg)

# Plot histogram for spd_max_gust
ggplot(weather_peak, aes(x = spd_max_gust)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of spd_max_gust Values", x = "spd_max_gust", y = "Frequency") +
  scale_y_continuous(labels = scales::comma) + 
  theme_minimal()

# Plot boxplot for spd_max_gust
ggplot(weather_peak, aes(x = factor(year), y = spd_max_gust)) +
  geom_boxplot(fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "spd_max_gust Distribution Across Years",
       x = "Year",
       y = "spd_max_gust") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10))

# Line plot for spd_max_gust over time
ggplot(monthly_avg, aes(x = month, y = avg_ws, color = factor(year), group = year)) +
  geom_line(size = 0.5, alpha = 0.6, linetype = "dotted") +  # raw data
  geom_smooth(se = FALSE, method = "loess", size = 1, linetype = "solid") +  # Smoothed trend line
  labs(title = "avg_ws by Month",
       x = "Month",
       y = "avg_ws",
       color = "Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# PCP
# Plot histogram for total_precip
ggplot(weather_peak, aes(x = total_precip)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of total_precip Values", x = "total_precip", y = "Frequency") +
  scale_y_continuous(labels = scales::comma) + 
  theme_minimal()

# Plot boxplot for total_precip
ggplot(weather_peak, aes(x = factor(year), y = total_precip)) +
  geom_boxplot(fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "total_precip Distribution Across Years",
       x = "Year",
       y = "total_precip") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10))

# Line plot for total_precip over time
ggplot(monthly_avg, aes(x = month, y = avg_pcp, color = factor(year), group = year)) +
  geom_line(size = 0.5, alpha = 0.6, linetype = "dotted") +  # raw data
  geom_smooth(se = FALSE, method = "loess", size = 1, linetype = "solid") +  # Smoothed trend line
  labs(title = "avg_pcp by Month",
       x = "Month",
       y = "avg_pcp",
       color = "Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# RAIN
# Plot histogram for total_rain
ggplot(weather_peak, aes(x = total_rain)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of total_rain Values", x = "total_rain", y = "Frequency") +
  scale_y_continuous(labels = scales::comma) + 
  theme_minimal()

# Plot boxplot for total_rain
ggplot(weather_peak, aes(x = factor(year), y = total_rain)) +
  geom_boxplot(fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "total_rain Distribution Across Years",
       x = "Year",
       y = "total_rain") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10))

# Line plot for total_rain over time
ggplot(monthly_avg, aes(x = month, y = avg_rain, color = factor(year), group = year)) +
  geom_line(size = 0.5, alpha = 0.6, linetype = "dotted") +  # raw data
  geom_smooth(se = FALSE, method = "loess", size = 1, linetype = "solid") +  # Smoothed trend line
  labs(title = "avg_rain by Month",
       x = "Month",
       y = "avg_rain",
       color = "Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


