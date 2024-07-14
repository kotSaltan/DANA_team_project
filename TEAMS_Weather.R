# Load the necessary packages

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



##############
 

# Subset the data for May to October
weather_peak <- weather %>%
  filter(month %in% c("05", "06", "07", "08", "09", "10"))

# Select only the specified variables
weather_peak <- weather_peak %>%
  select(station_name, station_id, lat, lon, date, year, month, day, 
         mean_temp, spd_max_gust, total_precip, total_rain)

library(dplyr)


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
numerical_summary <- describe_numerical(weather_peak, c("mean_temp", "spd_max_gust", "total_precip", "total_rain"))

# Display the summary table
numerical_summary

# Table shows NA values in spd_max_gust  (80%) and total_rain (54%)


# Calculate the count of missing values for total_rain and spd_max_gust by month
# to see when most data is missing
weather_peak %>%
  group_by(month) %>%
  summarize(
    NA_total_rain = sum(is.na(total_rain)),
    NA_spd_max_gust = sum(is.na(spd_max_gust))
  ) %>%
  arrange(desc(NA_total_rain), desc(NA_spd_max_gust))


# Calculate the count of missing values for total_rain and spd_max_gust by year
# to see when most data is missing
weather_peak %>%
  group_by(year) %>%
  summarize(
    NA_total_rain = sum(is.na(total_rain)),
    NA_spd_max_gust = sum(is.na(spd_max_gust))
  ) %>%
  arrange(desc(NA_total_rain), desc(NA_spd_max_gust))

# Missing values for total_rain are highest in May and lowest in September. 
# The highest number of missing values are 2020-2023
# The year 2024 has significantly fewer missing values



# NA values####
# Incluse the flag variables to see if it has info about the NA

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
  arrange(desc(NA_total_rain), desc(NA_spd_max_gust))

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



