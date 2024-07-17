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
library(ggbreak)

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

# Convert numeric month to ordered factor with month names
weather_peak$month <- factor(weather_peak$month, 
                             levels = c("05", "06", "07", "08", "09", "10"),
                             labels = c("May", "Jun", "Jul", "Aug", "Sep", "Oct"),
                             ordered = TRUE)

# Check the structure to confirm the change
str(weather_peak$month)


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
summary_weather_peak <- describe_numerical(weather_peak, c("mean_temp", "spd_max_gust", "total_precip"))

# Print the summary table
print(summary_weather_peak)


# Table shows NA values in spd_max_gust  (80%)




# Monthly averages 
monthly_avg_weather <- weather_peak %>%
  group_by(year, month) %>%
  summarise(avg_temp = mean(mean_temp, na.rm = TRUE),
            avg_ws = mean(spd_max_gust, na.rm = TRUE),
            avg_pcp = mean(total_precip, na.rm = TRUE),
            
            .groups = 'drop') 

print(monthly_avg_weather)

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







# Quality check of FLAG columns####


# Cross-reference missing values for spd_max_gust with the flag column
missing_ws_flag <- weather %>%
  group_by(spd_max_gust_flag) %>%
  summarise(
    total_count = n(),
    missing_ws_count = sum(is.na(spd_max_gust)),
    missing_ws_percentage = (missing_ws_count / total_count) * 100
  ) %>%
  arrange(desc(missing_ws_percentage))

print(missing_ws_flag)

# The M flag is a clear indicator of missing values for spd_max_gust, 
# as all entries with this flag have missing values.

# The NA flag is linked to missing spd_max_gust values.
# When the flag is NA, 78.4% of the time the spd_max_gust value is also missing.

# Only 2 of the E flagged entries have missing values for spd_max_gust. This is a estimation flag.

# Missing spd_max_gust values typically indicate that wind gust data 
# was not recorded or reported due to equipment malfunctions, maintenance periods, 
# or conditions where instruments were not operational. 
# It does not imply the absence of wind. 
# Calm winds (mean wind speeds less than 2 knots) can also contribute to missing entries 
# if the instruments do not detect significant movement
# https://www.canada.ca/en/environment-climate-change/services/weather-manuals-documentation/manobs-surface-observations.html
# https://www.canada.ca/en/environment-climate-change/services/climate-change/canadian-centre-climate-services/display-download/technical-documentation-hourly-data.html

# To visualize the data, remove the missing spd_max_gust values when plotting. 
# Can analyze the available data without the distortions caused by missing entries.






# Weather Variables Plots####
# NEED TO MAKE ALL FOUR VARIABLES AND DESCRIBE)

names(weather_peak)
monthly_avg_weather <- weather_peak %>%
  group_by(year, month) %>%
  summarise(avg_temp = mean(mean_temp, na.rm = TRUE),
            avg_ws = mean(spd_max_gust, na.rm = TRUE),
            avg_pcp = mean(total_precip, na.rm = TRUE),
            .groups = 'drop') 

print(monthly_avg_weather)


# Analyze the distribution of mean temperature

# Plot histogram for mean_temp
ggplot(weather_peak %>%
         filter(!is.na(mean_temp)),
       aes(x = mean_temp)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Mean Temperature Values", 
       x = "Mean Temperature (°C)", 
       y = "Frequency") +
  scale_y_continuous(labels = scales::comma) + 
  theme_minimal()


# Plot boxplot for mean_temp
ggplot(weather_peak %>%
         filter(!is.na(mean_temp)),
       aes(x = factor(year), y = mean_temp)) +
  geom_boxplot(fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "Mean Temperature Distribution Across Years",
       x = "Year",
       y = "Mean Temperature (°C)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10))

# The distribution is approximately bell-shaped, normal distribution of mean temperature values.
# Most temperatures are centered around 10-15°C.
# The temperatures range from approximately -15°C to 35°C, with the majority falling between 0°C and 25°C.
# There are several outliers each year, with some extreme low values (around -10°C) and high values (above 30°C).
# The boxplot shows the distribution of mean temperature values

# No significant changes in median temperatures over the years,
# there are stable temperature trends within the dataset.

# Line plot for mean_temp over time
ggplot(monthly_avg_weather, aes(x = month, y = avg_temp, color = factor(year), group = year)) +
  geom_line(size = 0.5, alpha = 0.6, linetype = "dotted") +  # Raw data as dotted lines
  geom_smooth(se = FALSE, method = "loess", size = 1, linetype = "solid") +  # Smoothed trend line
  labs(title = "Average Monthly Mean Temperature Over Years",
       x = "Month",
       y = "Average Mean Temperature (°C)",
       color = "Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.title = element_text(size = 15, hjust = 0.5),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8))

# The plot shows that the mean temperature generally peaks around July (07) and August (08) for most years.
# General pattern of temperature rise and fall is consistent across the years, similar seasonal temperature trends.
# The average mean temperature ranges from about 10°C in the earlier and later months (May and October) to around 20°C at the peak (July and August).



# Windspeed ####

# Analyze the distribution of Wind Speed

# Plot histogram for spd_max_gust
ggplot(weather_peak %>%
         filter(!is.na(spd_max_gust)),
       aes(x = spd_max_gust)) +
  geom_histogram(binwidth = 2, fill = "lightgreen", color = "black", alpha = 0.7) + # The bin width is set to 2 km/h.
  labs(title = "Distribution of Max Wind Speed Gust Values", 
       x = "Max Wind Speed Gust (km/h)", 
       y = "Frequency") +
  scale_y_continuous(labels = scales::comma) + 
  theme_minimal()


# Plot boxplot for spd_max_gust
ggplot(weather_peak %>%
         filter(!is.na(spd_max_gust)),
       aes(x = factor(year), y = spd_max_gust)) +
  geom_boxplot(fill = "lightblue", color = "black", alpha = 0.7) +
  labs(title = "Max Wind Speed Gust Distribution Across Years",
       x = "Year",
       y = "Max Wind Speed Gust (km/h)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10))

# The histogram shows a right-skewed distribution,
# that lower wind speeds are more frequent than higher wind speeds.
# Most max wind speed gust values are centered around 20-40 km/h.
# The wind speeds range from 0 to over 200 km/h, the majority is below 50 km/h.
# Despite some variations, the general pattern of wind speed is somewhat consistent across the years,
# indicating similar seasonal wind speed trends.

# Line plot for avg_ws over time
ggplot(monthly_avg_weather, aes(x = month, y = avg_ws, color = factor(year), group = year)) +
  geom_line(size = 0.5, alpha = 0.6, linetype = "dotted") +  # Raw data as dotted lines
  geom_smooth(se = FALSE, method = "loess", size = 1, linetype = "solid") +  # Smoothed trend line
  labs(title = "Average Monthly Wind Speed Over Years",
       x = "Month",
       y = "Average Wind Speed (km/h)",
       color = "Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.title = element_text(size = 15, hjust = 0.5),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8))

# The plot shows that wind speed generally increases from May to October for most years.
# Despite some variations, the pattern of wind speed rise and fall is consistent across the years,
# there are similar seasonal wind speed trends.

# PCP ####

# Analyze the distribution of Precipitation

# Plot histogram for total_precip
ggplot(weather_peak %>%
         filter(!is.na(total_precip)),
       aes(x = total_precip)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Total Precipitation Values", 
       x = "Total Precipitation (mm)", 
       y = "Frequency") +
  scale_y_continuous(labels = scales::comma) + 
  theme_minimal()


# Plot boxplot for total_precip
ggplot(weather_peak %>%
         filter(!is.na(total_precip)),
       aes(x = factor(year), y = total_precip)) +
  geom_boxplot(fill = "lightgreen", color = "black", alpha = 0.7) +
  labs(title = "Total Precipitation Distribution Across Years",
       x = "Year",
       y = "Total Precipitation (mm)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10))

# The histogram shows many days with little or no precipitation, mainly around zero.
# There are a few days with very high rainfall, as the distribution has a long tail to the right.

# Most rainfall amounts are low and close to zero.
# Each year has a few days with very high rainfall, shown as outliers.
# The median values remain relatively consistent.


# There is a large number of zero values that distort the distribution and hide meaningful patterns in the data.
# To create a cleared visualization and to see how the data can be cleaned,
# create the histogram plot with a gap using ggbreak

ggplot(weather_peak %>%
                      filter(!is.na(total_precip)),
                    aes(x = total_precip)) +
  geom_histogram(binwidth = 2, fill = "lightblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Total Precipitation Values", 
       x = "Total Precipitation (mm)", 
       y = "Frequency") +
  scale_y_continuous(labels = scales::comma) + 
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  ) +
  scale_x_break(c(30, 100), scales = 1) +
  theme(axis.ticks.y.right = element_blank(),
        axis.text.y.right = element_blank(),
        axis.title.y.right = element_blank())


# There are a few very high precipitation values (100 mm and above) that appear to be outliers. 
# These extreme values could be significant weather events that
# impact the distribution, skewing the analysis.



# Identify extreme values
extreme_values <- weather_peak %>%
  filter(total_precip > 200)

# Display extreme values
print(extreme_values)
 
# These values range from 208.2 mm to 286.6 mm.
# The dates of these extreme events are in the years 2017 and 2019.
# These high values likely correspond to significant weather events, such as heavy rainstorms.

# Identify heavy rains
rain_values <- weather_peak %>%
  filter(total_precip > 100)

# Count heavy rain events by year
heavy_rain_count <- rain_values %>%
  group_by(year) %>%
  summarise(count = n())

# Display the count of heavy rain events by year
print(heavy_rain_count)

# There are a total of 76 events in the overall 10 year dataset suggesting that the heavy rains occur few times a year.
# The exception is 2017 with 25 heavy rains and 2020 and 2021 with 11 heavy rain events


# Line plot for avg_pcp over time
ggplot(monthly_avg_weather, aes(x = month, y = avg_pcp, color = factor(year), group = year)) +
  geom_line(size = 0.5, alpha = 0.6, linetype = "dotted") +  # Raw data as dotted lines
  geom_smooth(se = FALSE, method = "loess", size = 1, linetype = "solid") +  # Smoothed trend line
  labs(title = "Average Monthly Precipitation Over Years",
       x = "Month",
       y = "Average Precipitation (mm)",
       color = "Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.title = element_text(size = 15, hjust = 0.5),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8))

# Precipitation generally increases towards the end of the season,
# with peaks in September and October.


# Each year shows unique trends, indicating variability in weather patterns.
# Some years, such as 2023, show significantly higher peaks in precipitation compared to others.
# DRAFT ####
names(weather)






# Filter out light rain values and heavy rain values (>50 mm)
filtered_precip <- weather_peak %>%
  filter(total_precip > 2.5 & total_precip <= 50)

# Plot histogram for filtered precipitation values
hist_filtered <- ggplot(filtered_precip,
                        aes(x = total_precip)) +
  geom_histogram(binwidth = 2, fill = "lightblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Total Precipitation Values (Excluding Extremes)", 
       x = "Total Precipitation (mm)", 
       y = "Frequency") +
  scale_y_continuous(labels = scales::comma) + 
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )

# Print the plot
print(hist_filtered)






# Based on the official guidelines from Environment and Climate Change Canada (ECCC), 
# rainfall intensity is classified into three main categories:

#   Light Rain:  
#   Rate: 2.5 mm/hour or less
# Description: Light rain usually has minimal impact on visibility and does not accumulate quickly.
# Moderate Rain:

#   Rate: Between 2.6 mm and 7.5 mm/hour
# Description: Moderate rain has a more noticeable impact, potentially forming puddles and causing slight visibility reductions.
# Heavy Rain:

#   Rate: Greater than 7.6 mm/hour
# Description: Heavy rain can lead to significant runoff, flooding, and substantial visibility reductions

