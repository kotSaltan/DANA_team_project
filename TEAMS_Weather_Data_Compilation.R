# Prepare the datasets

# Load the necessary packages

library(readr)
library(readxl)
library(tidyverse)
library(lubridate)
library(ggplot2)



# Create a new RProject, create data folder with the datasets, set working directory.

# Get the name of working directory in RProject
proj.path <- getwd()


# Read the weather datasets

weather_0 <- read_csv(file.path(proj.path,'data', 'Weather', 'weather_dataset_0_100.csv'))
weather_101 <- read_csv(file.path(proj.path,'data', 'Weather', 'weather_dataset_101_200.csv'))
weather_201 <- read_csv(file.path(proj.path,'data', 'Weather', 'weather_dataset_201_300.csv'))
weather_301 <- read_csv(file.path(proj.path,'data', 'Weather', 'weather_dataset_301_337.csv'))



# Combine data frames into a list for easier processing

dataframes <- list(weather_0, weather_101, weather_201, weather_301)
names(dataframes) <- c("df1", "df2", "df3", "df4")

# Ensure column types are consistent across all data frames.
# Column types have to be consistent for merging later.
# Use nested loops to check for overlaps between each pair of data frames.

# Extract the column names and types from each data frame

column_info <- lapply(dataframes, function(df) {
  data.frame(Column = names(df), Type = sapply(df, class))
})

# Create a summary table that lists the type of each variable for each data frame
all_columns <- unique(unlist(lapply(column_info, function(df) df$Column)))

get_column_type <- function(df_info, column_name) {
  if (column_name %in% df_info$Column) {
    return(df_info$Type[df_info$Column == column_name])
  } else {
    return(NA)
  }
}

column_summary <- data.frame(
  Column = all_columns,
  df1_type = sapply(all_columns, function(col) get_column_type(column_info[[1]], col)),
  df2_type = sapply(all_columns, function(col) get_column_type(column_info[[2]], col)),
  df3_type = sapply(all_columns, function(col) get_column_type(column_info[[3]], col)),
  df4_type = sapply(all_columns, function(col) get_column_type(column_info[[4]], col))
)

print(column_summary)

# There are some mismatches in the variable types across the datasets.
# The variable climate_id is a character in three datasets but numeric in the fourth.
# The variable snow_grnd_flag is character in three datasets but logical in the fourth.

# To ensure consistency, convert these variables to the same type across all datasets.

# Convert column types based on a reference data frame (df1)
convert_column_types <- function(df, ref_df) {
  for (col in names(ref_df)) {
    if (col %in% names(df)) {
      df[[col]] <- as(df[[col]], class(ref_df[[col]]))
    } else {
      df[[col]] <- NA  # Also check for columns with only NA values
    }
  }
  return(df)
}

# Use the first data frame as the reference
ref_df <- weather_0

# Apply the function to all other data frames
dataframes[[2]] <- convert_column_types(dataframes[[2]], ref_df)
dataframes[[3]] <- convert_column_types(dataframes[[3]], ref_df)
dataframes[[4]] <- convert_column_types(dataframes[[4]], ref_df)

# Extract column names and types again to verify
column_info <- lapply(dataframes, function(df) {
  data.frame(Column = names(df), Type = sapply(df, class))
})

all_columns <- unique(unlist(lapply(column_info, function(df) df$Column)))

# Function to get the column type for the summary table
# get_column_type <- function(df_info, column_name) {
#   if (column_name %in% df_info$Column) {
#     return(df_info$Type[df_info$Column == column_name])
#   } else {
#     return(NA)
#   }
# }

# Create the summary table
column_summary <- data.frame(
  Column = all_columns,
  df1_type = sapply(all_columns, function(col) get_column_type(column_info[[1]], col)),
  df2_type = sapply(all_columns, function(col) get_column_type(column_info[[2]], col)),
  df3_type = sapply(all_columns, function(col) get_column_type(column_info[[3]], col)),
  df4_type = sapply(all_columns, function(col) get_column_type(column_info[[4]], col))
)

# Print the summary table
print(column_summary)

# Column types are now consistent across all data frames.

# Merge the data frames into one
weather <- do.call(rbind, dataframes)
write.csv(weather, "weather.csv", row.names = FALSE)


##############################
# The analyses continues on the merged weather dataset

##############################

# Prepare the datasets

# Load the necessary packages

library(readr)
library(readxl)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(corrplot)
library(gridExtra)

# Create a new RProject, create data folder with the datasets, set working directory.

# Get the name of working directory in RProject
proj.path <- getwd()

# Read the weather datasets

weather <- read_csv(file.path(proj.path,'data', 'weather.csv'))
##########################################

# Check for duplicate rows in the combined data frame
sum(duplicated(weather))

# There are no duplicated rows

# Check structure and summary of the data
str(weather)
summary(weather)
names(weather)

# Calculate the number of missing values for each column
na_summary <- sapply(weather, function(x) sum(is.na(x)))

# Calculate the percentage of missing values for each column
na_percentage <- sapply(weather, function(x) mean(is.na(x)) * 100)

# Create a summary table
na_summary_table <- data.frame(Column = names(na_summary), NA_Count = na_summary, NA_Percentage = na_percentage)

# Print the summary table
print(na_summary_table)

# For description of the variables check the documentation:
# Glossary on ECCC's website (https://climate.weather.gc.ca/glossary_e.html).

# The weather dataset contains 1,000,928 entries with the following 38 variables:
  
# **station_name** - The name of the weather station where the data was collected.
# Character - no missing values.

# **station_id** - A unique identifier for the weather station, ranging from 1 to 55363. 
# Numeric - no missing values.

# **station_operator** - Indicates whether a station is operated (TRUE/FALSE).
# Logical - all values are NA.

# **prov**  - Provinces where the stations are located.
# Character - no missing values.
unique(weather$prov)
# The dataset has only one unique entry - "BC", it has been filtered and downloaded correctly.

# **lat** - Latitude of the weather station (geographical coordinate specifying the north-south position).\
# Latitude values ranging from 48.30 to 59.57. 
# Numeric - no missing values.
range(weather$lat)

# **lon** - Longitude of the weather station (geographical coordinate specifying the east-west position).\
# Longitude values ranging from -136.4 to -114.9.
# Numeric - no missing values.
range(weather$lon)

# BC is located between latitudes 48° to 60° N and longitudes -140° to -114° W (Government of British Columbia) 
# https://www2.gov.bc.ca/gov/content/data/geographic-data-services

# **elev** - Elevation of the weather station above sea level, measured in meters.\
# Elevation values ranging from 0 to 2055.0 meters.
# Numeric - no missing values.
range(weather$elev)
# British Columbia has different elevation levels ranging from sea level at the Pacific coast\
# to highest point Mount Fairweather (4,663 meters).
# Typical elevations for inhabited areas and weather stations range from sea level to about 1,500 meters.
# https://www2.gov.bc.ca/gov/content/data/geographic-data-services

ggplot(weather, aes(x = elev)) +
  geom_histogram(binwidth = 50, fill = "darkgreen", color = "black") +
  theme_minimal() +
  labs(title = "Histogram of Elevation", x = "Elevation (m)", y = "Count") +
  scale_y_continuous(labels = scales::comma) 
# The plot shows that the data falls in apropriate range for meteo station locations.

# **climate_id** - A unique identifier for the climate region associated with the weather station.
# The climate identifier is a 7 digit number 
# The first digit of the number indicates the province in which the observing site is located;\ 
# the second and third digits identify the climatological district within the province.
# Character - no missing values.
weather %>% 
  select(climate_id) %>% 
  count(climate_id) %>% 
  arrange(desc(n))

# Check if all ID have 7 characters
all(nchar((weather$climate_id)) == 7)

# Check if all ID start with same digit
first_digits <- substr(weather$climate_id, 1, 1)
unique(first_digits)

# The ID in the dataset fit the description for climate_id for BC

# **WMO_id** - The World Meteorological Organization identifier for the weather station.\
# Five-digit number assigned to Canadian weather stations by the World Meteorological Organization (WMO).\
# This ID is used to identify stations internationally.
# https://climate.weather.gc.ca/glossary_e.html#w
# IDs ranging from 71022 to 73090.
# Numeric - missing values: 561364. (56%)
range(weather$WMO_id, na.rm = TRUE)

weather %>% 
  select(WMO_id) %>% 
  count(WMO_id) %>% 
  arrange(desc(n))

# Check if all ID have 5 characters
all(nchar(weather$WMO_id[!is.na(weather$WMO_id)]) == 5)

# The ID in the dataset fit the description

# **TC_id** - An identifier for Tropical Cyclone data associated with the weather station.
# The ID help link specific weather events, such as cyclones, to the data recorded at the stations.
# Character - missing values: 415792. (41.5%)
# These identifiers are used by Transport Canada to get reports from airport observing sites\
# in real-time, the ID use aviation formats (three-character strings).
unique(weather$TC_id, na.rm = TRUE)
range(weather$TC_id, na.rm = TRUE)

weather %>% 
  select(TC_id) %>% 
  count(TC_id) %>% 
  arrange(desc(n))

# Check if all ID have 3 characters
all(nchar(weather$TC_id[!is.na(weather$TC_id)]) == 3)

# Verify the codes 
# Coordinates for Vancouver International Airport (YVR):
# Latitude: 49.1939
# Longitude: -123.184

# Filter the dataset for the Vancouver International Airport TC_id (YVR)
vancouver_data <- weather %>% filter(TC_id == "YVR")

# Check the coordinates of the filtered data
vancouver_coords <- vancouver_data %>% select(lat, lon)

# Print the coordinates
print(vancouver_coords)

kelowna_data <- weather %>% filter(TC_id == "YLW")
kelowna_data

# The information matches the entries from the dataset.

# **date** - Dates ranging from 2014-01-01 to 2024-06-13.
# Date - no missing values.
# Correct format of YYYY-MM-DD
str(weather$date)
range(weather$date)


# **year** - Years ranging from 2014 to 2024.
# Numeric- no missing values.
range(weather$year)

# **month** - Months of the year.
# Character - no missing values.
unique(weather$month)
# Data from 12 months is present in the dataset.
# There seem to be no incorrect labels.

# **day** - Days of the month. 
# Character - no missing values.
unique(weather$day)
# Data from up to 31 days in a month is present in the dataset.
# There seem to be no incorrect labels.


# **qual** - A quality flag indicating the reliability or quality of the data.
# Character - missing values: 574866. (57.4%)
unique(weather$qual)
# The dataset has two types of qual values: missing and "Only preliminary quality checking".
# The dataset's technical documentation shows that missing quality flags often imply no detected issues with the data,\
# while "Only preliminary quality checking" indicates a less thorough review.

# Visualise the quality trend over time

# Aggregate data by date and qual value
qual_by_date <- weather %>%
  group_by(date, qual) %>%
  summarise(count = n()) %>%
  ungroup()
# Replace NA with a meaningful label for plotting
qual_by_date$qual[is.na(qual_by_date$qual)] <- "No issue"

# Create the line plot
ggplot(qual_by_date, aes(x = date, y = count, color = qual, group = qual)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Quality Flag Values Over Time", x = "Date", y = "Count", color = "Quality Flag") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# By 2024, there is a sharp decline in the count of "Only preliminary quality checking" values,\
# suggesting more complete data during that period.
# The plot shows how the data quality has evolved over time,\
# with possible improvements in data collection and processing.

# The dataset includes several numerical variables that represent specific measurements of various weather conditions.\
# These variables provide detailed information on temperature, wind speed and direction, precipitation, and snow cover.

# **cool_deg_days** - Cooling degree days, a measure used to estimate the energy needed for air conditioning.\
# Cooling degree days, ranging from 0 to 18.3.
# Numeric - missing values: 183210. (18.3%)
range(weather$cool_deg_days, na.rm = TRUE)

# **cool_deg_days_flag** - A flag indicating the quality or status of the `cool_deg_days` data.
# Character - missing values: 968816. (96.7%)

# **dir_max_gust** - The direction of the maximum wind gust, measured in degrees, ranging from 0 to 36.
# Numeric - missing values: 799209. (79.8%)
range(weather$dir_max_gust, na.rm = TRUE)

# **dir_max_gust_flag** - A flag indicating the quality or status of the `dir_max_gust` data.
# Character - missing values: 933017. (93.2%)

# **heat_deg_days** - Heating degree days, a measure used to estimate the energy needed for heating.\
# Heating degree days, ranging from 0 to 59.
# Numeric - missing values: 183210. (18.3%)
range(weather$heat_deg_days, na.rm = TRUE)

# **heat_deg_days_flag** - A flag indicating the quality or status of the `heat_deg_days` data.
# Character - missing values: 968816. (96.8%)

# **max_temp** - The maximum temperature recorded on the observation date, measured in degrees Celsius.
# Maximum temperature, ranging from -99.0 to 50.0 degrees Celsius.
# Numeric - missing values: 175794. (17.5%)
range(weather$max_temp, na.rm = TRUE)

# A possible outlier is present (-99)
boxplot(weather$max_temp)

# Filter rows where max_temp is -99 and print them
weather %>% filter(max_temp == -99)

# There is only a single row with the incorrect value on 2015-03-22. Replace it with NA

# Replace -99 with NA in the max_temp column
weather$max_temp[weather$max_temp == -99] <- NA

boxplot(weather$max_temp)
range(weather$max_temp, na.rm = TRUE)
# Now the maximum temperature ranges from -37.5 to 50.0 degrees Celsius, which is a more reasonable range.

# **max_temp_flag** - A flag indicating the quality or status of the `max_temp` data.
# Character - missing values: 976049. (97.5%)

# **mean_temp** - The mean (average) temperature recorded on the observation date, measured in degrees Celsius.
# Mean temperature, ranging from -41.0 to 36.3 degrees Celsius. 
# Numeric - missing values: 183210. (18.3%)
range(weather$mean_temp, na.rm = TRUE)

# **mean_temp_flag** - A flag indicating the quality or status of the `mean_temp` data.
# Character - missing values: 968816. (96.8%)

# **min_temp** - The minimum temperature recorded on the observation date, measured in degrees Celsius.
# Minimum temperature, ranging from -50.0 to 29.0 degrees Celsius.
# Numeric - missing values: 175214. (17.5%)
range(weather$min_temp, na.rm = TRUE)

# **min_temp_flag** - A flag indicating the quality or status of the `min_temp` data.
# Character - missing values: 975034. (97.4%)

# **snow_grnd** - The depth of snow on the ground, measured in centimeters.
# Snow on the ground, ranging from 0 to 375.0 cm.
# Numeric - missing values: 525523. (52.5%)
range(weather$snow_grnd, na.rm = TRUE)

# **snow_grnd_flag** - A flag indicating the quality or status of the `snow_grnd` data.
# Character - missing values: 973169. (97.2%)

# **spd_max_gust** - The speed of the maximum wind gust, measured in kilometers per hour.
# Speed of maximum gust, ranging from 13.0 to 226.0 km/h.
# Numeric - missing values: 798943. (79.8%)
range(weather$spd_max_gust, na.rm = TRUE)

# **spd_max_gust_flag** - A flag indicating the quality or status of the `spd_max_gust` data. 
# Character - missing values: 932786. (93.2%)

# **total_precip** - The total precipitation (rain, snow, etc.) recorded on the observation date, measured in millimeters.
# Total precipitation, ranging from 0 to 300.6 mm.
# Numeric - missing values: 221178. (22.1%)
range(weather$total_precip, na.rm = TRUE)

# **total_precip_flag** - A flag indicating the quality or status of the `total_precip` data.
# Character - missing values: 890187. (88.9%)

# **total_rain** - The total rainfall recorded on the observation date, measured in millimeters.
# Total rainfall, ranging from 0 to 300.6 mm.
# Numeric - missing values: 540707. (54%)
range(weather$total_rain, na.rm = TRUE)

# **total_rain_flag** - A flag indicating the quality or status of the `total_rain` data.
# Character - missing values: 814576. (81.4%)

# **total_snow** - The total snowfall recorded on the observation date, measured in centimeters.
# Total snowfall, ranging from 0 to 123.0 cm.
# Numeric - missing values: 541673. (54.1%)
range(weather$total_snow, na.rm = TRUE)

# **total_snow_flag** - A flag indicating the quality or status of the `total_snow` data. 
# Character - missing values: 822278. (82.1%)

### Key Observations:

# Several columns have a significant number of missing values, such as `WMO_id`, `qual`, `dir_max_gust`, `mean_temp`, and `total_snow`.
# The dataset contains a mix of numeric, character, logical, and Date types.
# The numerical columns like `lat`, `lon`, `elev`, `max_temp`, `min_temp`, etc., have a wide range of values, indicating diverse geographic and climatic data.

# Flag columns present in the dataset
# Explained in documentation:
# https://climate.weather.gc.ca/doc/Technical_Documentation.pdf
# The flags indicate if the data has passed quality control checks or if it has any issues.

# Check the unique values in each flag column

flag_columns <- c("cool_deg_days_flag", "dir_max_gust_flag", "heat_deg_days_flag", "max_temp_flag",
                  "mean_temp_flag", "min_temp_flag", "snow_grnd_flag", "spd_max_gust_flag",
                  "total_precip_flag", "total_rain_flag", "total_snow_flag")

# Print unique values for each flag column
for (col in flag_columns) {
  print(paste("Unique values in", col, ":"))
  print(unique(weather[[col]]))
}

# Common Values in Flag Columns:
# Blank or NA: Indicates no issues with the data.
# E: Estimated value.
# T: Trace; this typically applies to precipitation measurements indicating a very small amount that is less than the measurable limit.
# M: Missing value.
# L: Precipitation may or may not have occurred; value is 0 or 0.1.
# TRUE: Indicates a logical TRUE value, often seen in older datasets where logical values might be used.



###################################### NUMERICAL DESCRIPTION


# Numerical columns present in the dataset.
# Check all numerical columns.

numerical_columns <- c("cool_deg_days",
                       "dir_max_gust",
                       "heat_deg_days",
                       "max_temp",
                        "mean_temp",
                       "min_temp",
                       "snow_grnd",
                       "spd_max_gust",
                        "total_precip",
                       "total_rain",
                       "total_snow")


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
describe_numerical(weather, numerical_columns)

# LINEPLOTS OF NUMERICAL ####
# 
# 
# # Check that the 'date' column is in Date format
# weather$date <- as.Date(weather$date)
# str(weather$date)
# 
# # Function to create line plots
# create_line_plot <- function(df, col) {
#   ggplot(df, aes(x = date, y = .data[[col]])) +
#     geom_line(color = "blue") +
#     theme_minimal() +
#     labs(title = paste("Line Plot of", col), x = "Date", y = col) +
#     theme(axis.text.x = element_text(angle = 45, hjust = 1))
# }
# 
# # Generate line plots for all numerical columns
# line_plots <- lapply(numerical_columns, function(col) {
#   create_line_plot(weather, col)
# })
# 
# # Arrange plots in a grid
# grid.arrange(grobs = line_plots, ncol = 2)
# 
# # Line Plot of cool_deg_days:
# # This plot shows the cooling degree days, which estimate the energy needed for air conditioning.
# # The plot shows a seasonal pattern, there are clear spikes each year, indicating higher cooling needs during summer.
# 
# # Line Plot of heat_deg_days:
# # This plot shows heating degree days, which estimate the energy needed for heating.
# # The plot shows a clear seasonal pattern with peaks in winter each year, indicating higher heating requirements.
# 
# # Line Plot of mean_temp: The temperature variation is typical with seasonal changes.
# 
# # Line Plot of snow_grnd: This plot shows the depth of snow on the ground.
# # Peaks are during winter months, with zero values during summer.
# 
# # Line Plot of dir_max_gust: The data appears without clear seasonal patterns.
# 
# # Line Plot of max_temp: A cyclic pattern with higher values in summer and lower values in winter.
# # Line Plot of min_temp: Similar to max_temp.
# 
# # Line Plot of spd_max_gust: There are occasional spikes with high wind speeds at irregular intervals.
# 
# # Line Plot of total_precip: Some periods show higher precipitation, indicating heavy rain or snow events.
# 
# # Line Plot of total_rain: Similar to total_precip, frequent and varying.
# 
# # Line Plot of total_snow: The plot shows distinct periods of snowfall, with zero values during summer.
# 
# 
# # The line plots show how different weather conditions change over time.\
# # You can see clear seasonal patterns in temperature and snow depth,\
# # with temperatures rising in summer and falling in winter, and snow appearing mostly in winter.\
# # Wind and precipitation data are more irregular and don't follow a clear pattern.\
# # These plots help us understand how weather changes throughout the years.
# 
# 
# 
# # BOXPLOTS OF NUMERICAL ####
# 
# 
# # Boxplots show distribution and outliers
# # Create individual boxplots
# boxplots <- lapply(numerical_columns, function(col) {
#   ggplot(weather, aes_string(y = col)) +
#     geom_boxplot() +
#     labs(title = col, y = col) +
#     theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
# })
# 
# # Arrange the boxplots in a 3x4 grid
# grid.arrange(grobs = boxplots, ncol = 4, nrow = 3)
# 
# # Most values cluster near zero with significant outliers. 
# 
# 
# 
# # Removing zero values (for plotting only) can give a clearer view of the data,\
# # especially for variables that are often zero (like precipitation and snowfall).\
# # For, example, there is clearly no snow in summer (see lineplots), but zero values affect the boxplot.
# 
# # Filter out zero values and create boxplots for the remaining data:
# 
# 
# create_filtered_boxplot <- function(df, col) {
#   filtered_df <- df %>% filter(.data[[col]] != 0)
#   
#   ggplot(filtered_df, aes_string(y = col)) +
#     geom_boxplot(fill = "lightblue") +
#     theme_minimal() +
#     labs(title = paste("Boxplot of", col, "(Non-zero values)"), y = col)
# }
# 
# 
# 
# # Generate boxplots for all numerical columns after filtering out zero values
# filtered_boxplots <- lapply(numerical_columns, function(col) {
#   create_filtered_boxplot(weather, col)
# })
# 
# # Arrange plots in a grid
# do.call(grid.arrange, c(filtered_boxplots, ncol = 4))
# 
# 
# # The new boxplots will show only the non-zero values, giving a clearer view of the variations and any unusual values in the dataset.
# # 
# # cool_deg_days: Most values are low, with some higher values up to about 15.
# # 
# # dir_max_gust: Values range from 0 to around 36, with no big outliers.
# # 
# # heat_deg_days: Most values are low, but some go up to 59.
# # 
# # max_temp: Temperatures range from about -50 to 50 degrees Celsius, with several low outliers.
# # 
# # mean_temp: Average temperature is around 8.5 degrees Celsius, with outliers on both ends.
# # 
# # min_temp: Temperatures range from -50 to 29 degrees Celsius, with several low outliers and a median just above 0.
# # 
# # snow_grnd: Most values are low, but some reach up to 375 cm, indicating heavy snowfall.
# # 
# # spd_max_gust: Wind speeds range widely, with some up to 226 km/h, and a median around 43 km/h.
# # 
# # total_precip: Most values are low, but some go up to 300.6 mm, indicating heavy precipitation.
# # 
# # total_rain: Similar to total_precip, most values are low, with some up to 300.6 mm.
# # 
# # total_snow: Most values are low, but some reach up to 123 cm, indicating heavy snowfall.
# 
# 
# # Calculate and visualize the correlation matrix
# cor_matrix <- cor(weather[numerical_columns], use = "complete.obs")
# cor_plot <- corrplot(cor_matrix, method = "circle", type = "lower", tl.col = "black", tl.srt = 45)
# 
# # Temperatures: Max, mean, and min temperatures are closely related and move together.
# # Heating vs. Cooling: Higher temperatures mean less heating and more cooling is needed.
# # Precipitation: More rain or snow leads to higher total precipitation.
# # Wind: Wind direction and speed don’t depend much on other weather conditions.
# # Snow: More snow on the ground means more heating is needed.
# 
# 
# # Create pairs scatterplot matrix
# ggpairs(weather[, numerical_columns],
#         upper = list(continuous = scatter_plot),
#         lower = list(continuous = scatter_plot),
#         title = "Scatterplot Matrix of Weather") + 
#   theme(
#     axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 6), 
#     axis.text.y = element_text(size = 6), 
#     strip.text = element_text(size = 8),
#     plot.title = element_text(hjust = 0.5, size = 12)
#   )
# 
# 
# 
# 
# # Numerical data
# 
# # Lineplots to visualize time trends
# 
# # Function to create lineplots
# create_line_plot <- function(df, col) {
#   ggplot(df, aes(x = date, y = .data[[col]])) +
#     geom_line(color = "darkseagreen", linewidth = 1) +
#     geom_point(color = "darkred", size = 0.5) +
#     geom_smooth(method = "loess", color = "steelblue", linewidth = 0.7) +
#     theme_minimal() +
#     labs(title = paste("Line Plot of", col), x = "Date", y = col) +
#     theme(
#       axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
#       axis.text.y = element_text(size = 5),
#       plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
#       axis.title = element_text(size = 12, face = "bold"),
#       panel.grid.major = element_line(color = "gray", linewidth = 0.5),
#       panel.grid.minor = element_line(color = "lightgray", linewidth = 0.25)
#     )
# }
# 
# 
# # Generate lineplots for the first group of numerical columns
# line_plots_1 <- lapply(numerical_columns[1:6], function(col) {
#   create_line_plot(weather, col)
# })
# 
# # Generate lineplots for the second group of numerical columns
# line_plots_2 <- lapply(numerical_columns[7:11], function(col) {
#   create_line_plot(weather, col)
# })
# 
# # Arrange plots in grids
# grid.arrange(grobs = line_plots_1, ncol = 2)
# grid.arrange(grobs = line_plots_2, ncol = 2)
#  