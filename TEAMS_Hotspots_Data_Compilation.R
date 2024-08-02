proj.path <- getwd()

# Load necessary packages

library(readr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(corrplot)
library(gridExtra)
library(reshape2)
library(GGally)
library(caret)



# Read the hotspots datasets
 
hotspots_2013 <- read_csv(file.path(proj.path,'data', 'CSV Files', '2013_Hotspots.csv'))
hotspots_2014 <- read_csv(file.path(proj.path,'data', 'CSV Files', '2014_Hotspots.csv'))
hotspots_2015 <- read_csv(file.path(proj.path,'data', 'CSV Files', '2015_Hotspots.csv'))
hotspots_2016 <- read_csv(file.path(proj.path,'data', 'CSV Files', '2016_Hotspots.csv'))
hotspots_2017 <- read_csv(file.path(proj.path,'data', 'CSV Files', '2017_Hotspots.csv'))
hotspots_2018 <- read_csv(file.path(proj.path,'data', 'CSV Files', '2018_Hotspots.csv'))
hotspots_2019 <- read_csv(file.path(proj.path,'data', 'CSV Files', '2019_Hotspots.csv'))
hotspots_2020 <- read_csv(file.path(proj.path,'data', 'CSV Files', '2020_Hotspots.csv'))
hotspots_2021 <- read_csv(file.path(proj.path,'data', 'CSV Files', '2021_Hotspots.csv'))
hotspots_2022 <- read_csv(file.path(proj.path,'data', 'CSV Files', '2022_Hotspots.csv'))
hotspots_2023 <- read_csv(file.path(proj.path,'data', 'CSV Files', '2023_Hotspots.csv'))

names(hotspots_2013)
names(hotspots_2014)
names(hotspots_2015)
names(hotspots_2016)
names(hotspots_2017)
names(hotspots_2018)
names(hotspots_2019)
names(hotspots_2020)
names(hotspots_2021)
names(hotspots_2022)
names(hotspots_2023)





# Combine data frames into a list for easier processing,\
# work on the column names and types in this list,\
# later can apply the modifications back to original 11 datasets

dataframes <- list(hotspots_2013,
                   hotspots_2014,
                   hotspots_2015,
                   hotspots_2016,
                   hotspots_2017,
                   hotspots_2018,
                   hotspots_2019,
                   hotspots_2020,
                   hotspots_2021,
                   hotspots_2022,
                   hotspots_2023)

# Apply tolower to the column names of all dataframes
dataframes <- lapply(dataframes, function(df) {
  names(df) <- tolower(names(df))
  return(df)
})
sapply(dataframes, names)


# Function to filter dataset for BC
filter_for_bc <- function(df) {
  df %>%
    filter(agency == "BC") %>% 
    droplevels() # Dropping unused factor levels if any
}

# Apply the function to each dataset in the list
dataframes <- lapply(dataframes, filter_for_bc)



## To check the column names across all 11 datasets create a comparison table
# Extract column names from each dataframe
column_names <- lapply(dataframes, names)

# Create a unique list of all column names
all_columns <- unique(unlist(column_names))

# all_columns <- unique(unlist(lapply(dataframes, names)))

# The function will identify which columns (rows in this table) are not present in which dataset (columnsin this table)
column_comparison <- data.frame(matrix(nrow = length(all_columns), ncol = length(dataframes)))
colnames(column_comparison) <- paste0("hotspots_", 2013:2023)

# Fill the comparison table
for (i in seq_along(dataframes)) {
  col_names <- names(dataframes[[i]])
  column_comparison[, i] <- all_columns %in% col_names
}

# Add row names as all unique column names
rownames(column_comparison) <- all_columns

# Display the comparison table
print(column_comparison)

# From the comparison table:
# Common Columns: lat, lon, rep_date, source, sensor, satellite, agency, temp,\
# rh, ws, wd, pcp, ffmc, dmc, dc, isi, bui, fwi, fuel, ros, sfc, tfc, bfc, hfi, cfb,\
# pcuring, greenup, elev are present in all datasets (TRUE across all columns).

# Unique/Additional Columns: cfl, tfc0, sfl, ecozone, sfc0, cbh are not present in some of the earlier datasets\
# (e.g., dataset_2013, dataset_2014, etc.) but are present in later ones.

# Dataframes 2018 and 2020 have the most of the unique columns, each missing 3.

# Choose 2020 as a model dataframe, add the 3 missing columns (filled with NA) to it - age, polyid and cfactor.





# Convert rep_date to date-time format
hotspots_2020$rep_date <- as.POSIXct(hotspots_2020$rep_date, format = "%Y/%m/%d %H:%M:%S")
hotspots_2020$age <- as.numeric(NA)
hotspots_2020$polyid <- as.numeric(NA)
hotspots_2020$cfactor <- as.numeric(NA)

str(hotspots_2020)





# After that check every other dataframe and add the missing columns to them, chech the format.

# Add missing columns and fill with NA
add_missing_columns <- function(df, all_columns) {
  missing_cols <- setdiff(all_columns, names(df))
  df[missing_cols] <- NA
  return(df)
}
# Apply the function
dataframes <- lapply(dataframes, add_missing_columns, all_columns = all_columns)


# Ensure all columns have consistent types and formats
standardize_types <- function(df) {
  # Convert rep_date to POSIXct date format
  if ("rep_date" %in% names(df)) {
    df$rep_date <- as.POSIXct(df$rep_date, format = "%Y/%m/%d %H:%M:%S")
  }
  
  # Ensure numeric columns
  numeric_columns <- c("lat", "lon", "uid", "temp", "rh", "ws", "wd", "pcp", 
                       "ffmc", "dmc", "dc", "isi", "bui", "fwi", "ros", "sfc", 
                       "tfc", "bfc", "hfi", "cfb", "estarea", "pcuring", "greenup", 
                       "elev", "sfl", "cfl", "tfc0", "sfc0", "cbh", "age", "polyid", "cfactor")
  for (col in numeric_columns) {
    if (col %in% names(df)) {
      df[[col]] <- as.numeric(df[[col]])
    }
  }
  
  # Ensure factor columns
  factor_columns <- c("ecozone", "fuel")
  for (col in factor_columns) {
    if (col %in% names(df)) {
      df[[col]] <- as.factor(df[[col]])
    }
  }
  
  return(df)
}
# Apply the function
dataframes <- lapply(dataframes, standardize_types)








# Verify the changes for the first dataset
str(dataframes[[1]])

# Assign the modified dataframes back to the original dataframes
hotspots_2013 <- dataframes[[1]]
hotspots_2014 <- dataframes[[2]]
hotspots_2015 <- dataframes[[3]]
hotspots_2016 <- dataframes[[4]]
hotspots_2017 <- dataframes[[5]]
hotspots_2018 <- dataframes[[6]]
hotspots_2019 <- dataframes[[7]]
hotspots_2020 <- dataframes[[8]]
hotspots_2021 <- dataframes[[9]]
hotspots_2022 <- dataframes[[10]]
hotspots_2023 <- dataframes[[11]]
 
hotspots <- do.call(rbind, dataframes)
write.csv(hotspots, "hotspots.csv", row.names = FALSE)


#### Work on hotspots.csv dataset ####
proj.path <- getwd()
hotspots <- read_csv(file.path(proj.path,'data', 'hotspots.csv'))

str(hotspots)
names(hotspots)


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
                       
boxplot(hotspots$cfb, na.rm = TRUE)

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
