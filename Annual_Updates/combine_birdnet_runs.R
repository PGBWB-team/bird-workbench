# This file will used to combine BirdNET-Analyzer results 

library(lubridate)
library(fst)
library(data.table)
library(DBI)
library(RSQLite)
library(dplyr)
library(fuzzyjoin)

#################################################
# Set file path variables (REQUIRES USER INPUT) #
#################################################

# These files contains the BirdNET-Analyzer results for each year and for all confidence levels
# *** Add a new file variable for a new year *** #
file_2024 <- "C:/Users/laure/Dropbox/Lauren Wick/2024 BirdNET runs//Combined_2024_V1.txt"
file_2023 <- "C:/Users/laure/Dropbox/Lauren Wick/2023 BirdNET runs//Combined_2023_V1.txt"
file_2022 <- "C:/Users/laure/Dropbox/Lauren Wick/2022 BirdNET runs//Combined_2022_V1.txt"
file_2021 <- "C:/Users/laure/Dropbox/Lauren Wick/2021 BirdNET runs//Combined_2021_V1.txt"
file_2020 <- "C:/Users/laure/Dropbox/Lauren Wick/2020 BirdNET runs//Combined_2020_V1.txt"

# Combine file variables into one list
# *** Add new file variable to the list function *** #
file_list <- c(file_2024, file_2023, file_2022, file_2021, file_2020)

# Set file path for writing .fst output
fst_output <- "C:/Users/laure/Dropbox/Prairie Haven/FST Output/fst_output_061125_weather.fst"

# File path to SQL weather database
weather_path <- "C:/Users/laure/Dropbox/Lauren Wick/Weather Data/valley.weather.db"

################
# Start script #
################

# Read each file as a table
data <- lapply(file_list, read.table, header=TRUE, sep="\t", colClasses="character", quote="\"")

# Filter out observations with less than 70% confidence: 
# First, convert "Confidence" column to numeric, handling any non-numeric values
data <- lapply(data, function(df) {
  df$Confidence <- suppressWarnings(as.numeric(as.character(df$Confidence)))
  df$Confidence[is.na(df$Confidence)] <- 0 # Replace NA with 0
  return(df)
})

# Subset rows with Confidence >= 0.7000
# *** Update this variable if you would like to change the confidence cutoff *** #
# data_70 <- lapply(data, subset, as.numeric(Confidence) >= 0.7000)
data_70 <- data

# Create function to normalize file name lengths
normalize_length <- function(x, target_length = 3) {
  if (length(x) > target_length) {
    return(x[1:target_length]) # truncate to the first 3 elements
  } else if (length(x) < target_length) {
    return(c(x, rep(NA, target_length - length(x)))) # pad with NA
  } else {
    return(x) # already at target length
  }
}

get_week_from_date <- function(date) {
  if (anyNA(date)) return(NA_integer_)
  
  # Extract year from the given date
  year <- as.integer(format(date, "%Y"))
  
  # Get the first day of the year
  first_day <- as.Date(paste0(year, "-01-01"))
  
  # Find the first Sunday of the year
  first_sunday <- first_day + (7 - lubridate::wday(first_day) + 1) %% 7
  
  # Calculate the difference in days between the given date and the first Sunday
  days_since_first_sunday <- as.integer(as.Date(date) - as.Date(first_sunday))
  
  # Determine week number
  week_number <- (days_since_first_sunday %/% 7) +1
  
  return(week_number)
}

# Add columns for Location, Date, and Date.Time, Day.Of.Year
# These variables are derived from the File Path variable
data_70 <- lapply(data_70, function(df) {
  df[c("Location", "Date", "Date.Time")] <- do.call(rbind, lapply(strsplit(basename(df$Begin.Path), split="_"), normalize_length))
  df["Date.Time"] <- paste(df$Date, substr(df$Date.Time, start=1, stop=6))
  df["Date"] <- as.Date(df$Date, "%Y%m%d")
  df["Date.Time"] <- ymd_hms(df$Date.Time, tz="UTC")
  df["Day.Of.Year"] <- yday(df$Date)
  df["Month"] <- factor(month.abb[lubridate::month(as.Date(df$Date))], levels = month.abb)
  df["Week"] <- get_week_from_date(df$Date)

  return(df)
})

# Remove file path and replace with only file name
data_70 <- lapply(data_70, function(df) {
  df["Begin.Path"] <- basename(df$Begin.Path)
  return(df)
})

# Rename recorders
data_70 <- lapply(data_70, function(df) {
  df$Location[df$Location == "SM-SCH1"] <- "House"
  df$Location[df$Location == "SM-SCH1-V2"] <- "House"
  df$Location[df$Location == "SMMIN1-GLEN"] <- "Glen"
  df$Location[df$Location == "SM-SCH2-GLEN"] <- "Glen"
  df$Location[df$Location == "SM1-PRA"] <- "Prairie"
  df$Location[df$Location == "SM-SCH2-PRA"] <- "Prairie"
  df$Location[df$Location == "SM-SCH2-WET"] <- "Wetland"
  df$Location[df$Location == "SM-SCH2-WET2"] <- "Wetland"
  df$Location[df$Location == "SM-SCH3-OAKS"] <- "Savanna"
  df$Location[df$Location == "SM-SCH3-OAK2"] <- "Savanna"
  df$Location[df$Location == "SM-SCH2-WROA"] <- "Forest"
  
  return(df)
})


# Subset to locations of interest
# There are some recorder names that don't correspond to the 6 locations, so we want to filter these out of the dataset
location_list <- c("House", "Glen", "Prairie", "Wetland", "Savanna", "Forest")
data_70_subset <- lapply(data_70, subset, Location %in% location_list)

# Combine all dataframes within list into one large data frame
all_data <- do.call('rbind', data_70_subset)

################
# WEATHER DATA #
################

# Connect to SQLite databae
con <- dbConnect(RSQLite::SQLite(),
                 weather_path)

# Query for hourly temp and wind
query <- "
WITH wind_hourly AS (
  SELECT 
    strftime('%Y-%m-%d %H:00:00', datetime(time, 'unixepoch')) AS hour,
    AVG(value) AS avg_windspeed
  FROM windSpeed
  WHERE 
    time >= strftime('%s', '2020-01-01') AND
    strftime('%S', datetime(time, 'unixepoch')) != '00'
  GROUP BY hour
),
temp_hourly AS (
  SELECT 
    strftime('%Y-%m-%d %H:00:00', datetime(time, 'unixepoch')) AS hour,
    AVG(value) AS avg_temperature
  FROM outdoorTemperature
  WHERE 
    time >= strftime('%s', '2020-01-01') AND
    strftime('%S', datetime(time, 'unixepoch')) != '00'
  GROUP BY hour
)

SELECT 
  w.hour,
  w.avg_windspeed,
  t.avg_temperature
FROM wind_hourly w
LEFT JOIN temp_hourly t ON w.hour = t.hour
ORDER BY w.hour;
"


hourly_avg <- dbGetQuery(con, query)
dbDisconnect(con)

#####################################
# JOIN WEATHER DATA TO OBSERVATIONS #
#####################################

all_data$Date.Time <- as.POSIXct(all_data$Date.Time, tz = "UTC")
hourly_avg$hour <- as.POSIXct(hourly_avg$hour, tz = "UTC")

# Add row IDs to preserve rows during join
all_data$row_id <- seq_len(nrow(all_data))

# Use fuzzy data to join based on time proximity
joined <- difference_left_join(
  all_data, hourly_avg,
  by = c("Date.Time" = "hour"),
  max_dist = as.difftime(60, units = "mins"),
  distance_col = "time_diff"
)

closest_match <- joined %>%
  group_by(row_id) %>%
  slice_min(abs(as.numeric(time_diff)), with_ties = FALSE) %>%
  ungroup()

closest_match <- closest_match %>%
  select(-row_id, -time_diff, -hour)

#############
# WRITE FST #
#############

# Save as fst file to read in the data in the future without needing to process it again. 
write_fst(closest_match, fst_output)

####################
# HELPER FUNCTIONS #
####################

# This function can be used to identify all unique recorder names. This can be helpful if new recorder names are given in future 
# years and they need to be identified so that they can be renamed to one of the common location names (i.e. "Prairie", "Savanna")

# location_tbls <- lapply(data_70, function(df) {
#   loc_tbl <- table(df[["Location"]])
#   return(loc_tbl)
# })
