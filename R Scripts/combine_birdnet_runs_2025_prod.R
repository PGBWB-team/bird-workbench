# This script:
# 
# Is downstream from the BirdNET_run_file_combiner_prod.sh BASH script,
# Reads a single-year text file of BirdNET IDs, 
# Cleans up the data, 
# Uses fuzzy logic to join each row with averages calculated from SQL-based local weather 
#    observations, 
# Creates a single-year FST file, and 
# Combines it with other such files to create an all-years FST that's used as input by 
#    the UI script.

library(lubridate)
library(fst)
library(data.table)
library(DBI)
library(RSQLite)
library(dplyr)
library(fuzzyjoin)

#########################################################################################
# File path variables - provided via command line - usually in the pipeline BASH script #
#########################################################################################

# Collect the arguments after --args in the BASH script
args <- commandArgs(trailingOnly = TRUE)

# Parse the arguments into a named list
parsed_args <- sapply(args, function(arg) {
  # Make sure each argument is treated as a string (character)
  if (is.character(arg)) {
    # Split on '=' to separate name and value
    key_value <- strsplit(arg, "=")[[1]]
    
    # Trim leading/trailing whitespaces (or tabs) from key and value
    key_value <- trimws(key_value)
    
    # Return the key-value pair as a list
    return(key_value)
  }
}, simplify = FALSE)

# Convert the list into a named vector for easy access
parsed_args <- setNames(sapply(parsed_args, `[`, 2), sapply(parsed_args, `[`, 1))

# # Print the parsed arguments
# print(parsed_args)
# 
# # Access each argument by its name
# 
# cat("input_COMBINED_file", parsed_args["input_COMBINED_file"], "\n")
# cat("fst_output", parsed_args["fst_output"], "\n")
# cat("yearly_FST_input_paths2020", parsed_args["yearly_FST_input_paths2020"], "\n")
# cat("yearly_FST_input_paths2021", parsed_args["yearly_FST_input_paths2021"], "\n")
# cat("yearly_FST_input_paths2022", parsed_args["yearly_FST_input_paths2022"], "\n")
# cat("yearly_FST_input_paths2023", parsed_args["yearly_FST_input_paths2023"], "\n")
# cat("yearly_FST_input_paths2024", parsed_args["yearly_FST_input_paths2024"], "\n")
# cat("yearly_FST_input_paths2025", parsed_args["yearly_FST_input_paths2025"], "\n")
# cat("combined_FST_path", parsed_args["combined_FST_path"], "\n")
# cat("weather_path", parsed_args["weather_path"], "\n")
# cat("start_date", parsed_args["start_date"], "\n")
# cat("end_date", parsed_args["end_date"], "\n")


# Now you can access each argument by its name:
# print(parsed_args["input_COMBINED_file"])  # Should print the path "/path/1.fst"
# print(parsed_args["fst_output"])  # Should print the path "/path/1.fst"
# print(parsed_args["yearly_FST_input_paths2020"])  # Should print the path "/path/1.fst"
# print(parsed_args["yearly_FST_input_paths2021"])  # Should print the path "/path/1.fst"
# print(parsed_args["yearly_FST_input_paths2022"])  # Should print the path "/path/1.fst"
# print(parsed_args["yearly_FST_input_paths2023"])  # Should print the path "/path/1.fst"
# print(parsed_args["yearly_FST_input_paths2024"])  # Should print the path "/path/1.fst"
# print(parsed_args["yearly_FST_input_paths2025"])  # Should print the path "/path/1.fst"
# print(parsed_args["combined_FST_path"])  # Should print the path "/path/1.fst"
# print(parsed_args["weather_path"])  # Should print the path "/path/1.fst"
# print(parsed_args["start_date"])  # Should print the path "/path/1.fst"
# print(parsed_args["end_date"])  # Should print the path "/path/1.fst"

# Load each parameter into an R variable
# input_COMBINED_file <- parsed_args["input_COMBINED_file"]
# fst_output <- parsed_args["fst_output"]
# yearly_FST_input_paths2020 <- parsed_args["yearly_FST_input_paths2020"]
# yearly_FST_input_paths2021 <- parsed_args["yearly_FST_input_paths2021"]
# yearly_FST_input_paths2022 <- parsed_args["yearly_FST_input_paths2022"]
# yearly_FST_input_paths2023 <- parsed_args["yearly_FST_input_paths2023"]
# yearly_FST_input_paths2024 <- parsed_args["yearly_FST_input_paths2024"]
# yearly_FST_input_paths2025 <- parsed_args["yearly_FST_input_paths2025"]
# combined_FST_path <- parsed_args["combined_FST_path"]
# weather_path <- parsed_args["weather_path"]
# start_date <- parsed_args["start_date"]
# end_date <- parsed_args["end_date"]
# # 
# # Print out the variables to confirm they've been loaded correctly
# 
# cat("input_COMBINED_file:", input_COMBINED_file, "\n")
# cat("input_COMBINED_file:", input_COMBINED_file, "\n")
# cat("yearly_FST_input_paths2020:", yearly_FST_input_paths2020, "\n")
# cat("yearly_FST_input_paths2021:", yearly_FST_input_paths2021, "\n")
# cat("yearly_FST_input_paths2021:", yearly_FST_input_paths2021, "\n")
# cat("yearly_FST_input_paths2021:", yearly_FST_input_paths2021, "\n")
# cat("yearly_FST_input_paths2024:", yearly_FST_input_paths2024, "\n")
# cat("yearly_FST_input_paths2025:", yearly_FST_input_paths2025, "\n")
# cat("combined_FST_path:", combined_FST_path, "\n")
# cat("weather_path:", weather_path, "\n")
# cat("start_date:", start_date, "\n")
# cat("end_date:", end_date, "\n")
# 
# # start_date <- '2025-01-01'
# # end_date <- '2025-12-31'
# # 
# # cat("direct-loaded-start_date:", start_date, "\n")
# # cat("direct-loaded-end_date:", end_date, "\n")
# 
# # load yearly input paths into the array
# 
# yearly_FST_input_paths <- c(
#   yearly_FST_input_paths2020,
#   yearly_FST_input_paths2021,
#   yearly_FST_input_paths2022,
#   yearly_FST_input_paths2023,
#   yearly_FST_input_paths2024,
#   yearly_FST_input_paths2025
# )
# 


# Gracefully exit if no arguments are passed 
if (length(args) == 0) {
  cat("No arguments passed. Exiting gracefully.\n")
  quit(save = "no", status = 1)  # Exit with status 1 (error condition)
}

# Halt - (for testing)
# cat("Script finished successfully.\n")
# quit(save = "no", status = 0)  # Exit with status 0 (successful exit)

# If we have arguments, process them 
cat("Arguments received, proceeding with the script...\n")

########################################
# Load variables from parsed arguments #
########################################

# This text file contains the BirdNET-Analyzer results for a single year.  It will be
# converted to a single-year FST file, ready to be merged with other yearly FST files. 
# Normal case: current-year results file.  But prior years can also be regenerated this
# way
input_COMBINED_file <- parsed_args["input_COMBINED_file"]

# Set file path for writing a single-year .fst file -- include a matching year in file 
# name as we are creating one .FST per year 
fst_output <- parsed_args["fst_output"]

# Specify paths to single-year FST files which will be combined into an all-years 
# combined FST.  This is a two-step process.

# First load the parsed arguments into R variables

yearly_FST_input_paths2020 <- parsed_args["yearly_FST_input_paths2020"]
yearly_FST_input_paths2021 <- parsed_args["yearly_FST_input_paths2021"]
yearly_FST_input_paths2022 <- parsed_args["yearly_FST_input_paths2022"]
yearly_FST_input_paths2023 <- parsed_args["yearly_FST_input_paths2023"]
yearly_FST_input_paths2024 <- parsed_args["yearly_FST_input_paths2024"]
yearly_FST_input_paths2025 <- parsed_args["yearly_FST_input_paths2025"]

# Then concatenate those file-path variables into a vector that will be used to 
# combine those files into a single FST
  
yearly_FST_input_paths <- c(
  yearly_FST_input_paths2020,
  yearly_FST_input_paths2021,
  yearly_FST_input_paths2022,
  yearly_FST_input_paths2023,
  yearly_FST_input_paths2024,
  yearly_FST_input_paths2025
)

print(yearly_FST_input_paths)


# Set file path for all-years combined .fst output file - this file (or a renamed 
# version of it) gets handed to the UI script as input 
combined_FST_path <- parsed_args["combined_FST_path"]

print(combined_FST_path)

# File path to SQL weather database
weather_path <- parsed_args["weather_path"]

print(weather_path)
## Set start end end dates for the SQL select that match the year of the input and output files.  
## Note that these are positional and will be evaluated sequentially by the RSQLite library.  
## The question marks in the SQL query indicate where the substitutions occur.

start_date <- parsed_args["start_date"]
end_date <- parsed_args["end_date"]

print(start_date)
print(end_date)

################
# Start script #
################

# Read the input (results) file as a table 
data <- lapply(input_COMBINED_file, read.table, header=TRUE, sep="\t", colClasses="character", quote="\"")

# Filter out observations with less than 70% confidence: 
# First, convert "Confidence" column to numeric, handling any non-numeric values
# (Currently overridden with a zero value, but left in to clean up non-numeric data)
# Note: BirdNET-Analyzer filters by Confidence with 25 as the default lower-bound --
# manage this when running BirdNET-Analyzer, not here)
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
  df <- df[complete.cases(df$Date),]
  df["Week"] <- get_week_from_date(df$Date)
  df["Obs.Time"] <- strftime(df$Date.Time + lubridate::seconds(as.numeric(df$Begin.Time..s.)), format="%H:%M:%S", tz = "UTC")

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

# Connect to SQLite database
con <- dbConnect(RSQLite::SQLite(), weather_path)

# SQL Query for hourly temp and wind
## Uses parameter binding for safe date substitution

query <- "
-- Get hourly wind speed
WITH wind_hourly AS (
  SELECT 
    strftime('%Y-%m-%d %H:00:00', datetime(time, 'unixepoch')) AS hour,
    AVG(value) AS avg_windspeed
  FROM windSpeed
  WHERE 
    time >= strftime('%s', ?) AND
    time <= strftime('%s', ?) AND
    strftime('%S', datetime(time, 'unixepoch')) != '00'
  GROUP BY hour
),
-- Get hourly temperature
temp_hourly AS (
  SELECT 
    strftime('%Y-%m-%d %H:00:00', datetime(time, 'unixepoch')) AS hour,
    AVG(value) AS avg_temperature
  FROM outdoorTemperature
  WHERE 
    time >= strftime('%s', ?) AND
    time <= strftime('%s', ?) AND
    strftime('%S', datetime(time, 'unixepoch')) != '00'
  GROUP BY hour
)

-- Join the two hourly tables
SELECT 
  w.hour,
  w.avg_windspeed,
  t.avg_temperature
FROM wind_hourly w
LEFT JOIN temp_hourly t ON w.hour = t.hour
ORDER BY w.hour;
"

# Execute query with parameters in correct order
hourly_avg <- dbGetQuery(con, query, params = list(
  start_date, end_date,  # for wind_hourly
  start_date, end_date   # for temp_hourly
))

# Disconnect from the database
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

####################
# WRITE ANNUAL FST #
####################

# Save as an annual .FST file
write_fst(closest_match, fst_output)

##########################
# MERGE ANNUAL FST FILES #
##########################

# Read and combine all annual .fst files
all_data <- rbindlist(lapply(yearly_FST_input_paths, read_fst), use.names = TRUE, fill = TRUE)

# Write the combined FST file (to be used by the UI script)
write_fst(all_data, combined_FST_path)

# Write completion to terminal

cat("Combined file written to:", combined_FST_path, "\n")	

####################
# HELPER FUNCTIONS #
####################

# This function can be used to identify all unique recorder names. This can be helpful if new recorder names are given in future 
# years and they need to be identified so that they can be renamed to one of the common location names (i.e. "Prairie", "Savanna")

# location_tbls <- lapply(data_70, function(df) {
#   loc_tbl <- table(df[["Location"]])
#   return(loc_tbl)
# })
