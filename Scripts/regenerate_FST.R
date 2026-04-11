# regenerate_FST.R
#
# This script processes BirdNET identification data through two passes:
#
# PASS 1 (always runs):
#   - Reads a single-year text file of BirdNET IDs (single_year_YYYY.txt)
#   - Cleans and processes the data
#   - Joins with weather data using fuzzy logic based on date/time proximity
#   - Writes a single-year FST file (single_year_YYYY.fst)
#
# PASS 2 (conditional, based on pass_mode parameter):
#   - Reads all yearly FST files (FIRST_YEAR through selected_year)
#   - Combines them into a single all-years FST file
#   - Output: all_years.fst (used by UI)
#
# USAGE:
#   Called from archive_to_FST_pipeline.sh
#   Rscript regenerate_FST.R \
#     input_combined_file=... \
#     single_year_fst_output=... \
#     yearly_fst_paths=... \
#     all_years_fst_output=... \
#     weather_path=... \
#     start_date=... \
#     end_date=... \
#     first_year=... \
#     selected_year=... \
#     pass_mode=...
#
# PARAMETERS:
#   input_combined_file    - Path to single_year_YYYY.txt file (input for PASS 1)
#   single_year_fst_output - Path for output single_year_YYYY.fst (output from PASS 1)
#   yearly_fst_paths       - Comma-separated list of yearly FST file paths (input for PASS 2)
#   all_years_fst_output   - Path for output all_years.fst (output from PASS 2)
#   weather_path           - Path to Open-Meteo SQLite database
#                            (open_metro_weather.db, populated by fetch_weather_data()
#                            in archive_to_FST_pipeline.sh before this script runs)
#   start_date             - Weather data start date (YYYY-MM-DD) for current year
#   end_date               - Weather data end date (YYYY-MM-DD) for current year
#   first_year             - First year in dataset (e.g., 2020)
#   selected_year          - Year being processed (e.g., 2026 or 2024)
#   pass_mode              - "pass1" (skip PASS 2) or "pass2" (run both passes)
#
# MODES:
#   pass1 - Generate single-year FST only, skip combining (faster, for testing)
#   pass2 - Generate single-year FST AND combine all years into all_years.fst (normal operation)

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

# Gracefully exit if no arguments are passed 

	if (length(args) == 0) {
	  cat("No arguments passed. Exiting gracefully.\n")
	  quit(save = "no", status = 1)  # Exit with status 1 (error condition)
	}

# If we have arguments, process them 

	cat("Arguments received, proceeding with the script...\n")

########################################
# Load variables from parsed arguments #
########################################

	cat("========================================\n")
	cat("regenerate_FST.R\n")
	cat("========================================\n")
	cat("Loading parameters...\n\n")

# Pass mode (determines whether we run PASS 2)

	pass_mode <- parsed_args["pass_mode"]
	if (is.na(pass_mode)) {
	  pass_mode <- "pass2"  # Default to full processing if not specified
	}
	cat("Pass mode:", pass_mode, "\n")

# Input text file (single year of BirdNET results)

	input_COMBINED_file <- parsed_args["input_combined_file"]
	cat("Input file:", input_COMBINED_file, "\n")

# Output path for single-year FST

	single_year_fst_output <- parsed_args["single_year_fst_output"]
	cat("Single-year FST output:", single_year_fst_output, "\n")

# Yearly FST paths (comma-separated string from BASH, split into vector)

	yearly_fst_paths_string <- parsed_args["yearly_fst_paths"]
	yearly_FST_input_paths <- strsplit(yearly_fst_paths_string, ",")[[1]]
	yearly_FST_input_paths <- trimws(yearly_FST_input_paths)  # Remove any whitespace
	cat("Yearly FST paths (", length(yearly_FST_input_paths), "files):\n", sep="")
	for (path in yearly_FST_input_paths) {
	  cat("  -", path, "\n")
	}

# Output path for all-years combined FST

	all_years_fst_output <- parsed_args["all_years_fst_output"]
	cat("All-years FST output:", all_years_fst_output, "\n")

# Year parameters

	first_year <- as.integer(parsed_args["first_year"])
	selected_year <- as.integer(parsed_args["selected_year"])
	cat("Year range:", first_year, "through", selected_year, "\n")

# File path to Open-Meteo SQLite weather database
# (populated by fetch_weather_data() in archive_to_FST_pipeline.sh)

	weather_path <- parsed_args["weather_path"]
	cat("Weather path:", weather_path, "\n")


## Set start end end dates for the SQL select that match the year of the input and output files.  
## Note that these are positional and will be evaluated sequentially by the RSQLite library.  
## The question marks in the SQL query indicate where the substitutions occur.

	start_date <- parsed_args["start_date"]
	end_date <- parsed_args["end_date"]
	cat("Run start date", start_date, "Run end date", end_date, "\n")

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

# Function to normalize file name lengths

	normalize_length <- function(x, target_length = 3) {
	  if (length(x) > target_length) {
		return(x[1:target_length]) # truncate to the first 3 elements
	  } else if (length(x) < target_length) {
		return(c(x, rep(NA, target_length - length(x)))) # pad with NA
	  } else {
		return(x) # already at target length
	  }
	}

# Function do to a lot of date stuff - needs clarification some day

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
			
		# If week is 0 or negative, this date belongs to the prior year's week system
			
			if (any(week_number <= 0)) {
			  prior_year <- year - 1
			  prior_first_day <- as.Date(paste0(prior_year, "-01-01"))
			  prior_first_sunday <- prior_first_day + (7 - lubridate::wday(prior_first_day) + 1) %% 7
			  prior_days <- as.integer(as.Date(date) - as.Date(prior_first_sunday))
			  prior_week <- (prior_days %/% 7) + 1
			  week_number <- ifelse(week_number <= 0, prior_week, week_number)
			}		  		  
		return(week_number)
		
	}
	
# Function to assign days in Week 0 to the prior year
# Week 0 belongs to the prior SundayWeekYear
# Weeks 1-52 belong to the current calendar year	

	get_sunday_week_year <- function(date) {
	  
	  calendar_year <- lubridate::year(date)
	  first_day <- as.Date(paste0(calendar_year, "-01-01"))
	  first_sunday <- first_day + (7 - lubridate::wday(first_day) + 1) %% 7
	  
	  # If date is before the first Sunday of its calendar year,
	  # it belongs to the prior year's Sunday week system
	  sunday_week_year <- ifelse(as.Date(date) < first_sunday,
	                             calendar_year - 1,
	                             calendar_year)
	  
	  return(sunday_week_year)
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
	  df["SundayWeekYear"] <- get_sunday_week_year(df$Date)
	  df["Obs.Time"] <- strftime(df$Date.Time + lubridate::seconds(as.numeric(df$Begin.Time..s.)), format="%H:%M:%S", tz = "UTC")
	
	  return(df)
	})

# Remove file path and replace with only file name

	data_70 <- lapply(data_70, function(df) {
	  df["Begin.Path"] <- basename(df$Begin.Path)
	  return(df)
	})

# Rename recorders (data_70 and related variables need review - may be obsolete)

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

# Connect to SQLite database and retrieve hourly weather data for the
# date range of this year's BirdNET observations.
#
# The database is populated upstream by fetch_weather_data() in
# archive_to_FST_pipeline.sh, which pulls from the Open-Meteo
# Historical Weather API before this script runs.
#
# Output: hourly_avg data frame with columns:
#   hour             - POSIXct timestamp (UTC)
#   avg_temperature  - degrees Celsius (2 m above ground)
#   avg_windspeed    - km/h (10 m above ground)
#
# Additional columns available in open_metro_weather_hourly but not yet
# joined to observations (add to SELECT and downstream use as needed):
#   precipitation       - mm (preceding hour sum)
#   wind_direction_10m  - degrees
#   wind_gusts_10m      - km/h
#   surface_pressure    - hPa
#   cloud_cover         - %

# ---------------------------------------------------------------------------
# LEGACY QUERY (local weather stations via valley.weather.db) - RETIRED
# Retained for reference. Replaced by Open-Meteo query below.
#
#	con <- dbConnect(RSQLite::SQLite(), weather_path)
#
#	query <- "
#	-- Get hourly wind speed
#	WITH wind_hourly AS (
#	  SELECT
#		strftime('%Y-%m-%d %H:00:00', datetime(time, 'unixepoch')) AS hour,
#		AVG(value) AS avg_windspeed
#	  FROM windSpeed
#	  WHERE
#		time >= strftime('%s', ?) AND
#		time <= strftime('%s', ?) AND
#		strftime('%S', datetime(time, 'unixepoch')) != '00'
#	  GROUP BY hour
#	),
#	-- Get hourly temperature
#	temp_hourly AS (
#	  SELECT
#		strftime('%Y-%m-%d %H:00:00', datetime(time, 'unixepoch')) AS hour,
#		AVG(value) AS avg_temperature
#	  FROM outdoorTemperature
#	  WHERE
#		time >= strftime('%s', ?) AND
#		time <= strftime('%s', ?) AND
#		strftime('%S', datetime(time, 'unixepoch')) != '00'
#	  GROUP BY hour
#	)
#	-- Join the two hourly tables
#	SELECT
#	  w.hour,
#	  w.avg_windspeed,
#	  t.avg_temperature
#	FROM wind_hourly w
#	LEFT JOIN temp_hourly t ON w.hour = t.hour
#	ORDER BY w.hour;
#	"
#
#	hourly_avg <- dbGetQuery(con, query, params = list(
#	  start_date, end_date,  # for wind_hourly
#	  start_date, end_date   # for temp_hourly
#	))
#
#	dbDisconnect(con)
# ---------------------------------------------------------------------------

# Connect to Open-Meteo SQLite database

	con <- dbConnect(RSQLite::SQLite(), weather_path)

# Query open_metro_weather_hourly for the year date range.
# Column aliases intentionally match the legacy query output so the
# fuzzy-join below requires no changes.
# Additional variables (precipitation, wind_direction_10m, wind_gusts_10m,
# surface_pressure, cloud_cover) are selected but not yet used downstream --
# extend the join and FST output here when ready.

	query <- "
	SELECT
	  hour,
	  wind_speed_10m                   AS avg_windspeed,
	  (temperature_2m * 9.0/5.0 + 32)  AS avg_temperature,
	  precipitation                    AS precipitation,
	  wind_direction_10m               AS wind_direction,
	  wind_gusts_10m                   AS wind_gusts,
	  surface_pressure                 AS surface_pressure,
	  cloud_cover                      AS cloud_cover
	FROM open_metro_weather_hourly
	WHERE hour >= ?
	  AND hour <= ?
	ORDER BY hour;
	"

	hourly_avg <- dbGetQuery(con, query, params = list(
	  paste0(start_date, "T00:00"),
	  paste0(end_date,   "T23:00")
	))

# Disconnect from the database

	dbDisconnect(con)

#####################################
# JOIN WEATHER DATA TO OBSERVATIONS #
#####################################

	all_data$Date.Time <- as.POSIXct(all_data$Date.Time, tz = "UTC")
	hourly_avg$hour    <- as.POSIXct(hourly_avg$hour, format = "%Y-%m-%dT%H:%M", tz = "UTC")

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

	cat("\n")
	cat("========================================\n")
	cat("PASS 1: Writing single-year FST file\n")
	cat("========================================\n")
	
	# Save as a single-year .FST file
	
		write_fst(closest_match, single_year_fst_output)
	
	# Report results

		cat("Single-year FST written to:", single_year_fst_output, "\n")
		cat("  Rows:", nrow(closest_match), "\n")
		cat("  Columns:", ncol(closest_match), "\n")
		cat("PASS 1 complete\n")
		
# #################################################################################
# # WRITE ANNUAL FST as text file - TESTING ONLY - COMMENT OUT OF PRODUCTION CODE #
# #################################################################################		
# 		
 	
	# Define txt output path
	
		single_year_txt_output <- sub("\\.fst$", ".txt", single_year_fst_output)
	
	# Write tab-delimited data file

		write.table(
		  closest_match,
		  file = single_year_txt_output,
		  sep = "\t",        # tab delimiter
		  row.names = FALSE, # no row numbers
		  col.names = TRUE,  # include header row
		  quote = FALSE      # do not quote fields
		)

	# Optional: console confirmation
	
		cat("Tab-delimited TXT written to:", single_year_txt_output, "\n")
		cat("  Rows:", nrow(closest_match), "\n")
		cat("  Columns:", ncol(closest_match), "\n")
		cat("PASS 1 complete\n")
		
	
##########################
# MERGE ANNUAL FST FILES #
##########################

# Only run PASS 2 if pass_mode is "pass2" - exit if yearly FST input files are missing

	if (pass_mode == "pass2") {
	  cat("\n")
	  cat("========================================\n")
	  cat("PASS 2: Combining all yearly FST files\n")
	  cat("========================================\n")
	  
	  cat("Reading", length(yearly_FST_input_paths), "yearly FST files...\n")
	  
	  # Verify all files exist before attempting to read

		  missing_files <- c()
		  for (fst_path in yearly_FST_input_paths) {
			if (!file.exists(fst_path)) {
			  missing_files <- c(missing_files, fst_path)
			}
		  }

		# If all files are available 
				  
			  if (length(missing_files) > 0) {
				cat("\nERROR: Missing yearly FST files:\n")
				for (missing in missing_files) {
				  cat("  -", missing, "\n")
				}
				stop("Cannot proceed with PASS 2 - missing files")
			  }
			  
			  cat("All yearly FST files exist, proceeding with combination...\n")
			  
		  # Read and combine all yearly .fst files
	
			  all_data <- rbindlist(lapply(yearly_FST_input_paths, read_fst), use.names = TRUE, fill = TRUE)
		  
		  # Report combined data stats
	
			  cat("Combined data:\n")
			  cat("  Total rows:", nrow(all_data), "\n")
			  cat("  Total columns:", ncol(all_data), "\n")
			  cat("  Year range:", min(year(all_data$Date), na.rm=TRUE), "to", max(year(all_data$Date), na.rm=TRUE), "\n")
		  
		  # Write the combined FST file (to be used by the UI script)
	
			  write_fst(all_data, all_years_fst_output)
		  
		  cat("All-years FST written to:", all_years_fst_output, "\n")
		  cat("PASS 2 complete\n")
		  
# #################################################################################
# # WRITE COMBINED FST as text file - TESTING ONLY - COMMENT OUT OF PRODUCTION CODE #
# #################################################################################		
		
	
	# Define txt output path
	
		all_years_fst_output <- sub("\\.fst$", ".txt", all_years_fst_output)
	
	# Write tab-delimited data file

		write.table(
		  all_data,
		  file = all_years_fst_output,
		  sep = "\t",        # tab delimiter
		  row.names = FALSE, # no row numbers
		  col.names = TRUE,  # include header row
		  quote = FALSE      # do not quote fields
		)
		
		  cat("All-years FST written to:", all_years_fst_output, "\n")
		  cat("PASS 2 complete\n")		
# 
# 	# Optional: console confirmation
# 	
# 		cat("Tab-delimited TXT written to:", single_year_txt_output, "\n")
# 		cat("  Rows:", nrow(closest_match), "\n")
# 		cat("  Columns:", ncol(closest_match), "\n")
# 		cat("PASS 1 complete\n")
# 				  

# Otherwise skip PASS2 	  

	} else {
	  cat("\n")
	  cat("========================================\n")
	  cat("PASS 2 skipped (pass_mode = pass1)\n")
	  cat("========================================\n")
	  cat("To combine all years, run with pass_mode=pass2\n")
	}

# Announce script completed
	
	cat("\n")
	cat("========================================\n")
	cat("Script completed successfully\n")
	cat("========================================\n")

####################
# HELPER FUNCTIONS #
####################

# This function can be used to identify all unique recorder names. This can be helpful if new recorder names are given in future 
# years and they need to be identified so that they can be renamed to one of the common location names (i.e. "Prairie", "Savanna")

# location_tbls <- lapply(data_70, function(df) {
#   loc_tbl <- table(df[["Location"]])
#   return(loc_tbl)
# })
