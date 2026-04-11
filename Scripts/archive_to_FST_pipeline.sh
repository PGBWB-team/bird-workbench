#!/bin/bash

################################################################################
# MAIN: RUN SCRIPTS IN SEQUENCE - Function, defined here, called at the bottom #
################################################################################

main() {

	# Capture and announce start time
	
	start_time=$(date +%s)
	print_blank_lines 2
	echo "Started at: $(date '+%Y:%m:%d %H:%M:%S')"
	start_time_formatted=$(date '+%Y:%m:%d %H:%M:%S')
	
	# This section runs the functions in sequence.  Toggle comments for run-control
	 
	# 	run_BirdNET_script
	#	run_file_combiner
	#	fetch_weather_data
		regen_FST_files
	#   copy_new_FST_to_UI_input_target -- placeholder
	
	#	check_prod_plist_exists
	#	reload_prod_launchagent
	#	check_test_plist_exists
	#	reload_test_launchagent
	#	check_tiny_shiny_plist_exists
	#	reload_tiny_shiny_launchagent
	
	#  NOTE: This last group are for testing - a new instance of the FST file gets pulled in each 
	#  time the UI is reloaded in the web browser - restarting the UI background task isn't 
	#  required.  To test various versions of the FST file, change the path in the UI
	#  script and hit refresh in the browser
	
}


# archive_to_FST_pipeline.sh - BirdNET Analysis Pipeline
#
# Orchestrates the three-step pipeline:
#   1. BirdNET-Analyzer: Identifies bird calls in audio files
#   2. combine_run_files: Combines BirdNET results into single-year text files
#   3. regenerate_FST_files: Generates FST files for UI consumption
#
# USAGE:
#   ./archive_to_FST_pipeline.sh mode first_year selected_year
#
# REQUIRED PARAMETERS:
#   mode        	- Processing mode (see below)
#   first_year  	- First year in dataset (4 digits, e.g., 2020)
#   selected_year	- Year to process (see below)
#
# YEAR VARIABLES EXPLAINED:
#   run_date_year    - Derived from system date, used for date-stamping outputs
#   selected_year    - Year to process (defaults to run_date_year, but could differ)
#   process_year     - Iterator in loops when processing multiple years
#   first_year       - Earliest year in dataset, supplied as parameter
#
# MODES:
#   single_year_additions    
#			- Process new audio files only (incremental, BirdNET skips existing)
#			  BirdNET: processes only new files in selected_year
#			  Combiner: rebuilds single_year_YYYY.txt for selected_year
#			  FST: regenerates single_year_YYYY.fst + all_years.fst
#
#   single_year_replacement  
#			- Reprocess ALL audio files for selected_year
#			  BirdNET: reanalyzes all files in selected_year (clears existing results)
#			  Combiner: rebuilds single_year_YYYY.txt for selected_year
#			  FST: regenerates single_year_YYYY.fst + all_years.fst
#
#   all_years_replacement    
#			- Reprocess ALL years (WARNING: 1-2 weeks)
#			  BirdNET: reanalyzes all files from first_year through selected_year
#			  Combiner: rebuilds single_year_YYYY.txt for all years
#			  FST: regenerates all single_year_YYYY.fst + all_years.fst
#			  Requires confirmation prompt
#
# EXAMPLES:
#   # Normal daily update (new recordings in 2026)
#   ./archive_to_FST_pipeline.sh single_year_additions 2020 2026
#
#   # Reprocess 2024 data with new inputs or settings
#   ./archive_to_FST_pipeline.sh single_year_replacement 2020 2024
#
#   # Complete reprocessing of multiple years (rare, takes several days/year)
#   # This example will process all years from 2020 through 2024
#   ./archive_to_FST_pipeline.sh all_years_replacement 2020 2024
#
# OUTPUT FILE NAMING:
#   Logs:             component_YYYY-MM-DD.log (dated with run_date)
#   Archives:         all_years_YYYY-MM-DD.fst (dated with run_date)
#   Working files:    single_year_YYYY.txt/.fst (dated with selected_year)
#
# OUTPUT LOCATIONS:
#   results:  Documents/BirdWorkbench/{component}/
#   logs:     Documents/BirdWorkbench/{component}/logs/
#   archives: Documents/BirdWorkbench/birdnet_analyzer_run_files_combined_FST/archive/
#
# RUN CONTROL:
#   Comment out function calls in MAIN section to skip steps
#   Example: Comment out run_BirdNET_script to only run combiner and FST regen
#
# NOTES:
#   - Logs retained for 3 most recent runs per environment
#   - all_years.fst archived before overwriting (2 generations kept)
#   - For test→production promotion: ./promote_test_to_prod.sh


########
# INIT #
########

# Exit script if any step fails

set -e

###########################
# CONFIGURATION CONSTANTS #
###########################

# Base directories

	HOME_DIR="$HOME"
	BASE_DIR="$HOME_DIR/Documents/BirdWorkbench"

# Component base directories (modify to reflect Test or Production)

	RUN_FILES_BASE="$BASE_DIR/Results_files/Test/birdnet_analyzer_RUN_FILES" 
	COMBINED_BASE="$BASE_DIR/Results_files/Test/birdnet_analyzer_run_files_COMBINED"
	FST_BASE="$BASE_DIR/Results_files/Test/birdnet_analyzer_run_files_combined_FST"

# BirdNET specific paths (currently only one version for production and test)

	BIRDNET_DIRECTORY="$HOME_DIR/Documents/BirdNetAnalyzer/BirdNET-Analyzer"
	BIRDNET_VENV_PATH="PGBWBvenv/bin/activate"
	
# Audio archive path (production: Volumes/Bio test: Volumes/Test/Bio 
	
	BIO_ARCHIVE_BASE="/Volumes/Bio/Test/Bio"

# BirdNET parameters

	#	Use this to tune how much of the CPU the script consumes (more = faster processing)
	
		THREADS=8
	
	#	If species are determined by LAT LON coordinates set these:
	
		LAT=44.415947	
		LON=-91.700659	
		
	#	If species are determined with custom species-list file put the path to that file 
	#	here
	
		SPECIES_LIST_FILE_PATH="$BASE_DIR/Species_Lists/Test/Prairie_Haven_species_list_all_test.txt"

	
# Weather database (legacy - local weather stations, retained for reference)

	WEATHER_DB_PATH="$BASE_DIR/weather_snoop_valley_weather/valley.weather.db"

# Open-Metro weather data directory (modify Test/Production to match other base dirs above)

	WEATHER_DATA_BASE="$BASE_DIR/Weather_data/Test"

# Open-Metro weather database and flat file paths (derived from WEATHER_DATA_BASE)

	OPEN_METRO_WEATHER_DB_PATH="$WEATHER_DATA_BASE/open_metro_weather.db"
	OPEN_METRO_WEATHER_TXT_PATH="$WEATHER_DATA_BASE/open_metro_weather_data.txt"

# Scripts

	SCRIPTS_DIR="$BASE_DIR/Scripts/Test"
	COMBINER_SCRIPT_NAME="combine_run_files.sh"
	FST_SCRIPT_NAME="regenerate_FST.R"

# LaunchAgent plists

	PROD_UI_PLIST_PATH="$HOME/Library/LaunchAgents/com.pgbwb.bird-workbench_UI.prod.plist"
	TEST_UI_PLIST_PATH="$HOME/Library/LaunchAgents/com.pgbwb.bird-workbench_UI.test.plist"
	TINY_SHINY_PLIST_PATH="$HOME/Library/LaunchAgents/com.pgbwb.tiny_shiny.plist"


##########################
# FUNCTION: SHOW USAGE   #
##########################

show_usage() {
	cat << EOF

USAGE:
  ./archive_to_FST_pipeline.sh mode first_year selected_year

REQUIRED PARAMETERS:
  mode          - Processing mode
  first_year    - First year in dataset (4 digits, e.g., 2020)
  selected_year - Year to process (4 digits, e.g., 2024)

MODES:
  single_year_additions    - Process new audio files only (incremental)
  single_year_replacement  - Reprocess all audio files for selected year
  all_years_replacement    - Reprocess ALL years (WARNING: 1-2 weeks)

EXAMPLES:
  ./archive_to_FST_pipeline.sh single_year_additions 2020 2026
  ./archive_to_FST_pipeline.sh single_year_replacement 2020 2024
  ./archive_to_FST_pipeline.sh all_years_replacement 2020 2023

For more details, see script header comments.

EOF
}


######################
# Workflow reminders #
######################

# Pre-run reminders: 
	echo " "
	echo " "
	echo "PRE-RUN REMINDERS"
	echo " "
	
	echo "Verified that run-control is set correctly?"
	echo "Have you SAVED the script file with the new run-control information?"
	echo " "
	read -p "Enter your response (yes/no): " response
	
	# Convert the response to lowercase for easier comparison
	response=$(echo "$response" | tr '[:upper:]' '[:lower:]')
	
	# Check the user's response
	if [[ "$response" == "yes" || "$response" == "y" ]]; then
	  echo "Continuing with the script..."
	  
	else
	  echo "Stopping the script."
	  exit 1
	fi
 
##########################################################################
# PARSE AND VALIDATE COMMAND-LINE ARGUMENTS                              #
##########################################################################

# Check for required number of arguments

	if [ "$#" -ne 3 ]; then
		echo "ERROR: Incorrect number of arguments (expected 3, got $#)"
		show_usage
		exit 1
	fi

# Derive run date from system clock

	run_date_year=$(date '+%Y')
	run_date_month=$(date '+%m')
	run_date_day=$(date '+%d')

# Parse arguments with clear variable names

	mode="$1"
	first_year="$2"
	selected_year="$3"

# Validate mode

	if [[ "$mode" != "single_year_additions" && \
		  "$mode" != "single_year_replacement" && \
		  "$mode" != "all_years_replacement" ]]; then
		echo "ERROR: Invalid mode '$mode'"
		echo "       Must be: single_year_additions, single_year_replacement, or all_years_replacement"
		show_usage
		exit 1
	fi

# Confirmation prompt for all_years_replacement (1-2 week operation)

	if [[ "$mode" == "all_years_replacement" ]]; then
		echo ""
		echo "WARNING: all_years_replacement mode could take 1-2 WEEKS to complete"
		echo " This will reprocess ALL years from $first_year through $selected_year"
		echo ""
		read -p "Are you sure you want to continue? (yes/no): " confirm
		confirm=$(echo "$confirm" | tr '[:upper:]' '[:lower:]')
		
		if [[ "$confirm" != "yes" && "$confirm" != "y" ]]; then
			echo "Operation cancelled"
			exit 0
		fi
	fi

# Validate first_year and selected_year format

	if [[ ! "$first_year" =~ ^[0-9]{4}$ ]]; then
		echo ""
		echo "ERROR: Invalid first_year format '$first_year'"
		echo "       Expected: YYYY"
		echo ""
		show_usage
		exit 1
	fi

	if [[ ! "$selected_year" =~ ^[0-9]{4}$ ]]; then
		echo ""
		echo "ERROR: Invalid selected_year format '$selected_year'"
		echo "       Expected: YYYY"
		echo ""
		show_usage
		exit 1
	fi

###########################################################################
# SET UP WORKING DIRECTORY PATHS AND DERIVED VARIABLES                 #
###########################################################################

# Working directory paths

	RUN_FILES_DIR="$RUN_FILES_BASE"
	COMBINED_DIR="$COMBINED_BASE"
	FST_DIR="$FST_BASE"

# Log directories (one per component)

	BIRDNET_LOG_DIR="$RUN_FILES_BASE/logs"
	COMBINER_LOG_DIR="$COMBINED_BASE/logs"
	FST_LOG_DIR="$FST_BASE/logs"

# Archive directory for all_years.fst backups

	ARCHIVE_DIR="$FST_BASE/archive"

# Create all necessary directories

	mkdir -p "$RUN_FILES_DIR"
	mkdir -p "$COMBINED_DIR"
	mkdir -p "$FST_DIR"
	mkdir -p "$BIRDNET_LOG_DIR"
	mkdir -p "$COMBINER_LOG_DIR"
	mkdir -p "$FST_LOG_DIR"
	mkdir -p "$ARCHIVE_DIR"
	mkdir -p "$WEATHER_DATA_BASE"

# Derived variables for this run

	RUN_DATE="${run_date_year}-${run_date_month}-${run_date_day}"

# Log file paths for this run (dated with run_date)

	BIRDNET_LOG="$BIRDNET_LOG_DIR/birdnet_${RUN_DATE}.log"
	COMBINER_LOG="$COMBINER_LOG_DIR/combiner_${RUN_DATE}.log"
	FST_LOG="$FST_LOG_DIR/FST_regen_${RUN_DATE}.log"

# Script paths

	COMBINER_SCRIPT="$SCRIPTS_DIR/$COMBINER_SCRIPT_NAME"
	FST_SCRIPT="$SCRIPTS_DIR/$FST_SCRIPT_NAME"

# BirdNET: Set skip_existing flag based on mode

	if [[ "$mode" == "single_year_additions" ]]; then
		SKIP_EXISTING="--skip_existing_results"
	else
		SKIP_EXISTING=""  # Reprocess all files in replacement modes
	fi

# File paths for current processing
# Note: These use selected_year for the year portion, not run_date_year

	COMBINER_OUTPUT="$COMBINED_DIR/single_year_${selected_year}.txt"
	SINGLE_YEAR_FST_OUTPUT="$FST_DIR/single_year_${selected_year}.fst"
	ALL_YEARS_FST_OUTPUT="$FST_DIR/all_years.fst"

# Weather database date range for current year processing

	WEATHER_START_DATE="${selected_year}-01-01"
	WEATHER_END_DATE="${selected_year}-12-31"

# Build comma-separated list of yearly FST paths for PASS 2
# This list includes all years from first_year through selected_year

	YEARLY_FST_PATHS=""
	
	for y in $(seq $first_year $selected_year); do
		yearly_fst="$FST_DIR/single_year_${y}.fst"
		if [ -n "$YEARLY_FST_PATHS" ]; then
			YEARLY_FST_PATHS="${YEARLY_FST_PATHS},${yearly_fst}"
		else
			YEARLY_FST_PATHS="$yearly_fst"
		fi
	done

# LaunchAgent plist path

	UI_PLIST_PATH="$TEST_UI_PLIST_PATH"


#########################
# FUNCTION: BLANK LINES #
#########################

	print_blank_lines() {
    	local count=$1
    	for ((i = 0; i < count; i++)); do
        	echo ""
	    done
	}
	
############################
# FUNCTION: LOG MANAGEMENT #
############################

# Clean up old log files, keeping only the 3 most recent

	cleanup_old_logs() {
		local log_dir=$1
		local log_pattern=$2
		
		if [ -d "$log_dir" ]; then
			# List files matching pattern, sorted by modification time (newest first)
			# Keep first 3, delete the rest
#			ls -t "$log_dir"/$log_pattern 2>/dev/null | tail -n +4 | xargs -r rm -f
			find "$log_dir" -maxdepth 1 -name "$log_pattern" | sort -r | tail -n +4 | xargs -r rm -f
		fi
	}

# Archive all_years.fst before overwriting, keeping 2 most recent archives

	archive_all_years_fst() {
		local fst_file="$ALL_YEARS_FST_OUTPUT"
		
		if [ -f "$fst_file" ]; then
			# Create archive with run_date timestamp
			local archive_name="all_years_${RUN_DATE}.fst"
			cp "$fst_file" "$ARCHIVE_DIR/$archive_name"
			echo "Archived existing all_years.fst to: $ARCHIVE_DIR/$archive_name"
			
			# Keep only 2 most recent archives, delete older ones
			find "$ARCHIVE_DIR" -maxdepth 1 -name "all_years_*.fst" | sort -r | tail -n +3 | xargs -r rm -f
			echo "Cleaned up old archives (keeping 2 most recent)"
		else
			echo "No existing all_years.fst to archive"
		fi
	}

#########################
# FUNCTION: RUN BIRDNET #
#########################

# Run BirdNET-Analyzer to identify bird calls in audio files
# 
# Behavior depends on mode:
#   single_year_additions    - Process only NEW files in selected_year (uses --skip_existing_results)
#   single_year_replacement  - Reprocess ALL files in selected_year (clears existing results first)
#   all_years_replacement    - Reprocess ALL files from first_year through selected_year
#
# Input:  Audio files in /Volumes/Bio/Bio YYYY/From Recorders
# Output: .BirdNET.selection.table.txt files in run_files_YYYY/ directories

	run_BirdNET_script() {
		print_blank_lines 2
		echo "============================================"
		echo "STARTING BIRDNET-ANALYZER"
		echo "============================================"
		echo "Mode: $mode"
		echo "Selected year: $selected_year"
		echo "Skip existing: $SKIP_EXISTING"
		echo "Log: $BIRDNET_LOG"
		echo "============================================"
		print_blank_lines 1
		
		# Clean up old log files (keep 3 most recent)

			cleanup_old_logs "$BIRDNET_LOG_DIR" "birdnet_*.log"
		
		# Determine year range based on mode
		
			if [[ "$mode" == "all_years_replacement" ]]; then
				echo "Processing ALL years from $first_year through $selected_year"
				year_range=$(seq $first_year $selected_year)
			else
				echo "Processing single year: $selected_year"
				year_range=$selected_year
			fi
		
		# Change to BirdNET directory

			echo "Entering BirdNET directory: $BIRDNET_DIRECTORY"
			cd "$BIRDNET_DIRECTORY" || exit 1
		
		# Activate Python virtual environment
		
			print_blank_lines 1
			echo "Activating Python virtual environment..."
			source "$BIRDNET_VENV_PATH"
		
		# Process each year in the range
		
			for process_year in $year_range; do
				print_blank_lines 1
				echo "----------------------------------------"
				echo "Processing year: $process_year"
				echo "----------------------------------------"
				
			# Set paths for this process_year
			
				local input_folder="$BIO_ARCHIVE_BASE/Bio ${process_year}/From Recorders"
				local output_folder="$RUN_FILES_DIR/run_files_${process_year}"
			
			# Create output directory if it doesn't exist
			
				mkdir -p "$output_folder"
			
			# In replacement modes, clear existing results before reprocessing
			
				if [[ "$mode" == "single_year_replacement" ]] || [[ "$mode" == "all_years_replacement" ]]; then
					echo "Clearing existing results for $process_year (replacement mode)..."
					rm -f "$output_folder"/*.BirdNET.selection.table.txt
					local cleared_count=$(find "$output_folder" -maxdepth 1 -name "*.BirdNET.selection.table.txt" | wc -l)
					echo "Cleared existing result files"
				fi
				
				echo "Input folder:  $input_folder"
				echo "Output folder: $output_folder"
				
			# Run BirdNET-Analyzer (output goes to log file)
				
				echo "Running BirdNET-Analyzer..."
				
			# Use LAT LON to generate species list, OR...

# 				python3 -m birdnet_analyzer.analyze \
# 					--threads "$THREADS" \
# 					--lat "$LAT" \
# 					--lon "$LON" \
# 					$SKIP_EXISTING \
# 					-o "$output_folder" \
# 					"$input_folder" 2>&1 | tee -a "$BIRDNET_LOG"

			# Use a species-list file 

				python3 -m birdnet_analyzer.analyze \
					--threads "$THREADS" \
					--slist "$SPECIES_LIST_FILE_PATH" \
					$SKIP_EXISTING \
					-o "$output_folder" \
					"$input_folder" 2>&1 | tee -a "$BIRDNET_LOG"					
				
			# Count output files
				
#				local result_count=$(ls -1 "$output_folder"/*.BirdNET.selection.table.txt 2>/dev/null | wc -l)
				local result_count=$(find "$output_folder" -maxdepth 1 -name "*.BirdNET.selection.table.txt" | wc -l)
				echo "Completed year $process_year: $result_count result files in output folder"
			done
		
		# Deactivate virtual environment
			print_blank_lines 1
			echo "Deactivating Python virtual environment..."
			deactivate
			
			print_blank_lines 2
			echo "============================================"
			echo "BIRDNET-ANALYZER COMPLETE"
			echo "============================================"
	}
	
##############################################
# FUNCTION: REGENERATE COMBINED-RESULTS FILE #
##############################################

# Combine individual BirdNET result files into single-year text files
#
# For each year in the range, this function:
#   - Takes all .BirdNET.selection.table.txt files from run_files_YYYY/ directory
#   - Combines them into a single single_year_YYYY.txt file
#   - Each year gets its own separate output file
#
# Behavior depends on mode:
#   single_year_additions/replacement - Combine files for selected_year only
#   all_years_replacement              - Combine files for ALL years (first_year through selected_year)
#
# Input:  Multiple .BirdNET.selection.table.txt files in run_files_YYYY/ directories
# Output: One single_year_YYYY.txt file per year processed
#
# Calls: BirdNET_run_file_combiner.sh (which expects: input_folder output_file)

	run_file_combiner() {
		print_blank_lines 2
		echo "============================================"
		echo "STARTING RUN-FILE COMBINER"
		echo "============================================"
		echo "Mode: $mode"
		echo "Script: $COMBINER_SCRIPT"
		echo "Log: $COMBINER_LOG"
		echo "============================================"
		print_blank_lines 1
		
		# Clean up old log files (keep 3 most recent)

			cleanup_old_logs "$COMBINER_LOG_DIR" "combiner_*.log"
		
		# Determine year range based on mode

			if [[ "$mode" == "all_years_replacement" ]]; then
				echo "Combining files for ALL years from $first_year through $selected_year"
				year_range=$(seq $first_year $selected_year)
			else
				echo "Combining files for single year: $selected_year"
				year_range=$selected_year
			fi
		
		# Process each year in the range
		# Each year gets its own separate single_year_YYYY.txt output file

			for process_year in $year_range; do
				print_blank_lines 1
				echo "----------------------------------------"
				echo "Combining year: $process_year"
				echo "----------------------------------------"
				
				# Set paths for this specific process_year
				local input_folder="$RUN_FILES_DIR/run_files_${process_year}"
				local output_file="$COMBINED_DIR/single_year_${process_year}.txt"
				
				# Verify input folder exists
				if [ ! -d "$input_folder" ]; then
					echo "ERROR: Input folder not found: $input_folder"
					echo "       Skipping year $process_year"
					continue
				fi
				
				# Count input files
				local input_count=$(find "$input_folder" -maxdepth 1 -name "*.BirdNET.selection.table.txt" | wc -l)
				
				if [ $input_count -eq 0 ]; then
					echo "WARNING: No BirdNET result files found in $input_folder"
					echo "         Skipping year $process_year"
					continue
				fi
				
				echo "Input folder: $input_folder ($input_count files)"
				echo "Output file:  $output_file"
				
				# Run the combiner script for this year (output goes to log file)
				# This script combines all .BirdNET.selection.table.txt files from input_folder
				# into a single output_file
				sh "$COMBINER_SCRIPT" "$input_folder" "$output_file" 2>&1 | tee -a "$COMBINER_LOG"
				
				# Verify output and count lines
				if [ -f "$output_file" ]; then
					local line_count=$(wc -l < "$output_file")
					echo "Created: $output_file ($line_count lines)"
				else
					echo "ERROR: Output file not created: $output_file"
				fi
			done
			
		print_blank_lines 2
		echo "============================================"
		echo "RUN-FILE COMBINER COMPLETE"
		echo "============================================"
	}
	
######################################################
# FUNCTION: FETCH OPEN-METRO WEATHER DATA           #
######################################################

# Fetches hourly weather data from the Open-Metro Historical Weather API
# and stores it in a local SQLite database for use by regenerate_FST.R.
#
# This replaces the manual process of copying data from local weather stations
# (which were unreliable and gap-prone) into valley.weather.db.
#
# API: https://open-meteo.com/en/docs/historical-weather-api
# Free for non-commercial use; no API key required.
#
# Variables fetched (stored in table: open_metro_weather_hourly):
#   temperature_2m      - Air temperature at 2m (°C)
#   precipitation       - Total precipitation, preceding hour sum (mm)
#   wind_speed_10m      - Wind speed at 10m (mph)
#   wind_direction_10m  - Wind direction at 10m (degrees)
#   wind_gusts_10m      - Wind gusts at 10m (mph)
#   surface_pressure    - Atmospheric pressure at surface (hPa)
#   cloud_cover         - Total cloud cover (%)
#
# Behavior:
#   - Fetches the FULL date range (first_year through selected_year) in a
#     single API call regardless of mode. At ~8,760 rows/year this is a
#     trivially small request even across 10+ years of data.
#   - Drops and recreates the table on every run -- no partial updates,
#     no existence checks, no year-by-year looping.
#   - Always writes both the SQLite DB and a flat txt file for debugging.
#
# Output files (both written to WEATHER_DATA_BASE):
#   open_metro_weather.db        - SQLite database (authoritative full record;
#                                  accumulates across all runs)
#   open_metro_weather_data.txt  - Tab-delimited flat file for debugging.
#                                  NOTE: This file reflects ONLY the date range
#                                  of the most recent run. It is overwritten each
#                                  time fetch_weather_data runs. If you need all
#                                  years in the txt file, run with first_year and
#                                  selected_year spanning the full range. The DB
#                                  is the authoritative record across all runs.
#
# Coordinates: $LAT, $LON (shared with BirdNET species filtering)
# Logging: appended to $FST_LOG

	fetch_weather_data() {

		print_blank_lines 2
		echo "============================================"
		echo "STARTING OPEN-METRO WEATHER FETCH"
		echo "============================================"
		echo "Mode:        $mode"
		echo "Date range:  ${first_year}-01-01 through ${selected_year}-12-31"
		echo "DB:          $OPEN_METRO_WEATHER_DB_PATH"
		echo "Flat file:   $OPEN_METRO_WEATHER_TXT_PATH"
		echo "Latitude:    $LAT"
		echo "Longitude:   $LON"
		echo "Log:         $FST_LOG"
		echo "============================================"
		print_blank_lines 1

		# Verify curl is available

			if ! command -v curl &> /dev/null; then
				echo "ERROR: curl is required but not found"
				exit 1
			fi

		# Verify sqlite3 is available

			if ! command -v sqlite3 &> /dev/null; then
				echo "ERROR: sqlite3 is required but not found"
				exit 1
			fi

		# Verify python3 is available

			if ! command -v python3 &> /dev/null; then
				echo "ERROR: python3 is required but not found"
				exit 1
			fi

		# Drop and recreate the table (full replace on every run)

			echo "Recreating open_metro_weather_hourly table..."
			sqlite3 "$OPEN_METRO_WEATHER_DB_PATH" "
				DROP TABLE IF EXISTS open_metro_weather_hourly;
				CREATE TABLE open_metro_weather_hourly (
					hour                TEXT PRIMARY KEY,
					temperature_2m      REAL,
					precipitation       REAL,
					wind_speed_10m      REAL,
					wind_direction_10m  REAL,
					wind_gusts_10m      REAL,
					surface_pressure    REAL,
					cloud_cover         REAL
				);
			"

		# Cap end date at today to avoid rejections from the historical API
			local today
			today=$(date '+%Y-%m-%d')
			local fetch_end
			if [[ "${selected_year}-12-31" > "$today" ]]; then
			    fetch_end="$today"
			else
			    fetch_end="${selected_year}-12-31"
			fi
		
		# Build API URL - single call covering full year range

			local api_url="https://archive-api.open-meteo.com/v1/archive"
			local api_params="latitude=${LAT}&longitude=${LON}"
			api_params+="&start_date=${first_year}-01-01&end_date=${fetch_end}"
			api_params+="&hourly=temperature_2m,precipitation,wind_speed_10m,wind_direction_10m,wind_gusts_10m,surface_pressure,cloud_cover"
			api_params+="&temperature_unit=celsius"
			api_params+="&wind_speed_unit=mph"
			api_params+="&timezone=UTC"

			local full_url="${api_url}?${api_params}"

		# Fetch from API into a temp file

			local tmp_json
			tmp_json=$(mktemp /tmp/open_metro_weather_XXXXXX.json)

			echo "Calling Open-Metro API..."
			echo "URL: $full_url"

			local http_status
			http_status=$(curl -s -w "%{http_code}" -o "$tmp_json" "$full_url")

			if [[ "$http_status" != "200" ]]; then
				echo "ERROR: Open-Metro API returned HTTP $http_status"
				rm -f "$tmp_json"
				exit 1
			fi

		# Check for API-level error in the JSON response

			if grep -q '"error":true' "$tmp_json"; then
				local api_reason
				api_reason=$(grep -o '"reason":"[^"]*"' "$tmp_json" | head -1)
				echo "ERROR: Open-Metro API error: $api_reason"
				rm -f "$tmp_json"
				exit 1
			fi

		# Parse JSON, insert into SQLite, and write tab-delimited txt file

			echo "Parsing response and writing to DB and flat file..."

			python3 - "$tmp_json" "$OPEN_METRO_WEATHER_DB_PATH" "$OPEN_METRO_WEATHER_TXT_PATH" << 'PYEOF'
import sys
import json
import sqlite3

json_path = sys.argv[1]
db_path   = sys.argv[2]
txt_path  = sys.argv[3]

with open(json_path) as f:
    data = json.load(f)

hourly = data.get("hourly", {})
times  = hourly.get("time",               [])
temp   = hourly.get("temperature_2m",     [])
precip = hourly.get("precipitation",      [])
wspd   = hourly.get("wind_speed_10m",     [])
wdir   = hourly.get("wind_direction_10m", [])
wgst   = hourly.get("wind_gusts_10m",     [])
pres   = hourly.get("surface_pressure",   [])
cloud  = hourly.get("cloud_cover",        [])

rows = list(zip(times, temp, precip, wspd, wdir, wgst, pres, cloud))

# Write to SQLite

con = sqlite3.connect(db_path)
cur = con.cursor()
cur.executemany(
    """INSERT OR REPLACE INTO open_metro_weather_hourly
       (hour, temperature_2m, precipitation,
        wind_speed_10m, wind_direction_10m, wind_gusts_10m,
        surface_pressure, cloud_cover)
       VALUES (?, ?, ?, ?, ?, ?, ?, ?)""",
    rows
)
con.commit()
con.close()

# Write to tab-delimited flat file (debugging convenience)
# Note: reflects only the date range of this run - see script header comments

header = "\t".join([
    "hour", "temperature_2m", "precipitation",
    "wind_speed_10m", "wind_direction_10m", "wind_gusts_10m",
    "surface_pressure", "cloud_cover"
])

with open(txt_path, "w") as f:
    f.write(header + "\n")
    for row in rows:
        f.write("\t".join(["" if v is None else str(v) for v in row]) + "\n")

print(f"  Inserted {len(rows)} rows into DB")
print(f"  Wrote {len(rows)} rows to flat file")
if times:
    print(f"  Date range in response: {times[0]} through {times[-1]}")
PYEOF

			local py_exit=$?
			rm -f "$tmp_json"

			if [[ $py_exit -ne 0 ]]; then
				echo "ERROR: Failed to parse/write weather data"
				exit 1
			fi

		# Confirm final row count in DB

			local total_rows
			total_rows=$(sqlite3 "$OPEN_METRO_WEATHER_DB_PATH" \
				"SELECT COUNT(*) FROM open_metro_weather_hourly;")
			echo "DB total rows: $total_rows"
			echo "Flat file:     $OPEN_METRO_WEATHER_TXT_PATH"

		print_blank_lines 2
		echo "============================================"
		echo "OPEN-METRO WEATHER FETCH COMPLETE"
		echo "============================================"
	}

######################################################
# FUNCTION: REGENERATE ANNUAL AND COMBINED FST FILES #
######################################################

# Regenerate FST files by running the R script
#
# PASS 1 (always runs):
#   - Reads single_year_YYYY.txt file(s)
#   - Joins with weather data
#   - Writes single_year_YYYY.fst file(s)
#
# PASS 2 (always runs):
#   - Reads all yearly FST files (first_year through selected_year)
#   - Combines into all_years.fst
#   - Archives old all_years.fst before overwriting
#
# Behavior depends on mode:
#   single_year_additions/replacement - Regenerate single_year_YYYY.fst for selected_year, then PASS 2
#   all_years_replacement              - Regenerate all single_year_YYYY.fst files, then PASS 2
#
# Input:  single_year_YYYY.txt files, Open-Meteo weather database
# Output: single_year_YYYY.fst files, all_years.fst
#
# Calls: regen_FST.R

	regen_FST_files() {

		print_blank_lines 2
		echo "============================================"
		echo "STARTING FST FILE REGENERATION"
		echo "============================================"
		echo "Mode: $mode"
		echo "Script: $FST_SCRIPT"
		echo "Log: $FST_LOG"
		echo "============================================"
		print_blank_lines 1
		
		# Clean up old log files (keep 3 most recent)
		
			cleanup_old_logs "$FST_LOG_DIR" "FST_regen_*.log"
		
		# Determine year range based on mode
		
			if [[ "$mode" == "all_years_replacement" ]]; then
				echo "Regenerating FST files for ALL years from $first_year through $selected_year"
				year_range=$(seq $first_year $selected_year)
			else
				echo "Regenerating FST file for single year: $selected_year"
				year_range=$selected_year
			fi
		
		# Archive existing all_years.fst before we start modifying files
		
			print_blank_lines 1
			echo "Archiving existing all_years.fst (if present)..."
			archive_all_years_fst
		
		# Validate that required yearly FST files exist for PASS 2
		# (Skip validation for years we're about to regenerate)
		
			print_blank_lines 1
			echo "Validating yearly FST files before PASS 2..."
			missing_files=0
			
			for y in $(seq $first_year $selected_year); do
				yearly_fst="$FST_DIR/single_year_${y}.fst"
				
				# Skip validation for years we're about to regenerate in this run
				
					should_skip=false
					for check_year in $year_range; do
						if [[ "$y" == "$check_year" ]]; then
							should_skip=true
							break
						fi
					done
					
					if [[ "$should_skip" == true ]]; then
						echo "  Year $y: Will be regenerated in this run (skipping validation)"
						continue
					fi
				
				# Check if file exists
				
					if [[ ! -f "$yearly_fst" ]]; then
						echo "  Year $y: MISSING - $yearly_fst"
						missing_files=$((missing_files + 1))
					else
						echo "  Year $y: OK"
					fi
			done
			
			if [[ $missing_files -gt 0 ]]; then
				echo ""
				echo "ERROR: $missing_files yearly FST file(s) are missing"
				echo "       Cannot proceed with PASS 2"
				echo "       Consider running in all_years_replacement mode to regenerate all files"
				exit 1
			fi
			echo "Validation passed: All required yearly FST files exist or will be created"
		
		# PASS 1: Generate single-year FST files
		
			print_blank_lines 1
			echo "============================================"
			echo "PASS 1: Generating single-year FST files"
			echo "============================================"
			
			for process_year in $year_range; do
				print_blank_lines 1
				echo "Processing year: $process_year"
				
				# Set paths for this process_year
				
				local input_txt="$COMBINED_DIR/single_year_${process_year}.txt"
				local output_fst="$FST_DIR/single_year_${process_year}.fst"
				local weather_start="${process_year}-01-01"
				local weather_end="${process_year}-12-31"
				
				# Verify input file exists
				
				if [ ! -f "$input_txt" ]; then
					echo "ERROR: Input file not found: $input_txt"
					echo "       Skipping year $process_year"
					continue
				fi
				
				echo "Input:  $input_txt"
				echo "Output: $output_fst"
				echo "Weather range: $weather_start to $weather_end"
				
				# Run R script for PASS 1 only (pass_mode=pass1)
				
				Rscript "$FST_SCRIPT" \
					input_combined_file="$input_txt" \
					single_year_fst_output="$output_fst" \
					yearly_fst_paths="$YEARLY_FST_PATHS" \
					all_years_fst_output="$ALL_YEARS_FST_OUTPUT" \
					weather_path="$OPEN_METRO_WEATHER_DB_PATH" \
					start_date="$weather_start" \
					end_date="$weather_end" \
					first_year="$first_year" \
					selected_year="$selected_year" \
					pass_mode="pass1" 2>&1 | tee -a "$FST_LOG"
				
				# Verify output
				
				if [ -f "$output_fst" ]; then
					local file_size=$(du -h "$output_fst" | cut -f1)
					echo "Created: $output_fst ($file_size)"
				else
					echo "ERROR: Output file not created: $output_fst"
				fi
			done
		
		# PASS 2: Combine all yearly FST files into all_years.fst
		
			print_blank_lines 1
			echo "============================================"
			echo "PASS 2: Combining all yearly FST files"
			echo "============================================"
			echo "Combining years $first_year through $selected_year"
			echo "Output: $ALL_YEARS_FST_OUTPUT"
			
			# Run R script for PASS 2 (pass_mode=pass2)
			# Note: We pass the most recent year's parameters, but R script ignores most of them for PASS 2
			
			Rscript "$FST_SCRIPT" \
				input_combined_file="$COMBINER_OUTPUT" \
				single_year_fst_output="$SINGLE_YEAR_FST_OUTPUT" \
				yearly_fst_paths="$YEARLY_FST_PATHS" \
				all_years_fst_output="$ALL_YEARS_FST_OUTPUT" \
				weather_path="$OPEN_METRO_WEATHER_DB_PATH" \
				start_date="$WEATHER_START_DATE" \
				end_date="$WEATHER_END_DATE" \
				first_year="$first_year" \
				selected_year="$selected_year" \
				pass_mode="pass2" 2>&1 | tee -a "$FST_LOG"
			
		# Verify PASS 2 output
		
			if [ -f "$ALL_YEARS_FST_OUTPUT" ]; then
				local file_size=$(du -h "$ALL_YEARS_FST_OUTPUT" | cut -f1)
				echo "Created: $ALL_YEARS_FST_OUTPUT ($file_size)"
			else
				echo "ERROR: all_years.fst not created"
			fi
		
		print_blank_lines 2
		echo "============================================"
		echo "FST FILE REGENERATION COMPLETE"
		echo "============================================"
	}
	
############################################### 
# FUNCTIONS: RESTART UI and Tiny.shiny PLISTS #
###############################################

	# Helper to check if prod-plist exists
	check_prod_plist_exists() {
		if [ ! -f "$PROD_UI_PLIST_PATH" ]; then
			print_blank_lines 2
			echo "Error: LaunchAgent plist not found at $PROD_UI_PLIST_PATH"
			exit 1
		fi
	}
	
	# Helper to check if test-plist exists
	check_test_plist_exists() {
		if [ ! -f "$TEST_UI_PLIST_PATH" ]; then
			print_blank_lines 2
			echo "Error: LaunchAgent plist not found at $TEST_UI_PLIST_PATH"
			exit 1
		fi
	}
	
	# Helper to check if Tiny-shiny-plist exists
	check_tiny_shiny_plist_exists() {
		if [ ! -f "$TINY_SHINY_PLIST_PATH" ]; then
			print_blank_lines 2
			echo "Error: LaunchAgent plist not found at $TINY_SHINY_PLIST_PATH"
			exit 1
		fi
	}
	
	# Reload prod-UI plist
	reload_prod_launchagent() {
		print_blank_lines 1
		echo "Unloading LaunchAgent: $PROD_UI_PLIST_PATH"
		launchctl unload "$PROD_UI_PLIST_PATH"
	
	#    print_blank_lines 1
		echo "Loading LaunchAgent: $PROD_UI_PLIST_PATH"
		launchctl load "$PROD_UI_PLIST_PATH"
	
		
		print_blank_lines 1
		echo "ProductionLaunchAgent reloaded."
	}
	
	# Reload test-UI plist
	reload_test_launchagent() {
		print_blank_lines 1
		echo "Unloading LaunchAgent: $TEST_UI_PLIST_PATH"
		launchctl unload "$TEST_UI_PLIST_PATH"
	
	#    print_blank_lines 1
		echo "Loading LaunchAgent: $TEST_UI_PLIST_PATH"
		launchctl load "$TEST_UI_PLIST_PATH"
	
		
		print_blank_lines 1
		echo "Test LaunchAgents reloaded."
	}
	
	# Reload tiny-shiny plist
	reload_tiny_shiny_launchagent() {
		print_blank_lines 1
		echo "Unloading LaunchAgent: $TINY_SHINY_PLIST_PATH"
		launchctl unload "$TINY_SHINY_PLIST_PATH"
	
	#    print_blank_lines 1
		echo "Loading LaunchAgent: $TINY_SHINY_PLIST_PATH"
		launchctl load "$TINY_SHINY_PLIST_PATH"
	
		
		print_blank_lines 1
		echo "Tiny-shiny LaunchAgents reloaded."
	}
		

# #########################################################################
# CALL "MAIN" FUNCTION TO RUN THE FUNCTIONS IN ORDER - PASSING PARAMETERS #
###########################################################################

main "$@"

# #################################
# # MAIN: RUN SCRIPTS IN SEQUENCE #
# #################################
# 
# 
# 	# Capture and announce start time
# 	
# 	start_time=$(date +%s)
# 	print_blank_lines 2
# 	echo "Started at: $(date '+%Y:%m:%d %H:%M:%S')"
# 	start_time_formatted=$(date '+%Y:%m:%d %H:%M:%S')
# 	
# 	# This section runs the functions in sequence.  Use comments for run-control
# 	 
# 		run_BirdNET_script
# 	#	run_file_combiner
# 	#	regen_FST_files
# 	#   copy_new_FST_to_UI_input_target -- placeholder
# 	
# 	#	check_prod_plist_exists
# 	#	reload_prod_launchagent
# 	#	check_test_plist_exists
# 	#	reload_test_launchagent
# 	#	check_tiny_shiny_plist_exists
# 	#	reload_tiny_shiny_launchagent
# 	
# 	#  NOTE: This last group are for testing - a new instance of the FST file gets pulled in each 
# 	#  time the UI is reloaded in the web browser - restarting the UI background task isn't 
# 	#  required.  To test various versions of the FST file, change the path in the UI
# 	#  script and hit refresh in the browser
	
	
###########
# WRAP UP #
###########

	# Capture end time
		end_time=$(date +%s)
		end_time_formatted=$(date '+%Y-%m-%d %H:%M:%S')
	
	# Calculate elapsed time
		elapsed=$((end_time - start_time))
		hours=$((elapsed / 3600))
		minutes=$(((elapsed % 3600) / 60))
		seconds=$((elapsed % 60))
	
		print_blank_lines 2
		
	echo "========================================"
	echo "PIPELINE EXECUTION COMPLETED"
	echo "========================================"
	echo "Started:  $start_time_formatted"
	echo "Ended:    $end_time_formatted"
	echo "Elapsed:  ${hours}h ${minutes}m ${seconds}s"
	echo "========================================"
	print_blank_lines 1
	
	echo "RUN SUMMARY:"
	echo "  Mode:          $mode"
	echo "  First year:    $first_year"
	echo "  Selected year: $selected_year"
	echo "  Run date:      $RUN_DATE"
	print_blank_lines 1
	
	echo "OUTPUT LOCATIONS:"
	echo "  BirdNET results: $RUN_FILES_DIR/run_files_*/"
	echo "  Combined files:  $COMBINED_DIR/single_year_*.txt"
	echo "  FST files:       $FST_DIR/single_year_*.fst"
	echo "  UI input file:   $ALL_YEARS_FST_OUTPUT"
	print_blank_lines 1
	
	echo "LOG FILES:"
	echo "  BirdNET:   $BIRDNET_LOG"
	echo "  Combiner:  $COMBINER_LOG"
	echo "  FST regen: $FST_LOG"
	print_blank_lines 1
	
	echo "NEXT STEPS:"
	echo "  1. Verify test results"
	echo "  2. Check UI with test all_years.fst: $ALL_YEARS_FST_OUTPUT"
	echo "  3. If results are good, promote to production:"
	echo "     ./promote_test_to_prod.sh"
	print_blank_lines 1
	
	echo "Pipeline completed successfully!"
	print_blank_lines 1