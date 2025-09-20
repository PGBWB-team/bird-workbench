#!/bin/bash

# Run this script any time the Bio Archive has been updated with new audio files
# from the recorders.  It combines the scripts to:
# run BirdNET-Analyzer, 
# combine BirdNEt-Analyzer run-files into a single file for the year, and
# regenerate the yearly and combined FST files that power the UI.


########
# INIT #
########

# Exit script if any step fails

set -e

######################
# Workflow reminders #
######################

# Pre-run reminders: 

	echo "Prerun reminders"
	echo " "
	
	echo "Updated the weather database?"
	echo "Updated the combined-FST output file name with run date?"
	echo "Verified that run-control is set correctly?"
	echo "Verified that the final/combined FST output path matches the INPUT"
	echo "  path of the production/test UI scripts?"
	read -p "Enter your response (yes/no): " response
	
	# Convert the response to lowercase for easier comparison
	response=$(echo "$response" | tr '[:upper:]' '[:lower:]')
	
	# Check the user's response
	if [[ "$response" == "yes" || "$response" == "y" ]]; then
	  echo "Continuing with the script..."
	  # Add your script logic here
	else
	  echo "Stopping the script."
	  exit 1
	fi
 
##########################################################################
# Initialize paths and variables to be passed to scripts in the pipeline #
##########################################################################

	####################################################################################
	# 	PARAMTERS FOR: BirdNET-Analyzer (Python script):							   #
	# 	(this script analyzes audio files and writes Bird-IDs to a text file for each) #
	####################################################################################
	
		BIRDNET_DIRECTORY_PATH="$HOME/Documents/BirdNET-Analyzer github/BirdNET-Analyzer"
		
#		BirdNET takes either a file or folder path - in our case we're pointing at a
#		yearly folder of bioacoustic recordings	
		BIRDNET_INPUT_FOLDER_PATH="/Volumes/Bio/Bio 2025/From Recorders"
		
#		Parameters for BirdNET-Analyzer	
		THREADS=8
		LAT=44.415947
		LON=-91.700659
		SKIP_EXISTING="--skip_existing_results"

#		Output folder - a yearly repository of files containing rows of BirdNET IDs for
#		each file in the input folder.  BirdNET is set to skip files that it has already 
#		analyzed and adds a new file to this folder for each unprocessed file encountered
#		in the input folder.  This folder becomes the input file to the next step in the 
#		pipeline.
		BIRDNET_OUTPUT_FOLDER_PATH="$HOME/Documents/BirdWorkbench/birdnet_analyzer_RUN_FILES/run_files_2025"	
 
	
	############################################################################
	#	PARAMTERS FOR: "run_file_combiner" (BASH script):					   # 
	#   (this script combines all rows from all BirdNET-Analyzer results-files # 
	#	into a single file)  												   #
	############################################################################

#		Path to the script
		RUN_FILE_COMBINER_SHELL_SCRIPT_PATH="$HOME/Documents/BirdWorkbench/BirdWorkbench_BASH_scripts/BirdNET_run_file_combiner_prod.sh"	

#		Path to the output file - which becomes the input file to the next step in the 
#		pipeline			
		RUN_FILE_COMBINER_OUTPUT_FILE_PATH="$HOME/Documents/BirdWorkbench/birdnet_analyzer_run_files_COMBINED/run_files_COMBINED_2025_Sep19.txt"


	###########################################################
	#	PARAMTERS FOR: "regenerate_FST_files" (R script:	  # 
	#   (this script updates the FST file that drives the UI) # 
	#	into a single file)  								  #
	###########################################################  

#		Path to the script	
		REGENERATE_FST_FILES_SCRIPT_PATH="$HOME/Documents/BirdWorkbench/BirdWorkbench_R_Scripts/regenerate_FST_files_2025_prod.R"

#		PASS 1: 
#		Regenerate a YEARLY FST file using the yearly combined_run text file from the 
#		prior step in the script (a text file with all the BirdNET-Analyzer rows for 
#		the year)	

#			Pass-1 Output file path - a ONE-YEAR FST file - which becomes the input file  
#			to second step in the in this script			  
			REGENERATE_FST_FILES_fst_output="$HOME/Documents/BirdWorkbench/birdnet_analyzer_run_files_combined_FST/run_files_combined_FST.2025_Sep19.fst" 

	
#		PASS 2: 
#		Combine all the yearly FST files into a single FST input-file for the UI script 
#		(this isn't generated all in one go because eventually the FST-generating process 
#		will run out of memory - this yearly hack allows the workflow to scale)
	
#			Input (yearly) file paths.  Pay attention to how the Step 1 output file is brought
#			forward.  Normally it's pointed at the most-current year, but this allows for
#			reprocessing prior-years if needed.		
			REGENERATE_FST_FILES_yearly_FST_input_paths2020="$HOME/Documents/BirdWorkbench/birdnet_analyzer_run_files_combined_FST/run_files_combined_FST.2020.fst" 
			REGENERATE_FST_FILES_yearly_FST_input_paths2021="$HOME/Documents/BirdWorkbench/birdnet_analyzer_run_files_combined_FST/run_files_combined_FST.2021.fst" 
			REGENERATE_FST_FILES_yearly_FST_input_paths2022="$HOME/Documents/BirdWorkbench/birdnet_analyzer_run_files_combined_FST/run_files_combined_FST.2022.fst" 
			REGENERATE_FST_FILES_yearly_FST_input_paths2023="$HOME/Documents/BirdWorkbench/birdnet_analyzer_run_files_combined_FST/run_files_combined_FST.2023.fst" 
			REGENERATE_FST_FILES_yearly_FST_input_paths2024="$HOME/Documents/BirdWorkbench/birdnet_analyzer_run_files_combined_FST/run_files_combined_FST.2024.fst" 
			REGENERATE_FST_FILES_yearly_FST_input_paths2025=$REGENERATE_FST_FILES_fst_output
		
#			Pass-2 Output file path - an ALL-YEARS FST file - which becomes the input 
#			file to the UI script	 
			REGENERATE_FST_FILES_combined_FST_path="$HOME/Documents/BirdWorkbench/birdnet_analyzer_run_files_combined_FST/run_files_combined_Sep19.fst" 
			
#			Parameters for the SQL query that pulls in weather data
			REGENERATE_FST_FILES_weather_path="$HOME/Documents/BirdWorkbench/weather_snoop_valley_weather/valley.weather.db" 
			REGENERATE_FST_FILES_start_date="2025-01-01" 
			REGENERATE_FST_FILES_end_date="2025-12-31"
			
			
	####################################################################################
	#	Paths to the test and production LaunchAgent .plist files: 					   # 
	#   This script will usually restart both production and test versions of the UI   # 
	#	ito bring in the new FST file  												   #
	####################################################################################


	PROD_UI_PLIST_PATH="$HOME/Library/LaunchAgents/com.pgbwb.bird-workbench_UI.prod.plist"	
	TEST_UI_PLIST_PATH="$HOME/Library/LaunchAgents/com.pgbwb.bird-workbench_UI.test.plist"
	TINY_SHINY_PLIST_PATH="$HOME/Library/LaunchAgents/com.pgbwb.tiny_shiny.plist"



#########################
# FUNCTION: BLANK LINES #
#########################

	print_blank_lines() {
    	local count=$1
    	for ((i = 0; i < count; i++)); do
        	echo ""
	    done
	}
	

#########################
# FUNCTION: RUN BIRDNET #
#########################

# Run birdnet_analyzer.analyze to identify calls in newly-added audio-archive files.
# Rerun the analysis of the current-year directory of the bioacoustic audio archive to 
# create a selection.table file of ID/Results for each audio file that hasn’t already been 
# analyzed  

run_BirdNET_script () {

    print_blank_lines 2
    echo "STARTING BIRDNET-ANALYZER RUN"
    
    print_blank_lines 2
    echo "Entering BirdNET-Analyzer directory"
    cd "$BIRDNET_DIRECTORY_PATH"  || exit 1
    
	print_blank_lines 2
    echo "Starting venv"
    source venv/bin/activate

    print_blank_lines 2
    echo "Starting BirdNET"
    

	# Call the script, with parameters	(formatting is tricky, test with echo)
	python3 -m birdnet_analyzer.analyze \
	 	  --threads "$THREADS" \
		  --lat "$LAT" \
		  --lon "$LON" \
	 	  $SKIP_EXISTING \
	 	  -o "$BIRDNET_OUTPUT_FOLDER_PATH" \
	 	  "$BIRDNET_INPUT_FOLDER_PATH"

	# (formatting is tricky, here's an echo statement to test with) 
	# echo 	python3 -m birdnet_analyzer.analyze \
	# 	 	  --threads "$THREADS" \
	# 		  --lat "$LAT" \
	# 		  --lon "$LON" \
	# 	 	  $SKIP_EXISTING \
	# 	 	  -o "$BIRDNET_OUTPUT_FOLDER_PATH" \
	# 	 	  "$BIRDNET_INPUT_FOLDER_PATH"
	# exit 0	 	  	 	  
       
    print_blank_lines 2
    echo "Stopping venv"
    deactivate

    print_blank_lines 2
    echo "BIRDNET-ANALYZER RUN COMPLETE"
}

	
##############################################
# FUNCTION: REGENERATE COMBINED-RESULTS FILE #
##############################################		

# Combine the selection.table files to regenerate a run_files_COMBINED_20nn.txt file with # all the results for the year

run_file_combiner () {

	print_blank_lines 2
	echo "STARTING BirdNET RUN-FILE COMBINER SCRIPT"
	
	sh "$RUN_FILE_COMBINER_SHELL_SCRIPT_PATH" \
		"$BIRDNET_OUTPUT_FOLDER_PATH" \
		"$RUN_FILE_COMBINER_OUTPUT_FILE_PATH"
	
	print_blank_lines 2
	echo "RUN-FILES ARE COMBINED"
	}
	
######################################################
# FUNCTION: REGENERATE ANNUAL AND COMBINED FST FILES #
######################################################

# Update the annual and combined .FST files by running the combine_birdnet_runs.R script
# NOTE: this script must be updated at the beginning of each year

regen_FST_files () {
	print_blank_lines 2
	echo "STARTING FST FILE-REGEN SCRIPT"
	
	Rscript "$REGENERATE_FST_FILES_SCRIPT_PATH" \
		input_COMBINED_file="$RUN_FILE_COMBINER_OUTPUT_FILE_PATH" \
		fst_output="$REGENERATE_FST_FILES_fst_output" \
		yearly_FST_input_paths2020="$REGENERATE_FST_FILES_yearly_FST_input_paths2020" \
		yearly_FST_input_paths2021="$REGENERATE_FST_FILES_yearly_FST_input_paths2021" \
		yearly_FST_input_paths2022="$REGENERATE_FST_FILES_yearly_FST_input_paths2022" \
		yearly_FST_input_paths2023="$REGENERATE_FST_FILES_yearly_FST_input_paths2023" \
		yearly_FST_input_paths2024="$REGENERATE_FST_FILES_yearly_FST_input_paths2024" \
		yearly_FST_input_paths2025="$REGENERATE_FST_FILES_yearly_FST_input_paths2025" \
		combined_FST_path="$REGENERATE_FST_FILES_combined_FST_path" \
		weather_path="$REGENERATE_FST_FILES_weather_path" \
		start_date="$REGENERATE_FST_FILES_start_date" \
		end_date="$REGENERATE_FST_FILES_end_date"
 		
	print_blank_lines 2
	echo "FST FILE IS REGENERATED"
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
		

#################################
# MAIN: RUN SCRIPTS IN SEQUENCE #
#################################


	# Capture and announce start time
	
	start_time=$(date +%s)
	print_blank_lines 2
	echo "Started at: $(date '+%Y:%m:%d %H:%M:%S')"
	start_time_formatted=$(date '+%Y:%m:%d %H:%M:%S')
	
	# This section runs the functions in sequence.  Use comments for run-control
	 
		run_BirdNET_script
		run_file_combiner
		regen_FST_files
	#   copy_new_FST_to_UI_input_target (placeholder)
	#	check_prod_plist_exists
	#	reload_prod_launchagent
		check_test_plist_exists
		reload_test_launchagent
	#	check_tiny_shiny_plist_exists
	#	reload_tiny_shiny_launchagent

###########
# WRAP UP #
###########

	# Capture end time and announce start, end and elapsed times
	end_time=$(date +%s)
	print_blank_lines 1
	echo "Started at: $start_time_formatted"	
	echo "Ended at  : $(date '+%Y:%m:%d %H:%M:%S')"	
	
	elapsed=$((end_time - start_time))	
	hours=$((elapsed / 3600))
	minutes=$(((elapsed % 3600) / 60))
	seconds=$((elapsed % 60))
	
	
	echo "Elapsed time: ${hours}h ${minutes}m ${seconds}s"
	print_blank_lines 1
	
	# Post-run
	
	echo "REMINDER: verify that the production and test UI R script FST INPUT paths"
	echo "          match the OUTPUT path from this script"

