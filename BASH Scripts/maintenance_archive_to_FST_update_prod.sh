#!/bin/bash

# Run this script any time the Bio Archive has been updated with new audio files
# from the recorders.  It combines the scripts to run BirdNET-Analyzer, combine the
# run files and regenerate the yearly and combined FST files that power the UI.

# Note: the paths and parameters for each script should be verified before launching
# this.

########
# INIT #
########

# Exit script if any step fails

set -e

################################
# Workflow notes and reminders #
################################

# Pre-run

# Update weather-data db before running
# Check file-name flow through the file_combiner and UI R scripts 
# Update combined-FST file name with run date

# Post-run

# Copy combined-FST file to files named correctly for UI-script inputs
 


########################################################
# Initialize paths and variables - User input required #
########################################################

# BirdNET-Analyzer paths: 
# Revise to point input and output file paths at the correct year
# Check parameters in the line that calls the Python script if needed

	BIRDNET_DIRECTORY_PATH="$HOME/Documents/BirdNET-Analyzer github/BirdNET-Analyzer"

	BIRDNET_INPUT_FILE_PATH="/Volumes/Bio/Bio 2025/From Recorders"

	BIRDNET_OUTPUT_FILE_PATH="$HOME/Documents/BirdWorkbench/birdnet_analyzer_RUN_FILES/run_files_2025"

# Path to the BASH script that combines all BirdNET IDs into a single yearly file:  
# Revise file paths WItHIN THE SCRIPT to point input and output paths at the correct year
	
	RUN_FILE_COMBINER_SHELL_SCRIPT_PATH="$HOME/Documents/BirdWorkbench/BirdWorkbench_BASH_scripts/BirdNET_run_file_combiner_prod.sh"

# Path to the R script that regenerates annual and combined FST files:  
# Revise file paths WItHIN THE SCRIPT to point input and output paths at the correct year

	REGENERATE_FST_FILES_SCRIPT_PATH="$HOME/Documents/BirdWorkbench/BirdWorkbench_R_Scripts/combine_birdnet_runs_2025_prod.R"

# Paths to the test and production LaunchAgent .plist files:  
# This script will usually restart both production and test versions of the UI to bring in the new FST file

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

# Announce and capture start time

start_time=$(date +%s)
print_blank_lines 2
echo "Started at: $(date '+%Y:%m:%d %H:%M:%S')"
start_time_formatted=$(date '+%Y:%m:%d %H:%M:%S')
	

#########################
# FUNCTION: RUN BIRDNET #
#########################

# Run BirdNET-Analyzer to identify calls in newly-added audio-archive files
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
    
    python3 -m birdnet_analyzer.analyze --threads 8 --lat 44.415947 --lon -91.700659 --skip_existing_results -o "$BIRDNET_OUTPUT_FILE_PATH" "$BIRDNET_INPUT_FILE_PATH"

        
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

# NOTE: Edit the Directory statements at the top of the script to point to input 
# (yearly results-file directory) and output (yearly combined-results file) paths

run_file_combiner () {
	print_blank_lines 2
	echo "STARTING BirdNET RUN-FILE COMBINER SCRIPT"
#	sh $HOME/Documents/BirdWorkbench/BirdWorkbench_BASH_scripts/BirdNET_run-file_combinerV3.sh
	sh "$RUN_FILE_COMBINER_SHELL_SCRIPT_PATH"
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
	rscript "$REGENERATE_FST_FILES_SCRIPT_PATH"
	print_blank_lines 2
	echo "FST FILE IS REGENERATED"
	}

##############################
# FUNCTION: RESTART UI PLIST #
##############################

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

# This section runs the functions in sequence.  Use comments for run-control
 
#	run_BirdNET_script
#	run_file_combiner
#	regen_FST_files
	check_prod_plist_exists
	reload_prod_launchagent
#	check_test_plist_exists
#	reload_test_launchagent
#	check_tiny_shiny_plist_exists
#	reload_tiny_shiny_launchagent

###########
# WRAP UP #
###########

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
echo "UPDATE WEATHER DB ASAP"
print_blank_lines 1

