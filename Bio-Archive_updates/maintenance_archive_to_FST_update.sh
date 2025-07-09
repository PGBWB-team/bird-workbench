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

########################################################
# Initialize paths and variables - User input required #
########################################################

BIRDNET_DIRECTORY_PATH="$HOME/Documents/BirdNET-Analyzer github/BirdNET-Analyzer"

BIRDNET_INPUT_FILE_PATH="/Volumes/Bio/Bio 2025/From Recorders"

BIRDNET_OUTPUT_FILE_PATH="$HOME/Documents/BirdWorkbench/birdnet_analyzer_RUN_FILES/run_files_2025"
	
RUN_FILE_COMBINER_SHELL_SCRIPT_PATH="$HOME/Documents/BirdWorkbench/BirdWorkbench_BASH_scripts/BirdNET_run_file_combinerV3.sh"

REGENERATE_FST_FILES_SCRIPT_PATH="$HOME/Documents/BirdWorkbench/BirdWorkbench_R_Scripts/combine_birdnet_runs.m18.looper_2025.R"
	
UI_PLIST_PATH="$HOME/Library/LaunchAgents/com.pgbwb.bird-workbench_UI.test.plist"

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
    echo "BirdNET run complete"
}

	
##############################################
# FUNCTION: REGENERATE COMBINED-RESULTS FILE #
##############################################		

# Combine the selection.table files to regenerate a run_files_COMBINED_20nn.txt file with # all the results for the year

# NOTE: Edit the Directory statements at the top of the script to point to input 
# (yearly results-file directory) and output (yearly combined-results file) paths

run_file_combiner () {
	print_blank_lines 2
	echo "starting BirdNET run-file combiner script"
#	sh $HOME/Documents/BirdWorkbench/BirdWorkbench_BASH_scripts/BirdNET_run-file_combinerV3.sh
	sh "$RUN_FILE_COMBINER_SHELL_SCRIPT_PATH"
	print_blank_lines 2
	echo "run_files_combined complete"
	}
	
######################################################
# FUNCTION: REGENERATE ANNUAL AND COMBINED FST FILES #
######################################################

# Update the annual and combined .FST files by running the combine_birdnet_runs.R script
# NOTE: this script must be updated at the beginning of each year

regen_FST_files () {
	print_blank_lines 2
	echo "starting FST file update script"
#	rscript $HOME/Documents/BirdWorkbench/BirdWorkbench_R_Scripts/combine_birdnet_runs.m18.looper_2025.R
	rscript "$REGENERATE_FST_FILES_SCRIPT_PATH"
	print_blank_lines 2
	echo "FST file-update complete"
	}

##############################
# FUNCTION: RESTART UI PLIST #
##############################

# Helper to check if plist exists
check_plist_exists() {
    if [ ! -f "$UI_PLIST_PATH" ]; then
        print_blank_lines 2
        echo "Error: LaunchAgent plist not found at $UI_PLIST_PATH"
        exit 1
    fi
}

# Reload UI plist
reload_launchagent() {
    print_blank_lines 1
    echo "Unloading LaunchAgent: $UI_PLIST_PATH"
    launchctl unload "$UI_PLIST_PATH"

    print_blank_lines 1
    echo "Loading LaunchAgent: $UI_PLIST_PATH"
    launchctl load "$UI_PLIST_PATH"

    
    print_blank_lines 1
    echo "LaunchAgent reloaded."
}
	

#################################
# MAIN: RUN SCRIPTS IN SEQUENCE #
#################################

#	run_BirdNET_script
#	run_file_combiner
#	regen_FST_files
	check_plist_exists
	reload_launchagent

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

