fd#!/bin/bash

# This script is typically called as a part of the Archive to FST Update process
# after BirdNET-Analyzer has created new results files that need to be merged into
# a single results file for the year.  It deletes the old combined-results file,
# loops through the directory containing BirdNET's results files for the year and merges all the
# results into a single file for the year

########################################################
# Initialize paths and variables - User input required #
########################################################

INPUT_DIRECTORY_PATH="$HOME/Documents/BirdWorkbench/birdnet_analyzer_RUN_FILES/run_files_2025"

OUTPUT_FILE_PATH="$HOME/Documents/BirdWorkbench/birdnet_analyzer_run_files_COMBINED/run_files_COMBINED_2025.txt"

# EDIT - Directory containing the files to process
# input_dir="/Users/mikeoconnor/Documents/BirdWorkbench/birdnet_analyzer_RUN_FILES/run_files_2025" 
# EDIT - Output file
# output_file="/Users/mikeoconnor/Documents/BirdWorkbench/birdnet_analyzer_run_files_COMBINED/run_files_COMBINED_2025.txt"

# Ensure the output file is empty initially
> "$OUTPUT_FILE_PATH"

# Variable to track if the header line has been written
header_written=false

# Loop through each file in the directory
for file in "$INPUT_DIRECTORY_PATH"/*; do

  # Skip files containing "analysis_params" in the filename
  if [[ "$file" == *analysis_params* ]]; then
    continue
  fi
  
  # Read each line in the file
  while IFS= read -r line || [[ -n "$line" ]]; do
    # Skip blank lines
    if [[ -z "$line" ]]; then
      continue
    fi
    
    # Check if line contains "Confidence"
    if [[ "$line" == *"Confidence"* ]]; then
      # Only write the header line once
      if [[ "$header_written" == false ]]; then
        echo "$line" >> "$OUTPUT_FILE_PATH"
        header_written=true
      fi
    else
      # Append non-header line to output file
      echo "$line" >> "$OUTPUT_FILE_PATH"
	  
    fi
  done < "$file"

 
done
