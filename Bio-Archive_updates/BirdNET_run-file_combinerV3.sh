#!/bin/bash

# This script is typically called as a part of the Archive to FST Update process
# after BirdNET-Analyzer has created new results files that need to be merged into
# a single results file for the year.  It deletes the old combined-results file,
# loops through the directory containing BirdNET's results files for the year and merges all the
# results into a single file for the year

# EDIT - Directory containing the files to process
input_dir="/Users/mikeoconnor/Documents/BirdWorkbench/birdnet_analyzer_RUN_FILES/run_files_2025" 
# EDIT - Output file
output_file="/Users/mikeoconnor/Documents/BirdWorkbench/birdnet_analyzer_run_files_COMBINED/run_files_COMBINED_2025.txt"

# Ensure the output file is empty initially
> "$output_file"

# Variable to track if the header line has been written
header_written=false

# Loop through each file in the directory
for file in "$input_dir"/*; do

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
        echo "$line" >> "$output_file"
        header_written=true
      fi
    else
      # Append non-header line to output file
      echo "$line" >> "$output_file"
	  
    fi
  done < "$file"

 
done
