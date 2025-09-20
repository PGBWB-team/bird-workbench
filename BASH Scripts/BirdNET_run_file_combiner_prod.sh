#!/bin/bash

################################################################
# Initialize paths and variables - provided via command line   #
################################################################

# Check for required arguments
if [ "$#" -ne 2 ]; then
  echo
  echo "Missing arguments.  Expected paths for input_dir and output_file"
  exit 1
  echo
fi

# Directory containing the files to process
# input_dir="/Users/mikeoconnor/Documents/BirdWorkbench/birdnet_analyzer_RUN_FILES/run_files_2025" 

input_dir="$1" 

# Output file
# output_file="/Users/mikeoconnor/Documents/BirdWorkbench/birdnet_analyzer_run_files_COMBINED/run_files_COMBINED_2025.txt"

output_file="$2"

echo "success:"
echo "Input Directory: $input_dir"
echo "Output File: $output_file"

# Halt test run

# exit 1


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