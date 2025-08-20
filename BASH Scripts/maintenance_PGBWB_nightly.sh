#!/bin/bash

# PGBEB writes temporary files to this directory when users request audio for an ID
# this nightly script empties the folder they're written to.

# rm -rf /Users/mikeoconnor/Documents/BirdWorkbench/BirdWorkbench_R_Scripts/www/*


# Suppress macOS crash dialogs
export __CF_USER_TEXT_ENCODING=$(id -u):0:0
export NO_ATALK=1

# Exit on error, undefined var, or pipe fail
set -euo pipefail

# Custom error trap
trap 'echo "Status: no files to delete" >&2; exit 1' ERR

# Path to clean
TARGET_DIR="/Users/mikeoconnor/Documents/BirdWorkbench/BirdWorkbench_R_Scripts/www"

# Ensure path exists before attempting to remove
if [ -d "$TARGET_DIR" ]; then
    rm -rf "${TARGET_DIR:?}/"* 2>/dev/null
fi
