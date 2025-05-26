#!/bin/bash

# This script in the home for nightly maintenance.  

# PGBWB's R UI writes temporary disk files to a directory when users request audio for an ID.
# This step empties that directory.

rm -rf /Users/mikeoconnor/Documents/BirdWorkbench/BirdWorkbench_R_Scripts/www/*
