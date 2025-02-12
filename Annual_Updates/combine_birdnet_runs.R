# This file will used to combine BirdNET-Analyzer results 

library(lubridate)
library(fst)

# Set file path variables
# These files contains the BirdNET-Analyzer results for each year and for all confidence levels
# *** Add a new file variable for a new year *** #
file_2024 <- "your_directory/Combined_2024_V1.txt"
file_2023 <- "your_directory/Combined_2023_V1.txt"
file_2022 <- "your_directory/Combined_2022_V1.txt"
file_2021 <- "your_directory/Combined_2021_V1.txt"
file_2020 <- "your_directory/Combined_2020_V1.txt"

# Combine file variables into one list
# *** Add new file variable to the list function *** #
file_list <- c(file_2024, file_2023, file_2022, file_2021, file_2020)

# Read each file as a table
data <- lapply(file_list, read.table, header=TRUE, sep="\t", colClasses="character", quote="\"")

# Filter out observations with less than 70% confidence: 
# First, convert "Confidence" column to numeric, handling any non-numeric values
data <- lapply(data, function(df) {
  df$Confidence <- suppressWarnings(as.numeric(as.character(df$Confidence)))
  df$Confidence[is.na(df$Confidence)] <- 0 # Replace NA with 0
  return(df)
})

# Subset rows with Confidence >= 0.7000
# *** Update this variable if you would like to change the confidence cutoff *** #
data_70 <- lapply(data, subset, as.numeric(Confidence) >= 0.7000)

# Create function to normalize file name lengths
normalize_length <- function(x, target_length = 3) {
  if (length(x) > target_length) {
    return(x[1:target_length]) # truncate to the first 3 elements
  } else if (length(x) < target_length) {
    return(c(x, rep(NA, target_length - length(x)))) # pad with NA
  } else {
    return(x) # already at target length
  }
}

# Add columns for Location, Date, and Date.Time, Day.Of.Year
# These variables are derived from the File Path variable
data_70 <- lapply(data_70, function(df) {
  df[c("Location", "Date", "Date.Time")] <- do.call(rbind, lapply(strsplit(basename(df$Begin.Path), split="_"), normalize_length))
  df["Date.Time"] <- paste(df$Date, substr(df$Date.Time, start=1, stop=6))
  df["Date"] <- as.Date(df$Date, "%Y%m%d")
  df["Date.Time"] <- ymd_hms(df$Date.Time, tz="UTC")
  df["Day.Of.Year"] <- yday(df$Date)
  return(df)
})

# Remove file path and replace with only file name
data_70 <- lapply(data_70, function(df) {
  df["Begin.Path"] <- basename(df$Begin.Path)
  return(df)
})

# Rename recorders
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

# Save as fst file to read in the data in the future without needing to process it again. 
write_fst(all_data, "your_directory/70conf_2020_to_2024.fst")

####################
# HELPER FUNCTIONS #
####################

# This function can be used to identify all unique recorder names. This can be helpful if new recorder names are given in future 
# years and they need to be identified so that they can be renamed to one of the common location names (i.e. "Prairie", "Savanna")

# location_tbls <- lapply(data_70, function(df) {
#   loc_tbl <- table(df[["Location"]])
#   return(loc_tbl)
# })
