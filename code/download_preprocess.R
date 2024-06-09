library(dplyr)

# Define the years to download
years <- 2004:2023

# Create directories if they do not exist
if (!dir.exists("data")) {
  dir.create("data")
}
if (!dir.exists("data/zip")) {
  dir.create("data/zip")
}
if (!dir.exists("data/csv")) {
  dir.create("data/csv")
}

# Loop through each year and download the corresponding zip file
for (year in years) {
  # Construct the URL and file path
  url <- paste0("https://gis.penndot.gov/gishub/crashZip/County/Philadelphia/Philadelphia_", year, ".zip")
  destfile <- paste0("data/Philadelphia_", year, ".zip")
  
  # Download the file
  download.file(url, destfile)
  
  # Unzip the file
  unzip(destfile, exdir = "data/csv")
  
  # Move the original zip file to the zip directory
  file.rename(destfile, paste0("data/zip/Philadelphia_", year, ".zip"))
}

cat("Files downloaded and unzipped successfully.")

# List of data set names
data_sets <-
  c("CRASH",
    "COMMVEH",
    "CYCLE",
    "FLAG",
    "PERSON",
    "ROADWAY",
    "TRAILVEH",
    "VEHICLE")

data <- list()

handle_mismatch <- function(df1, df2) {
  for (colname in colnames(df2)) {
    if (colname %in% colnames(df1)) {
      if (class(df1[[colname]]) != class(df2[[colname]])) {
        df1[[colname]] <- as.character(df1[[colname]])
        df2[[colname]] <- as.character(df2[[colname]])
      }
    }
  }
  return(list(df1, df2))
}
for (data_set in data_sets) {
  file_path <-
    Sys.glob(paste0("data/csv/", data_set, "*"))
  
  for (i in seq_along(file_path)) {
    # Load the data set from the CSV file
    new_data <- read.csv(file_path[i], stringsAsFactors = FALSE)
    
    # Combine the data
    if (i == 1) {
      data[[tolower(data_set)]] <- new_data
    } else {
      handled_data <- handle_mismatch(data[[tolower(data_set)]], new_data)
      data[[tolower(data_set)]] <-
        bind_rows(handled_data[[1]], handled_data[[2]])
    }
  }
}

save(data, file = here::here("data/processed/preprocessed.Rdata"))
