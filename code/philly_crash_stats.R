library(rvest)
# https://crashinfo.penndot.gov/PCIT/welcome.html
library(data.table)
setwd("/home/arta/Documents/GitHub/Philly-Crash-Stats/")
library(here)
library(dplyr)
library(ggplot2)

# Initialize the list
data <- list()

# List of years
years <- 2002:2021

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

# Initialize an empty list to hold the data
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
    Sys.glob(paste0("data/zip/*/", data_set, "*"))
  
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

# EDA ####
## GENERAL ####

# Convert your table into a data frame
crash_year_table <- table(data$crash$CRASH_YEAR)
crash_year_df <-
  data.frame(Year = as.integer(names(crash_year_table)),
             Count = as.integer(crash_year_table))

# Create the histogram with a trend line
ggplot(crash_year_df, aes(x = Year, y = Count)) +
  geom_col(fill = "steelblue") +
  geom_smooth(method = "lm",
              se = FALSE,
              color = "red") +
  labs(x = "Crash Year", y = "Count", title = "Number of Crashes by Year") +
  theme_minimal()

# Perform a linear regression for the downtrend
lm_result <- lm(Count ~ Year, data = crash_year_df)
summary(lm_result)

crash_month_table <- table(data$crash$CRASH_MONTH)

crash_month_df <-
  data.frame(Month = as.integer(names(crash_month_table)),
             Count = as.integer(crash_month_table))

ggplot(crash_month_df, aes(x = Month, y = Count)) +
  geom_col(fill = "steelblue") +
  labs(x = "Crash Month", y = "Count", title = "Number of Crashes by Month") +
  theme_minimal()

## BICYCLE ####
sum(data$crash$BICYCLE_DEATH_COUNT)

data$crash %>%
  group_by(CRASH_YEAR) %>%
  summarise(total_deaths = sum(BICYCLE_DEATH_COUNT, na.rm = TRUE)) %>%
  ggplot(aes(x = CRASH_YEAR, y = total_deaths)) +
  geom_line() +
  labs(title = "Total Bicycle Deaths per Year in Pennsylvannia",
       x = "Year",
       y = "Total Deaths")

cycle_crash_data <- merge(data$crash, data$cycle, by = "CRN")

cycle_crash_data <- cycle_crash_data[cycle_crash_data$PC_HLMT_IND != "",]

cycle_crash_data %>% 
  group_by(PC_HLMT_IND) %>%
  summarise(BICYCLE_COUNT = sum(BICYCLE_COUNT, na.rm = TRUE)) %>%
  ggplot(aes(x = PC_HLMT_IND, y = BICYCLE_COUNT)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Bicycle-Involed Crashes by Helmet Usage",
       x = "Helmet Usage",
       y = "Total Crashes")

cycle_crash_data %>%
  group_by(PC_HLMT_IND) %>%
  summarise(total_deaths = sum(BICYCLE_DEATH_COUNT, na.rm = TRUE)) %>%
  ggplot(aes(x = PC_HLMT_IND, y = total_deaths)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Bicycle Deaths by Helmet Usage",
       x = "Helmet Usage",
       y = "Total Deaths")

cycle_crash_data %>%
  group_by(CRASH_YEAR, PC_HLMT_IND) %>%
  summarise(total_deaths = sum(BICYCLE_DEATH_COUNT, na.rm = TRUE)) %>%
  ggplot(aes(x = CRASH_YEAR, y = PC_HLMT_IND, fill = total_deaths)) +
  geom_tile() +
  labs(title = "Heatmap of Bicycle Deaths by Year and Helmet Usage",
       x = "Year",
       y = "Helmet Usage",
       fill = "Total Deaths")

# Getting county codes
url <- "https://www.revenue.pa.gov/TaxTypes/InheritanceTax/Pages/County%20Codes.aspx"

webpage <- read_html(url)

counties <- html_nodes(webpage, "table") %>% html_table()

counties[[1]][3] <- NULL

counties <- counties[[1]]

counties2 <- counties[, 3:4]
counties <- counties[,1:2]

counties <- rbind(counties, counties2)
counties <- counties[-68,]

# for (i in seq_along(data)) {
#   if (i == 1){
#     merged_data <- data[[i]]
#   } else{
#     merged_data <- merge(data[[i]], merged_data)
#   }
# }

# Merge the two data frames by the common column
merged_data <- merge(data[["crash"]], counties, by.x = "COUNTY", by.y = "County#", all.x = TRUE)

# Replace COUNTY column with County
merged_data$COUNTY <- merged_data$County
merged_data$County <- NULL

