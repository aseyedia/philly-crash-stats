library(sf)
library(ggmap)
library(rvest)
# https://crashinfo.penndot.gov/PCIT/welcome.html
library(data.table)
setwd("/home/arta/Documents/GitHub/Philly-Crash-Stats/")
library(here)
library(dplyr)
library(ggplot2)
library(gganimate)

register_google("AIzaSyAc-8C3pgIyfiouzhX1K1iklFUABLn4aC4")

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

cycle_crash_data <-
  cycle_crash_data[cycle_crash_data$PC_HLMT_IND != "", ]

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
url <-
  "https://www.revenue.pa.gov/TaxTypes/InheritanceTax/Pages/County%20Codes.aspx"

webpage <- read_html(url)

counties <- html_nodes(webpage, "table") %>% html_table()

counties[[1]][3] <- NULL

counties <- counties[[1]]

counties2 <- counties[, 3:4]
counties <- counties[, 1:2]

counties <- rbind(counties, counties2)
counties <- counties[-68, ]

# for (i in seq_along(data)) {
#   if (i == 1){
#     merged_data <- data[[i]]
#   } else{
#     merged_data <- merge(data[[i]], merged_data)
#   }
# }

# Merge the two data frames by the common column
merged_data <-
  merge(data[["crash"]],
        counties,
        by.x = "COUNTY",
        by.y = "County#",
        all.x = TRUE)

# Replace COUNTY column with County
merged_data$COUNTY <- merged_data$County
merged_data$County <- NULL

# wtf. all the county codes are 67, for York. They muffed it.

# Get the map of philly from Google Maps
philly_map <-
  get_map(
    location = 'Philadelphia',
    zoom = 11,
    maptype = 'roadmap',
    source = 'google'
  )

# Create a ggplot object with the map as the background
philly_ggmap <- ggmap(philly_map)

# Assuming that your data frame is named data, and it has the columns lon for longitude and lat for latitude
# Add the points from your dataset
# p <- p + geom_point(data = data[data_subset["crash"]], aes(x = DEC_LONG, y = DEC_LAT), color = "orange", alpha = 0.5)

### Philly Total Cyclist Crashes ####
data_subset <- data[["crash"]][data[["crash"]]$BICYCLE_COUNT > 0,]

data_subset$BICYCLE_DEATH_COUNT <-
  as.factor(data_subset$BICYCLE_DEATH_COUNT)

data_subset <- merge(data_subset, data[['cycle']], by = "CRN")

data_subset <- data_subset[order(data_subset$BICYCLE_DEATH_COUNT),]

# Add the points from your dataset
p <-
  philly_ggmap + geom_point(
    data = data_subset,
    aes(x = DEC_LONG, y = DEC_LAT, color = BICYCLE_DEATH_COUNT),
    alpha = .5
  )

# Set the color scale to differentiate between helmeted and non-helmeted fatalities
philly_cycle_crashes <- p + scale_color_manual(values = c("0" = "gray", "1" = "red"))

ggsave(
  philly_cycle_crashes + labs(title = "Philadelphia Bicyclist Crashes Colored by Whether They Resulted in a Death, 2002-2021", color = "Were they wearing a helmet?"),
  filename = "processed/total_bicycle_crashes.png"
)

# Animate the plot over time
p <- philly_cycle_crashes + transition_time(CRASH_YEAR) +
  labs(title = "Philadelphia Bicyclist Crashes Colored by Whether They Resulted in a Death, 2002-2021\nYear: {frame_time}", color = "How many cyclists died??")

# Display the plot
p <- animate(
  p,
  fps = 1,
  duration = 19,
  height = 800,
  width = 800
)

gganimate::anim_save("processed/total_bicycle_crashes.gif", p)

### Philly Helmet Deaths ####
# Display the plot
data_subset <-
  data[["crash"]][data[["crash"]]$BICYCLE_DEATH_COUNT > 0,]

data_subset <- merge(data_subset, data[['cycle']], by = "CRN")

data_subset[data_subset$PC_HLMT_IND == "", ]$PC_HLMT_IND <- "U"

# Add the points from your dataset
p <-
  philly_ggmap + geom_point(data = data_subset,
                            aes(x = DEC_LONG, y = DEC_LAT, color = PC_HLMT_IND),
                            alpha = 0.5)

# Set the color scale to differentiate between helmeted and non-helmeted fatalities
p <-
  p + scale_color_manual(values = c(
    "N" = "red",
    "U" = "blue",
    "Y" = "green"
  ))

ggsave(
  p + labs(title = "Philadelphia Bicyclist Deaths By Whether They Were Wearing a Helmet, 2002-2021", color = "Were they wearing a helmet?"),
  filename = "processed/total_bicycle_deaths.png"
)

# Animate the plot over time
p <- p + transition_time(CRASH_YEAR) +
  labs(title = "Philadelphia Bicyclist Deaths By Whether They Were Wearing a Helmet, 2002-2021\nYear: {frame_time}", color = "Were they wearing a helmet?")

# Display the plot
p <- animate(
  p,
  fps = 1,
  duration = 19,
  height = 800,
  width = 800
)

gganimate::anim_save("processed/deaths_by_helmet.gif", p)

### Philly Serious Injuries ####
data_subset <-
  data[["crash"]][data[["crash"]]$BICYCLE_SUSP_SERIOUS_INJ_COUNT > 0,]

data_subset <- merge(data_subset, data[['cycle']], by = "CRN")

data_subset[data_subset$PC_HLMT_IND == "", ]$PC_HLMT_IND <- "U"

# Add the points from your dataset
p <-
  philly_ggmap + geom_point(data = data_subset,
                            aes(x = DEC_LONG, y = DEC_LAT, color = PC_HLMT_IND),
                            alpha = 0.5)

# Set the color scale to differentiate between helmeted and non-helmeted fatalities
p <-
  p + scale_color_manual(values = c(
    "N" = "red",
    "U" = "blue",
    "Y" = "green"
  ))

ggsave(
  p + labs(title = "Philadelphia Bicyclist Serious Injuries By Whether They Were Wearing a Helmet, 2002-2021", color = "Were they wearing a helmet?"),
  filename = "processed/total_bicycle_serious_inj.png"
)

# Animate the plot over time
p <- p + transition_time(CRASH_YEAR) +
  labs(title = "Philadelphia Bicyclist Serious Injuries By Whether They Were Wearing a Helmet, 2002-2021\nYear: {frame_time}", color = "Were they wearing a helmet?")

# Display the plot
p <- animate(
  p,
  fps = 1,
  duration = 19,
  height = 800,
  width = 800
)

gganimate::anim_save("processed/srs_inj_by_helmet.gif", p)

### Philly Cyclist Deaths by Passenger ####
data_subset <-
  data[["crash"]][data[["crash"]]$BICYCLE_DEATH_COUNT > 0,]

data_subset <- merge(data_subset, data[['cycle']], by = "CRN")

data_subset[data_subset$PC_HLMT_IND == "", ]$PC_HLMT_IND <- "U"

# Add the points from your dataset
p <-
  philly_ggmap + geom_point(data = data_subset,
                            aes(x = DEC_LONG, y = DEC_LAT, color = PC_HLMT_IND),
                            alpha = 0.5)

# Set the color scale to differentiate between helmeted and non-helmeted fatalities
p <-
  p + scale_color_manual(values = c(
    "N" = "red",
    "U" = "blue",
    "Y" = "green"
  ))

ggsave(
  p + labs(title = "Philadelphia Bicyclist Serious Injuries By Whether They Were Wearing a Helmet, 2002-2021", color = "Were they wearing a helmet?"),
  filename = "processed/total_bicycle_serious_inj.png"
)

# Animate the plot over time
p <- p + transition_time(CRASH_YEAR) +
  labs(title = "Philadelphia Bicyclist Serious Injuries By Whether They Were Wearing a Helmet, 2002-2021\nYear: {frame_time}", color = "Were they wearing a helmet?")

# Display the plot
p <- animate(
  p,
  fps = 1,
  duration = 19,
  height = 800,
  width = 800
)

gganimate::anim_save("processed/srs_inj_by_helmet.gif", p)

### Which Road is Most Dangerous for Cyclists? ####
data_subset <- data[["crash"]][data[["crash"]]$BICYCLE_COUNT > 0,]

data_subset <- merge(data_subset, data[['roadway']], by = "CRN")

# Count accidents and fatalities by street name
street_summary <- data_subset %>%
  group_by(STREET_NAME) %>%
  summarise(
    AccidentCount = n(),
    FatalityCount = sum(BICYCLE_DEATH_COUNT),
    .groups = "drop"
  )

# Order the data by accident count and fatality count for the respective plots
street_summary_accidents <- street_summary %>%
  arrange(desc(AccidentCount))

street_summary_fatalities <- street_summary %>%
  arrange(desc(FatalityCount))

# Create the plot for accident count
p1 <- ggplot(street_summary_accidents[1:10, ], aes(x = reorder(STREET_NAME, AccidentCount), y = AccidentCount)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Street Name", y = "Number of Accidents", title = "Top 10 Streets by Number of Accidents")

# Create the plot for fatality count
p2 <- ggplot(street_summary_fatalities[1:10, ], aes(x = reorder(STREET_NAME, FatalityCount), y = FatalityCount)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Street Name", y = "Number of Fatalities", title = "Top 10 Streets by Number of Fatalities")

# Combine the plots side by side
grid.arrange(philly_cycle_crashes, p1, p2, ncol = 3)


