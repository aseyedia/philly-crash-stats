library(tidyverse)

load("data/processed/preprocessed.Rdata")

# Extract relevant columns from each table

# CRASH table
crash_data <- data$crash %>%
  select(
    CRN,
    COUNTY,
    CRASH_YEAR,
    FATAL_COUNT,
    MUNICIPALITY,
    DEC_LAT,
    DEC_LONG,
    MAX_SEVERITY_LEVEL,
    CRASH_MONTH,
    DAY_OF_WEEK,
    HOUR_OF_DAY,
    ROAD_CONDITION,
    ILLUMINATION,
    COLLISION_TYPE,
    INTERSECTION_RELATED,
    AUTOMOBILE_COUNT,
    MOTORCYCLE_COUNT,
    BICYCLE_COUNT,
    HEAVY_TRUCK_COUNT,
    BELTED_DEATH_COUNT,
    MCYCLE_DEATH_COUNT,
    BICYCLE_DEATH_COUNT,
    WORK_ZONE_IND,
    INTERSECT_TYPE,
    RELATION_TO_ROAD,
    WEATHER1,
    WEATHER2,
    DRIVER_COUNT_16YR,
    DRIVER_COUNT_17YR,
    DRIVER_COUNT_18YR,
    DRIVER_COUNT_19YR,
    DRIVER_COUNT_20YR,
    DRIVER_COUNT_50_64YR,
    DRIVER_COUNT_65_74YR,
    DRIVER_COUNT_75PLUS
  )

# FLAG table
flag_data <- data$flag %>%
  select(CRN, DRINKING_DRIVER, DRUGGED_DRIVER)

# VEHICLE table
vehicle_data <- data$vehicle %>%
  select(CRN, BODY_TYPE)

# ROADWAY table
roadway_data <- data$roadway %>%
  select(CRN, ROAD_OWNER, STREET_NAME)

# PERSON table
person_data <- data$person %>%
  select(CRN, AGE, PERSON_TYPE)

# Filter for Philadelphia and the most recent year
philly_crashes <- crash_data %>% 
  filter(CRASH_YEAR == 2023)

# Join additional data to the Philadelphia crashes
philly_crashes <- philly_crashes %>%
  left_join(flag_data, by = "CRN") %>%
  left_join(vehicle_data, by = "CRN") %>%
  left_join(roadway_data, by = "CRN") %>%
  left_join(person_data, by = "CRN")

# Summarize fatalities
total_fatalities <- sum(philly_crashes$FATAL_COUNT)

print(paste("Total fatalities in Philadelphia:", total_fatalities))

# You can now use philly_crashes for your analysis and visualization

# write csv philly_crashes
write_csv(philly_crashes, "data/processed/philly_crashes.csv")
