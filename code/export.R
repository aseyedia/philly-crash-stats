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

# in the roadway data, some street names are encoded with words (i.e FORTYSIXTH ST) instead of numbers (i.e 46TH ST). They should be numbers.
# we will convert them to numbers
roadway_data$STREET_NAME <- roadway_data$STREET_NAME %>%
  str_replace_all("FIRST", "1ST") %>%
  str_replace_all("SECOND", "2ND") %>%
  str_replace_all("THIRD", "3RD") %>%
  str_replace_all("FOURTH", "4TH") %>%
  str_replace_all("FIFTH", "5TH") %>%
  str_replace_all("SIXTH", "6TH") %>%
  str_replace_all("SEVENTH", "7TH") %>%
  str_replace_all("EIGHTH", "8TH") %>%
  str_replace_all("NINTH", "9TH") %>%
  str_replace_all("TENTH", "10TH") %>%
  str_replace_all("ELEVENTH", "11TH") %>%
  str_replace_all("TWELFTH", "12TH") %>%
  str_replace_all("THIRTEENTH", "13TH") %>%
  str_replace_all("FOURTEENTH", "14TH") %>%
  str_replace_all("FIFTEENTH", "15TH") %>%
  str_replace_all("SIXTEENTH", "16TH") %>%
  str_replace_all("SEVENTEENTH", "17TH") %>%
  str_replace_all("EIGHTEENTH", "18TH") %>%
  str_replace_all("NINETEENTH", "19TH") %>%
  str_replace_all("TWENTIETH", "20TH") %>%
  str_replace_all("TWENTYFIRST", "21ST") %>%
  str_replace_all("TWENTYSECOND", "22ND") %>%
  str_replace_all("TWENTYTHIRD", "23RD") %>%
  str_replace_all("TWENTYFOURTH", "24TH") %>%
  str_replace_all("TWENTYFIFTH", "25TH") %>%
  str_replace_all("TWENTYSIXTH", "26TH") %>%
  str_replace_all("TWENTYSEVENTH", "27TH") %>%
  str_replace_all("TWENTYEIGHTH", "28TH") %>%
  str_replace_all("TWENTYNINTH", "29TH") %>%
  str_replace_all("THIRTIETH", "30TH") %>%
  str_replace_all("THIRTYFIRST", "31ST") %>%
  str_replace_all("THIRTYSECOND", "32ND") %>%
  str_replace_all("THIRTYTHIRD", "33RD") %>%
  str_replace_all("THIRTYFOURTH", "34TH") %>%
  str_replace_all("THIRTYFIFTH", "35TH") %>%
  str_replace_all("THIRTYSIXTH", "36TH") %>%
  str_replace_all("THIRTYSEVENTH", "37TH") %>%
  str_replace_all("THIRTYEIGHTH", "38TH") %>%
  str_replace_all("THIRTYNINTH", "39TH") %>%
  str_replace_all("FORTIETH", "40TH") %>%
  str_replace_all("FORTYFIRST", "41ST") %>%
  str_replace_all("FORTYSECOND", "42ND") %>%
  str_replace_all("FORTYTHIRD", "43RD") %>%
  str_replace_all("FORTYFOURTH", "44TH") %>%
  str_replace_all("FORTYFIFTH", "45TH") %>%
  str_replace_all("FORTYSIXTH", "46TH") %>%
  str_replace_all("FORTYSEVENTH", "47TH") %>%
  str_replace_all("FORTYEIGHTH", "48TH") %>%
  str_replace_all("FORTYNINTH", "49TH") %>%
  str_replace_all("FIFTIETH", "50TH") %>%
  str_replace_all("FIFTYFIRST", "51ST") %>%
  str_replace_all("FIFTYSECOND", "52ND") %>%
  str_replace_all("FIFTYTHIRD", "53RD") %>%
  str_replace_all("FIFTYFOURTH", "54TH") %>%
  str_replace_all("FIFTYFIFTH", "55TH") %>%
  str_replace_all("FIFTYSIXTH", "56TH") %>%
  str_replace_all("FIFTYSEVENTH", "57TH") %>%
  str_replace_all("FIFTYEIGHTH", "58TH") %>%
  str_replace_all("FIFTYNINTH", "59TH") %>%
  str_replace_all("SIXTIETH", "60TH") %>%
  str_replace_all("SIXTYFIRST", "61ST") %>%
  str_replace_all("SIXTYSECOND", "62ND") %>%
  str_replace_all("SIXTYTHIRD", "63RD") %>%
  str_replace_all("SIXTYFOURTH", "64TH") %>%
  str_replace_all("SIXTYFIFTH", "65TH") %>%
  str_replace_all("SIXTYSIXTH", "66TH") %>%
  str_replace_all("SIXTYSEVENTH", "67TH") %>%
  str_replace_all("SIXTYEIGHTH", "68TH") %>%
  str_replace_all("SIXTYNINTH", "69TH")

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
write_csv(philly_crashes, "data/philly_crashes.csv")

philly_crashes$STREET_NAME %>% 
  unique() %>% 
  sort() %>% 
  write(., sep = "\n", file = "street_names.txt")
