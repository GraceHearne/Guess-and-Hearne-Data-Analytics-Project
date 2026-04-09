# ============================================================
# CLEAN ACCIDENT-LEVEL FARS DATA
# Research question: How does car size affect fatality risk in crashes?
# Dataset role: This file gives crash-level conditions that can later be
# used as controls when you merge to the vehicle- and person-level files. 
# ============================================================
#-----------------------------
# Load packages
#-----------------------------
library(tidyverse)

#-----------------------------
# Create output folder
#-----------------------------
dir.create("clean_data", showWarnings = FALSE)

#============================================================
# STEP 1. Convert file formats as necessary, and import data.
# Why: We start by reading the raw CSV exactly as provided, without
#      altering the original file. This follows good cleaning
#      practice because the raw external source should remain
#      untouched and reproducible.
#============================================================
accident_raw <- read_csv("accident copy.csv", show_col_types = FALSE)

# Look at the data immediately so we can catch problems early.
glimpse(accident_raw)

#============================================================
# STEP 2. Structure data into tidy format if not already.
# Why: The FARS Accident file is already one row per crash, which
#      is the correct tidy structure for crash-level data. So here
#      we do not reshape; we simply verify the structure.
#============================================================
accident_raw |>
  summarise(
    n_rows = n(),
    n_unique_cases = n_distinct(ST_CASE)
  ) |>
  print()

#============================================================
# STEP 3. Remove irrelevant, garbage, or empty columns and rows.
# Why: The raw file includes many *_NAME columns that are human-
#      readable labels for coded variables. Those are helpful for
#      manual inspection, but for regression work and merging they
#      are redundant. Keeping both the code and the label version
#      of each variable makes the data wider, harder to inspect,
#      and easier to accidentally merge or model incorrectly.
#
#      We also keep only crash-level variables that are relevant to
#      the research question as controls or merge keys.
#============================================================
accident_step3 <- accident_raw |>
  select(
    ST_CASE,
    STATE,
    YEAR,
    MONTH,
    DAY,
    DAY_WEEK,
    HOUR,
    MINUTE,
    COUNTY,
    CITY,
    FATALS,
    VE_TOTAL,
    VE_FORMS,
    PERSONS,
    ROUTE,
    RUR_URB,
    FUNC_SYS,
    RD_OWNER,
    NHS,
    SP_JUR,
    MILEPT,
    LATITUDE,
    LONGITUD,
    HARM_EV,
    MAN_COLL,
    RELJCT1,
    RELJCT2,
    TYP_INT,
    REL_ROAD,
    WRK_ZONE,
    LGT_COND,
    WEATHER,
    SCH_BUS,
    NOT_HOUR,
    NOT_MIN,
    ARR_HOUR,
    ARR_MIN,
    HOSP_HR,
    HOSP_MN
  ) |>
  filter(!is.na(ST_CASE))

#============================================================
# STEP 4. Identify the primary key, or define a surrogate key.
# Why: The FARS manual states that ST_CASE is the unique case number
#      assigned to each crash and is the crash-level merge key.
#      That means ST_CASE should uniquely identify rows in the
#      Accident file and later link to Vehicle and Person files.
#============================================================
# We do not create a surrogate key because ST_CASE is already the
# substantive and official crash identifier.

#============================================================
# STEP 5. Resolve duplicates.
# Why: The Accident file should have one record per crash. If the
#      same ST_CASE appears more than once, that would create false
#      duplication when we merge later and would over-weight some
#      crashes in analysis.
#============================================================
dup_cases <- accident_step3 |>
  count(ST_CASE) |>
  filter(n > 1)

print(dup_cases)

accident_step5 <- accident_step3 |>
  distinct(ST_CASE, .keep_all = TRUE)

#============================================================
# STEP 6. Understand the definition, origin, and units of each
#         variable, and document as necessary.
# Why: In FARS many variables are coded categories, not continuous
#      measurements. Treating a code such as WEATHER = 2 as if it
#      means “twice as much weather” would be wrong. So we document
#      the intended use here in comments before modeling.
#
# Notes for this project:
# - ST_CASE = crash identifier.
# - FATALS = number of fatalities in the crash, but this should not
#   replace the person-level fatality indicator when modeling
#   individual fatality risk.
# - WEATHER, LGT_COND, MAN_COLL, etc. are coded categorical controls.
# - LATITUDE/LONGITUD are geographic coordinates and may be useful
#   for descriptive checks, but not necessary in every regression.
#============================================================

#============================================================
# STEP 7. Rename variables as necessary, to be succinct and descriptive.
# Why: Cleaner names reduce coding mistakes later and make the merged
#      dataset easier to read. We keep names short but meaningful.
#============================================================
accident_step7 <- accident_step5 |>
  rename(
    state = STATE,
    st_case = ST_CASE,
    year = YEAR,
    month = MONTH,
    day = DAY,
    day_week = DAY_WEEK,
    hour = HOUR,
    minute = MINUTE,
    county = COUNTY,
    city = CITY,
    fatals_crash = FATALS,
    vehicles_total = VE_TOTAL,
    vehicles_in_transport = VE_FORMS,
    persons_total = PERSONS,
    route = ROUTE,
    rural_urban = RUR_URB,
    functional_system = FUNC_SYS,
    road_owner = RD_OWNER,
    nhs = NHS,
    special_jurisdiction = SP_JUR,
    milepoint = MILEPT,
    latitude = LATITUDE,
    longitude = LONGITUD,
    first_harmful_event = HARM_EV,
    manner_collision = MAN_COLL,
    rel_jct_interchange = RELJCT1,
    rel_jct_location = RELJCT2,
    intersection_type = TYP_INT,
    relation_to_road = REL_ROAD,
    work_zone = WRK_ZONE,
    light_condition = LGT_COND,
    weather = WEATHER,
    school_bus_related = SCH_BUS,
    ems_notified_hour = NOT_HOUR,
    ems_notified_min = NOT_MIN,
    ems_arrival_hour = ARR_HOUR,
    ems_arrival_min = ARR_MIN,
    hospital_hour = HOSP_HR,
    hospital_min = HOSP_MN
  )

#============================================================
# STEP 8. Convert variable formats as necessary.
# Why: Merge keys and coded categorical variables should have stable,
#      appropriate classes. IDs should stay as integers/character-like
#      identifiers, while categorical crash descriptors are usually
#      better stored as factors for later modeling clarity.
#============================================================
accident_step8 <- accident_step7 |>
  mutate(
    st_case = as.integer(st_case),
    state = as.integer(state),
    year = as.integer(year),
    month = as.integer(month),
    day = as.integer(day),
    hour = as.integer(hour),
    minute = as.integer(minute),
    fatals_crash = as.integer(fatals_crash),
    across(
      c(day_week, route, rural_urban, functional_system, road_owner,
        nhs, special_jurisdiction, first_harmful_event,
        manner_collision, rel_jct_interchange, rel_jct_location,
        intersection_type, relation_to_road, work_zone,
        light_condition, weather, school_bus_related),
      as.factor
    )
  )

#============================================================
# STEP 9. Understand patterns of missing values.
# Why: In FARS, missingness is often coded with special numeric values
#      such as 8, 9, 98, 99, 888..., or 999.... If we leave those in
#      place, the software will treat them as real values rather than
#      unknowns, which would bias summaries and regressions.
#============================================================
accident_step9 <- accident_step8 |>
  mutate(
    county = ifelse(county %in% c(998, 999), NA, county),  #998 = Not Reported 999 = Unknown 0 = Not Applicable
    city = ifelse(city %in% c(9898, 9999), NA, city),      #9898 = Not Reported 9999 = Unknown 0 = Not Applicable
    month = ifelse(month == 99, NA, month),                #99 = Unknown
    day = ifelse(day == 99, NA, day),                      #99 = Unknown
    hour = ifelse(hour %in% c(88, 99), NA, hour),          #88 = Not Applicable or Not Notified 99 = Unknown 
    minute = ifelse(minute %in% c(88, 99), NA, minute),    #88 = Not Applicable or Not Notified 99 = Unknown 
    milepoint = ifelse(milepoint %in% c(99998, 99999), NA, milepoint), #99998 = Not Reported 99999 = Unknown
    latitude = ifelse(latitude %in% c(77.7777000, 88.8888000, 99.9999000), NA, latitude), #77.7777000 = Not Reported 88.8888000 = Not Available (if State Exempt) 99.9999000 = Reported as Unknown 
    longitude = ifelse(longitude %in% c(777.7777000, 888.8888000, 999.9999000), NA, longitude), #777.7777000 = Not Reported 888.8888000 = Not Available (if State Exempt) 999.9999000 = Reported as Unknown 
    ems_notified_hour = ifelse(ems_notified_hour %in% c(88, 99), NA, ems_notified_hour), #88 = Not Applicable or Not Notified 99 = Unknown Hour or Unknown if Notified (when NOT_MIN = 98)
    ems_notified_min  = ifelse(ems_notified_min  %in% c(88, 98, 99), NA, ems_notified_min), #88 = Not Applicable or Not Notified 98 = Unknown if Notified 99 = Unknown Minutes 
    ems_arrival_hour  = ifelse(ems_arrival_hour  %in% c(88, 99), NA, ems_arrival_hour),
    ems_arrival_min   = ifelse(ems_arrival_min   %in% c(88, 97, 98, 99), NA, ems_arrival_min),
    hospital_hour     = ifelse(hospital_hour     %in% c(88, 99), NA, hospital_hour),
    hospital_min      = ifelse(hospital_min      %in% c(88, 96, 97, 98, 99), NA, hospital_min)
  )

#============================================================
# STEP 10. Make units and scales consistent.
# Why: Accident-level timing and roadway variables are already in
#      common FARS coding units. The main consistency step we need is
#      to avoid mixing coded categories with numeric measurements.
#      Therefore we keep categorical controls as factors and preserve
#      latitude/longitude as numeric coordinates.
#============================================================

#============================================================
# STEP 11. Enforce logical conditions on quantitative variables.
# Why: Crash date/time values should satisfy obvious bounds. Out-of-
#      range values usually indicate coding problems or unknown codes
#      that were missed.
#============================================================
accident_clean <- accident_step9 |>
  mutate(
    month = ifelse(month < 1 | month > 12, NA, month),
    day = ifelse(day < 1 | day > 31, NA, day),
    hour = ifelse(hour < 0 | hour > 23, NA, hour),
    minute = ifelse(minute < 0 | minute > 59, NA, minute),
    fatals_crash = ifelse(fatals_crash < 1, NA, fatals_crash)
  )
#============================================================
# STEP 12. Clean string variables if necessary.
# Why: This file contains very few string variables after selection.
#      We dropped most label columns and there are no major text fields
#      needed for analysis, so no substantive string cleaning is needed.
#============================================================

#============================================================
# STEP 13. Save your clean data to disk before further manipulation.
# Why: The cleaning checklist recommends saving a clean version before
#      merging or transforming further. That way the "cleaning" phase
#      is distinct from the "analysis" phase, and you can always go
#      back to this version if a later merge introduces problems.
#============================================================
write_csv(accident_clean, "clean_data/accident_clean.csv", na = "")



