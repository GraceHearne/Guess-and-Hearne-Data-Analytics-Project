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
#============================================================
accident_raw <- read_csv("Raw Data/accident.csv", show_col_types = FALSE)

# Look at the data immediately so we can catch problems early.
glimpse(accident_raw)

#============================================================
# STEP 2. Structure data into tidy format if not already.
# The FARS Accident file is already one row per crash, which
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
# The raw file includes many *_NAME columns that are humanreadable labels for 
#.     coded variables. Those are helpful for
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
    FATALS,
    PERSONS, #no. of people in the car when it crashed
    MAN_COLL, #type of collision (if involves other vehicle or not)
    HOUR, # time of crash
    ARR_HOUR, # when emergency vehicles arrived on scene
  ) |>
  filter(!is.na(ST_CASE))

#============================================================
# STEP 4. Identify the primary key, or define a surrogate key.
#      The FARS manual states that ST_CASE is the unique case number
#      assigned to each crash and is the crash-level merge key.
#      That means ST_CASE should uniquely identify rows in the
#      Accident file and later link to Vehicle and Person files.
#============================================================
# We do not create a surrogate key because ST_CASE is already the
# substantive and official crash identifier.

#============================================================
# STEP 5. Resolve duplicates.
#    The Accident file should have one record per crash. If the
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
#     In FARS many variables are coded categories, not continuous
#      measurements. Treating a code such as WEATHER = 2 as if it
#      means “twice as much weather” would be wrong. So we document
#      the intended use here in comments before modeling.
#
# Notes for this project:
# - ST_CASE = crash identifier.
# - FATALS = number of fatalities in the crash, but this should not
#   replace the person-level fatality indicator when modeling
#   individual fatality risk.
# - MAN_COLL and HARM_EV is coded categorical controls.
#============================================================

#============================================================
# STEP 7. Rename variables as necessary, to be succinct and descriptive.
#============================================================
accident_step7 <- accident_step5 |>
  rename(
    st_case = ST_CASE,
    hour = HOUR,
    fatals_crash = FATALS,
    persons_total = PERSONS,
    manner_collision = MAN_COLL,
    ems_arrival_hour = ARR_HOUR,
  )

#============================================================
# STEP 8. Convert variable formats as necessary.
#     Merge keys and coded categorical variables should have stable,
#  appropriate classes. IDs should stay as integers/character-like
#      identifiers, while categorical crash descriptors are usually
#      better stored as factors for later modeling clarity.
#============================================================
accident_step8 <- accident_step7 |>
  mutate(
    st_case = as.integer(st_case),
    hour = as.integer(hour),
    fatals_crash = as.integer(fatals_crash),
    manner_collision = as.factor(manner_collision)
  )

#============================================================
# STEP 9. Understand patterns of missing values.
#   FARS often uses special codes like 8, 9, 97, 98, 99, 998,
#      or 999 for unknown/not reported. If we leave those untouched,
#      they will contaminate regression inputs.
#     For more details about theses specific code indicators see code book. 
#     For our purposes essentially NA.
#============================================================
accident_step9 <- accident_step8 |>
  mutate(
    hour = ifelse(hour %in% c(88, 99), NA, hour),          #88 = Not Applicable or Not Notified 99 = Unknown 
    ems_arrival_hour  = ifelse(ems_arrival_hour  %in% c(88, 99), NA, ems_arrival_hour),
  )

#============================================================
# STEP 10. Make units and scales consistent.
#  Accident-level timing and roadway variables are already in
# common FARS coding units. The main consistency step we need is
#  to avoid mixing coded categories with numeric measurements.
# Therefore we keep categorical controls as factors.
#============================================================

#============================================================
# STEP 11. Enforce logical conditions on quantitative variables.
#  Crash date/time values should satisfy obvious bounds. Out-of-
#      range values usually indicate coding problems or unknown codes
#      that were missed.
#============================================================
accident_clean <- accident_step9 |>
  mutate(
    hour = ifelse(hour < 0 | hour > 23, NA, hour),
    fatals_crash = ifelse(fatals_crash < 1, NA, fatals_crash)
  )
#============================================================
# STEP 12. Clean string variables if necessary.
# This file contains very few string variables after selection.
#We dropped most label columns and there are no major text fields
#      needed for analysis, so no substantive string cleaning is needed.
#============================================================

#============================================================
# STEP 13. Save clean data to disk before further manipulation.
#============================================================
write_csv(accident_clean, "clean_data/Unmerged/accident_clean.csv", na = "")



