# ============================================================
# CLEAN PERSON-LEVEL FARS DATA
# Research question: How does car size affect fatality risk in crashes?
# Dataset role: This file contains the person-level fatality outcome and
# person characteristics that matter for risk, such as age, sex, restraint
# use, air bag deployment, and seating position.
# ============================================================

#-----------------------------
# Load packages
#-----------------------------
library(tidyverse)

#============================================================
# STEP 1. Import data
#============================================================
person_raw <- readRDS("Raw Data/person.rds")

# Inspect immediately to catch issues early.
glimpse(person_raw)

#============================================================
# STEP 2. Structure data into tidy format if not already.
#       The Person file is already tidy because each row is one person
#      involved in a crash. That is the natural unit for modeling
#      fatality risk.
#============================================================
person_raw |>
  summarise(
    n_rows = n(),
    n_unique_persons = n_distinct(ST_CASE, VEH_NO, PER_NO)
  ) |>
  print()

#============================================================
# STEP 3. Remove irrelevant, garbage, or empty columns and rows.
#       The raw Person file contains many label columns and many fields
#      not needed for your main analysis. We keep variables that help
#      define the outcome, describe the occupant, or serve as plausible
#      controls when studying fatality risk.
# Note:
# Variables ending in "_NAME" are descriptive label variables provided
# for readability in the raw FARS data. These duplicate the information
# in the coded variables (e.g., MOD_YEAR vs MOD_YEARNAME) but are stored
# as strings and are not suitable for regression analysis.
#
# To keep the dataset efficient and avoid redundancy, we retain only
# the coded variables and drop all "*_NAME" variables.
#============================================================
person_step3 <- person_raw |>
  select(
    ST_CASE,
    VEH_NO, #part of key
    PER_NO, #part of key
    PER_TYP, #distinguishes drivers, passengers, and non-motorists.
    INJ_SEV, # severity of injury, including fatal
    SEAT_POS, #where person was in vehicle
    REST_USE, #safety features in use
    AIR_BAG, #if deployed
    EJECTION, #if ejected
    DOA,# if died at scene of crash (7 if died at scene, 8 died in route, 9 unknown, 0 na)
    VPICMAKENAME,
    MOD_YEAR,
    VPICMODELNAME, #model of car
    VPICBODYCLASSNAME, #truck or sedan etc
    GVWR_FROM, # vehicle weight, keep for validation later
    GVWR_TO
  ) |>
  filter(!is.na(ST_CASE), !is.na(PER_NO))


#============================================================
# STEP 4. Identify the primary key, or define a surrogate key.
#     The FARS manual states that PER_NO is the person number within
#      a crash/vehicle context. For the full Person file, the person-
#      level key is ST_CASE + VEH_NO + PER_NO. Non-motorists can have
#      VEH_NO = 0, which is important to remember when merging :)
#============================================================

#============================================================
# STEP 5. Resolve duplicates.
#      If a person appears more than once, later merges will duplicate
#      observations and distort fatality rates. So we identify and
#      remove exact duplicates on the proper person-level key.
#============================================================
dup_persons <- person_step3 |>
  count(ST_CASE, VEH_NO, PER_NO) |>
  filter(n > 1)

print(dup_persons)

person_step5 <- person_step3 |>
  distinct(ST_CASE, VEH_NO, PER_NO, .keep_all = TRUE)

#============================================================
# STEP 6. Understand the definition, origin, and units of each
#         variable, and document as necessary.
#      This is especially important in FARS!!!! because injury severity,
#      restraint use, ejection, and alcohol/drug variables are coded
#      categories, not continuous measures.
#
# Key project notes:
# - INJ_SEV is the core source of the fatality outcome.
# - PER_TYP distinguishes drivers, passengers, and non-motorists.
# - REST_USE and AIR_BAG are key occupant protection variables.
# - VEH_NO = 0 identifies non-motorists, who do not have a matching
#   Vehicle file record.
#============================================================

#============================================================
# STEP 7. Rename variables as necessary, to be succinct and descriptive.
#      Clear names reduce confusion after merging multiple FARS files.
#============================================================
person_step7 <- person_step5 |>
  rename(
    st_case = ST_CASE,
    veh_no = VEH_NO,
    per_no = PER_NO,
    person_type = PER_TYP,
    inj_sev = INJ_SEV,
    seat_pos = SEAT_POS,
    rest_use = REST_USE,
    air_bag = AIR_BAG,
    ejection = EJECTION,
    doa = DOA,
    make = VPICMAKENAME,
    car_year = MOD_YEAR,
    model = VPICMODELNAME, #model of car
    body_class = VPICBODYCLASSNAME, #truck or sedan etc
    min_veh_weight = GVWR_FROM, # vehicle weight, keep for validation later
    max_veh_weight = GVWR_TO
  )

#============================================================
# STEP 8. Convert variable formats as necessary.
#  IDs should be integer-like. Person and injury descriptors are
#      coded categories and are best handled as factors for analysis.
#============================================================
person_step8 <- person_step7 |>
  mutate(
    st_case = as.integer(st_case),
    veh_no = as.integer(veh_no),
    per_no = as.integer(per_no),
    car_year = as.numeric(car_year),
    model = toupper(as.character(model)),
    make = toupper(as.character(make)),
    body_class = as.character(body_class)
  )

#============================================================
# STEP 9. Understand patterns of missing values.
#   FARS often uses special codes like 8, 9, 97, 98, 99, 998,
#      or 999 for unknown/not reported. If we leave those untouched,
#      they will contaminate regression inputs.
#     For more details about theses specific code indicators see code book. 
#     For our purposes essentially NA.
#============================================================
person_step9 <- person_step8 |>
  mutate(
    person_type = ifelse(person_type %in% c(99), NA, person_type),
    seat_pos = ifelse(seat_pos %in% c(98, 99), NA, seat_pos),
    rest_use = ifelse(rest_use %in% c(8, 9, 98, 99), NA, rest_use),
    air_bag = ifelse(air_bag %in% c(8, 9), NA, air_bag),
    ejection = ifelse(ejection %in% c(8, 9), NA, ejection),
    doa = ifelse(doa %in% c(8, 9), NA, doa),
    car_year = ifelse(car_year %in% c(9998, 9999), NA, car_year),
    min_veh_weight = ifelse(min_veh_weight %in% c(98, 99), NA, min_veh_weight),
    max_veh_weight = ifelse(max_veh_weight %in% c(98, 99), NA, max_veh_weight)
  )

#============================================================
# STEP 10. Make units and scales consistent.
#   Create a clear, analysis-ready fatality indicator from the coded injury severity
#      variable, rather than using the raw category codes directly.
#============================================================
person_step10 <- person_step9 |>
  mutate(
    fatal = case_when(
      inj_sev == 4 ~ 1,
      inj_sev %in% c(0, 1, 2, 3) ~ 0,
      TRUE ~ NA_real_
    )
  )

#============================================================
# STEP 11. Enforce logical conditions on quantitative variables.
#  Age cannot be negative or implausibly high. Vehicle year cannot
#      exceed the crash-year context by much in a standard FARS file.
#      These checks prevent impossible values from entering analysis.
#============================================================
person_clean <- person_step10 |>
  mutate(
    car_year = ifelse(car_year < 1900 | car_year > 2030, NA, car_year),
  )

#============================================================
# STEP 12. Clean string variables if necessary.
#  This reduced person file contains mostly coded numeric fields.
#      No major free-text cleaning is required here.
#============================================================

#============================================================
# STEP 13. Save clean data to disk before further manipulation.
#============================================================
write_csv(person_clean, "clean_data/Unmerged/person_clean.csv", na = "")

#-----------------------------
# Final look
#-----------------------------
glimpse(person_clean)
summary(person_clean)
