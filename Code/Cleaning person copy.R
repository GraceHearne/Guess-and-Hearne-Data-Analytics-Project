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

#-----------------------------
# Create output folder
#-----------------------------
dir.create("clean_data", showWarnings = FALSE)

#============================================================
# STEP 1. Convert file formats as necessary, and import your data.
# Why: Read the raw CSV exactly as provided and preserve the original.
#============================================================
person_raw <- read_csv("person copy.csv", show_col_types = FALSE)

# Inspect immediately to catch issues early.
glimpse(person_raw)

#============================================================
# STEP 2. Structure data into tidy format if not already.
# Why: The Person file is already tidy because each row is one person
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
# Why: The raw Person file contains many label columns and many fields
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
    STATE,
    VEH_NO,
    PER_NO,
    AGE,
    SEX,
    PER_TYP,
    INJ_SEV,
    SEAT_POS,
    REST_USE,
    REST_MIS,
    AIR_BAG,
    EJECTION,
    EJ_PATH,
    EXTRICAT,
    DRINKING,
    ALC_STATUS,
    ALC_RES,
    DRUGS,
    DSTATUS,
    HOSPITAL,
    DOA,
    DEATH_HR,
    DEATH_MN,
    LAG_HRS,
    LAG_MINS,
    WORK_INJ,
    HISPANIC,
    HELM_USE,
    HELM_MIS,
    MAKE,
    BODY_TYP,
    MOD_YEAR,
    VPICMAKE,
    VPICMODEL,
    VPICBODYCLASS,
    GVWR_FROM,
    GVWR_TO
  ) |>
  filter(!is.na(ST_CASE), !is.na(PER_NO))


#============================================================
# STEP 4. Identify the primary key, or define a surrogate key.
# Why: The FARS manual states that PER_NO is the person number within
#      a crash/vehicle context. For the full Person file, the person-
#      level key is ST_CASE + VEH_NO + PER_NO. Non-motorists can have
#      VEH_NO = 0, which is important to remember when merging.
#============================================================

#============================================================
# STEP 5. Resolve duplicates.
# Why: If a person appears more than once, later merges will duplicate
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
# Why: This is especially important in FARS because injury severity,
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
# Why: Clear names reduce confusion after merging multiple FARS files.
#============================================================
person_step7 <- person_step5 |>
  rename(
    st_case = ST_CASE,
    state = STATE,
    veh_no = VEH_NO,
    per_no = PER_NO,
    age = AGE,
    sex = SEX,
    person_type = PER_TYP,
    inj_sev = INJ_SEV,
    seat_pos = SEAT_POS,
    rest_use = REST_USE,
    rest_mis = REST_MIS,
    air_bag = AIR_BAG,
    ejection = EJECTION,
    ejection_path = EJ_PATH,
    extricated = EXTRICAT,
    drinking = DRINKING,
    alc_status = ALC_STATUS,
    alc_res = ALC_RES,
    drugs = DRUGS,
    drug_status = DSTATUS,
    transported_by = HOSPITAL,
    doa = DOA,
    death_hr = DEATH_HR,
    death_mn = DEATH_MN,
    lag_hrs = LAG_HRS,
    lag_mins = LAG_MINS,
    work_injury = WORK_INJ,
    hispanic = HISPANIC,
    helmet_use = HELM_USE,
    helmet_misuse = HELM_MIS,
    make = MAKE,
    body_typ = BODY_TYP,
    mod_year = MOD_YEAR,
    vpicmake = VPICMAKE,
    vpicmodel = VPICMODEL,
    vpicbodyclass = VPICBODYCLASS,
    gvwr_from = GVWR_FROM,
    gvwr_to = GVWR_TO
  )

#============================================================
# STEP 8. Convert variable formats as necessary.
# Why: IDs should be integer-like. Person and injury descriptors are
#      coded categories and are best handled as factors for analysis.
#============================================================
person_step8 <- person_step7 |>
  mutate(
    st_case = as.integer(st_case),
    state = as.integer(state),
    veh_no = as.integer(veh_no),
    per_no = as.integer(per_no),
    age = as.numeric(age),
    mod_year = as.numeric(mod_year)
  )

#============================================================
# STEP 9. Understand patterns of missing values.
# Why: FARS often uses special codes like 8, 9, 97, 98, 99, 998,
#      or 999 for unknown/not reported. If we leave those untouched,
#      they will contaminate regression inputs.
#============================================================
person_step9 <- person_step8 |>
  mutate(
    age = ifelse(age %in% c(998, 999), NA, age),
    sex = ifelse(sex %in% c(8, 9), NA, sex),
    person_type = ifelse(person_type %in% c(99), NA, person_type),
    seat_pos = ifelse(seat_pos %in% c(98, 99), NA, seat_pos),
    rest_use = ifelse(rest_use %in% c(8, 9, 98, 99), NA, rest_use),
    rest_mis = ifelse(rest_mis %in% c(8, 9), NA, rest_mis),
    air_bag = ifelse(air_bag %in% c(8, 9), NA, air_bag),
    ejection = ifelse(ejection %in% c(8, 9), NA, ejection),
    ejection_path = ifelse(ejection_path %in% c(8, 9), NA, ejection_path),
    extricated = ifelse(extricated %in% c(8, 9), NA, extricated),
    drinking = ifelse(drinking %in% c(8, 9), NA, drinking),
    alc_status = ifelse(alc_status %in% c(8, 9), NA, alc_status),
    alc_res = ifelse(alc_res %in% c(998, 999), NA, alc_res),
    drugs = ifelse(drugs %in% c(8, 9), NA, drugs),
    drug_status = ifelse(drug_status %in% c(8, 9), NA, drug_status),
    transported_by = ifelse(transported_by %in% c(8, 9), NA, transported_by),
    doa = ifelse(doa %in% c(8, 9), NA, doa),
    death_hr = ifelse(death_hr %in% c(88, 99), NA, death_hr),
    death_mn = ifelse(death_mn %in% c(88, 99), NA, death_mn),
    lag_hrs = ifelse(lag_hrs %in% c(998, 999), NA, lag_hrs),
    lag_mins = ifelse(lag_mins %in% c(998, 999), NA, lag_mins),
    work_injury = ifelse(work_injury %in% c(8, 9), NA, work_injury),
    hispanic = ifelse(hispanic %in% c(8, 9), NA, hispanic),
    helmet_use = ifelse(helmet_use %in% c(8, 9), NA, helmet_use),
    helmet_misuse = ifelse(helmet_misuse %in% c(8, 9), NA, helmet_misuse),
    mod_year = ifelse(mod_year %in% c(9998, 9999), NA, mod_year),
    vpicmake = ifelse(vpicmake %in% c(99998, 99999), NA, vpicmake),
    vpicmodel = ifelse(vpicmodel %in% c(99998, 99999), NA, vpicmodel),
    vpicbodyclass = ifelse(vpicbodyclass %in% c(998, 999), NA, vpicbodyclass),
    gvwr_from = ifelse(gvwr_from %in% c(98, 99), NA, gvwr_from),
    gvwr_to = ifelse(gvwr_to %in% c(98, 99), NA, gvwr_to)
  )

#============================================================
# STEP 10. Make units and scales consistent.
# Why: The most important consistency step here is to create a clear,
#      analysis-ready fatality indicator from the coded injury severity
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
# Why: Age cannot be negative or implausibly high. Vehicle year cannot
#      exceed the crash-year context by much in a standard FARS file.
#      These checks prevent impossible values from entering analysis.
#============================================================
person_step11 <- person_step10 |>
  mutate(
    age = ifelse(age < 0 | age > 120, NA, age),
    mod_year = ifelse(mod_year < 1900 | mod_year > 2030, NA, mod_year),
    death_hr = ifelse(death_hr < 0 | death_hr > 23, NA, death_hr),
    death_mn = ifelse(death_mn < 0 | death_mn > 59, NA, death_mn)
  )

#============================================================
# STEP 12. Clean string variables if necessary.
# Why: This reduced person file contains mostly coded numeric fields.
#      No major free-text cleaning is required here.
#============================================================
person_clean <- person_step11 |>
  mutate(
    across(
      c(sex, person_type, seat_pos, rest_use, rest_mis, air_bag,
        ejection, ejection_path, extricated, drinking, alc_status,
        drugs, drug_status, transported_by, doa, work_injury,
        hispanic, helmet_use, helmet_misuse, body_typ,
        vpicbodyclass, gvwr_from, gvwr_to),
      as.factor
    )
  )

#============================================================
# STEP 13. Save your clean data to disk before further manipulation.
# Why: Save the clean person-level file before merging with vehicle or
#      accident data so that the cleaning phase is separate from the
#      analysis/merge phase.
#============================================================
write_csv(person_clean, "clean_data/person_clean.csv", na = "")

#-----------------------------
# Final look
#-----------------------------
glimpse(person_clean)
summary(person_clean)
