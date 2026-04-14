#============================================================
# MERGE CLEANED FARS DATASETS INTO ONE PERSON-LEVEL FILE
# Research question: How does car size affect fatality risk in crashes?
#
# Final unit of observation:
# PERSON-LEVEL
#
# Why person-level?
# Fatality is measured for each person, so the final merged data
# should keep one row per person.
#============================================================


#============================================================
# STEP 1. Load package
#============================================================
library(tidyverse)

#============================================================
# STEP 2. Load the cleaned datasets
#============================================================
accident <- read_csv("clean_data/accident_clean.csv")
person   <- read_csv("clean_data/person_clean.csv")
vehicle  <- read_csv("clean_data/vehicle_clean.csv")
vpic     <- read_csv("clean_data/vpicdecode_clean.csv")


#============================================================
# STEP 3. Inspect datasets before merging
# This confirms the files loaded correctly and that the key
# variables needed for the merges are present.
#============================================================
glimpse(accident)
glimpse(person)
glimpse(vehicle)
glimpse(vpic)


#============================================================
# STEP 4. Check that each dataset is unique at its proper key
# Note that if a file is not unique at the correct key, a join can
# create duplicate rows and corrupt the final merged dataset.
#
# Merge keys based on the FARS structure:
# Accident: st_case
# Vehicle: st_case + veh_no
# Person:  st_case + veh_no + per_no
# vPIC:    st_case + veh_no
#============================================================

#-----------------------------
# Check Accident key: st_case
#-----------------------------
accident_dups <- accident |>
  count(st_case) |>
  filter(n > 1)

print(accident_dups)

#-----------------------------
# Check Vehicle key: st_case + veh_no
#-----------------------------
vehicle_dups <- vehicle |>
  count(st_case, veh_no) |>
  filter(n > 1)

print(vehicle_dups)

#-----------------------------
# Check Person key: st_case + veh_no + per_no
#-----------------------------
person_dups <- person |>
  count(st_case, veh_no, per_no) |>
  filter(n > 1)

print(person_dups)

#-----------------------------
# Check vPIC key: st_case + veh_no
#-----------------------------
vpic_dups <- vpic |>
  count(st_case, veh_no) |>
  filter(n > 1)

print(vpic_dups)


#============================================================
# STEP 5. Start with PERSON as the base file
# The outcome variable, fatal, is in the person file.
# Starting from person preserves the correct unit of observation:
# one row per person.
#============================================================
fars_merged <- person

cat("Starting rows in PERSON file:\n")
print(nrow(fars_merged))


#============================================================
# STEP 6. Merge VEHICLE onto PERSON
# merging by = c("st_case", "veh_no")
#
# st_case identifies the crash
# veh_no identifies the vehicle within that crash
#
# Vehicle contributes vehicle-level information such as:
# - model
# - travel speed
# - vehicle age
# - speed limit
# - body type
# - roadway and driver-related characteristics
#
# use left_join() b/c
# We want to keep every person from the person file, even if some
# vehicle information is missing.
#============================================================
fars_merged <- fars_merged |>
  left_join(
    vehicle,
    by = c("st_case", "veh_no"),
    suffix = c("_person", "_vehicle")
  )

cat("Rows after merging VEHICLE onto PERSON by st_case + veh_no:\n")
print(nrow(fars_merged))


#============================================================
# STEP 7. Merge ACCIDENT onto the person-vehicle data
# merging by = "st_case"
#
# Accident is a crash-level file, so one accident row applies to
# everyone in that crash.
#
# Accident contributes crash-level conditions such as:
# - weather
# - light condition
# - time of crash
# - roadway environment
#
# We again use left_join() so we do not lose any people.
#============================================================
fars_merged <- fars_merged |>
  left_join(
    accident,
    by = "st_case"
  )

cat("Rows after merging ACCIDENT onto person-vehicle data by st_case:\n")
print(nrow(fars_merged))


#============================================================
# STEP 8. Merge VPIC onto the person-vehicle-accident data
# merging by = c("st_case", "veh_no")
#vPIC is a vehicle-level file, so it must be linked using the
# crash identifier and vehicle identifier together.
# vPIC contributes the best technical vehicle-size measures for
# your research question, such as:
# - body_class
# - curb_weight_lb
# - track_width_in
# - wheelbase measures
#
# We use left_join() because some VINs may not decode cleanly,
# but we still want to keep those people in the merged dataset.
#============================================================
fars_merged <- fars_merged |>
  left_join(
    vpic,
    by = c("st_case", "veh_no"),
    suffix = c("", "_vpic")
  )

cat("Rows after merging VPIC onto person-vehicle-accident data by st_case + veh_no:\n")
print(nrow(fars_merged))


#============================================================
# STEP 9. Check whether the final dataset is still one row per
# person
# The final merged file should remain person-level.
# Each st_case + veh_no + per_no combination should appear once.
# If duplicates appear, then a merge created multiple matches.
#============================================================
merge_dups <- fars_merged |>
  count(st_case, veh_no, per_no) |>
  filter(n > 1)

print(merge_dups)


#============================================================
# STEP 10. Compare row counts before and after merging
# Since we started with person as the base file, the final
# number of rows should remain the same if the joins worked
# correctly and did not create duplicates.
#============================================================
cat("Original rows in PERSON:\n")
print(nrow(person))

cat("Final rows in merged dataset:\n")
print(nrow(fars_merged))


#============================================================
# STEP 11. Check that important variables actually merged in
# A join can run without an error but still fail in practice
# if keys do not match well. These summaries help confirm that
# variables from each file are present in the final dataset.
#============================================================

#-----------------------------
# Person variables
#-----------------------------
summary(fars_merged$fatal)
summary(fars_merged$age)
summary(fars_merged$person_type)

#-----------------------------
# Vehicle variables
# Note: overlapping names from person and vehicle receive suffixes
# such as _person and _vehicle after the merge.
#-----------------------------
summary(fars_merged$trav_sp)
summary(fars_merged$vehicle_age)
summary(fars_merged$body_typ_vehicle)
summary(fars_merged$speed_limit)

#-----------------------------
# Accident variables
#-----------------------------
summary(fars_merged$weather)
summary(fars_merged$light_condition)
summary(fars_merged$rural_urban)

#-----------------------------
# vPIC variables
#-----------------------------
summary(fars_merged$body_class)
summary(fars_merged$curb_weight_lb)
summary(fars_merged$track_width_in)
summary(fars_merged$wheelbase_in_from)
summary(fars_merged$wheelbase_in_to)
summary(fars_merged$wheelbase_in_mid)


#============================================================
# STEP 12. Create a merge-quality summary
#main explanatory variables are vehicle-size measures,
# so it is useful to document how much of the final merged data
# has non-missing size information.
#============================================================
merge_summary <- fars_merged |>
  summarize(
    total_rows = n(),
    unique_crashes = n_distinct(st_case),
    unique_vehicles = n_distinct(paste(st_case, veh_no)),
    unique_persons = n_distinct(paste(st_case, veh_no, per_no)),
    pct_missing_trav_sp = mean(is.na(trav_sp)) * 100,
    pct_missing_vehicle_age = mean(is.na(vehicle_age)) * 100,
    pct_missing_body_class = mean(is.na(body_class)) * 100,
    pct_missing_curb_weight = mean(is.na(curb_weight_lb)) * 100,
    pct_missing_track_width = mean(is.na(track_width_in)) * 100,
    pct_missing_wheelbase_mid = mean(is.na(wheelbase_in_mid)) * 100
  )

print(merge_summary)


#============================================================
# STEP 13. Save ONE merged dataset
#============================================================
write_csv(fars_merged, "fars_merged_person_level.csv")