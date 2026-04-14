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
accident <- read_csv("clean_data/Unmerged/accident_clean.csv")
person   <- read_csv("clean_data/Unmerged/person_clean.csv")
vehicle  <- read_csv("clean_data/Unmerged/vehicle_clean.csv")
vpicdecode  <- read_csv("clean_data/Unmerged/vpicdecode_clean.csv")
safercar <- read_csv("clean_data/Unmerged/safercar_clean.csv", show_col_types = FALSE)
car_dimensions <- read_csv("clean_data/Unmerged/AI_car_dimensions.csv", show_col_types = FALSE)


#============================================================
# STEP 3. Inspect datasets before merging
# This confirms the files loaded correctly and that the key
# variables needed for the merges are present.
#============================================================
glimpse(accident)
glimpse(person)
glimpse(vehicle)
glimpse(safercar)
glimpse(car_dimensions)


#============================================================
# STEP 4. Check that each dataset is unique at its proper key
# Note that if a file is not unique at the correct key, a join can
# create duplicate rows and corrupt the final merged dataset.
#
# Merge keys based on the FARS structure:
# Accident: st_case
# Vehicle: st_case + veh_no
# Person:  st_case + veh_no + per_no
# safercar: make + model + model_year
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
vpicdecode_dups <- vpicdecode |>
  count(st_case, veh_no) |>
  filter(n > 1)

print(person_dups)

#-----------------------------
# Check safercar key: make, model, car_year, drive_train
#-----------------------------
safercar_dups <- safercar |>
  count(make, model, car_year, drive_train) |>
  filter(n > 1)

print(safercar_dups)

#-----------------------------
# Check car_dimensions key: make, model, car_year, drive_train
#-----------------------------
car_dimensions_dups <- car_dimensions |>
  count(make, model, car_year, drive_train) |>
  filter(n > 1)

print(car_dimensions_dups)

#============================================================
# STEP 5. Start with PERSON as the base file
# The outcome variable, fatal, is in the person file.
# Starting from person preserves the correct unit of observation:
# one row per person.
#============================================================
fars_merged <- person
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
    by = c("st_case", "veh_no", "make", "model", "car_year")
  )

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
print(nrow(fars_merged))


#============================================================
# STEP 8. Merge vpicdecode onto the person-vehicle-accident data
# merging by = c("st_case", "veh_no")
#safercar is a vehicle-level file, so it must be linked using the
# vehicle identifier.
#
# We use left_join() because some not every vehicle was in a crash in our dataset
#============================================================
fars_merged <- fars_merged |>
  left_join(
    vpicdecode,
    by = c("st_case", "veh_no")
  )
print(nrow(fars_merged))

#============================================================
# STEP 9. Merge safercar onto the person-vehicle-accident data
# merging by = c("make", "model", "model_year", "drive_train")
#safercar is a vehicle-level file, so it must be linked using the
# vehicle identifier.
#
# We use left_join() because some not every vehicle was in a crash in our dataset
#============================================================
fars_and_safer <- fars_merged |>
  left_join(
    safercar,
    by = c("make", "model", "car_year", "drive_train")
  )

#============================================================
# STEP 10. Merge car_dimensions onto the person-vehicle-accident-safercar data
# merging by = c("make", "model", "model_year", "drive_train")
# car_dimensions is a vehicle-level file, so it must be linked using the
# vehicle identifier.
#
# We use left_join() because some not every vehicle was in a crash in our dataset
#============================================================
crash_data <- fars_and_safer |>
  left_join(
    car_dimensions,
    by = c("make", "model", "car_year", "drive_train")
  )

#============================================================
# STEP 11. Check whether the final dataset is still one row per
# person
# The final merged file should remain person-level.
# Each st_case + veh_no + per_no combination should appear once.
# If duplicates appear, then a merge created multiple matches.
#============================================================
merge_dups <- crash_data|>
  count(st_case, veh_no, per_no) |>
  filter(n > 1)

print(merge_dups)


#============================================================
# STEP 12. Compare row counts before and after merging
# Since we started with person as the base file, the final
# number of rows should remain the same if the joins worked
# correctly and did not create duplicates.
#============================================================
print(nrow(person))

print(nrow(crash_data))


#============================================================
# STEP 15. Keep only data that we have our covariates and regressors for
#============================================================
crash_data_usable <- crash_data |>
  filter(!is.na(curb_weight))


#============================================================
# STEP 15. Save ONE merged dataset
#============================================================
write_csv(crash_data_usable, "clean_data/crash_data.csv")