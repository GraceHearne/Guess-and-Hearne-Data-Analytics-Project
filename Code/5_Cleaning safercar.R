#============================================================
# CLEANING SCRIPT FOR SAFERCAR_DATA.CSV
# Research question:How does car size affect fatality risk in crashes?
#
# Purpose of this script:
# This script cleans the SaferCar / NHTSA vehicle-features file so
# it can later be merged to the FARS data using make, model, and
# model year.
#
# Why this cleaning is needed:
# The raw SaferCar file contains many vehicle-feature and crash-test
# variables. Some of these are useful for measuring vehicle size and
# safety ratings, but many are administrative, highly detailed, or
# mostly missing. Cleaning the file first makes merging easier and
# reduces the chance of bringing unnecessary noise into the final
# analysis.
#============================================================


#============================================================
# Load packages
#============================================================
library(tidyverse)
library(dplyr)

#============================================================
# STEP 2. Load the raw SaferCar file
#============================================================
safercar_raw <- read_csv("Raw Data/Safercar_data.csv", show_col_types = FALSE)

#============================================================
# STEP 3. Inspect dataset
#============================================================
glimpse(safercar_raw)
names(safercar_raw)


#============================================================
# STEP 4. Keep only relevant variables
# Why:
# We retain only variables directly related to:
# 1. Vehicle safety ratings (front crash + rollover risk)
# 2. Vehicle identifiers needed for merging (make, model, year)
# 3. Basic vehicle characteristics (drive train)
#
# This reduces noise and ensures the dataset is focused on the
# research question.
#============================================================
safercar_step4 <- safercar_raw |>
  select(
    MAKE,
    MODEL,
    MODEL_YR,
    DRIVE_TRAIN,
    
    OVERALL_FRNT_STARS,
    FRNT_PASS_STARS,
    FRNT_DRIV_STARS,
    ROLLOVER_POSSIBILITY
  )


#============================================================
# STEP 5. Clean text variables for merging
# Differences in capitalization or spacing can break merges.
# We standardize make and model to ensure consistent matching
# with the FARS datasets.
#============================================================
clean_text <- function(x) {
  x |>
    as.character() |>
    toupper() |>
    str_trim()
}

safercar_step5 <- safercar_step4 |>
  mutate(
    MAKE  = clean_text(MAKE),
    MODEL = clean_text(MODEL),
    MODEL_YR  = as.numeric(MODEL_YR)
  )


#============================================================
# STEP 6. Rename variables for clarity and consistency
# Lowercase, consistent naming makes merging and analysis easier.
#============================================================
safercar_step6 <- safercar_step5 |>
  rename(
    make = MAKE,
    model = MODEL,
    car_year = MODEL_YR,
    drive_train = DRIVE_TRAIN,
    
    overall_front_safetyrating = OVERALL_FRNT_STARS,
    front_passenger_safetyrating = FRNT_PASS_STARS,
    front_driver_safetyrating = FRNT_DRIV_STARS,
    rollover_risk = ROLLOVER_POSSIBILITY
  )


#============================================================
# STEP 7. Convert variables to correct formats
# Safety ratings and rollover risk should be numeric so they can
# be used directly in regression analysis.
#============================================================
safercar_step7 <- safercar_step6 |>
  mutate(
    overall_front_safetyrating = as.numeric(overall_front_safetyrating),
    front_passenger_safetyrating = as.numeric(front_passenger_safetyrating),
    front_driver_safetyrating = as.numeric(front_driver_safetyrating),
    rollover_risk = as.numeric(rollover_risk),
    drive_train = trimws(drive_train),
    drive_train = case_when(
      is.na(drive_train) ~ NA_character_,
      drive_train == "2WD" ~ NA_character_,
      drive_train %in% c("2WD/AWD", "AWD", "ADW", "AWD/2WD", "AWD/FWD", "AWD/RWD", "FWD/AWD", "RWD/AWD") ~ "AWD",
      drive_train %in% c("4WD", "4WD/2WD", "4x4", "FWD/4WD", "RWD/4WD") ~ "4WD",
      drive_train == "4x2" ~ "RWD",
      drive_train == "FWD" ~ "FWD",
      drive_train == "RWD" ~ "RWD",
      TRUE ~ NA_character_
    )
  )

safercar_dedup <- safercar_step7 |>
  group_by(make, model, car_year, drive_train) |>
  summarize(
    make = first(make),
    model = first(model),
    car_year = first(car_year),
    drive_train = first(drive_train),
    
    # average safety measures
    overall_front_safetyrating = mean(overall_front_safetyrating, na.rm = TRUE),
    front_passenger_safetyrating = mean(front_passenger_safetyrating, na.rm = TRUE),
    front_driver_safetyrating = mean(front_driver_safetyrating, na.rm = TRUE),
    rollover_risk = mean(rollover_risk, na.rm = TRUE),
    
    .groups = "drop"
  )
#============================================================
# STEP 8. Inspect missingness
# Some safety variables may not be available for all vehicles.
# We examine missingness to understand which variables are usable.
#============================================================
missing_summary <- safercar_dedup |>
  summarize(across(everything(), ~ sum(is.na(.)))) |>
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "n_missing"
  ) |>
  mutate(pct_missing = 100 * n_missing / nrow(safercar_step7)) |>
  arrange(desc(pct_missing))

print(missing_summary)

#============================================================
# STEP 9. Save cleaned dataset
# This creates a clean, merge-ready dataset with only the variables
# needed for analysis.
#============================================================
write_csv(safercar_dedup, "clean_data/Unmerged/safercar_clean.csv")


#============================================================
# STEP 10. Final checks
# Confirm structure and size after cleaning.
#============================================================
cat("Final rows:\n")
print(nrow(safercar_step7))

cat("Final columns:\n")
print(ncol(safercar_step7))