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
    make_clean  = clean_text(MAKE),
    model_clean = clean_text(MODEL),
    year_clean  = as.numeric(MODEL_YR)
  )


#============================================================
# STEP 6. Rename variables for clarity and consistency
# Lowercase, consistent naming makes merging and analysis easier.
#============================================================
safercar_step6 <- safercar_step5 |>
  rename(
    make = MAKE,
    model = MODEL,
    model_year = MODEL_YR,
    drive_train = DRIVE_TRAIN,
    
    overall_front_stars = OVERALL_FRNT_STARS,
    front_passenger_stars = FRNT_PASS_STARS,
    front_driver_stars = FRNT_DRIV_STARS,
    rollover_risk = ROLLOVER_POSSIBILITY
  )


#============================================================
# STEP 7. Convert variables to correct formats
# Safety ratings and rollover risk should be numeric so they can
# be used directly in regression analysis.
#============================================================
safercar_step7 <- safercar_step6 |>
  mutate(
    overall_front_stars = as.numeric(overall_front_stars),
    front_passenger_stars = as.numeric(front_passenger_stars),
    front_driver_stars = as.numeric(front_driver_stars),
    rollover_risk = as.numeric(rollover_risk)
  )


#============================================================
# STEP 8. Inspect missingness
# Some safety variables may not be available for all vehicles.
# We examine missingness to understand which variables are usable.
#============================================================
missing_summary <- safercar_step7 |>
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
write_csv(safercar_step7, "clean_data/safercar_clean.csv")


#============================================================
# STEP 10. Final checks
# Confirm structure and size after cleaning.
#============================================================
cat("Final rows:\n")
print(nrow(safercar_step7))

cat("Final columns:\n")
print(ncol(safercar_step7))