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
car_dimensions_raw <- read_csv("Raw Data/AI Generated Car Dimensions.csv", show_col_types = FALSE)

#============================================================
# STEP 3. Inspect dataset
#============================================================
glimpse(car_dimensions_raw)
names(car_dimensions_raw)


#============================================================
# STEP 4. Keep only relevant variables
# Why:
# We retain only variables directly related to:
# 1. Vehicle identifiers needed for merging (make, model, year)
# 2. Basic vehicle characteristics (drive train)
#
# Already done when generated data
#============================================================

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

car_dimensions_step5 <- car_dimensions_raw |>
  mutate(
    Make  = clean_text(Make),
    Model = clean_text(Model),
    Year  = as.numeric(Year)
  )


#============================================================
# STEP 6. Rename variables for clarity and consistency
# Lowercase, consistent naming makes merging and analysis easier.
#============================================================
car_dimensions_step6 <- car_dimensions_step5 |>
  rename(
    make = Make,
    model = Model,
    car_year = Year,
    drive_train = 'Drive Train',
    curb_weight = 'Kerb Weight',
    car_height = 'Car Height',
    car_length = 'Car Length',
    car_width = 'Car Width'
  )


#============================================================
# STEP 7. Convert variables to correct formats
# Safety ratings and rollover risk should be numeric so they can
# be used directly in regression analysis.
#============================================================
car_dimensions_step7 <- car_dimensions_step6 |>
  mutate(
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

car_dimensions_dedup <- car_dimensions_step7 |>
  group_by(make, model, car_year, drive_train) |>
  summarize(
    make = first(make),
    model = first(model),
    car_year = first(car_year),
    drive_train = first(drive_train),
    
    # average safety measures
    curb_weight = mean(curb_weight, na.rm = TRUE),
    car_height = mean(car_height, na.rm = TRUE),
    car_width = mean(car_width, na.rm = TRUE),
    car_length = mean(car_length, na.rm = TRUE),
    
    .groups = "drop"
  )
#============================================================
# STEP 8. Inspect missingness
# Generated data ourselves, no missing
#============================================================

#============================================================
# STEP 9. Save cleaned dataset
# This creates a clean, merge-ready dataset with only the variables
# needed for analysis.
#============================================================
write_csv(car_dimensions_dedup, "clean_data/Unmerged/AI_car_dimensions.csv")


#============================================================
# STEP 10. Final checks
# Confirm structure and size after cleaning.
#============================================================
cat("Final rows:\n")
print(nrow(car_dimensions_dedup))

cat("Final columns:\n")
print(ncol(car_dimensions_dedup))