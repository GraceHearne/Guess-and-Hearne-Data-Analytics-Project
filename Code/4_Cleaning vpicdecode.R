#============================================================
# CLEAN PERSON-LEVEL FARS DATA
# Research question: How does car size affect fatality risk in crashes?
# Why this file matters:
# This is the most important file for main explanatory variable.
# The vPIC file contains vehicle features decoded from the VIN, such
# as body class, curb weight, wheelbase, track width, seats, doors,
# drivetrain, and engine characteristics. Compared with the standard
# FARS vehicle file, this is the best source for concrete size-related
# measures.
#============================================================

#-----------------------------
# Load packages
#-----------------------------
library(tidyverse)

#============================================================
# STEP 1. Convert file formats as necessary, and import your data.
#============================================================
vpic_raw <- read_csv("Raw Data/vpicdecode.csv", show_col_types = FALSE)

# Inspect immediately.
glimpse(vpic_raw)

#============================================================
# STEP 2. Structure data into tidy format if not already.
#      The vPIC file is already tidy: one row per vehicle. That is the
#      correct structure for joining to the Vehicle file by ST_CASE +
#      VEH_NO.
#============================================================
vpic_raw |>
  summarise(
    n_rows = n(),
    n_unique_vehicles = n_distinct(ST_CASE, VEH_NO)
  ) |>
  print()
#all distinct

#============================================================
# STEP 3. Remove irrelevant, garbage, or empty columns and rows.
#     The raw vPIC file is extremely wide, but the research question
#      is about vehicle size. So we keep the columns that directly help
#      measure size, class, weight, wheelbase, seating, and closely
#      related descriptors. This keeps the data analysis-focused and
#      avoids carrying hundreds of unused fields into the merge.
#============================================================
vpic_step3 <- vpic_raw |>
  select(
    ST_CASE, VEH_NO, DriveTypeId,
  ) |>
  filter(!is.na(ST_CASE), !is.na(VEH_NO))

#============================================================
# STEP 4. Identify the primary key, or define a surrogate key.
#  This file is a vehicle-level file, so ST_CASE + VEH_NO is the
#      correct key for merging back to the Vehicle file.
#============================================================

#============================================================
# STEP 5. Resolve duplicates.
#      Duplicate ST_CASE + VEH_NO rows would create incorrect one-to-
#      many merges and duplicate vehicles in the final analysis file.
#============================================================
dup_vpic <- vpic_step3 |>
  count(ST_CASE, VEH_NO) |>
  filter(n > 1)

print(dup_vpic)

vpic_step5 <- vpic_step3 |>
  distinct(ST_CASE, VEH_NO, .keep_all = TRUE)

#============================================================
# STEP 6. Understand the definition, origin, and units of each
#         variable, and document as necessary.
#     This file mixes numeric IDs, text descriptors, and genuine size
#      measurements. We need to distinguish actual physical measures
#      from lookup IDs.
#
# Key project notes:
# - DriveTypeID is classified 1 to 7
#============================================================

#============================================================
# STEP 7. Rename variables as necessary, to be succinct and descriptive.
#============================================================
vpic_step7 <- vpic_step5 |>
  rename(
    st_case = ST_CASE,
    veh_no = VEH_NO,
    drive_type_id = DriveTypeId,
  )

#============================================================
# STEP 8. Convert variable formats as necessary.
# IDs and merge keys should be integer-like, physical measures
#      should be numeric, and text fields should remain character.
#============================================================
vpic_step8 <- vpic_step7 |>
  mutate(
    st_case = as.integer(st_case),
    veh_no  = as.integer(veh_no),
    drive_train = case_when(
      drive_type_id == 7 ~ "RWD",
      drive_type_id == 6 ~ "Other",
      drive_type_id == 5 ~ "6x4",
      drive_type_id == 4 ~ "RWD",
      drive_type_id == 3 ~ "AWD",
      drive_type_id == 2 ~ "4WD",
      drive_type_id == 1 ~ "FWD",
      TRUE ~ NA_character_
    )
  )

#============================================================
# STEP 9. Understand patterns of missing values.
# In this file, missingness is usually real NA from the VIN
#      decoder rather than a special numeric code. We therefore focus
#      on keeping those as NA and not forcing them into fake values.
#============================================================

#============================================================
# STEP 10. Make units and scales consistent.
#  Unnecessary here 
#============================================================
#============================================================
# STEP 11. Enforce logical conditions on quantitative variables.
# Physical dimensions and counts should fall in plausible ranges.
# Extremely impossible values usually reflect decoding or import
# problems and should not be fed into analysis silently.
#     unnecessary here 
#============================================================
#============================================================
# STEP 12. Clean string variables if necessary.
# unnecessary
#============================================================

#============================================================
# STEP 13. Save your clean data to disk before further manipulation.
#============================================================
write_csv(vpic_step8, "clean_data/Unmerged/vpicdecode_clean.csv", na = "")

#-----------------------------
# Final inspection
#-----------------------------
glimpse(vpic_step8)
summary(vpic_step8)
