#============================================================
# CLEAN VEHICLE-LEVEL FARS DATA
# Research question: How does car size affect fatality risk in crashes?
# The Vehicle file is the main motor-vehicle-in-transport file in
# FARS. It has one record per vehicle and gives the vehicle-side
# characteristics that help explain fatality risk, such as body type,
# model year, travel speed, rollover, impact location, driver presence,
# and roadway/pre-crash context. It is also one of the core merge files
# because vehicle and person observations link through ST_CASE + VEH_NO.
#============================================================

#-----------------------------
# Load packages
#-----------------------------
library(tidyverse)

#============================================================
# STEP 1. Convert file formats and import data
#============================================================
vehicle_raw <- readRDS("Raw Data/vehicle.rds")

# Inspect immediately.
glimpse(vehicle_raw)

#============================================================
# STEP 2. Structure data into tidy format if not already.
#  The Vehicle file is already tidy: one row per motor vehicle
#      in-transport. That is the correct structure for later merging.
#============================================================
vehicle_raw |>
  summarise(
    n_rows = n(),
    n_unique_vehicles = n_distinct(ST_CASE, VEH_NO)
  ) |>
  print()

#============================================================
# STEP 3. Remove irrelevant, garbage, or empty columns and rows.
#        The raw Vehicle file is very wide. We mainly
#      need vehicle descriptors, crash-mechanics variables, driver-
#      related controls, and merge keys. We therefore keep a focused
#      set of columns and drop the many *_NAME label columns.
#============================================================
vehicle_step3 <- vehicle_raw |>
  select(
    ST_CASE, 
    VEH_NO, 
    NUMOCCS,
    MAKENAME, 
    VPICMODELNAME, 
    MOD_YEAR,
    DEATHS
  ) |>
  filter(!is.na(ST_CASE), !is.na(VEH_NO))

#============================================================
# STEP 4. Identify the primary key, or define a surrogate key.
#      The FARS manual uses ST_CASE + VEH_NO as the vehicle-level key.
#      That is the official merge key for linking vehicles to persons
#      and to VIN-decoded vPIC data.
#============================================================

#============================================================
# STEP 5. Resolve duplicates.
#     There should be one row per ST_CASE + VEH_NO. If duplicates are
#      present, a later merge would multiply records incorrectly.
#============================================================
dup_vehicles <- vehicle_step3 |>
  count(ST_CASE, VEH_NO) |>
  filter(n > 1)

print(dup_vehicles)

vehicle_step5 <- vehicle_step3 |>
  distinct(ST_CASE, VEH_NO, .keep_all = TRUE)

#============================================================
# STEP 6. Understand the definition, origin, and units of each
#         variable, and document as necessary.
#       Many vehicle variables are coded categories, not continuous
#      scales. For example BODY_TYP and VPICBODYCLASS identify type,
#      not numeric size. TRAV_SP is a measured speed but uses special
#      unknown codes. MOD_YEAR is the manufacturer model year and can
#      be turned into vehicle age for analysis.
#============================================================

#============================================================
# STEP 7. Rename variables as necessary, to be succinct and descriptive.
#============================================================
vehicle_step7 <- vehicle_step5 |>
  rename(
    st_case = ST_CASE,
    veh_no = VEH_NO,
    num_occupants = NUMOCCS,
    make = MAKENAME, 
    model = VPICMODELNAME, 
    car_year = MOD_YEAR,
    deaths_in_vehicle = DEATHS
  )

#============================================================
# STEP 8. Convert variable formats as necessary.
# IDs stay integer-like, VIN stays as character, measured values
#      remain numeric, and coded descriptors will later become factors.
#============================================================
vehicle_step8 <- vehicle_step7 |>
  mutate(
    st_case = as.integer(st_case),
    veh_no = as.integer(veh_no),
    car_year = as.numeric(car_year),
    deaths_in_vehicle = as.numeric(deaths_in_vehicle),
    model = toupper(as.character(model)),
    make = toupper(as.character(make)),
  )


##============================================================
# STEP 9. Understand patterns of missing values.
#   FARS often uses special codes like 8, 9, 97, 98, 99, 998,
#      or 999 for unknown/not reported. If we leave those untouched,
#      they will contaminate regression inputs.
#     For more details about theses specific code indicators see code book. 
#     For our purposes essentially NA.
#============================================================
vehicle_step9 <- vehicle_step8 |>
  mutate(
    num_occupants = ifelse(num_occupants %in% c(97, 98, 99), NA, num_occupants),
    car_year = ifelse(car_year %in% c(9998, 9999), NA, car_year)
  )

#============================================================
# STEP 10. Make units and scales consistent.
#    A vehicle's model year is more interpretable in analysis when
#      converted to vehicle age. Travel speed and driver body measures
#      should remain numeric because those are measured quantities.
#============================================================
vehicle_step10 <- vehicle_step9 |>
  mutate(
    vehicle_age = ifelse(!is.na(car_year), 2023 - car_year, NA_real_)
  )

#============================================================
# STEP 11. Enforce logical conditions on quantitative variables.
#  Prevent impossible values from entering later regressions.
#============================================================
vehicle_clean <- vehicle_step10 |>
  mutate(
    vehicle_age = ifelse(vehicle_age < 0 | vehicle_age > 100, NA, vehicle_age)
  )

#============================================================
# STEP 12. Clean string variables if necessary.
#    VIN is a string of characters/digits and should remain a
#      character field. We also trim whitespace to avoid accidental
#      merge or comparison problems later.
#============================================================

#============================================================
# STEP 13. Save the clean data to disk before further manipulation.
# Save the clean vehicle-level data before merging with person or
#      vPIC files, so the cleaning phase is reproducible and separate.
#============================================================
write_csv(vehicle_clean, "clean_data/Unmerged/vehicle_clean.csv", na = "")

#-----------------------------
# Final inspection
#-----------------------------
glimpse(vehicle_clean)
summary(vehicle_clean)
