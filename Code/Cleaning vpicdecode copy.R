#============================================================
# 04_clean_vpicdecode.R
# Purpose: Clean the FARS vPIC-decode file for a study of how vehicle
#          size affects fatality risk in crashes.
#
# Why this file matters:
# This is the most important file for your main explanatory variable.
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

#-----------------------------
# Create output folder
#-----------------------------
dir.create("clean_data", showWarnings = FALSE)

#============================================================
# STEP 1. Convert file formats as necessary, and import your data.
# Why: Read the raw vPIC CSV as provided. We keep the original file
#      untouched and work only from this imported object.
#============================================================
vpic_raw <- read_csv("vpicdecode copy.csv", show_col_types = FALSE)

# Inspect immediately.
glimpse(vpic_raw)

#============================================================
# STEP 2. Structure data into tidy format if not already.
# Why: The vPIC file is already tidy: one row per vehicle. That is the
#      correct structure for joining to the Vehicle file by ST_CASE +
#      VEH_NO.
#============================================================
vpic_raw |>
  summarise(
    n_rows = n(),
    n_unique_vehicles = n_distinct(ST_CASE, VEH_NO)
  ) |>
  print()

#============================================================
# STEP 3. Remove irrelevant, garbage, or empty columns and rows.
# Why: The raw vPIC file is extremely wide, but your research question
#      is about vehicle size. So we keep the columns that directly help
#      measure size, class, weight, wheelbase, seating, and closely
#      related descriptors. This keeps the data analysis-focused and
#      avoids carrying hundreds of unused fields into the merge.
#============================================================
vpic_step3 <- vpic_raw |>
  select(
    ST_CASE, STATE, VEH_NO, VehicleDescriptor, VINDecodedOn,
    VINDecodeError, VehicleTypeId, VehicleType, MakeId, Make, ModelId,
    Model, ModelYear, ManufacturerFullName, BodyClassId, BodyClass,
    DoorsCount, TrackWidthIN, CurbWeightLB, WheelBaseIN_from,
    WheelBaseIN_To, WheelsCount, WheelSizeFrontIN, WheelSizeRearIN,
    SeatsCount, SeatRowsCount, DriveTypeId, DriveType,
    EngineCylindersCount, EngineBrakeHP_From, EngineBrakeHP_To,
    DisplacementL, FuelTypePrimary, SeatBeltType, AirBagLocFront,
    ElectronicStabilityControl, AntilockBrakeSystem, TPMS,
    EventDataRecorder
  ) |>
  filter(!is.na(ST_CASE), !is.na(VEH_NO))

#============================================================
# STEP 4. Identify the primary key, or define a surrogate key.
# Why: This file is a vehicle-level file, so ST_CASE + VEH_NO is the
#      correct key for merging back to the Vehicle file.
#============================================================

#============================================================
# STEP 5. Resolve duplicates.
# Why: Duplicate ST_CASE + VEH_NO rows would create incorrect one-to-
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
# Why: This file mixes numeric IDs, text descriptors, and genuine size
#      measurements. We need to distinguish actual physical measures
#      from lookup IDs.
#
# Key project notes:
# - CurbWeightLB is a direct weight-based size measure.
# - BodyClass is a categorical size/type descriptor.
# - WheelBaseIN_from / WheelBaseIN_To and TrackWidthIN are dimensional
#   size measures.
# - SeatsCount and DoorsCount are not size itself, but can proxy for
#   vehicle class.
# - VINDecodeError tells us whether the VIN decode may be unreliable.
#============================================================

#============================================================
# STEP 7. Rename variables as necessary, to be succinct and descriptive.
# Why: Cleaner names make the later merge and modeling code much easier
#      to read and less error-prone.
#============================================================
vpic_step7 <- vpic_step5 |>
  rename(
    st_case = ST_CASE,
    state = STATE,
    veh_no = VEH_NO,
    vehicle_descriptor = VehicleDescriptor,
    vin_decoded_on = VINDecodedOn,
    vin_decode_error = VINDecodeError,
    vehicle_type_id = VehicleTypeId,
    vehicle_type = VehicleType,
    make_id = MakeId,
    make = Make,
    model_id = ModelId,
    model = Model,
    model_year = ModelYear,
    manufacturer = ManufacturerFullName,
    body_class_id = BodyClassId,
    body_class = BodyClass,
    doors_count = DoorsCount,
    track_width_in = TrackWidthIN,
    curb_weight_lb = CurbWeightLB,
    wheelbase_in_from = WheelBaseIN_from,
    wheelbase_in_to = WheelBaseIN_To,
    wheels_count = WheelsCount,
    wheel_size_front_in = WheelSizeFrontIN,
    wheel_size_rear_in = WheelSizeRearIN,
    seats_count = SeatsCount,
    seat_rows_count = SeatRowsCount,
    drive_type_id = DriveTypeId,
    drive_type = DriveType,
    engine_cylinders = EngineCylindersCount,
    engine_hp_from = EngineBrakeHP_From,
    engine_hp_to = EngineBrakeHP_To,
    displacement_l = DisplacementL,
    fuel_type_primary = FuelTypePrimary,
    seat_belt_type = SeatBeltType,
    airbag_loc_front = AirBagLocFront,
    esc = ElectronicStabilityControl,
    abs = AntilockBrakeSystem,
    tpms = TPMS,
    edr = EventDataRecorder
  )

#============================================================
# STEP 8. Convert variable formats as necessary.
# Why: IDs and merge keys should be integer-like, physical measures
#      should be numeric, and text fields should remain character.
#============================================================
vpic_step8 <- vpic_step7 |>
  mutate(
    st_case = as.integer(st_case),
    state = as.integer(state),
    veh_no = as.integer(veh_no),
    model_year = as.numeric(model_year),
    doors_count = as.numeric(doors_count),
    track_width_in = as.numeric(track_width_in),
    curb_weight_lb = as.numeric(curb_weight_lb),
    wheelbase_in_from = as.numeric(wheelbase_in_from),
    wheelbase_in_to = as.numeric(wheelbase_in_to),
    wheels_count = as.numeric(wheels_count),
    wheel_size_front_in = as.numeric(wheel_size_front_in),
    wheel_size_rear_in = as.numeric(wheel_size_rear_in),
    seats_count = as.numeric(seats_count),
    seat_rows_count = as.numeric(seat_rows_count),
    engine_cylinders = as.numeric(engine_cylinders),
    engine_hp_from = as.numeric(engine_hp_from),
    engine_hp_to = as.numeric(engine_hp_to),
    displacement_l = as.numeric(displacement_l)
  )

#============================================================
# STEP 9. Understand patterns of missing values.
# Why: In this file, missingness is usually real NA from the VIN
#      decoder rather than a special numeric code. We therefore focus
#      on keeping those as NA and not forcing them into fake values.
#============================================================

#============================================================
# STEP 10. Make units and scales consistent.
# Why: Some size measures are given as lower/upper bounds. For analysis,
#      a single representative value is easier to use, so we create a
#      midpoint wheelbase when possible. We also keep weight in pounds
#      because that is the native variable and easy to interpret.
#============================================================
vpic_step10 <- vpic_step8 |>
  mutate(
    wheelbase_in_mid = case_when(
      !is.na(wheelbase_in_from) & !is.na(wheelbase_in_to) ~ (wheelbase_in_from + wheelbase_in_to) / 2,
      !is.na(wheelbase_in_from) &  is.na(wheelbase_in_to) ~ wheelbase_in_from,
      is.na(wheelbase_in_from) & !is.na(wheelbase_in_to) ~ wheelbase_in_to,
      TRUE ~ NA_real_
    )
  )

#============================================================
# STEP 11. Enforce logical conditions on quantitative variables.
# Why: Physical dimensions and counts should fall in plausible ranges.
#      Extremely impossible values usually reflect decoding or import
#      problems and should not be fed into analysis silently.
#============================================================
vpic_step11 <- vpic_step10 |>
  mutate(
    model_year = ifelse(model_year < 1900 | model_year > 2030, NA, model_year),
    doors_count = ifelse(doors_count < 0 | doors_count > 8, NA, doors_count),
    seats_count = ifelse(seats_count < 0 | seats_count > 20, NA, seats_count),
    seat_rows_count = ifelse(seat_rows_count < 0 | seat_rows_count > 6, NA, seat_rows_count),
    curb_weight_lb = ifelse(curb_weight_lb < 500 | curb_weight_lb > 20000, NA, curb_weight_lb),
    track_width_in = ifelse(track_width_in < 20 | track_width_in > 120, NA, track_width_in),
    wheelbase_in_mid = ifelse(wheelbase_in_mid < 40 | wheelbase_in_mid > 300, NA, wheelbase_in_mid),
    engine_cylinders = ifelse(engine_cylinders < 0 | engine_cylinders > 16, NA, engine_cylinders),
    displacement_l = ifelse(displacement_l < 0 | displacement_l > 20, NA, displacement_l)
  )

#============================================================
# STEP 12. Clean string variables if necessary.
# Why: String fields like Make and Model are often used for diagnostic
#      checks or backup merges. Standardizing case and trimming spaces
#      prevents false mismatches caused by inconsistent text formatting.
#============================================================
clean_text <- function(x) {
  x |>
    as.character() |>
    str_to_upper() |>
    str_trim()
}

vpic_clean <- vpic_step11 |>
  mutate(
    make_clean = clean_text(make),
    model_clean = clean_text(model),
    body_class_clean = clean_text(body_class),
    vehicle_type_clean = clean_text(vehicle_type)
  )

#============================================================
# STEP 13. Save your clean data to disk before further manipulation.
# Why: This saves a clean VIN-decoded vehicle-size file before you do
#      any merging or sample restrictions.
#============================================================
write_csv(vpic_clean, "clean_data/vpicdecode_clean.csv", na = "")

#-----------------------------
# Final inspection
#-----------------------------
glimpse(vpic_clean)
summary(vpic_clean)
