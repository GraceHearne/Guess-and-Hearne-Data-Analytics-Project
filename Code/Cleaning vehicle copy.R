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

#-----------------------------
# Create output folder
#-----------------------------
dir.create("clean_data", showWarnings = FALSE)

#============================================================
# STEP 1. Convert file formats as necessary, and import your data.
# Why: Read the raw CSV without modifying the source file.
#============================================================
vehicle_raw <- read_csv("vehicle copy.csv", show_col_types = FALSE)

# Inspect immediately.
glimpse(vehicle_raw)

#============================================================
# STEP 2. Structure data into tidy format if not already.
# Why: The Vehicle file is already tidy: one row per motor vehicle
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
# Why: The raw Vehicle file is very wide. For your question we mainly
#      need vehicle descriptors, crash-mechanics variables, driver-
#      related controls, and merge keys. We therefore keep a focused
#      set of columns and drop the many *_NAME label columns.
#============================================================
vehicle_step3 <- vehicle_raw |>
  select(
    ST_CASE, STATE, VEH_NO, VE_FORMS, NUMOCCS, UNITTYPE, HIT_RUN,
    REG_STAT, OWNER, MAKE, MODEL, MAK_MOD, BODY_TYP, MOD_YEAR, VIN,
    TOW_VEH, J_KNIFE, V_CONFIG, CARGO_BT, BUS_USE, SPEC_USE, EMER_USE,
    TRAV_SP, UNDERIDE, ROLLOVER, ROLINLOC, IMPACT1, DEFORMED, TOWED,
    M_HARM, FIRE_EXP, DR_PRES, L_STATE, DR_ZIP, L_STATUS, L_TYPE,
    CDL_STAT, L_ENDORS, L_COMPL, L_RESTRI, DR_HGT, DR_WGT, PREV_ACC,
    PREV_SUS1, PREV_SUS2, PREV_SUS3, PREV_DWI, PREV_SPD, PREV_OTH,
    FIRST_MO, FIRST_YR, LAST_MO, LAST_YR, SPEEDREL, VTRAFWAY, VNUM_LAN,
    VSPD_LIM, VALIGN, VPROFILE, VPAVETYP, VSURCOND, VTRAFCON, VTCONT_F,
    P_CRASH1, P_CRASH2, P_CRASH3, PCRASH4, PCRASH5, ACC_TYPE, DEATHS,
    DR_DRINK, VPICMAKE, VPICMODEL, VPICBODYCLASS, ICFINALBODY,
    GVWR_FROM, GVWR_TO
  ) |>
  filter(!is.na(ST_CASE), !is.na(VEH_NO))

#============================================================
# STEP 4. Identify the primary key, or define a surrogate key.
# Why: The FARS manual uses ST_CASE + VEH_NO as the vehicle-level key.
#      That is the official merge key for linking vehicles to persons
#      and to VIN-decoded vPIC data.
#============================================================

#============================================================
# STEP 5. Resolve duplicates.
# Why: There should be one row per ST_CASE + VEH_NO. If duplicates are
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
# Why: Many vehicle variables are coded categories, not continuous
#      scales. For example BODY_TYP and VPICBODYCLASS identify type,
#      not numeric size. TRAV_SP is a measured speed but uses special
#      unknown codes. MOD_YEAR is the manufacturer model year and can
#      be turned into vehicle age for analysis.
#============================================================

#============================================================
# STEP 7. Rename variables as necessary, to be succinct and descriptive.
# Why: Consistent names make later merges and regression code much
#      easier to understand.
#============================================================
vehicle_step7 <- vehicle_step5 |>
  rename(
    st_case = ST_CASE, state = STATE, veh_no = VEH_NO,
    vehicles_in_transport = VE_FORMS, num_occupants = NUMOCCS,
    unit_type = UNITTYPE, hit_run = HIT_RUN, reg_state = REG_STAT,
    owner = OWNER, make = MAKE, model = MODEL, mak_mod = MAK_MOD,
    body_typ = BODY_TYP, mod_year = MOD_YEAR, vin = VIN,
    towed_vehicle = TOW_VEH, jackknife = J_KNIFE,
    vehicle_config = V_CONFIG, cargo_body_type = CARGO_BT,
    bus_use = BUS_USE, special_use = SPEC_USE, emergency_use = EMER_USE,
    trav_sp = TRAV_SP, underride = UNDERIDE, rollover = ROLLOVER,
    rollover_location = ROLINLOC, impact1 = IMPACT1, deformed = DEFORMED,
    towed = TOWED, most_harmful_event = M_HARM, fire_exp = FIRE_EXP,
    driver_present = DR_PRES, driver_license_state = L_STATE,
    driver_zip = DR_ZIP, license_status = L_STATUS, license_type = L_TYPE,
    cdl_status = CDL_STAT, license_endorsements = L_ENDORS,
    license_compliance = L_COMPL, license_restrictions = L_RESTRI,
    driver_height = DR_HGT, driver_weight = DR_WGT, prev_acc = PREV_ACC,
    prev_sus1 = PREV_SUS1, prev_sus2 = PREV_SUS2, prev_sus3 = PREV_SUS3,
    prev_dwi = PREV_DWI, prev_spd = PREV_SPD, prev_oth = PREV_OTH,
    first_mo = FIRST_MO, first_yr = FIRST_YR, last_mo = LAST_MO,
    last_yr = LAST_YR, speed_related = SPEEDREL, roadway_flow = VTRAFWAY,
    lanes = VNUM_LAN, speed_limit = VSPD_LIM, alignment = VALIGN,
    profile = VPROFILE, pavement_type = VPAVETYP,
    surface_condition = VSURCOND, traffic_control = VTRAFCON,
    traffic_control_function = VTCONT_F, precrash_movement = P_CRASH1,
    critical_event = P_CRASH2, avoidance_maneuver = P_CRASH3,
    preimpact_stability = PCRASH4, preimpact_location = PCRASH5,
    crash_type = ACC_TYPE, deaths_in_vehicle = DEATHS,
    driver_drinking = DR_DRINK, vpicmake = VPICMAKE,
    vpicmodel = VPICMODEL, vpicbodyclass = VPICBODYCLASS,
    final_body = ICFINALBODY, gvwr_from = GVWR_FROM, gvwr_to = GVWR_TO
  )

#============================================================
# STEP 8. Convert variable formats as necessary.
# Why: IDs stay integer-like, VIN stays as character, measured values
#      remain numeric, and coded descriptors will later become factors.
#============================================================
vehicle_step8 <- vehicle_step7 |>
  mutate(
    st_case = as.integer(st_case),
    state = as.integer(state),
    veh_no = as.integer(veh_no),
    vin = as.character(vin),
    mod_year = as.numeric(mod_year),
    trav_sp = as.numeric(trav_sp),
    driver_height = as.numeric(driver_height),
    driver_weight = as.numeric(driver_weight)
  )

#============================================================
# STEP 9. Understand patterns of missing values.
# Why: FARS uses many special unknown codes in vehicle data. Those must
#      be converted to NA so they are not treated as real observed
#      values.
#============================================================
vehicle_step9 <- vehicle_step8 |>
  mutate(
    num_occupants = ifelse(num_occupants %in% c(97, 98, 99), NA, num_occupants),
    unit_type = ifelse(unit_type %in% c(99), NA, unit_type),
    hit_run = ifelse(hit_run %in% c(8, 9), NA, hit_run),
    reg_state = ifelse(reg_state %in% c(91, 99), NA, reg_state),
    owner = ifelse(owner %in% c(9), NA, owner),
    body_typ = ifelse(body_typ %in% c(98, 99), NA, body_typ),
    mod_year = ifelse(mod_year %in% c(9998, 9999), NA, mod_year),
    trav_sp = ifelse(trav_sp %in% c(997, 998, 999), NA, trav_sp),
    underride = ifelse(underride %in% c(8, 9), NA, underride),
    rollover = ifelse(rollover %in% c(8, 9), NA, rollover),
    rollover_location = ifelse(rollover_location %in% c(8, 9), NA, rollover_location),
    impact1 = ifelse(impact1 %in% c(98, 99), NA, impact1),
    deformed = ifelse(deformed %in% c(8, 9), NA, deformed),
    towed = ifelse(towed %in% c(8, 9), NA, towed),
    most_harmful_event = ifelse(most_harmful_event %in% c(98, 99), NA, most_harmful_event),
    fire_exp = ifelse(fire_exp %in% c(8, 9), NA, fire_exp),
    driver_present = ifelse(driver_present %in% c(8, 9), NA, driver_present),
    driver_license_state = ifelse(driver_license_state %in% c(91, 99), NA, driver_license_state),
    license_status = ifelse(license_status %in% c(8, 9), NA, license_status),
    license_type = ifelse(license_type %in% c(8, 9), NA, license_type),
    cdl_status = ifelse(cdl_status %in% c(8, 9), NA, cdl_status),
    license_endorsements = ifelse(license_endorsements %in% c(8, 9), NA, license_endorsements),
    license_compliance = ifelse(license_compliance %in% c(8, 9), NA, license_compliance),
    license_restrictions = ifelse(license_restrictions %in% c(8, 9), NA, license_restrictions),
    driver_height = ifelse(driver_height %in% c(998, 999), NA, driver_height),
    driver_weight = ifelse(driver_weight %in% c(998, 999), NA, driver_weight),
    prev_acc = ifelse(prev_acc %in% c(98, 99), NA, prev_acc),
    prev_sus1 = ifelse(prev_sus1 %in% c(98, 99), NA, prev_sus1),
    prev_sus2 = ifelse(prev_sus2 %in% c(98, 99), NA, prev_sus2),
    prev_sus3 = ifelse(prev_sus3 %in% c(98, 99), NA, prev_sus3),
    prev_dwi = ifelse(prev_dwi %in% c(98, 99), NA, prev_dwi),
    prev_spd = ifelse(prev_spd %in% c(98, 99), NA, prev_spd),
    prev_oth = ifelse(prev_oth %in% c(98, 99), NA, prev_oth),
    first_mo = ifelse(first_mo %in% c(98, 99), NA, first_mo),
    first_yr = ifelse(first_yr %in% c(9998, 9999), NA, first_yr),
    last_mo = ifelse(last_mo %in% c(98, 99), NA, last_mo),
    last_yr = ifelse(last_yr %in% c(9998, 9999), NA, last_yr),
    speed_related = ifelse(speed_related %in% c(8, 9), NA, speed_related),
    lanes = ifelse(lanes %in% c(98, 99), NA, lanes),
    speed_limit = ifelse(speed_limit %in% c(98, 99), NA, speed_limit),
    alignment = ifelse(alignment %in% c(8, 9), NA, alignment),
    profile = ifelse(profile %in% c(8, 9), NA, profile),
    pavement_type = ifelse(pavement_type %in% c(8, 9), NA, pavement_type),
    surface_condition = ifelse(surface_condition %in% c(8, 9), NA, surface_condition),
    traffic_control = ifelse(traffic_control %in% c(98, 99), NA, traffic_control),
    traffic_control_function = ifelse(traffic_control_function %in% c(8, 9), NA, traffic_control_function),
    precrash_movement = ifelse(precrash_movement %in% c(98, 99), NA, precrash_movement),
    critical_event = ifelse(critical_event %in% c(98, 99), NA, critical_event),
    avoidance_maneuver = ifelse(avoidance_maneuver %in% c(98, 99), NA, avoidance_maneuver),
    preimpact_stability = ifelse(preimpact_stability %in% c(8, 9), NA, preimpact_stability),
    preimpact_location = ifelse(preimpact_location %in% c(8, 9), NA, preimpact_location),
    crash_type = ifelse(crash_type %in% c(998, 999), NA, crash_type),
    driver_drinking = ifelse(driver_drinking %in% c(8, 9), NA, driver_drinking),
    vpicmake = ifelse(vpicmake %in% c(99998, 99999), NA, vpicmake),
    vpicmodel = ifelse(vpicmodel %in% c(99998, 99999), NA, vpicmodel),
    vpicbodyclass = ifelse(vpicbodyclass %in% c(998, 999), NA, vpicbodyclass),
    final_body = ifelse(final_body %in% c(998, 999), NA, final_body),
    gvwr_from = ifelse(gvwr_from %in% c(98, 99), NA, gvwr_from),
    gvwr_to = ifelse(gvwr_to %in% c(98, 99), NA, gvwr_to)
  )

#============================================================
# STEP 10. Make units and scales consistent.
# Why: A vehicle's model year is more interpretable in analysis when
#      converted to vehicle age. Travel speed and driver body measures
#      should remain numeric because those are measured quantities.
#============================================================
vehicle_step10 <- vehicle_step9 |>
  mutate(
    vehicle_age = ifelse(!is.na(mod_year), 2023 - mod_year, NA_real_)
  )

#============================================================
# STEP 11. Enforce logical conditions on quantitative variables.
# Why: Prevent impossible values from entering later regressions.
#============================================================
vehicle_step11 <- vehicle_step10 |>
  mutate(
    trav_sp = ifelse(trav_sp < 0 | trav_sp > 200, NA, trav_sp),
    speed_limit = ifelse(speed_limit < 0 | speed_limit > 100, NA, speed_limit),
    driver_height = ifelse(driver_height < 36 | driver_height > 96, NA, driver_height),
    driver_weight = ifelse(driver_weight < 50 | driver_weight > 700, NA, driver_weight),
    vehicle_age = ifelse(vehicle_age < 0 | vehicle_age > 100, NA, vehicle_age)
  )

#============================================================
# STEP 12. Clean string variables if necessary.
# Why: VIN is a string of characters/digits and should remain a
#      character field. We also trim whitespace to avoid accidental
#      merge or comparison problems later.
#============================================================
vehicle_clean <- vehicle_step11 |>
  mutate(
    vin = str_trim(vin),
    across(
      c(unit_type, hit_run, owner, body_typ, towed_vehicle, jackknife,
        vehicle_config, cargo_body_type, bus_use, special_use,
        emergency_use, underride, rollover, rollover_location,
        impact1, deformed, towed, most_harmful_event, fire_exp,
        driver_present, license_status, license_type, cdl_status,
        license_endorsements, license_compliance, license_restrictions,
        speed_related, roadway_flow, lanes, alignment, profile,
        pavement_type, surface_condition, traffic_control,
        traffic_control_function, precrash_movement, critical_event,
        avoidance_maneuver, preimpact_stability, preimpact_location,
        crash_type, driver_drinking, vpicbodyclass, final_body,
        gvwr_from, gvwr_to),
      as.factor
    )
  )

#============================================================
# STEP 13. Save your clean data to disk before further manipulation.
# Why: Save the clean vehicle-level data before merging with person or
#      vPIC files, so the cleaning phase is reproducible and separate.
#============================================================
write_csv(vehicle_clean, "clean_data/vehicle_clean.csv", na = "")

#-----------------------------
# Final inspection
#-----------------------------
glimpse(vehicle_clean)
summary(vehicle_clean)
