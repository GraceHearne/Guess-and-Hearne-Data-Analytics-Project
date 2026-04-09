#============================================================
# MERGE FARS + SAFERCAR WITH IMPROVED MATCHING
#============================================================

library(tidyverse)

#-----------------------------
# Load datasets
#-----------------------------
fars <- read_csv("clean_data/fars_merged_person_level.csv", show_col_types = FALSE)
safercar <- read_csv("clean_data/Unmerged/safercar_clean.csv", show_col_types = FALSE)

#-----------------------------
# Helper functions
#-----------------------------
clean_text <- function(x) {
  x |>
    as.character() |>
    str_to_upper() |>
    str_trim()
}

clean_model <- function(x) {
  x |>
    as.character() |>
    str_to_upper() |>
    str_replace_all("-", "") |>
    str_replace_all("/", " ") |>
    str_replace_all("[^A-Z0-9 ]", "") |>
    str_squish()
}

#-----------------------------
# Create merge keys in FARS
#-----------------------------
fars <- fars |>
  mutate(
    make_clean  = clean_text(make),
    model_clean = clean_model(model_vpic),
    year_clean  = as.numeric(model_year)
  ) |>
  filter(
    !is.na(make_clean),
    make_clean != "NA",
    !is.na(model_clean),
    model_clean != "NA",
    !is.na(year_clean)
  )

#-----------------------------
# Drop makes SaferCar is unlikely to cover
#-----------------------------
exclude_makes <- c(
  "HARLEY DAVIDSON", "YAMAHA", "KAWASAKI", "SUZUKI",
  "PETERBILT", "FREIGHTLINER", "KENWORTH", "MACK",
  "INTERNATIONAL", "VOLVO TRUCK", "HINO"
)

fars <- fars |>
  filter(!make_clean %in% exclude_makes)

#-----------------------------
# Clean SaferCar merge fields
#-----------------------------
safercar <- safercar |>
  mutate(
    make_clean  = clean_text(make_clean),
    model_clean = clean_model(model_clean),
    year_clean  = as.numeric(year_clean)
  )

#-----------------------------
# Deduplicate SaferCar
#-----------------------------
safercar_dedup <- safercar |>
  group_by(make_clean, model_clean, year_clean) |>
  summarize(
    across(everything(), ~ {
      x <- .x[!is.na(.x)]
      if (length(x) == 0) NA else x[1]
    }),
    .groups = "drop"
  )

#-----------------------------
# Merge
#-----------------------------
fars_Merging <- fars |>
  left_join(
    safercar_dedup,
    by = c("make_clean", "model_clean", "year_clean"),
    suffix = c("", "_safercar")
  )

#-----------------------------
# Checks
#-----------------------------
cat("\n================ ROW COUNTS ================\n")
cat("Rows in cleaned FARS before merge:\n")
print(nrow(fars))

cat("Rows after merge:\n")
print(nrow(fars_Merging))

cat("\n================ PERSON-LEVEL DUPLICATE CHECK ================\n")
fars_Merging |>
  count(st_case, veh_no, per_no) |>
  filter(n > 1) |>
  print()

cat("\n================ MATCH RATE ================\n")
fars_Merging |>
  summarize(
    matched_n = sum(!is.na(overall_front_stars)),
    total_n = n(),
    pct_matched = 100 * mean(!is.na(overall_front_stars)),
    pct_missing_safety = 100 * mean(is.na(overall_front_stars))
  ) |>
  print()

cat("\n================ SAMPLE UNMATCHED OBSERVATIONS ================\n")
fars_Merging |>
  filter(is.na(overall_front_stars)) |>
  select(make, model_vpic, model_year, make_clean, model_clean, year_clean) |>
  distinct() |>
  slice_head(n = 30) |>
  print(n = 30)

write_csv(fars_Merging, "clean_data/fars_Merging_with_safercar.csv")
cat("\nSaved file: clean_data/fars_Merging_with_safercar.csv\n")