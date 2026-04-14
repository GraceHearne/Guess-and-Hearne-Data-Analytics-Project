# Load libraries
library(dplyr)
library(tidyverse)

# Pull Cleaned AI Data
crashes_and_cars = read.csv("clean_data/crash_data.csv")

# Filter to only AI data and identifiers
set.seed(123)

AI_Data = crashes_and_cars |>
  select(make, model, car_year, drive_train, curb_weight, car_height, car_length, car_width)

AI_Validation_Sample = AI_Data |>
  sample_frac(0.01)

write_csv(AI_Validation_Sample, "clean_data/AI_Validation_Sample_1pct.csv")