
#-----------------------------
# Load CSV files
#-----------------------------
vehicle <- read.csv("vehicle_copy.csv", stringsAsFactors = FALSE)
person  <- read.csv("person copy.csv", stringsAsFactors = FALSE)

#-----------------------------
# Save as compressed RDS files
#-----------------------------
saveRDS(vehicle, "vehicle.rds", compress = "xz")
saveRDS(person,  "person.rds",  compress = "xz")

#-----------------------------
# Check that it Worked
#-----------------------------
list.files()
file.exists("vehicle.rds")
file.exists("person.rds")

vehicle_test <- readRDS("vehicle.rds")
person_test  <- readRDS("person.rds")

dim(vehicle_test)
dim(person_test)
head(vehicle_test)
head(person_test)