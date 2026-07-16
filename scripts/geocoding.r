# Script for Extracting Missing Coordinates from US Census

# Packages
library(tidyverse)
library(vroom)
library(tidygeocoder)

# Data
data <- vroom("Motor_Vehicle_Collisions_-_Crashes_20260612.csv") |>
  janitor::clean_names()

missing_coords <- data |>
  filter(is.na(longitude) & is.na(latitude), !is.na(on_street_name)) |>
  select(!c(latitude, longitude))

# handling missing data by splitting up the dataset into subsets for accessible debugging purposes
n <- ceiling(nrow(missing_coords) / 10000)

ds <- split(missing_coords, factor(sort(rank(row.names(missing_coords)) %% n)))

# creating dataset for recovered coordinates
census_records <- data.frame()

# applied function across each subset, using US census geocoder to recover possible coordinates
ds <- lapply(ds, function(d) {
  as.data.frame(d) |>
    # added "NY" for more specificity in extracting coordinates
    mutate(on_street_name = paste0(on_street_name, ", NY")) |>
    geocode(
      address = on_street_name,
      method = "census",
      lat = "latitude",
      long = "longitude"
    )
})

# joined all recovered coodinates to data
census_records <- bind_rows(ds) |>
  filter(!is.na(latitude))

# dataset of all unrecovered coordinates
ds <- bind_rows(ds) |>
  filter(is.na(latitude))

# removed "NY" from street name
census_records$on_street_name <- census_records$on_street_name |>
  str_remove(regex(", NY"))

# saved data
saveRDS(census_records, "census_records.rds")
