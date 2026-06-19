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

#| label: handling missing data 
n <- ceiling(nrow(missing_coords) / 10000)

ds <- split(missing_coords, factor(sort(rank(row.names(missing_coords))%%n)))

census_records <- data.frame()

ds <- lapply(ds, function(d) {
  as.data.frame(d) |>
    mutate(on_street_name = paste0(on_street_name, ", NY")) |>
    geocode(
      address = on_street_name,
      method = "census",
      lat = "latitude",
      long = "longitude"
    )
})

 census_records <- bind_rows(ds) |>
  filter(!is.na(latitude)) 

 ds <- bind_rows(ds) |>
  filter(is.na(latitude))

 census_records$on_street_name <- census_records$on_street_name |>
  str_remove(regex(", NY"))

 saveRDS(census_records, "census_records.rds")



