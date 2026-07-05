
# packages
library(tidyverse)
library(vroom)
library(tidygeocoder)
library(maps)
library(ggrepel)
library(sf)
library(tictoc)
library(janitor)

# wrangling data
my_sf <- read_sf("data/UHF42.geo.json") |>
  clean_names()

census_records <- readRDS("data/census_records.rds")

data <- vroom("data/Motor_Vehicle_Collisions_-_Crashes_20260612.csv") |>
    clean_names() |>
    mutate(crash_date = mdy(crash_date),
    longitude = ifelse(is.na(longitude), census_records$longitude[match(on_street_name, census_records$on_street_name)], longitude),
    latitude = ifelse(is.na(latitude), census_records$latitude[match(on_street_name, census_records$on_street_name)], latitude)) |>
    drop_na(longitude) |>
    select(-c(borough, on_street_name, cross_street_name, off_street_name, contributing_factor_vehicle_2, zip_code, contributing_factor_vehicle_3, contributing_factor_vehicle_4, contributing_factor_vehicle_5, vehicle_type_code_5)) |>
    filter(!is.na(longitude), !is.na(latitude), latitude != 0, longitude != 0) |>
    mutate(longitude = ifelse(longitude < -200, yes =longitude + 127.2832727, longitude)) |>
    select(-c(vehicle_type_code_1, vehicle_type_code_2, vehicle_type_code_3, vehicle_type_code_4))

coords_data <- data |>
  select(c(longitude, latitude, collision_id))
  

# turning coordinates into sf_point
tic()
data_sf <- coords_data |> distinct() |> st_as_sf(
  coords = c("longitude", "latitude"),
  crs = 4326,
  remove = FALSE
) |> 
  st_make_valid()
toc()

# ensuring both sfs are compatible for merging and joining
my_sf <- my_sf |> st_make_valid()

data_sf <- st_transform(data_sf, st_crs(my_sf))

st_crs(data_sf)
st_crs(my_sf)

# spherical coordinates take a long time, so i'm turning it off. Planar coords work just fine
sf_use_s2(FALSE)

# combining both sfs 
combined <- st_join(my_sf, data_sf) 

# merging original data with the spatial and geographical data from combined
merged_data <- merge(data, combined, by.x = "collision_id", by.y = "collision_id", all.x = TRUE) |>
  select(-c(latitude.x, longitude.x)) |>
  rename(longitude = longitude.y,
  latitude = latitude.y) |> 
  filter(borough != "N/A") |> 
  select(-id) |>
    mutate(number_of_persons_killed = ifelse(is.na(number_of_persons_killed), 0, number_of_persons_killed),
    number_of_persons_injured = ifelse(is.na(number_of_persons_injured), 0, number_of_persons_injured),
    contributing_factor_vehicle_1 = ifelse(is.na(contributing_factor_vehicle_1), "Unspecified", contributing_factor_vehicle_1)) |>
    drop_na(location) |>
  distinct()
toc()


# adding count of how many collisions per neighborhood
geoname_count <- merged_data |> group_by(geoname) |>
  summarize(count = n()) 

my_sf <- my_sf |>
  left_join(geoname_count, by = "geoname") |> drop_na()

# saved data
 saveRDS(my_sf, "data/my_sf.rds")
 saveRDS(merged_data, "data/collisions_data.rds")



