library(tidyverse)
library(here)
library(sf)

ais_zoe <- read_sf(here("data/Zoes_data/zoes_trips_ais.geojson"))

trips_ais <- ais_zoe |>
  as.data.frame() |>
  select(trip_id) |>
  distinct() |>
  unlist()

for (i in trips_ais){
  
trip_i <- ais_zoe |>
  filter(trip_id == i)
  
  st_write(trip_i, 
           dsn = here(paste("data/Zoes_data/trip", i, ".geojson",
                            sep = "")),
           layer = paste("trip", i, ".geojson",
                         sep = ""))
}
