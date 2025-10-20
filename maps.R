library(tidyverse)
library(here)
library(sf)

##### trip_nr_map() function must be loaded from trip_map.R #####

trips_ais <- read_sf(here("data/trips_ais.geojson")) |>
  as.data.frame() |>
  select(trip_id) |>
  distinct() #|>
  #unlist()

i <- 691
while(i <=  726)  #length(trips_ais$trip_id))
{
  trip_i <- trip_nr_map(trips_ais$trip_id[i]) +
    geom_text(aes(label = trips_ais$trip_id[i], 
                  x = 16.13, 
                  y = 69.32), 
              colour = "white") 
  
  jpeg(filename = paste("trip_", 
                        trips_ais$trip_id[i], 
                        ".jpeg", sep = ""))
  plot(trip_i)
  dev.off
  
  i <- i + 1
}
