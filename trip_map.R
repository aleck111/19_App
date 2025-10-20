library(tidyterra)
library(terra)
library(tidyverse)
library(conflicted)
library(sf)
library(here)

conflict_prefer_all("dplyr", quiet = TRUE)
conflict_prefer("aes", winner = "ggplot2", quiet = TRUE)

load_merge_bc_tifs <- function(){
  bath1 <- rast(here("data/bc_50m/NHS-D2830_50M_E25833.tif"))
  bath2 <- rast(here("data/bc_50m/NHS-D2831_50M_E25833.tif"))
  bath3 <- rast(here("data/bc_50m/NHS-D2832_50M_E25833.tif"))
  bath4 <- rast(here("data/bc_50m/NHS-D2833_50M_E25833.tif"))
  bath5 <- rast(here("data/bc_50m/NHS-D2930_50M_E25833.tif"))
  bath6 <- rast(here("data/bc_50m/NHS-D2931_50M_E25833.tif"))
  bath7 <- rast(here("data/bc_50m/NHS-D2932_50M_E25833.tif"))
  bath8 <- rast(here("data/bc_50m/NHS-D2933_50M_E25833.tif"))
  bath9 <- rast(here("data/bc_50m/NHS-D2829_50M_E25833.tif"))
  bath10 <- rast(here("data/bc_50m/NHS-D2929_50M_E25833.tif"))

  bath_col <- sprc(bath1, bath2, bath3, bath4, bath5, bath6, bath7, bath8, bath9, bath10)
  bath <- merge(bath_col)
  bath
}

change_projection <- function(bath){
  bath_wgs84 <- bath |>
    project("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  bath_wgs84
}

crop_projection <- function(bath_wgs84, extent = ext(14.5, 17, 69, 70)){
  bath_cropped <- crop(bath_wgs84, extent) 
  bath_cropped
}

plot_basemap <- function(bath_crpd){
map_base <- ggplot() +
  geom_spatraster(data = bath_crpd) +
  # geom_polygon(aes(x = c(14.5, 15.98, 14.5, 14.5),
  #                  y = c(69.68, 70, 70, 69.68)),
  #                 colour = "dodgerblue4", fill = "dodgerblue4") +
  scale_fill_gradient(low = "dodgerblue4", high = "lightskyblue") +
  theme_bw() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(axis.title = element_blank()) +
  NULL

map_base
}

create_basemap <- function(extent_basemap = ext(14.5, 17, 69, 70)){
  load_merge_bc_tifs() |>
    change_projection() |>
    crop_projection(extent = extent_basemap) |>
    plot_basemap()
}

trip_nr_map <- function(trip_number) {
  track_trip <- read_sf(here("data/trips_ais.geojson")) |>
    filter(trip_id == trip_number) |>
    st_coordinates(geometry) |>
    as.data.frame()
  
  extent_map <- ext(min(track_trip$X) - 0.05,
                    max(track_trip$X) + 0.05,
                    min(track_trip$Y) - 0.02,
                    max(track_trip$Y) + 0.02)
  
  sightings_trip <- read_delim(here("data/sighting_data.csv"),
                               delim = ",",
                               show_col_types = FALSE) |>
    filter(trip_id == trip_number)
  
  create_basemap(extent_basemap = extent_map) +
    geom_path(track_trip,
              mapping = aes(x = X, y = Y)) +
    geom_point(sightings_trip, 
               mapping = aes(x = long_dec, y = lat_dec, colour = species)) 
}
