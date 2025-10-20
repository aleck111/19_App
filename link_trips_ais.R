library(sf)
library(tidyverse)
library(here)

##### run datasheets.R first to create relevant files if not already available #####

# read Zoë's data and convert times to UTC ####
trips_zoe <- read_delim(here("data/Zoes_data/trips_w2s.csv"),
                        show_col_types = FALSE) |>
  mutate(time_start = force_tz(time_start, "Europe/Oslo")) |>
  mutate(time_end = force_tz(time_end, "Europe/Oslo")) |>
  mutate(time_start_utc = force_tzs(time_start, 
                                    tzone = "Europe/Oslo",
                                    tzone_out = "UTC")) |>
  mutate(time_end_utc = force_tzs(time_end, 
                                  tzone = "Europe/Oslo",
                                  tzone_out = "UTC")) 


# read Whale Safari data and convert times to UTC ####
trips_ws <- read_delim(here("data/WS_data/trips_WSA.csv"),
                       show_col_types = FALSE) |>
  mutate(date = ymd(date),
         time_start_utc = ymd_hm(paste0(date, 
                                        substr(time_start, 1, 4))),
         time_end_utc = ymd_hm(paste0(date, 
                                      substr(time_end, 1, 4))),
         mmsi = case_when(vessel == "Reine" ~ "257518500",
                          TRUE ~ "other")) 
                               


# function to connect the ais data of a selected boat to Zoë's data ####
filter_ais <- function(boat_name, trips_zoe){
  # connect mmsi to boat
  mmsi_input <- case_when(boat_name == "yam" ~ "257856500",
                          boat_name == "w2s1" ~ "259014440",
                          boat_name == "w2s2" ~ "259022180",
                          boat_name == "ah1" ~ "258029590",
                          boat_name == "reine" ~ "257518500",
                          boat_name == "w2s3" ~ "259032110")
  
  # read ais files and force UTC as time zone
  filename <- paste("data/AIS_tracks/", boat_name, ".geojson", sep = "")
  
  boat_ais <- read_sf(here(filename)) |>
    mutate(starttime = force_tz(starttime, "UTC")) |>
    mutate(endtime = force_tz(endtime, "UTC"))
  
  # split Zoë's data into separate variables for each vessel
  boattrips_zoe <- trips_zoe |>
    filter(mmsi == mmsi_input) |>
    rename(trip_id = id) |>
    select(trip_id, time_start_utc, time_end_utc)
  
  # link Zoë's data to the ais data by creating all possible combinations 
  # and then filtering for the ones that have the same time range
  boattrips_zoe_ais <- boattrips_zoe |>
    crossing(boat_ais) |>
    filter(time_start_utc - hms("00:15:00") <= starttime 
           & time_end_utc + hms("00:15:00") >= endtime)
    # filter(time_start_utc - hms("00:15:00") <= starttime 
    #        & time_end_utc + hms("00:15:00") >= endtime)
  }


# run filter_ais function for all vessels ####
trips_yam <- filter_ais("yam", trips_zoe) |>
  mutate(platform = "Whale2Sea")
trips_w2s1 <- filter_ais("w2s1", trips_zoe) |>
  mutate(platform = "Whale2Sea")
trips_w2s2 <- filter_ais("w2s2", trips_zoe) |>
  mutate(platform = "Whale2Sea")
trips_ah1 <- filter_ais("ah1", trips_zoe) |>
  mutate(platform = "Whale2Sea")
trips_w2s3 <- filter_ais("w2s3", trips_zoe) |>
  mutate(platform = "Whale2Sea")
trips_reine <- rbind(filter_ais("reine", trips_zoe),
                     filter_ais("reine", trips_ws)) |>
  mutate(platform = "Whale Safari")


# combine trips for the different vessels to a single variable ####
trips_ais_zoe <- rbind(trips_yam, 
                       trips_w2s1, 
                       trips_w2s2,
                       trips_ah1,
                       trips_w2s3)
trips_ais <- rbind(trips_ais_zoe,
                   trips_reine) |>
  distinct()

# write geojson files ####
st_write(trips_ais_zoe, 
         dsn = here("data/Zoes_data/zoes_trips_ais.geojson"),
         layer = "zoes_trips_ais.geojson",
         append = FALSE)
st_write(trips_reine, 
         dsn = here("data/WS_data/ws_trips_ais.geojson"),
         layer = "ws_trips_ais.geojson",
         append = FALSE)
st_write(trips_ais, 
         dsn = here("data/trips_ais.geojson"),
         layer = "trips_ais.geojson",
         append = FALSE)
