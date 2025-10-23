library(here)
library(tidyverse)
library(conflicted)
library(readxl)
library(janitor)

conflict_prefer_all("dplyr", quiet = TRUE)

# Load data from Zoe's file ####
pm <- read_xlsx(here("data/Zoes_data/Database_2024.xlsx"), 
                sheet = "Sperm_Whales") |>
  clean_names()

otherspecies <- read_xlsx(here("data/Zoes_data/Database_2024.xlsx"), 
                          sheet = "Other_species") |>
  clean_names() 

trips_w2s <- read_xlsx(here("data/Zoes_data/Database_2024.xlsx"), 
                       sheet = "Trips",
                       col_types = c("numeric", "numeric", "numeric", "date",
                                     "date", "date", "date", "date", "text",
                                     "text", "text", "numeric", "numeric",
                                     "numeric", "numeric", "text", "text", 
                                     "text", "date", "text", "text",
                                     "text", "text", "text", "text")) |>
  clean_names() |>
  mutate(time_start = format(time_start, format = "%H:%M:%S")) |>
  mutate(time_start = as.POSIXct(paste(date, time_start), 
                                 format = "%Y-%m-%d %H:%M:%S")) |>
  mutate(time_end = format(time_end, format = "%H:%M:%S")) |>
  mutate(time_end = as.POSIXct(paste(date, time_end), 
                               format = "%Y-%m-%d %H:%M:%S")) |>
  mutate(mmsi = case_when(boat == "3" ~ "257856500",
                          boat == "7" ~ "259014440",
                          boat == "8" ~ "259022180",
                          boat == "9" ~ "258029590",
                          boat == "10" ~ "257518500",
                          boat == "13" ~ "259032110",
                          TRUE ~ "other")) |>
  mutate(platform = "Whale2Sea")

boats_id <- read_xlsx(here("data/Zoes_data/Database_2024.xlsx"), 
                      sheet = "Boats_ID")


# Coordinate conversion function XX°XX.XXX' to XX.XXXXX° ####
lat_dec <- function(dms_string) {
  # Extract components
  deg <- parse_number(dms_string)
  min <- parse_number(gsub("^...", "", dms_string))
  
  # Convert to decimal degrees
  dec_deg <- deg + min / 60
  
  # Handle potential NA values
  result <- ifelse(is.na(dec_deg), NA, dec_deg)
  
  dec_deg
}


# Coordinate conversion function ####
latitude <- function(coordinate) {
  # Extract integer part
  integer_part <- as.numeric(gsub("\\..*", "", coordinate))
   
  # Extract decimal part
  decimal_part <- gsub("^[^\\.]*\\.", ".", coordinate)
  decimal_part <- as.numeric(gsub("^\\.", "", decimal_part))
}


# Convert coordinates XX°XX.XXX' to XX.XXXXX° ####
otherspecies$lat_dec <- lat_dec(otherspecies$n_latitude)
otherspecies$long_dec <- lat_dec(otherspecies$e_longitude) 
pm$lat_dec <- lat_dec(pm$n_latitude)
pm$long_dec <- lat_dec(pm$e_longitude)


# Merging pm with other species ####
pm$species <- "Sperm whale"
otherspecies <- subset(otherspecies, select = c(date, 
                                                trip_id,
                                                species, 
                                                lat_dec, 
                                                long_dec)) |>
  mutate(species = fct_recode(species, 
             "Killer whale" = "Killer Whales",
             "Fin whale" = "Fin Whales",
             "Fin whale" = "Fin Whlaes",
             "Fin whale" = "Fin Whale",
             "Humpback whale" = "Humpback Whales",
             "Humpback whale" = "Humpback Whale",
             "Risso's dolphin" = "Risso's Dolphins",
             "Long-finned pilot whale" = "Pilot Whales",
             "Long-finned pilot whale" = "Pilot whale",
             "Atlantic white-sided dolphin" = "Atlantic White Sided Dolphins",
             "Minke whale" = "Minke Whale",
             "Minke whale" = "Minke Whales",
             "White-beaked dolphin" = "White beaked dolphins",
             "White-beaked dolphin" = "White Beaked Dolphins",
             "Harbor porpoise" = "Harbor Porpoise",
             "Harbor porpoise" = "Harbor Porpoises",
             "Harbor porpoise" = "Harbour Porpoise",
             "Harbor porpoise" = "Harbour Porpoises"))
pm <- subset(pm, select = c(date, 
                            trip_id,
                            species, 
                            lat_dec, 
                            long_dec))
sghts_w2s <- rbind(otherspecies, pm) 
sghts_w2s$platform <- "Whale2Sea"


# Load data from Whale Safari trip reports ####
ws <- read_xlsx(here("data/WS_data/trip_reports_2003_2023_clean.xlsx"),
                col_types = c()) |>
  clean_names() |>
  rename(id = trip_id)


# Create Whale Safari trip list ####
trips_wsa <- ws |>
  mutate(date = ymd(date),
         platform = "Whale Safari Andenes",
         trip_id_wsa = id,
         id = 10000 + as.numeric(factor(id)))


# Create Whale Safari sighting list ####
sghts_wsa <- ws |>
  mutate(trip_id = 10000 + as.numeric(factor(id))) |>
  select(date, trip_id, species_common_name, lat_dec, long_dec) |>
  mutate(date = ymd(date),
         species_common_name = as.factor(species_common_name),
         species_common_name = fct_recode(species_common_name,
            "Atlantic white-sided dolphin" = "Atlantic White Sided Dolphins",
            "Harbor porpoise" = "Harbour Porpoise",
            "Harbor porpoise" = "Harbour porpoise",
            "Humpback whale" = "Humpback Whale",
            "Long-finned pilot whale" = "Long-finned pilot whaleilot whale",
            "Minke whale" = "Minke Whale",
            "Risso's dolphin" = "Rissos dolphin")) |>
  rename(species = species_common_name) 
  
sghts_wsa$platform <- "Whale Safari Andenes"


# Combine W2S + WSA ####
sghts <- rbind(sghts_w2s, sghts_wsa)

trips_zoe_comb <- trips_w2s |>
  select(date, id, platform)
trips_wsa_comb <- trips_wsa |>
  select(date, id, platform)

trips_comb <- rbind(trips_zoe_comb, trips_wsa_comb)



# Creating files ####
write.csv(sghts_w2s, file = here("data/Zoes_data/sighting_data_W2S.csv"),
          row.names = FALSE)
write.csv(trips_w2s, file = here("data/Zoes_data/trips_W2S.csv"),
          row.names = FALSE)
write.csv(sghts_wsa, file = here("data/WS_data/sighting_data_WSA.csv"),
          row.names = FALSE)
write.csv(trips_wsa, file = here("data/WS_data/trips_WSA.csv"),
          row.names = FALSE)
write.csv(sghts, file = here("data/sighting_data.csv"),
          row.names = FALSE)
write.csv(trips_comb, file = here("data/trips.csv"),
          row.names = FALSE)

# ID-link Whale Safari Andenes ####
id_link <- trips_wsa |>
  select(trip_id_wsa, id) |>
  distinct() 
write.csv(id_link, file = here("data/WS_data/ID_key.csv"),
          row.names = FALSE)

