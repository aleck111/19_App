library(shiny)
library(bslib)
library(sf)
library(leaflet)
library(here)
library(tidyverse)
library(shinydashboard)
library(janitor)
library(conflicted)

conflict_prefer_all("dplyr", quiet = TRUE)

# UI -----
ui <- navbarPage(
  
  title = "Sighting history Andenes, Norway",
  tabPanel(
    "Encounter location & time",
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          width = 2, 
          checkboxGroupInput(inputId = "species",
                             label = "Species",
                             choices = list("Atlantic white-sided dolphin",
                                            "Fin whale",
                                            "Harbor porpoise",
                                            "Humpback whale",
                                            "Killer whale",
                                            "Long-finned pilot whale",
                                            "Minke whale",
                                            "Risso's dolphin",
                                            "Sperm whale",
                                            "White-beaked dolphin"),
                             selected = c("Long-finned pilot whale", 
                                          "Risso's dolphin")),
          sliderInput(inputId = "yearRange",
                      label = 'Year range',
                      min = 2003,
                      max = 2025,
                      value = c(2021, 2025),
                      ticks = FALSE),
          checkboxGroupInput(inputId = "months",
                             label = "Month",
                             choices = list("January" = 1,
                                            "February" = 2,
                                            "March" = 3,
                                            "April" = 4,
                                            "May" = 5,
                                            "June" = 6,
                                            "July" = 7,
                                            "August" = 8,
                                            "Setptemper" = 9,
                                            "October" = 10,
                                            "November" = 11,
                                            "December" = 12),
                             selected = c(6, 7, 8, 9)),
          checkboxGroupInput(inputId = "sightplatform",
                             label = "Sighting platform",
                             choices = list("Whale2Sea" = "Whale2Sea",
                                            "Whale Safari" = "Whale Safari Andenes"),
                             selected = c("Whale2Sea")),
          # checkboxGroupInput(inputId = "AIS", 
          #                    label = "Boat tracks",
          #                    choices = list("Whale2Sea" = "Whale2Sea",
          #                                   "Whale Safari" = "Whale Safari"))
          ),
        
        mainPanel(
          fluidPage(
            # map
            leafletOutput("map"),
            
            # sightings per month ww distance vs. year
            plotOutput("sightdays"),
            
            # Note
            textOutput("note"))))))
  )

# Server ------
server <- function(input, output){
  # load data
  sightings <- read_delim(here("data/sighting_data.csv"), 
                          show_col_types = FALSE) 
  
  trips <- read_delim(here("data/trips.csv"),
                      show_col_types = FALSE)
  
  # w2s1 <- read_sf(here("data/AIS_tracks/w2s1.geojson"))
  # w2s2 <- read_sf(here("data/AIS_tracks/w2s2.geojson")) 
  # w2s3 <- read_sf(here("data/AIS_tracks/w2s3.geojson")) 
  # w2s4 <- read_sf(here("data/AIS_tracks/w2s4.geojson")) 
  # ah1 <- read_sf(here("data/AIS_tracks/ah1.geojson")) 
  # yam <- read_sf(here("data/AIS_tracks/yam.geojson")) 
  # 
  # ribs <- rbind(w2s1, w2s2, w2s3, w2s4, ah1, yam) |>
  #   mutate(platform = "Whale2Sea")
  # 
  # reine <- read_sf(here("data/AIS_tracks/reine.geojson")) |>
  #   mutate(platform = "Whale Safari")
  # 
  # ais <- rbind(ribs, reine)
  
  
  # filter data
  sightingsInput <- reactive({
    filter(sightings, 
           year(date) >= input$yearRange[1] & 
           year(date) <= input$yearRange[2] &
             month(date) %in% input$months) |>
      filter(platform %in% input$sightplatform) |>
      filter(species %in% input$species)})
  
  sightingDaysInput <- reactive({
    filter(sightings, 
           year(date) >= input$yearRange[1] & 
           year(date) <= input$yearRange[2] &
             month(date) %in% input$months) |>
      filter(platform %in% input$sightplatform) |>
      filter(species %in% input$species) |>
      select(date, species) |>
      distinct()})
  
  tripDaysInput <- reactive({
    filter(trips, 
           year(date) >= input$yearRange[1] & 
             year(date) <= input$yearRange[2] &
             month(date) %in% input$months) |>
      filter(platform %in% input$sightplatform) |>
      mutate(year = year(date), month = month(date), day = day(date)) |>
      select(year, month, day, date) |>
      distinct() |>
      group_by(year, month) |>
      summarise(date = date,
                tripdays = n(),
                .groups = "drop") |>
      mutate(year_month = format(date, "%Y-%m"))
      })
  
  # aisInput <- reactive({
  #   filter(ais,
  #          year(starttime) >= input$yearRange[1] &
  #          year(starttime) <= input$yearRange[2] &
  #            month(starttime) %in% input$months) |>
  #     filter(platform %in% input$AIS)})
  
  
  # Tab 1: 
  # Encounter location & time
  # Card 1: map output
  output$map <- renderLeaflet({
    
    # pal_ais <- colorFactor(palette = c("Whale2Sea" = "darkgrey",
    #                                    "Whale Safari" = "lightgrey"),
    #                        levels = c("Whale2Sea", "Whale Safari"))
    
    pal_species <- colorFactor(palette = c("Sperm whale" = "blue",
                                           "Atlantic white-sided dolphin" = "white",
                                           "Fin whale" = "lightgrey",
                                           "Harbor porpoise" = "black",
                                           "Humpback whale" = "green",
                                           "Killer whale" = "yellow",
                                           "Long-finned pilot whale" = "orange",
                                           "Minke whale" = "darkgrey",
                                           "Risso's dolphin" = "magenta",
                                           "White-beaked dolphin" = "red"),
                               levels = c("Sperm whale",
                                          "Atlantic white-sided dolphin",
                                          "Fin whale",
                                          "Harbor porpoise",
                                          "Humpback whale",
                                          "Killer whale",
                                          "Long-finned pilot whale",
                                          "Minke whale",
                                          "Risso's dolphin",
                                          "White-beaked dolphin"))
    
    # plot map                               
    leaflet() |>
      addTiles() |>
      # addPolylines(data = aisInput(),
      #              weight = 1,
      #              color = ~pal_ais(platform)) |>
      addCircleMarkers(data = sightingsInput(),
                       ~long_dec, 
                       ~lat_dec, 
                       popup = ~paste("<strong> Species: </strong>", species, 
                                      "<br>", 
                                      "<strong> Date: </strong>", date,
                                      "<br>",
                                      "<strong> Platform: </strong>", platform,
                                      "<br>",
                                      "<strong> Trip ID: </strong>", trip_id),
                       color = ~pal_species(species),
                       radius = 0.4) |>
      setView( lng = 16, lat = 69.5, zoom = 6.8 ) |>
      addProviderTiles("Esri.WorldImagery")})
  
  # Card 2: sightings per month output 
  output$sightdays <- renderPlot({

    # Sightings + trips plot
    ggplot() +
      geom_bar(data = sightingDaysInput(), 
               aes(x = format(as.Date(date), "%Y-%m"), 
                   fill = species)) + 
      geom_point(data = tripDaysInput(),
                aes(x = year_month,
                    y = tripdays),
                color = "black") +
      labs(color = "Species", x = "Year - month") +
      scale_y_continuous(name = "Sighting days / months ",
                         sec.axis = sec_axis(~., name = "Days with trips")
      ) +
      scale_fill_manual(values = c("Atlantic white-sided dolphin" = "white",
                                   "Fin whale" = "lightgrey",
                                   "Harbor porpoise" = "black",
                                   "Humpback whale" = "green",
                                   "Killer whale" = "yellow",
                                   "Long-finned pilot whale" = "orange",
                                   "Minke whale" = "darkgrey",
                                   "Risso's dolphin" = "magenta",
                                   "Sperm whale" = "blue",
                                   "White-beaked dolphin" = "red")) +
      theme_bw(base_size = 18) +
      theme(legend.position = "bottom",
            axis.text.x = element_text(angle = 90))
  })
  
  # Card 3: 
  output$note <- renderText({
    ("Whale2Sea data collected and compiled by Zoë Morange with the assistance of Elena Catasús")
  })

}

shinyApp(ui = ui, server = server)