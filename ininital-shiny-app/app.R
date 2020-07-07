#script to make an interactive shiny map.

#this is going to be very preliminary

library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(tmap)
library(leaflet)
library(sf)
library(janitor)
library(USAboundaries)
library(here)
library(leaflet)

# estuary_reactive

# Doing the data processing
# Would be better to save/cache the data somewhere and read it in instead


SNAPP_estuary_points <- read_sf(dsn = here("locations"), layer = "FINAL_SNAPP_ESTUARIES_POINTS-44") %>%
  select(-NCEASmap, - Latitude, -Longitude) %>%
  gather(score_type, score, -Name, -geometry)

basemap_streets <- tm_basemap("Esri.WorldStreetMap")


##### Beginning of the App


ui <- fluidPage(
  titlePanel("Conservation Aquaculture Interactive Map"),
  sidebarLayout(
    sidebarPanel("Score Widget",
                 selectInput(inputId = "aqua_score",
                             label = "Choose Score",
                             choices = unique(SNAPP_estuary_points$score_type)
                 ),
                 sliderInput(inputId = "aqua_score_range",
                             label = "Choose Score Range",
                             min = 0, max = 1, value = c(0,1)
                             )
                 
    ),
    mainPanel("Output Map",
              leafletOutput(outputId = "Score_Map", 
                            width = 550, height = 800
                            ))
  )
)



server <- function(inputs, outputs) {
  # Filter the data
  estuary_shiny <- reactive({
    SNAPP_estuary_points %>%
      filter(score_type %in% (inputs$aqua_score)) %>%
      filter(score %in% (inputs$aqua_score_range))
  })
  
  # Render the map
  outputs$Score_Map <- renderLeaflet({
    
    SNAPP_estuary_map_points <- tm_shape(estuary_shiny()["score"]) +
      tm_dots(labels = "Name", col = "score", n = 5, style = "pretty", size = 0.25) +
      basemap_streets
    tmap_leaflet(SNAPP_estuary_map_points)
    
  })
  
}

shinyApp(ui = ui, server = server)