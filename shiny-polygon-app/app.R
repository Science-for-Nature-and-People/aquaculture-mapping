# Shiny app using the polygon data

# This looks like a usefule resource for integrating shiny with leaflet: https://cran.r-project.org/web/packages/tmap/vignettes/tmap-getstarted.html#interactive-maps

library(tidyverse)
library(shiny)
library(shinythemes)
library(shinydashboard)
library(sf)
library(janitor)
library(here)
library(leaflet)
library(tmap)
library(tmaptools)

SNAPP_estuary_polygons <- read_sf(dsn = here("locations"), layer = "NEAR_Final_SNAPP_Estuary_Polygons_060420") %>%
  select(-NCEAM, -OBJECTID, -Shape_Leng, -Shape_Area)

  
polygons_gather <- SNAPP_estuary_polygons %>%
  gather(score_field, score, -Estuary_Na, -geometry)

basemap_streets <- tm_basemap("Esri.WorldStreetMap")

## Beginning the App

ui <- fluidPage(
  titlePanel("Conservation Aquaculture Interactive Map"),
  sidebarLayout(
    sidebarPanel("Score Widget",
                 selectInput(inputId = "aqua_score",
                             label = "Choose a field",
                             choices = unique(polygons_gather$score_field)
                 )
    ),
    mainPanel("Output Map",
              leafletOutput(outputId = "Score_Map"))
    
  )
  
)


server <- function(inputs, outputs) {
  estuary_polygons_shiny <- reactive({
    polygons_gather %>%
      filter(score_field %in% (inputs$aqua_score))

  })
  
  outputs$Score_Map <- renderLeaflet({
    
    sf::st_is_valid(estuary_polygons_shiny())
    
    SNAPP_estuary_map_polygons <- tm_shape(estuary_polygons_shiny()) +
      tm_polygons(col = "score") +
      basemap_streets
    tmap_leaflet(SNAPP_estuary_map_polygons)
    
  })
  
  
  
}

shinyApp(ui = ui, server = server)
