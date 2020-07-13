# Shiny app using the polygon data

# This looks like a usefule resource for integrating shiny with leaflet: https://cran.r-project.org/web/packages/tmap/vignettes/tmap-getstarted.html#interactive-maps

library(tidyverse)
library(shiny)
library(shinythemes)
library(shinydashboard)
library(sf)
library(here)
library(leaflet)
library(tmap)
library(tmaptools)

SNAPP_estuary_polygons <- read_sf(dsn = here("locations"), layer = "NEAR_Final_SNAPP_Estuary_Polygons_060420") %>%
  select(-NCEAM)

  gather(score_field, score, -Estuary_Na, -geometry)
polygons_gather <- SNAPP_estuary_polygons %>%

basemap_streets <- tm_basemap("Esri.WorldStreetMap")

## Beginning the App

ui <- fluidPage(
  titlePanel("Conservation Aquaculture Interactive Map"),
  sidebarLayout(
    sidebarPanel("Score Widget",
                 selectInput(inputId = "aqua_score",
                             label = "Choose a field",
                             choices = uniquie(polygons_gather$score_field)
                 )
    ),
    mainPanel("Output Map",
              leafletOutput(outputId = "Score_Map"))
    
  )
  
)


server <- function(inputs, outputs) {
  estuary_polygons_shiny <- reactive({
    polygons_gather %>%
      filiter(score_field %in% (inputs$aqua_score))
  })
  
  outputs$Score_Map <- renderLeaflet({
    SNAPP_estuary_map_polygons <- tm_shape(estuary_polygons_shiny()) +
      tm_fill(col = "score") +
      basemap_streets
    tmap_leaflet(SNAPP_estuary_map_polygons)
    
    
  })
}

shinyApp(ui = ui, server = server)
