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
#palette_explorer() # can be used to find better color palettes for the map. 


SNAPP_estuary_points <- read_sf(dsn = here("locations"), layer = "FINAL_SNAPP_ESTUARIES_POLYGONS-66") %>%
  st_transform(crs = 3310) %>%
  st_centroid(geometry) %>%
  mutate("Ecology" = Ecol1, "Restoration" = Restor1, "Harvest" = Harvest1, "Community" = Comm1, "Ecology2" = Ecol1, "Restoration2" = Restor1, "Harvest2" = Harvest1, "Community2" = Comm1) %>%
  select(-NCEAM) %>%
  gather(score_type, score, -Estuary_Na, -geometry, -Ecology, -Restoration, -Harvest, -Community, -Ecology2, -Restoration2, -Harvest2, -Community2) %>%
  mutate(score_type1 = score_type)

basemap_streets <- tm_basemap("Esri.WorldStreetMap")


##### Beginning of the App


ui <- fluidPage(
  titlePanel("Conservation Aquaculture Interactive Map"),
  sidebarLayout(
    sidebarPanel("Score Widget",
                 selectInput(inputId = "aqua_score",
                             label = "Select Score",
                             choices = c(Ecology = "Ecol1", Restoration = "Restor1", Harvest = "Harvest1", "Community" = "Comm1")
                 ),
                 
                 selectInput(inputId = "slider_select",
                             label = "select slider score",
                             choices = c(Ecology = "Ecology2", Restoration = "Restoration2", Harvest = "Harvest2", "Community" = "Community2")
                             ),
                 
                 
                 sliderInput(inputId = "aqua_score_range",
                             label = "Select Score Range",
                             min = -1, max = 1, value = c(-1,1), step = 0.25, ticks = TRUE
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
     #select(inputs$slider_select, Estuary_Na, score, Ecology, Restoration, Harvest, Community ) %>%
    filter(score >= inputs$aqua_score_range[1] & score <= inputs$aqua_score_range[2]) 
  })
  
  # Render the map
  outputs$Score_Map <- renderLeaflet({
    
    SNAPP_estuary_map_points <- tm_shape(estuary_shiny()) +
      tm_dots(
        col = "score", 
        style = "fixed", 
        breaks = c(-1, 0, 0.25, 0.5, 0.75, 1), 
        labels = c("-1 - 0", "0 - 0.25", "0.25 - 0.5", "0.5 - 0.75", "0.75 - 1"), 
        size = 0.25, 
        palette = "Purples", 
        title = "Score", 
        popup.vars = c("Ecology", "Restoration", "Harvest", "Community")
        ) +
      basemap_streets
    tmap_leaflet(SNAPP_estuary_map_points)
    
  })
  
}

shinyApp(ui = ui, server = server)