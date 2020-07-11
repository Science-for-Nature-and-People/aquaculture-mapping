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
                             choices = c(Ecology = "Ecol1", Restoration = "Resto1", Harvest = "Harvest1", "Community Engagement" = "Comm1")
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
      filter(score_type %in% (inputs$aqua_score)) #%>%
      #filter(score %in% (inputs$aqua_score_range))
  })
  
  # Render the map
  outputs$Score_Map <- renderLeaflet({
    
    SNAPP_estuary_map_points <- tm_shape(estuary_shiny()) +
      tm_dots(col = "score", style = "fixed", n = 4, breaks = c(0, 0.25, 0.5, 0.75, 1), labels = c("0 - 0.25", "0.25 - 0.5", "0.5 - 0.75", "0.75 - 1"), size = 0.25, palette = "Purples", title = "Score") +
      basemap_streets
    tmap_leaflet(SNAPP_estuary_map_points)
    
  })
  
}

shinyApp(ui = ui, server = server)