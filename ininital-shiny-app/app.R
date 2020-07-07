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
data_goals <- read_csv(here("data", "data_goals.csv"))
data_end_users <- read_csv(here("data", "data_end_users.csv"))
scores_clean <- read_csv(here("data", "scores_clean.csv"))
data_scores <- full_join(data_goals, scores_clean)
estuary_sf <- data_scores %>%
  drop_na("Long") %>%
  st_as_sf(coords = c("Long", "Lat"), crs = 4326) %>%
  clean_names()
estuary_reactive <- estuary_sf %>%
  gather(score_type, score, -estuary_or_subbasin, -geometry)

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
              leafletOutput(outputId = "Score_Map"))
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
      tm_dots(label = "Name", col = "score") +
      basemap_streets
    tmap_leaflet(SNAPP_estuary_map_points)
    
    #ggplot() +
     # geom_sf(data = us_boundaries()) +
      #geom_sf(data = estuary_shiny()["score"], aes(color = score, size = score)) +
      #scale_color_gradientn(colors = c(
      #  "red",
       # "green",
        #"blue"
    #  )) +
      #coord_sf(xlim = c(-124.5, -117.5), ylim = c(33, 48.5)) +
      #theme_minimal() +
      #scale_x_continuous(breaks = c(-124, -121, 118))
    #plot(estuary_shiny()["score"])
  })
  
}

shinyApp(ui = ui, server = server)