# Shiny app using the polygon data

library(tidyverse)
library(shiny)
library(shinythemes)
library(shinydashboard)
library(sf)
library(here)

SNAPP_estuary_polygons <- read_sf(dsn = here("locations"), layer = "NEAR_Final_SNAPP_Estuary_Polygons_060420") %>%
  select(-NCEAM, -Shape_Leng, -Shape_Area)

polygons_gather <- SNAPP_estuary_polygons %>%
  gather(score_field, score, -OBJECTID, -Estuary_Na)

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
              plotOutput(outputId = "Score_Map"))
    
  )
  
)


server <- function(inputs, outputs) {
  estuary_polygons_shiny <- reactive({
    polygons_gather %>%
      filiter(score_field %in% (inputs$aqua_score))
  })
  
  outputs$Score_Map <- renderPlot({
    ggplot() +
      geom_sf(data = estuary_polygons_shiny()["score"], aes(fill = score)) 
    
    
  })
}

shinyApp(ui = ui, server = server)
