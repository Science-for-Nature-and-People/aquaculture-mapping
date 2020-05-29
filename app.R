#script to make an interactive shiny map.

#this is going to be very preliminary

library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(tmap)
library(leaflet)

# estuary_reactive

# Doing the data processing
# Would be better to save/cache the data somewhere and read it in instead
data_goals <- read_csv("data_goals.csv")
data_end_users <- read_csv("data_end_users.csv")
scores_clean <- read_csv("scores_clean.csv")
data_scores <- full_join(data_goals, scores_clean)
estuary_sf <- data_scores %>%
  drop_na("Long") %>%
  st_as_sf(coords = c("Long", "Lat"), crs = 4326) %>%
  clean_names()
estuary_reactive <- estuary_sf %>%
  gather(score_type, score, -estuary_or_subbasin, -geometry)


##### Beginning of the App


ui <- fluidPage(
  titlePanel("Conservation Aquaculture Interactive Map"),
  sidebarLayout(
    sidebarPanel("Score Widget",
                 selectInput(inputId = "aqua_score",
                              label = "Choose Score",
                              choices = unique(estuary_reactive$score_type)
                              )
                 ),
    mainPanel("Output Map",
              plotOutput(outputId = "Score_Map"))
  )
)



server <- function(inputs, outputs) {
  # Filter the data
  estuary_shiny <- reactive({
    estuary_reactive %>%
      filter(score_type %in% (inputs$aqua_score))
  })

  # Render the map
  outputs$Score_Map <- renderPlot({
    plot(estuary_shiny()["score"])
  })
  
}

shinyApp(ui = ui, server = server)
