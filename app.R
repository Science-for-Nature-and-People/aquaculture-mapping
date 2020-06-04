#script to make an interactive shiny map.

#this is going to be very preliminary

library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(tmap)
library(leaflet)

estuary_reactive_lat_lon

ui <- fluidPage(
  titlePanel("title"),
  sidebarLayout(
    sidebarPanel("Score Widget",
                 radioButtons(inputId = "score_select",
                              label = "Choose Score Type",
                              choices = unique(estuary_reactive_format$score_type)
                              )
                 ),
    mainPanel("Output Map",
              plotOutput(outputId = "Score_Map"))
  )
)


server <- function(inputs, outputs) {
  
estuary_shiny <- reactive({
  estuary_reactive_format %>%
    filter(score_type  == input$score_select)
})

outputs$Score_Map <- renderPlot({
  tm_shape(estuary_shiny) +
    tm_dots(labels = "estuary_or_subbasin", col = "score") +
    tmap_mode("view")
  
})
  
}

shinyApp(ui = ui, server = server)
