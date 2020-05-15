#script to make an interactive shiny map.

#this is going to be very preliminary

library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(tmap)
library(leaflet)

estuary_reactive

ui <- fluidPage(
  titlePanel("title"),
  sidebarLayout(
    sidebarPanel("Score Widget",
                 radioButtons(inputId = "score",
                              label = "Choose Score",
                              choices = unique(estuary_reactive$score_type)
                              )
                 ),
    mainPanel("Output Map",
              plotOutput(outputId = "Score_Map"))
  )
)


server <- function(inputs, outputs) {
  
estuary_shiny <- reactive({
  estuary_reactive %>%
    filter(score_type %in% (input$score))
})

outputs$Score_Map <- renderPlot({
  estuary_shiny()
})
  
}

shinyApp(ui = ui, server = server)
