#script to make an interactive shiny map.

#this is going to be very preliminary

library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(tmap)
library(tmaptools)
library(leaflet)
library(sf)
library(janitor)
library(USAboundaries)
library(here)
library(leaflet)
library(leafpop)

# estuary_reactive

# Doing the data processing
# Would be better to save/cache the data somewhere and read it in instead
#palette_explorer() # can be used to find better color palettes for the map. 


#This reads in the data and formats to be used by the shiny app
SNAPP_estuary_points <- read_sf(dsn = here("locations"), layer = "SNAPP_estuary_centroids") %>%
#############################################################################################################################
  
#This section of the code is only going to be for the preliminary data, with the final data this will not be needed. #This is to make all of the scores thata re less than zero go to zero.

    mutate(Ecol1 = case_when( 
    Ecol1 < 0 ~ 0,
    Ecol1 >= 0 ~ Ecol1
  )) %>%
  mutate(Restor1 = case_when(
    Restor1 < 0 ~ 0,
    Restor1 >= 0 ~ Restor1
  )) %>%
  mutate(Harvest1 = case_when(
    Harvest1 < 0 ~ 0,
    Harvest1 >= 0 ~ Harvest1
  )) %>%
  mutate(Comm1 = case_when(
    Comm1 < 0 ~ 0,
    Comm1 >= 0 ~ Comm1
  )) %>%
  
  
##########################################################################################################################################  
  mutate("Ecology" = Ecol1, "Restoration" = Restor1, "Harvest" = Harvest1, "Community" = Comm1, "Ecology2" = Ecol1, "Restoration2" = Restor1, "Harvest2" = Harvest1, "Community2" = Comm1 
         #"Ecology3" = Ecol1, "Restoration3" = Restor1, "Harvest3" = Harvest1, "Community3" = Comm1
         ) %>%
  select(-NCEAM) %>%
  gather(score_type, score, -Estuary_Na, -geometry, -Ecology, -Restoration, -Harvest, -Community, -Ecology2, -Restoration2, -Harvest2, -Community2
         #-Ecology3, -Restoration3, -Harvest3, -Community3
         ) 

basemap_streets <- tm_basemap("Esri.WorldStreetMap")

text <- as.character("Click points to see more information.")



##### Beginning of the App


ui <- fluidPage(
  titlePanel("Conservation Aquaculture Interactive Map"),
  sidebarLayout(
    sidebarPanel(
                 #This dropdown controls the category that is being used to color the estuaries
                  selectInput(inputId = "aqua_score_color",
                            label = "Cateory for Color Ramp",
                            choices = c(Ecology = "Ecology2", Restoration = "Restoration2", Harvest = "Harvest2", "Community" = "Community2")
                 ),
                 
                 #This dropdown controls the category that is being used to determine the size the estuary dots
                 #It has been decided that the size select is making the map to busy and more difficult to understand so it is being commented out.
                 #selectInput(inputId = "aqua_score_size",
                  #           label = "Category for Size",
                   #          choices = c(Ecology = "Ecology3", Restoration = "Restoration3", Harvest = "Harvest3", "Community" = "Community3")
                    #         ),
                 
      
                 #This dropdown controls the category that will be filtered with the slide tool
                 selectInput(inputId = "slider_select",
                             label = "Select Cateory for Slider",
                             choices = c(Ecology = "Ecol1", Restoration = "Restor1", Harvest = "Harvest1", "Community" = "Comm1")
                 ),
                 
                 #This is a slide tool that filters the scores of the estuaries
                 sliderInput(inputId = "slider_score_range",
                             label = "Score Range",
                             min = 0, max = 1, value = c(-1,1), step = 0.1, ticks = TRUE
                 )
                 
    ),
    mainPanel("Estuaries for Conservation Aquaculture",
              leafletOutput(outputId = "Score_Map", 
                            width = 550, height = 800
              ))
  )
)



server <- function(inputs, outputs) {
  # Filter and select the data that will be used in the map
  estuary_shiny <- reactive({
    SNAPP_estuary_points %>%
      filter(score_type %in% (inputs$slider_select)) %>%
      select(inputs$aqua_score_color, inputs$aqua_score_size, Estuary_Na,  score, score_type, Ecology, Restoration, Harvest, Community) %>%
      filter(score >= inputs$slider_score_range[1] & score <= inputs$slider_score_range[2]) 
  })
  
  # Render the map
  outputs$Score_Map <- renderLeaflet({
    
    #determines the the map desplay
    SNAPP_estuary_map_points <- tm_shape(estuary_shiny()) +
      tm_dots(
        size = 0.15,
        #sizes.legend = c(0.01, 0.25, 0.5, 0.75, 1),
        scale = 1,
        alpha = 1, #this controls the transparency of the points
        col = inputs$aqua_score_color,
        style = "fixed", 
        breaks = c(0, 0.25, 0.5, 0.75, 1),
        labels = c("0 - 0.25", "0.25 - 0.5", "0.5 - 0.75", "0.75 - 1"), 
        #palette = "Purples", #tmaptools::palette_explorer to find other palettes
        n = 4,
        contrast = c(0.1, 0.8),
        title = case_when(
          inputs$aqua_score_color == "Ecology" ~ "Ecology Score",
          inputs$aqua_score_color == "Restoration" ~ "Restoration Score",
          inputs$aqua_score_color == "Harvest" ~ "Harvest Score",
          inputs$aqua_score_color == "Community" ~ "Community Score"
        ),
        id = "Estuary_Na",
        popup.vars = c("Ecology", "Restoration", "Harvest", "Community"),
        clustering = FALSE #This if for clustering the points when zoomed out
        #legend.size.show = TRUE
        ) +
      tm_legend(
        legend.show = TRUE
                ) +
      basemap_streets
    tmap_leaflet(SNAPP_estuary_map_points)

    
  })
  
}

shinyApp(ui = ui, server = server)
