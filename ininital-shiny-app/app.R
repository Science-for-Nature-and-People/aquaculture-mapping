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
SNAPP_estuary_points <- read_sf(dsn = here("locations"), layer = "SNAPP_estuary_centroids_final") %>%
 
  mutate("Ecological Priority" = eclgcl_, "Community Restoration" = cmmnty_r, "Community Harvest" = cmmnty_h, "Commercial Growers" = cmmrcl_, "Ecological Priority2" = eclgcl_, "Community Restoration2" = cmmnty_r, "Community Harvest2" = cmmnty_h, "Commercial Growers2" = cmmrcl_
         ) %>%
  gather(score_type, score, -estuary, -geometry, -"Ecological Priority", -"Community Restoration", -"Community Harvest", -"Commercial Growers", -"Ecological Priority2", -"Community Restoration2", -"Community Harvest2", -"Commercial Growers2", -mp_nmbr
         ) 

basemap_streets <- tm_basemap("Esri.WorldStreetMap")
basemap_imagery <- tm_basemap("Esri.WorldImagery")
basemap_physical <- tm_basemap("Esri.WorldPhysical")
basemap_gray <- tm_basemap("Esri.WorldGrayCanvas")
basemap_topo <- tm_basemap("Esri.WorldTopoMap")



text <- as.character("Click points to see more information.")



##### Beginning of the App


ui <- fluidPage(
  titlePanel("Conservation Aquaculture Interactive Map"),
  sidebarLayout(
    sidebarPanel(
                 #This dropdown controls the category that is being used to color the estuaries
                  selectInput(inputId = "aqua_score_color",
                            label = "Select Cateory for Color Ramp",
                            choices = c("Ecological Priority"="Ecological Priority2", "Community Restoration"="Community Restoration2", "Community Harvest"="Community Harvest2", "Commercial Growers"="Commercial Growers2")
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
                             choices = c("Ecological Priority" = "eclgcl_","Community Restoration" = "cmmnty_r", "Community Harvest" = "cmmnty_h", "Commercial Growers" = "cmmrcl_")
                 ),
                 
                 #This is a slide tool that filters the scores of the estuaries
                 sliderInput(inputId = "slider_score_range",
                             label = "Filter Score Range",
                             min = 0, max = 1, value = c(0,1), step = 0.1, ticks = TRUE
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
      select(inputs$aqua_score_color, inputs$aqua_score_size, estuary,  score, score_type, "Ecological Priority","Community Restoration", "Community Harvest", "Commercial Growers") %>%
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
        palette = case_when( #tmaptools::palette_explorer() to find other palettes
          inputs$aqua_score_color == "Ecological Priority2" ~ "Greens",
          inputs$aqua_score_color == "Community Restoration2" ~ "Blues",
          inputs$aqua_score_color == "Community Harvest2" ~ "Oranges",
          inputs$aqua_score_color == "Commercial Growers2" ~ "Purples"
          ), 
        n = 4,
        contrast = c(0.1, 0.8),
        title = case_when(
          inputs$aqua_score_color == "Ecological Priority2" ~ "Ecological Priority Score",
          inputs$aqua_score_color == "Community Restoration2" ~ "Community Restoration Score",
          inputs$aqua_score_color == "Community Harvest2" ~ "Community Harvest Score",
          inputs$aqua_score_color == "Commercial Growers2" ~ "Commercial Growers Score"
        ),
        id = "estuary",
        popup.vars = c("Ecological Priority", "Community Restoration", "Community Harvest", "Commercial Growers"),
        clustering = FALSE #This if for clustering the points when zoomed out
        #legend.size.show = TRUE
        ) +
      tm_legend(
        legend.show = TRUE
                ) +
      basemap_topo +
      basemap_streets +
      basemap_imagery +
      basemap_physical +
      basemap_gray 
    tmap_leaflet(SNAPP_estuary_map_points)

    
  })
  
}

shinyApp(ui = ui, server = server)
