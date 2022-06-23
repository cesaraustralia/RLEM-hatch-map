library(shiny)
library(sf)
library(raster)
library(tmap)
library(dplyr)
library(stringr)
library(leaflet)


rasters = list.files("./plots/hatchraster/", ".tif", full.names = TRUE)
rdates = as.Date(str_extract(rasters, "\\d+(?=.tif)"), "%Y%m%d")



# Define UI for app that draws a histogram ----
ui <- fluidPage(
  absolutePanel(
    top = 0,
    bottom = 0,
    left = 0,
    right = 0,
    leafletOutput(height = "100%", outputId = "map")
    
  ),
  HTML('<br>'),
  column(
    width = 3,
    offset = 9,
    selectInput(
      inputId = "date",
      label = 'Simulation date',
      choices = rdates,
      selected = max(rdates)
    )
    
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      addMarkers(lng=174.768, lat=-36.852, popup="The birthplace of R")
  })
  
}

shinyApp(ui = ui, server = server)
