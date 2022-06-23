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
  output$map <- renderLeaflet({
    hatch = raster(rasters[which(rdates == input$date)])
    h = hatch[]
    
    # plot model output
    currentday = max(rdates) %>%
      format("%j") %>% as.numeric()
    hatchtime = tibble(hatchtime = h - currentday)  %>%
      mutate(buckets = case_when(hatchtime < -14 ~ 1,
                                 hatchtime <= 0   ~ 2,
                                 hatchtime > 0  ~ 3)) %>%
      pull(buckets)
    
    hatch[] = hatchtime
    hatch[h == 0] = NA
    labs = c(
      "hatched more than 2 weeks ago",
      "hatched less than 2 weeks ago",
      "hatching in coming weeks"
    )
    
    pal = RColorBrewer::brewer.pal(3, "Pastel1")
    
    leaflet() %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      addRasterImage(hatch, colors = pal, opacity = 0.7) %>% 
      addLegend(position = "bottomleft", colors = pal, labels = labs)
    
  })
  
}

shinyApp(ui = ui, server = server)
