library(shiny)
library(sf)
library(raster)
library(tmap)
library(dplyr)
library(stringr)


rasters = list.files("./plots/hatchraster/", ".tif", full.names = TRUE)
rdates = as.Date(str_extract(rasters, "\\d+(?=.tif)"), "%Y%m%d")



# Define UI for app that draws a histogram ----
ui <- fluidPage(
  absolutePanel(
    top = 0,
    bottom = 0,
    left = 0,
    right = 0,
    tmapOutput(height = "100%", outputId = "map")
    
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
  output$map <- renderTmap({
    
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
    
    pal = RColorBrewer::brewer.pal(4, "Pastel1")
    unique(h[])
    
    tm_shape(hatch) +
      tm_raster(
        title = sprintf("Predicted hatch status\nat %s",
                        format(max(rdates), "%d %b %Y")),
        palette = pal,
        style = "cat",
        labels = labs,
        alpha = 0.5
      ) +
      tm_view(view.legend.position = c("left", "bottom"))
    
  })
  
}

shinyApp(ui = ui, server = server)
