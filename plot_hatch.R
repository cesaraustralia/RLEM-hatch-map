################## PLOT FUNCTION ########################
plot_hatch <- function(raster_file = NULL) {
  library(sf)
  library(raster)
  library(tmap)
  library(dplyr)

  if (is.null(raster_file)) {
    raster_files <- list.files("plots/hatchraster/", pattern = ".tif$", full.names = TRUE)
    raster_file <- raster_files[length(raster_files)]
  }

  h <- raster(raster_file)
  hatch <- h[]
  aus <- read_sf("./aus/aus.shp")

  # plot model output
  currentday <- Sys.Date() %>%
    format("%j") %>%
    as.numeric()
  hatchtime <- tibble(hatchtime = hatch[] - currentday) %>%
    mutate(buckets = case_when(
      hatchtime < -14 ~ 1,
      hatchtime < 0 ~ 2,
      hatchtime >= 0 ~ 3,
      # hatchtime >= 14 ~ 4
    )) %>%
    pull(buckets)

  h[] <- hatchtime
  h[hatch == 0] <- NA
  # h[] = hatch>0
  labs <- c(
    "hatched more than 2 weeks ago",
    "hatched less than 2 weeks ago",
    "hatching within next 2 weeks"
  )

  pal <- RColorBrewer::brewer.pal(4, "Pastel1")
  unique(h[])

  for (region in c("East", "West")) {
    if (region == "East") mybbox <- st_bbox(c(xmin = 130, xmax = 155, ymax = -30, ymin = -45), crs = st_crs(4326))
    if (region == "West") mybbox <- st_bbox(c(xmin = 110, xmax = 130, ymax = -25, ymin = -40), crs = st_crs(4326))

    tm1 <- aus %>%
      tm_shape(bbox = mybbox) +
      tm_fill(col = "white") +
      tm_shape(mask(h, aus)) +
      tm_raster(
        title = sprintf(
          "Predicted hatch status\nat %s",
          format(Sys.Date(), "%d %b %Y")
        ),
        palette = pal, style = "cat",
        labels = labs
      ) +
      tm_shape(aus) +
      tm_borders(lwd = 1, col = "black") +
      tm_layout(
        bg.color = "lightblue",
        legend.position =
          c(ifelse(region == "West", "right", "left"), "bottom")
      )
    tm1
    tmap_save(tm1, sprintf("plots/hatchc_%s_%s.png", Sys.Date(), region), width = 7, height = 6)
  }
}

plot_hatch()
