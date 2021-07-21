# Preamble ----------------------------------------------------------------
library(ggplot2)

# Auxiliary functions -----------------------------------------------------
theme_map    <- function() {
  theme_bw() +
    theme(
      axis.line         = element_blank(),
      axis.text.x       = element_blank(),
      axis.text.y       = element_blank(),
      axis.ticks        = element_blank(),
      axis.title.x      = element_blank(),
      axis.title.y      = element_blank(),
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank(),
      plot.background   = element_blank(),
      legend.position   = "bottom",
      legend.text.align = 0.5,
      legend.key.width  = unit(2, "cm")
    )
}

# Read data ---------------------------------------------------------------
ymPlotDF <- read.csv(file.path(".", "data", "ym", "basswood_2020.csv"))

# Munge data --------------------------------------------------------------
#

# Visualize data ----------------------------------------------------------
# Create plot object
ymMapPlot <-
  ggplot(na.omit(ymPlotDF)) + # Omits 95 pixels without information
  geom_polygon(aes(
    x     = long,           # Longitudes in the horizontal axis
    y     = lat,            # Latitude in the vertical axis
    group = group,          # More than one data frame row belong to the same poly
    fill  = yieldMgHaMean   # Fill the polygon with the yield mean
  )) +
  scale_fill_distiller(     # Palette from https://colorbrewer2.org/#type=sequential&scheme=Greens&n=3
    palette   = "Greens",   # 'cause chlorophyll
    direction = 1,          # Darker is higher
    limits    = c(0, NA)    # Set color bar minimum at zero, max TBD by ggplot
  ) +
  labs(
    title    = "Yield map",
    subtitle = "Basswood 2020 (Maize)",
    fill     = expression("Yield in" ~ MgHa^-1 ~ "Darker is higher")
  ) +
  theme_map() +
  theme( # Play with background color to decide if gray helps with contrast
    panel.background = element_rect(fill = "gray80")
  )

# Print plot object
plot(ymMapPlot)

# Write to disk
# ggsave(
#   filename = "",        # Path to file
#   plot     = ymMapPlot,
#   device   = "png",     # Might be a bit too loaded for a pdf reader
#   width    = 16,        # 16:9 widescreen aspect ratio
#   height   = 9,
#   dpi      = 300
# )
