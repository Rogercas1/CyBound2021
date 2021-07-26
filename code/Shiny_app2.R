# Load packages
library(shiny)
library(shinythemes)
library(tidyverse)

source("theme_map.R")
source("emissions.R")

ymPlotDF <- read.csv(file.path("..", "data", "ym", "basswood_2020.csv")) %>%
  dplyr::mutate(wheatplot=yieldMgHaMean*0.9)

x <- c(0:350)
y<-emissions(x)
N.data<-data.frame(x,y)

ui <- fluidPage(theme = shinytheme("lumen"),
                titlePanel("N2O Emissions"),
                sidebarLayout(
                  sidebarPanel(
                    
                    # Select whether to overlay smooth trend line
                    checkboxInput(inputId = "smoother", label = strong("Use Nitrogen fertilizer"), value = FALSE),
                    
                    # Display only if the smoother is checked
                    conditionalPanel(condition = "input.smoother == true",
                                     sliderInput(inputId = "x", label = "Tons of Nitrogen Fertilizer",
                                                 min = 1, max = 350, value = 150, step = 1,
                                                 animate = animationOptions(interval = 100)),
                                     HTML("Select the x value for which you want a prediction of y.")
                    )
                  ),
                  
                  # Output: Description, lineplot, and reference
                  mainPanel(
                    plotOutput(outputId = "lineplot", height = "300px"), 
                    textOutput(outputId = "desc")
                  )
                )
)



# Define server function
server <- function(input, output) {
  
  emissions(x)
  
#ScatterPlot 
  output$lineplot <- renderPlot({
    
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
    
  })

  
 # Pull in description of trend
  output$desc <- renderText({
    if(input$smoother){  
      trend_text <- emissions(input$x)
      paste(trend_text, "predicted emissions")
    }
  })
  
}

 

# Create Shiny object
shinyApp(ui = ui, server = server)




