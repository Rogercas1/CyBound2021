# Load packages
library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library(tidyverse)

source("emissions.R")

x <- c(0:350)
y<-{2.04*sqrt(exp(.0135*x)*(4090-34*x+.075*x^2))}
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
  
  x.emissions <- emissions
  
#ScatterPlot 
  output$lineplot <- renderPlot({
    color = "#434343"
    par(mar = c(4, 4, 1, 1))
    plot(N.data$x, N.data$y, type = "p",
         xlab = "x", ylab = "y")
    # Display only if smoother is checked
    if(input$smoother){
      sorted <- sort(N.data$x)
      smooth_curve <- apply(matrix(sorted,100,1),1,N.data)
      lines(sorted, smooth_curve, col = "#E6553A", lwd = 3)
      points(input$x, x.emissions(input$x), col = 'blue', cex = 2, pch = 19)
    }
  })

  
 # Pull in description of trend
  output$desc <- renderText({
    if(input$smoother){  
      trend_text <- x.emissions(input$x)
      paste(trend_text, "predicted emissions")
    }
  })
  
}

 

# Create Shiny object
shinyApp(ui = ui, server = server)
