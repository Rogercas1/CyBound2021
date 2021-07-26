# Load packages
library(shiny)
library(shinythemes)
library(dplyr)
library(readr)

# made-up data
set.seed(20210714)
x <- runif(100)
y <- 2*exp(3*x)+rnorm(100,0,2)
trend_data<-data.frame(x,y)


# Define UI
ui <- fluidPage(theme = shinytheme("lumen"),
                titlePanel("Fitting a line"),
                sidebarLayout(
                  sidebarPanel(

                    # Select whether to overlay smooth trend line
                    checkboxInput(inputId = "smoother", label = strong("Overlay fitted line"), value = FALSE),
                    
                    # Display only if the smoother is checked
                    conditionalPanel(condition = "input.smoother == true",
                                     sliderInput(inputId = "f", label = "Value for prediction:",
                                                 min = 0.01, max = 1, value = 0.67, step = 0.01,
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
  l.y<-log(trend_data$y)
  exp_curve <- lm(l.y~trend_data$x)
  a<-exp(exp_curve$coefficients[1])
  b<-exp_curve$coefficients[2]
  f.curve<-function(x) a*exp(b*x)
  
  # Create scatterplot object the plotOutput function is expecting
  output$lineplot <- renderPlot({
    color = "#434343"
    par(mar = c(4, 4, 1, 1))
    plot(trend_data$x, trend_data$y, type = "p",
         xlab = "x", ylab = "y")
    # Display only if smoother is checked
    if(input$smoother){
      sorted<-sort(trend_data$x)
      smooth_curve <- apply(matrix(sorted,100,1),1,f.curve)
      lines(sorted, smooth_curve, col = "#E6553A", lwd = 3)
      points(input$f,f.curve(input$f), col = 'blue', cex = 2, pch = 19)
    }
  })


  # Pull in description of trend
  output$desc <- renderText({
    if(input$smoother){  
    trend_text <- f.curve(input$f)
    paste(trend_text, "is the prediction")
    }
  })

}

# Create Shiny object
shinyApp(ui = ui, server = server)
