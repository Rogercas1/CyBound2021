# Load packages
library(shiny)
library(shinythemes)
library(tidyverse)
library(gridExtra)

source("theme_map.R")
source("emissions.R")
source("yield.R")

ymPlotDF <- read.csv(file.path("basswood_2020.csv")) %>%
  dplyr::mutate(wheatplot=yieldMgHaMean*0.9, 
                emplot=(1/yieldMgHaMean)*1000+2000, 
                leachplot=5*(1/yieldMgHaMean)+5)%>%
  select(long, lat, group, wheatplot, emplot, leachplot)%>%
  na.omit



em_proportion<-function(N.rate){
  return( exp((N.rate-300)/300) )
}

leach_proportion<-function(N.rate){
  if(N.rate < 140){
    prop <- 1
  }else {
    prop <- 3*((N.rate - 140)/140)    
  }
  return(prop)
}

g_yield<-ggplot(ymPlotDF) + # Omits 95 pixels without information
  geom_polygon(aes(
    x     = long,           # Longitudes in the horizontal axis
    y     = lat,            # Latitude in the vertical axis
    group = group,          # More than one data frame row belong to the same poly
    fill  =  wheatplot*yield_proportion(300)   # Fill the polygon with the yield mean
  )) +
  scale_fill_distiller(     # Palette from https://colorbrewer2.org/#type=sequential&scheme=Greens&n=3
    palette   = "Greens",   # 'cause chlorophyll
    direction = 1,          # Darker is higher
    limits    = c(0, 15)    # Set color bar minimum at zero, max TBD by ggplot
  ) +
  labs(
    title    = "Current practice (300 Units of Nitrogen Fertilizer (kg ha^-1)",
    subtitle = "Wheat Yield",
    fill     = expression("Yield in" ~ MgHa^-1 ~ "Darker is higher")
  ) +
  theme_map() +
  theme( # Play with background color to decide if gray helps with contrast
    panel.background = element_rect(fill = "gray80")
  )

g_yield_emissions<-ggplot(ymPlotDF) + # Omits 95 pixels without information
  geom_polygon(aes(
    x     = long,           # Longitudes in the horizontal axis
    y     = lat,            # Latitude in the vertical axis
    group = group,          # More than one data frame row belong to the same poly
    fill  =  emplot*em_proportion(300)   # Fill the polygon with the yield mean
  )) +
  scale_fill_distiller(     # Palette from https://colorbrewer2.org/#type=sequential&scheme=Greens&n=3
    palette   = "Reds",   # 'cause chlorophyll
    direction = 1,          # Darker is higher
    limits    = c(0, 3000)    # Set color bar minimum at zero, max TBD by ggplot
  ) +
  labs(
    title    = "",
    subtitle = "Emissions",
    fill     = expression("Emissions in" ~ gHa^-1 ~ "Darker is higher")
  ) +
  theme_map() +
  theme( # Play with background color to decide if gray helps with contrast
    panel.background = element_rect(fill = "gray80")
  )

g_yield_leaching<-ggplot(ymPlotDF) + # Omits 95 pixels without information
  geom_polygon(aes(
    x     = long,           # Longitudes in the horizontal axis
    y     = lat,            # Latitude in the vertical axis
    group = group,          # More than one data frame row belong to the same poly
    fill  =  leachplot*leach_proportion(300)   # Fill the polygon with the yield mean
  )) +
  scale_fill_distiller(     # Palette from https://colorbrewer2.org/#type=sequential&scheme=Greens&n=3
    palette   = "Blues",   # 'cause chlorophyll
    direction = 1,          # Darker is higher
    limits    = c(0, 15)    # Set color bar minimum at zero, max TBD by ggplot
  ) +
  labs(
    title    = "",
    subtitle = "Nitrate Leaching",
    fill     = expression("Leaching in" ~ MgHa^-1 ~ "Darker is higher")
  ) +
  theme_map() +
  theme( # Play with background color to decide if gray helps with contrast
    panel.background = element_rect(fill = "gray80")
  )



ui <- fluidPage(theme = shinytheme("lumen"),
                titlePanel("Prototype:N Fertilizer Relationships"),
                sidebarLayout(
                  sidebarPanel(
                
                   
                                     sliderInput(inputId = "x", label = "Nitrogen Fertilizer (kg ha^-1)",
                                                 min = 0, max = 350, value = 0, step = 1,
                                                 animate = animationOptions(interval = 100)),
                                     HTML("Select Nitrogen Fertilizer rate")
                  
                  ),
                  
                  # Output: Description, lineplot, and reference
                  mainPanel(
                    column(width=12,
                    plotOutput(outputId = "lineplot", height=600)), 
                  )
                )
)



# Define server function
server <- function(input, output) {
  

  
#ScatterPlot 
  
  g_yield_modified<- ggplot(ymPlotDF) + # Omits 95 pixels without information
    geom_polygon(aes(
      x     = long,           # Longitudes in the horizontal axis
      y     = lat,            # Latitude in the vertical axis
      group = group,          # More than one data frame row belong to the same poly
      fill  =  wheatplot*yield_proportion(input$x)  # Fill the polygon with the yield mean
    )) +
    scale_fill_distiller(     # Palette from https://colorbrewer2.org/#type=sequential&scheme=Greens&n=3
      palette   = "Greens",   # 'cause chlorophyll
      direction = 1,          # Darker is higher
      limits    = c(0, 15)    # Set color bar minimum at zero, max TBD by ggplot
    ) +
    labs(
      title    = "Farmer selected amount of N Fertilizer",
      subtitle = "Wheat Yield",
      fill     = expression("Yield in" ~ MgHa^-1 ~ "Darker is higher")
    ) +
    theme_map() +
    theme( # Play with background color to decide if gray helps with contrast
      panel.background = element_rect(fill = "gray80")
    )
  
  g_yield_emissions_modified<-ggplot(ymPlotDF) + # Omits 95 pixels without information
    geom_polygon(aes(
      x     = long,           # Longitudes in the horizontal axis
      y     = lat,            # Latitude in the vertical axis
      group = group,          # More than one data frame row belong to the same poly
      fill  =  emplot*em_proportion(input$x)   # Fill the polygon with the yield mean
    )) +
    scale_fill_distiller(     # Palette from https://colorbrewer2.org/#type=sequential&scheme=Greens&n=3
      palette   = "Reds",   # 'cause chlorophyll
      direction = 1,          # Darker is higher
      limits    = c(0, 3000)    # Set color bar minimum at zero, max TBD by ggplot
    ) +
    labs(
      title    = "",
      subtitle = "Emissions",
      fill     = expression("Emissions in" ~ gHa^-1 ~ "Darker is higher")
    ) +
    theme_map() +
    theme( # Play with background color to decide if gray helps with contrast
      panel.background = element_rect(fill = "gray80")
    )
  
  g_yield_leaching_modified<-ggplot(ymPlotDF) + # Omits 95 pixels without information
    geom_polygon(aes(
      x     = long,           # Longitudes in the horizontal axis
      y     = lat,            # Latitude in the vertical axis
      group = group,          # More than one data frame row belong to the same poly
      fill  =  leachplot*leach_proportion(input$x)   # Fill the polygon with the yield mean
    )) +
    scale_fill_distiller(     # Palette from https://colorbrewer2.org/#type=sequential&scheme=Greens&n=3
      palette   = "Blues",   # 'cause chlorophyll
      direction = 1,          # Darker is higher
      limits    = c(0, 15)    # Set color bar minimum at zero, max TBD by ggplot
    ) +
    labs(
      title    = "",
      subtitle = "Nitrate Leaching",
      fill     = expression("Leaching in" ~ MgHa^-1 ~ "Darker is higher")
    ) +
    theme_map() +
    theme( # Play with background color to decide if gray helps with contrast
      panel.background = element_rect(fill = "gray80"))
  
      output$lineplot <- renderPlot({
    
    grid.arrange(g_yield, g_yield_modified, g_yield_emissions, g_yield_emissions_modified, g_yield_leaching, g_yield_leaching_modified, ncol=2, nrow=3, heights=c(10,10,10))
    
    
  })
  
    
  
 # Pull in description of trend

}


# Create Shiny object
shinyApp(ui = ui, server = server)




