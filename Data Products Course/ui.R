
library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Central Limit Theorm & Normalizing Data!"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
            numericInput("min", "What do you want the minimum number in the sample to be?",
                         value = 0, step = 1),
            numericInput("max", "What do you want the maximum number in the sample to be?",
                         value = 10, step = 1),
            sliderInput("trials",
                        "Number of trials:",
                        min = 3,
                        max = 10000,
                        value = 10)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       tabsetPanel(type = "tabs",
                   tabPanel("Central Limit Theorem", plotOutput("histPlot")),
                   tabPanel("Center or Normalize Data", plotOutput("Plot"))
    )
  )
)))
