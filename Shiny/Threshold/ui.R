library(shiny)
library(ggplot2)

load(file = "logfit.Rdata")

fluidPage(
  
  # Application title
  titlePanel("Threshold Analysis"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("threshold",
                  "Threshold:",
                  min = 0.00,
                  max = 0.50,
                  value = 0.25),
      selectInput("models",
                  "Model",
                  choices = c("log", "lda", "qda"))
    ),
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("logfit"),
      plotOutput("threshold")
    )
  )
)