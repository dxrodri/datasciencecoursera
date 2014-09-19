require(shiny)
require(ggvis)

shinyUI(
 pageWithSidebar(
  headerPanel("MPG Predictor!"),
  sidebarPanel(
      helpText("Select the weight, tranmission type and number of cylinders of the new car to find approximate miles per gallon. "),
      helpText("The details of the new car will be shown as *** New Car *** in the graph. Hover over the markers to see details about the car."),
      br(),
      sliderInput("wt", label=h3("Weight (in tons)"),
                  min=1, max=10, value=2, step = 1),
      radioButtons("am", label = h3("Transmission Type"),
                   choices = list("Automatic" = 0, "Manual" = 1), 
                   selected = 0),
      radioButtons("cyl", label = h3("Cylinders"),
                   choices = list("4" = 4, "6" = 6,"8" = 8), 
                   selected = 4),
      tags$small(paste0(
        "Note: MPG Predictor uses data from 32 1973-1974 model cars",
        " and hence not applicable to newer model cars."
      )),
      br(),
      br(),
      tags$small(paste0(
        " Project submitted as part of Cousera Data Science Specialization - DomR"
      )),
      uiOutput("plot_ui")
  ),
  mainPanel(
    ggvisOutput("plot")
    ,wellPanel(
      span("Predicted mpg for the New Car:",
           textOutput("predictedMpg")
      )
    )
    ,tableOutput("carTable")
  )
))
