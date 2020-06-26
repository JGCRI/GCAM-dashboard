
library(shiny)
library(GCAMdashboard)

# Define UI for application that draws a histogram
shinyUI(fluidPage(theme="style.css",
  # Application title
  titlePanel("EPPA Dashboard"),

  # Sidebar with user controls
  sidebarLayout(
    sidebarPanel(
      fileInput('projectFile', 'EPPA Project Data File'),
      selectInput('plotScenario', 'Select Scenario to Plot', choices=list()),
      selectInput('plotQuery', 'Select Data to Plot', choices=list()),
      checkboxInput('diffCheck', 'Plot Difference vs Another Scenario'),
      conditionalPanel(
        condition = "input.diffCheck == true",
        selectInput('diffScenario', 'Select Difference Scenario', choices=list())
      )
    ),

    # main display area
    mainPanel(
      plotOutput('timePlot',
                 height='600px',
                 hover = hoverOpts("exploreHover", delay = 50, delayType = 'throttle')),
      barChartHoverUI("timePlot"),
      conditionalPanel(
        condition = "output.show_breakdown_input",
        selectInput('subcategorySelect', 'Break plot down by:', choices=c('none','region'))
      ),
      checkboxInput('tvFilterCheck', 'Limit plot to selected regions'),
      checkboxGroupInput('tvRgns', 'Regions', choices=c())
    ) #  main Panel
  )   # sidebar layout
))
