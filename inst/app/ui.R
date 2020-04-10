
library(shiny)
library(GCAMdashboard)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  # Application title
  titlePanel("EPPA Dashboard"),

  # Sidebar with user controls
  sidebarLayout(
    sidebarPanel(
      fileInput('projectFile', 'EPPA Project Data File'),
      fluidRow(
        column(8,
               selectInput('plotScenario', 'Select Scenario to Plot', choices=list()))),
      fluidRow(
        column(8,
               selectInput('plotQuery', 'Select Query to Plot',
                           choices=list())),
        column(4,
               checkboxInput('inclSpatial', 'Include Spatial Queries', value=TRUE))
      ),
      checkboxInput('diffCheck', 'Plot Difference vs Another Scenario'),
      conditionalPanel(
        condition = "input.diffCheck == true",
        fluidRow(column(8,
                        selectInput('diffScenario', 'Select Difference Scenario', choices=list())))
      )
    ),

    # main display area
    mainPanel(
      plotOutput('timePlot', height='600px'),
      h3('Options'),
      selectInput('tvSubcatVar', 'Break totals into subcategories by:',
                  choices=c('none','region')),
      h4('Regions'),
      fluidRow(
        checkboxInput('tvFilterCheck',
                      'Limit plot to selected regions')),
      fluidRow(
        column(1,checkboxGroupInput('tvRgns', 'Regions', choices=c()))
      )
    ) #  main Panel
  )   # sidebar layout
))
