library(shiny)
library(rgcam)
library(magrittr)
library(readxl)
library(dplyr)
library(readr)
library(purrr)
library(fs)
library(GCAMdashboard)
library(tibble)
library(stringr)
library(randomcoloR)

options(shiny.maxRequestSize=512*1024^2) # 512 MB max file upload size.

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

    ## Set up some UI state
    scenarios <- ""
    queries <- ""

    ## Get the new data file on upload
    rFileinfo <- reactive({
        fileinfo <- input$projectFile
        project.settings <- loadDefaultProjectSettings()
        project.data <- loadDefault()

        if(!is.null(fileinfo)) {
            extraData <- loadProject2(fileinfo$datapath)
            extraScenario <- attr(extraData, "scenario_name")
            project.data[[extraScenario]] <- extraData
        }

        updateSelectInput(session, 'scenarioInput', choices=rev(listScenarios(project.data)))
        list(project.data=project.data,
             project.settings=project.settings)
    })

    ## Update controls on sidebar in response to user selections
    observe({
        if(is.null(rFileinfo()$project.data)) {
            new.scenarios <- list()
        }
        else {
            new.scenarios <- getProjectScenarios(rFileinfo)
        }

        if(!all(scenarios == new.scenarios)) {
            scenarios <<- new.scenarios # Update UI state
            updateSelectInput(session, 'plotScenario', choices=scenarios)
            updateSelectInput(session, 'diffScenario', choices=scenarios)
        }

        if(!is.null(rFileinfo()$project.data)) {
            if(input$plotScenario == "") {
                # When first loading a dataset, no scenario is selected
                qscenarios <- scenarios
            }
            else if(input$diffCheck) {
                qscenarios <- c(input$plotScenario, input$diffScenario)
            }
            else {
                qscenarios <- input$plotScenario
            }
            new.queries <- getScenarioQueries(rFileinfo, qscenarios)

            if(!identical(queries,new.queries)) {
                ## capture new query list
                queries <<- new.queries
                ## preserve selected value if possible
                sel <- input$plotQuery
                if(!(sel %in% queries))
                    sel <- NULL          # allow update to reset selection
                updateSelectInput(session, 'plotQuery', choices=queries,
                                  selected=sel)
            }
        }

    })

    observe({
        ## update the subcategory selector on the time value plot.
        ## Only do this when the selected plot query changes.
        scen <- isolate(input$plotScenario)
        prj <- isolate(rFileinfo()$project.data)
        query <- input$plotQuery
        if(uiStateValid(prj, scen, query)) {
            ## Assumes that a particular query has the same columns in all scenarios
            subcategories <- getSubcategories()
            prevSubcat <- if(input$subcategorySelect %in% subcategories) input$subcategorySelect else 'none'
            updateSelectInput(session, 'subcategorySelect', choices=c('none', subcategories),
                              selected=prevSubcat)
        }
    })

    getSubcategories <- reactive({
        scen <- isolate(input$plotScenario)
        prj <- isolate(rFileinfo()$project.data)
        query <- input$plotQuery
        data <- getQuery(prj, query, scen)
        possible_subcategories <- data %>% names
        subcategories <- list()

        i <- 1
        for (subcategory in possible_subcategories) {
            if (!all(is.na(data[subcategory]))) {
                subcategories[[i]] <- subcategory
                i <- i + 1
            }
        }

        subcategories[!subcategories %in% c('scenario', 'order', 'Units', 'year', 'value')]
    })

    output$scenarios <- renderText({
        getProjectScenarios(rFileinfo, concat='\n')
    })

    output$queries <- renderText({
        getScenarioQueries(rFileinfo, input$scenarioInput, concat='\n')
    })

    output$timePlot <- renderPlot({
        prj <- rFileinfo()$project.data
        settings <- rFileinfo()$project.settings
        scen <- input$plotScenario
        query <- input$plotQuery
        plot_type <- filter(settings, query == !!query)$type

        if(uiStateValid(prj, scen, query)) {
            diffscen <- if(input$diffCheck) {
                input$diffScenario
            } else {
                NULL
            }
            subcategorySelect <- input$subcategorySelect

            region.filter <- input$tvRgns
            last.region.filter <<- region.filter

            # If the query has changed, the value of the subcategory selector
            # may not be valid anymore. Change it to none.
            if(!subcategorySelect %in% names(getQuery(prj, query, scen))) {
                subcategorySelect <- 'none'
            }

            plotTime(prj, plot_type, query, scen, diffscen, subcategorySelect,
                     input$tvFilterCheck, region.filter)
        }
        else {                          # UI state is invalid
            default.plot('Updating')
        }
    })

    output$show_breakdown_input <- reactive({
        settings <- rFileinfo()$project.settings
        query <- input$plotQuery
        plot_type <- filter(settings, query == !!query)$type
        plot_type != "line"
    })

    observe({
        prj <- rFileinfo()$project.data
        scen <- input$plotScenario
        query <- input$plotQuery
        if(uiStateValid(prj, scen, query)) {
            tbl <- getQuery(prj,query,scen)
            rgns <- unique(tbl$region) %>% sort
            updateCheckboxGroupInput(session, 'tvRgns', choices = rgns,
                                     selected = last.region.filter)
        }
    })
    # Debugging
    observe({
        print('****************Change of Input****************')
        cat('plotScenario: ', input$plotScenario, '\n')
        cat('diffScenario: ', input$diffScenario, '\n')
        cat('plotQuery: ', input$plotQuery, '\n')
    })

    outputOptions(output, "show_breakdown_input", suspendWhenHidden = FALSE)
})
