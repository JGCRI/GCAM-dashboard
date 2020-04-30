library(readxl)
library(purrr)
library(tibble)
library(dplyr)
library(stringr)
library(randomcoloR)
library(fs)

### Helper functions for the server side of the app.

### Conventions:
###    rFileinfo:  The reactive fileinfo structure returned by the file browser

tag.noscen <- '->No scenarios selected<-'     # placeholder when no scenario selected

#### State variables
last.region.filter <- NULL

#' Load default data into UI
#'
#' Returns the data from the default data file
#' @export
loadDefault <- function()
{
  filenames <- list.files("./data")
  data <- list()

  for (filename in filenames) {
    data[[scenarioName(filename)]] <- loadProject2(file.path('./data', filename))
  }

  data
}

#' Load the default project file into the settings
#'
#' Returns the settings from the default project file
#' @export
loadDefaultProjectSettings <- function()
{
  loadProjectSettings('./data/Reference.xls')
}


#' Load a file into the UI
#'
#' Returns the data from the project file, if valid
#' @param projFile Path to the project file
#' @export
loadProject2 <- function(projFile)
{
    if (is.character(projFile)) {
        if (file.exists(projFile)) {
            if (file.access(projFile, mode = 6) != 0) {
                stop("File ", projFile, " exists but lacks either read or write permission.")
            }
            prjdata <- readFromExcel(projFile)
        }
        else {
            prjdata <- list()
        }
         attr(prjdata, "file") <- projFile
         attr(prjdata, "scenario_name") <- scenarioName(projFile)
    }
    else {
        stop("loadProject2: invalid object passed as proj argument; proj must be a filename.")
    }
    prjdata
}

scenarioName <- function(file_path) {
  file_path %>% path_file() %>% path_ext_remove()
}

#' Load a file into the settings
#'
#' Returns the settings from the project file
#' @param proj Path to the project file
#' @export
loadProjectSettings <- function(file) {
    read_excel(file,
              sheet = "query",
              cell_cols("A:C"),
              col_names = c("query", "order", "type")) %>%
      mutate(order = as.integer(order)) %>%
      mutate(query = as.factor(query)) %>%
      mutate(type = as.factor(type)) %>%
      arrange(query) %>%
      distinct(query, .keep_all = TRUE)
}

readFromExcel <- function(file) {
    scenario_name <- scenarioName(file)
    data <- read_excel(file,
                       col_types = c("guess", "text", "text", "guess", "guess", "guess", "text"),
                       col_names = c("variable", "sector", "order", "Units", "year", "region", "value")) %>%
        add_column(scenario = scenario_name)

    # replace GAMS "Eps" output with 0.
    # See https://www.gams.com/latest/docs/gamside/special_values.htm
    data[data$value == "Eps", "value"] <- "0"

    # Convert numeric columns to numeric
    # I'm not able to read them as numeric without getting tons of warnings
    data$value <- as.numeric(data$value)
    data$year <- as.numeric(data$year)
    data$order <- as.numeric(data$order)

    # Replace _ with space in region names
    # GAMS cannot output region names with spaces in them, but we want them to be human-readable
    data <- mutate(data, region = str_replace_all(region, "_", " "))

    # split single table into list of tables, named by variable
    # See https://stackoverflow.com/questions/57107721/how-to-name-the-list-of-the-group-split-output-in-dplyr
    data <- mutate(data, variable = factor(variable, levels = unique(variable)))
    data %>%
        group_split(variable, keep = FALSE) %>%
        setNames(unique(data$variable))
}

#' Get the scenarios in the project for display
#'
#' Returns a place holder string if no project has been loaded yet.
#' @param rFileinfo Reactive fileinfo object returned by file browser in the UI.
#' @param concat Separator string to use when concatenating scenario names.
#' @importFrom magrittr "%>%"
#' @export
getProjectScenarios <- function(rFileinfo, concat=NULL)
{
    pd <- rFileinfo()$project.data
    if(is.null(pd)) {
        '->none<-'
    } else {
        rgcam::listScenarios(rFileinfo()$project.data) %>% paste(collapse=concat)
    }
}

#' Get the queries for a project and scenario(s) for display
#'
#' @param rFileinfo Reactive fileinfo object returned by file browser in the UI.
#' @param scenarios List of scenarios.
#' @param concat Separator string for concatenating query names.
#' @importFrom magrittr "%>%"
#' @export
getScenarioQueries <- function(rFileinfo, scenarios, concat=NULL)
{
    prj <- rFileinfo()$project.data
    settings <- rFileinfo()$project.settings
    if(is.null(prj)) {
        if(is.null(concat))
            ''                          # probably not intended for display
        else
            '->none<-'                  # probably intended for display
    }
    else if(length(scenarios) == 0 || all(scenarios=='')) {
        if(is.null(concat))
            ''                          # probably not intended for display
        else
            tag.noscen                  # probably intended for display
    }
    else {
        queries <- tryCatch(
            lapply(scenarios, . %>% rgcam::listQueries(prj, .)) %>%
            Reduce(intersect,.) %>%
            sort %>%
            paste(collapse=concat),
            ## errors in the pipeline above are caused by selecting a new data
            ## set that doesn't contain the current scenario.  The problem will
            ## clear up once the scenario selector is repopulated.
            error = function(e) {
                if(is.null(concat)) '' else tag.noscen
            })
        tibble(query = queries) %>%
        left_join(settings) %>%
        arrange(order, query) %>%
        pull(query)
    }
}

getQueryOrder <- function(queries, settings) {
  settings[[query]][[0]]$order
}

#' Indicate whether the UI is in an obviously invalid state.
#'
#' Invalid states frequently occur as transients when a new project is being
#' loaded and the UI elements are being updated.
#'
#' @param prj Project data structure
#' @param scenario Scenario name
#' @param query Query name
#' @return Boolean indicating whether the UI state appears to be valid.
#' @export
uiStateValid <- function(prj, scenario, query)
{
    valid.values <- !(is.null(prj) || scenario == '' || query == '' ||
                          query==tag.noscen)
    if(valid.values) {
        prjscens <- listScenarios(prj)
        valid.scen <- all(scenario %in% prjscens)
    }
    else {
        valid.scen <- FALSE
    }

    ## This if block is the return value
    if(valid.scen) {
        scenqueries <- listQueries(prj, scenario)
        all(query %in% scenqueries)
    }
    else {
        FALSE
    }
}


#' Get the years for which a query is defined
#'
#' @param prj Project data structure
#' @param scenario Name of the scenario
#' @param query Name of the query
#' @export
getQueryYears <- function(prj, scenario, query)
{
    if(!uiStateValid(prj, scenario, query)) {
        c(2005, 2100)
    }
    else {
        range(getQuery(prj, query, scenario)["year"])
    }
}


### Helpers for making plots

#' Plot a default panel
#'
#' Mainly intended for use when no data has been loaded.
#'
#' @param label.text Text to display in the middle of the panel
#' @importFrom ggplot2 ggplot geom_label theme_minimal aes aes_
#' @export
default.plot <- function(label.text='No data selected')
{
    ggplot(mapping=aes(x=0,y=0)) + geom_label(aes_(label=label.text), size=10) +
        theme_minimal()
}

### Data wrangling

#' Extract and format data for a plot
#'
#' @param prjdata Project data structure
#' @param query Name of the query to plot
#' @param pltscen Name of the scenario to plot
#' @param diffscenDifference scenario, if any
#' @param key Aggregation variable.  (e.g., 'region' or 'sector')
#' @param filtervar If not NULL, filter on this variable before aggregating
#' @param filterset:  Set of values to include in the filter operation.  Ignored
#'   if filtervar is NULL.
#' @keywords internal
getPlotData <- function(prjdata, query, pltscen, diffscen, key, filtervar=NULL,
                        filterset=NULL)
{
    tp <- getQuery(prjdata, query, pltscen) # 'table plot'
    if(!is.null(diffscen)) {
        dp <- getQuery(prjdata, query, diffscen) # 'difference plot'
    }
    else {
        dp <- NULL
    }

    if(!is.null(dp)) {
        ## We're doing a difference plot, so subtract the difference scenario.
        ## Join the data sets first so that we can be sure that we have matched
        ## the rows and columns correctly
        varnames <- names(tp)
        mergenames <- varnames[!varnames %in% c('scenario', 'order', 'Units', 'value')]

        joint.data <- merge(tp, dp, by=mergenames, all=TRUE)
        if(anyNA(joint.data))
            joint.data[is.na(joint.data)] <- 0 # zero out missing values

        value <- joint.data$value.x - joint.data$value.y

        mergenames <- sapply(mergenames, as.name) # Don't eval hyphenated col names

        # Construct the new data frame.  We use the scenario name from the left
        # (dp) data frame.
        tp <- dplyr::rename(joint.data, scenario=scenario.x, Units=Units.x) %>%
           dplyr::select_(.dots=c('scenario', mergenames, 'Units')) %>% cbind(value)
    }

    ## If filtering is in effect, do it now
    if(!is.null(filtervar) &&
       !is.null(filterset) &&
       length(filterset) > 0 &&
       filtervar %in% names(tp)
       ) {

        tp <- dplyr::filter_(tp, lazyeval::interp(~y %in% x, y = as.name(filtervar), x = filterset))
    }

    ## Select the key and year columns, then sum all values with the same
    ## key.  Force the sum to have the name 'value'.
    if(!is.null(key) &&
       toString(key) %in% (tp %>% names %>% setdiff(c('year', 'Units')))
       ) {
      if (any(is.na(tp$order)) || key != "sector") {
        # Do not enforce any special ordering unless we're breaking down by sector and have
        # numbers in the order column
        tp <- dplyr::group_by_(tp, key, 'year', 'Units') %>%
              dplyr::summarise(value = sum(value))
      } else {
        ordered_subcategories <- unique(arrange(tp, desc(order))[[key]])
        tp <- tp %>%
          dplyr::mutate(!!key := factor(!!key, levels = ordered_subcategories, ordered = TRUE))
          dplyr::group_by_(tp, key, 'year', 'Units') %>%
          dplyr::summarise(value = sum(value), order = first(order))
      }
    }
    else {
      tp <- dplyr::group_by_(tp, 'year', 'Units') %>%
            dplyr::summarise(value = sum(value))
    }
    ## Occasionally you get a region with "0.0" for the unit string because
    ## most of its entries were zero. Fix these so that the column all has the
    ## same unit.
    tp$Units <- summarize.unit(tp$Units)
    tp
}

#' Summarize the unit column of a GCAM data frame by taking the most common
#' entry.
#'
#' In theory the unit should have a single, common value, but in practice GCAM
#' isn't always great about getting its unit strings consistent.
#' @param unitcol Character vector of unit names.
#' @keywords internal
summarize.unit <- function(unitcol)
{
    unitcol[which.max(table(unitcol))]
}

getSubcategoryValues <- function(df, subcategory_name)
{
    subcategory_values <- unique(df[[subcategory_name]])
    subcategory_values
}

getColorPalette <- function(subcategory_values)
{
    set.seed(1890)
    color_palette <- distinctColorPalette(length(subcategory_values))
    names(color_palette) <- sort(subcategory_values)
    color_palette
}

#' Plot values over time as a bar chart
#' @param prjdata A project data structure
#' @param plot_type The type to plot: stacked or line
#' @param query  Name of the query to plot
#' @param scen  Name of the scenario to plot
#' @param diffscen  Name of the difference scenario, or NULL if none
#' @param subcatvar  Variable to use for subcategories in the plot
#' @param filter  If TRUE, then filter to regions in the rgns argument
#' @param rgns  Regions to filter to, if filter is TRUE.
#' @importFrom magrittr "%>%"
#' @importFrom ggplot2 ggplot aes_string geom_bar geom_line theme_minimal ylab scale_fill_manual scale_color_manual
#' @export
plotTime <- function(prjdata, plot_type, query, scen, diffscen, subcatvar, filter, rgns)
{
    if(is.null(prjdata)) {
        default.plot()
    }
    else {
        if(filter)
            filtervar <- 'region'
        else
            filtervar <- NULL

        if(subcatvar=='none')
            subcatvar <- NULL
        else
            subcatvar <- as.name(subcatvar)

        pltdata <- getPlotData(prjdata, query, scen, diffscen, subcatvar,
                               filtervar, rgns)

        plt <- ggplot(pltdata, aes_string('year','value', fill=subcatvar, color=subcatvar)) +
          theme_minimal(base_size = 16) +
          ylab(pltdata$Units)

        if (is.null(plot_type) || plot_type == "stacked" || is.null(subcatvar) || subcatvar != "region") {
          plt <- plt + geom_bar(stat='identity')
        } else {
          plt <- plt + geom_line(size = 1)
        }

        if(is.null(subcatvar)) {
            plt
        }
        else {
            unfiltered_pltdata <- getPlotData(prjdata, query, scen, diffscen, subcatvar,
                                              NULL, NULL)
            subcategory_values <- getSubcategoryValues(unfiltered_pltdata, subcatvar)
            color_palette <- getColorPalette(subcategory_values)
            plt +
              scale_fill_manual(values = color_palette) +
              scale_color_manual(values = color_palette)
        }
    }
}
