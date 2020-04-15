library(readxl)
library(purrr)
library(tibble)
library(dplyr)
library(stringr)
library(randomcoloR)

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
  loadProject2('./data/out_v-ref_p0_r0_gdpg-m_aeeg-m_sekl-m_dash.xls')
}


#' Load a file into the UI
#'
#' Returns the data from the project file, if valid
#' @param proj Path to the project file
#' @export
loadProject2 <- function(proj)
{
    if (is.character(proj)) {
        projFile <- proj
        if (file.exists(projFile)) {
            if (file.access(projFile, mode = 6) != 0) {
                stop("File ", projFile, " exists but lacks either read or write permission.")
            }

            prjdata <- readFromExcel(projFile)

            if (!exists("prjdata", inherits = FALSE)) {
                message(paste("File", projFile, "does not contain valid project data."))
                message("Try loading the file into an R session and verify that it contains the variable 'prjdata'.")
                stop("Unable to load project file ", projFile)
            }
            attr(prjdata, "file") <- projFile
        }
        else {
            prjdata <- list()
            attr(prjdata, "file") <- projFile
        }
    }
    else {
        stop("loadProject2: invalid object passed as proj argument; proj must be a filename.")
    }
    prjdata
}

readFromExcel <- function(file) {
    data <- read_excel(file,
                       col_types = c("guess", "text", "text", "guess", "guess", "guess", "text"),
                       col_names = c("variable", "sector", "order", "Units", "year", "region", "value")) %>%
        add_column(scenario = "EPA")

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
    data <- data %>%
        group_split(variable, keep = FALSE) %>%
        setNames(unique(data$variable))

    return(list(EPA = data))
}

#' Get the name of the project for display
#'
#' Returns a place holder string if no project has been loaded yet.
#' @param rFileinfo Reactive fileinfo object returned by the file browser in the UI
#' @export
getProjectName <- function(rFileinfo)
{
    fn <- rFileinfo()$project.filename
    if(is.null(fn)) {
        '->none<-'
    } else {
        rFileinfo()$project.filename
    }
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
        tryCatch(
            lapply(scenarios, . %>% rgcam::listQueries(prj, .)) %>%
                Reduce(intersect,.) %>% sort %>%
                    paste(collapse=concat),
            ## errors in the pipeline above are caused by selecting a new data
            ## set that doesn't contain the current scenario.  The problem will
            ## clear up once the scenario selector is repopulated.
            error = function(e) {
                if(is.null(concat)) '' else tag.noscen
            })
    }
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
      tp <- dplyr::group_by_(tp, key, 'year', 'Units') %>%
            dplyr::summarise(value = sum(value))
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
#' @param query  Name of the query to plot
#' @param scen  Name of the scenario to plot
#' @param diffscen  Name of the difference scenario, or NULL if none
#' @param subcatvar  Variable to use for subcategories in the plot
#' @param filter  If TRUE, then filter to regions in the rgns argument
#' @param rgns  Regions to filter to, if filter is TRUE.
#' @importFrom magrittr "%>%"
#' @importFrom ggplot2 ggplot aes_string geom_bar theme_minimal ylab scale_fill_manual
#' @export
plotTime <- function(prjdata, query, scen, diffscen, subcatvar, filter, rgns)
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

        plt <- ggplot(pltdata, aes_string('year','value', fill=subcatvar)) +
          geom_bar(stat='identity') +
          theme_minimal(base_size = 16) +
          ylab(pltdata$Units)

        if(is.null(subcatvar)) {
            plt
        }
        else {
            unfiltered_pltdata <- getPlotData(prjdata, query, scen, diffscen, subcatvar,
                                              NULL, NULL)
            subcategory_values <- getSubcategoryValues(unfiltered_pltdata, subcatvar)
            color_palette <- getColorPalette(subcategory_values)
            plt + scale_fill_manual(values = color_palette)
        }
    }
}
