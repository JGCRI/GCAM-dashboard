# Bar Chart Hover ---------------------------------------------------------

barChartHoverUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns('hoverInfo'))
}

barChartHover <- function(input, output, session, hover, data, subcategory) {
  output$hoverInfo <- renderUI({
    hover <- hover()
    df <- data()
    subcat <- subcategory()

    val <- calculateHoverValue(hover, df, subcat)
    if(is.null(val)) return(NULL)


    # Calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)

    # Calculate distance from left and bottom side of the picture in pixels
    left_px <- left_pct * (hover$range$right - hover$range$left) + hover$range$left
    top_px <- top_pct * (hover$range$bottom - hover$range$top) + hover$range$top

    left <- round(left_px) - 30
    top <- round(top_px) - 30
    if(left < 0 || top < 0) return(NULL)

    # Hover tooltip is created as absolutePanel
    absolutePanel(
      class = 'hoverPanel',
      left = paste0(left, "px"),
      top = paste0(top, "px"),
      p(HTML(val)))

  })
}

calculateHoverValue <- function(hover, df, subcat) {

  # Make sure we're working with valid values
  if(is.null(hover) || is.null(df)) return(NULL)

  # Detect the year of the bar that is being hovered over
  hoverYear <- df[which.min(abs(df$year - hover$x)), 'year'][[1]]
  df <- dplyr::filter(df, year == hoverYear)

  # If there are negative values (most likely from a diff plot), flip their
  # signs so we can use the same logic as we do for positive bars.
  y <- hover$y
  if(y < 0) {
    df <- df[which(df$value < 0), ]
    df$value <- abs(df$value)
    y <- abs(y)
  } else {
    df <- df[which(df$value > 0), ]
  }

  if(y > sum(df$value)) return(NULL) # Above the bar

  # If there's a subcategory, we also need to find which category is being
  # hovered over
  if(subcat == 'none') {
    val <- round(df$value, digits = 1) * sign(hover$y)
    as.character(val)
  }
  else {
    # Find which segment of the stacked bar the hover is closest to
    stackedSum <- sum(df$value) - cumsum(df$value)
    index <- which(stackedSum - y < 0)[1]

    # Get the region name and value for display
    regionName <- df[[subcat]][index]
    val <- round(df$value[index], digits = 1) * sign(hover$y)
    paste0(regionName, ': ', val)
  }
}
