#' @import data.table
#' @importFrom sf st_read
NULL

#' @title Make Interactive Maps
#'
#' @description Make interactive maps utilizing \link[ggiraph]{ggiraph}.
#'
#' @param tbl Analysis table (data.table object).
#' @param tbl_geo_id Name of the geography variable in tbl.
#' @param geo_layer A spatial feature object or path to shapefile. The geography layer to display.
#' @param geo_layer_id The unique id of the spatial feature object. By default, guesses the id using guess_geo_layer_id.
#' @param geo_layer_name The name field in the spatial feature object. By default, guesses the name using guess_geo_layer_name.
#' @param tbl2 Optional second table. Requires same geography group variable as tbl. tbl2 gets passed to \link[surveyvisualize]{make_chart} and output as an interactive tooltip over the matching tbl geography.
#' @param confidence Confidence level for margin of error calculation. Defaults to 0.90. Set to NULL for standard error.
#' @param ... Optional formatting arguments. See \link[surveyvisualize]{format_values}.
#' @return ggiraph/htmlwidget class object
#' 
#' @import ggiraph

#' @export
make_map <- function(tbl, tbl_geo_id, geo_layer, geo_layer_id = guess_geo_layer_id(geo_layer),
                      geo_layer_name = guess_geo_layer_name(geo_layer), tbl2 = NULL, 
                     confidence = 0.95, use_ggiraph = TRUE, ...) {
  
  loginfo(paste('Calling map maker:', format(match.call())))

  # If geo_layer is character, assume it is a shapefile and pass to st_read
  if (is.character(geo_layer)) {
    if (!file.exists(geo_layer)) stop(geo_layer, ' file does not exist.')
    geo_layer <- st_read(geo_layer, quiet = TRUE)
  }
  
  # Apply margin of error
  tbl <- use_moe(tbl, confidence)

  # Define feature and response variables
  value_vars <- c('Estimate','SE','Survey','N')
  response <- colnames(tbl)[colnames(tbl) %in% value_vars]
  by <- colnames(tbl)[!colnames(tbl) %in% response]

  # Override formatting options if specified
  format_values_call <- function(x) {
    do.call(format_values, c(list(x = x), list(...)))
  }

  # Table can only have 1 group by variable (a geography)
  if (length(by) > 1) stop('tbl parameter has too many "by" variables.')

  # Merge geography layer with aggregate table
  geo_data <- merge(geo_layer, tbl, by.x = geo_layer_id, by.y = tbl_geo_id)

  if (!is.null(tbl2)) {

    # Initialize progress bar
    progress_bar <- txtProgressBar(min = 0, max = nrow(tbl), style = 3)
    current_index <- 0

    group_var <- colnames(tbl2)[!colnames(tbl2) %in% c(value_vars, tbl_geo_id)]

    # Loop over the secondary table (split by the geography variable)
    # Construct non-interactive svg charts to be used as tooltips for each layer
    gg_html <- sapply(split(tbl2, by = tbl_geo_id), function(tbl_i) {
      current_index <<- current_index + 1
      setTxtProgressBar(progress_bar, current_index)
      tbl_i <- tbl_i[, mget(c(group_var, response))]
      g <- make_chart(tbl_i, interactive = F, ...)
      g <- gsub("'", "\"", g$x$html) # single quotes not supported in tooltips
      g <- gsub("[\n]", " ", g) # hard \n also not supported https://github.com/davidgohel/ggiraph/issues/18
      return(g)
    })

    close(progress_bar)

    # wrap up list object of bar charts in data.frame for merging back to data
    gg_html <- data.table(
      geography = attr(gg_html, "names"),
      tooltip = unlist(gg_html)
    )

    geo_data <- merge(geo_data, gg_html, by.x = geo_layer_id, by.y = 'geography')

    tooltip_css <- 'background-color:#F2F2F2; width:40%; padding:10px; border-radius:10px 20px 10px 20px'

  } else {

    # Construct map tooltip
    geo_data$tooltip <- sprintf(
      fmt = '<b>%s</b><br>%s &plusmn; %s',
      geo_data[[geo_layer_name]],
      format_values_call(geo_data$Estimate),
      format_values_call(geo_data$SE)
    )

    # css options for tooltip
    tooltip_css <- "background-color:#F2F2F2; padding:10px; border-radius:10px 20px 10px 20px"

  }

  # Construct interactive spatial feature plot
  gg <- ggplot(geo_data) +
    geom_sf(colour = "white") +
    geom_sf_interactive(
      color = "#FFFFFF",
      size = 0.35,
      mapping = aes(
        fill = Estimate,
        tooltip = tooltip,
        data_id = get(geo_layer_id)
      )
    )

  # Additional options/styling
  gg <- gg +
    theme_minimal() +
    theme(
      axis.text.x=element_blank(), 
      axis.text.y=element_blank(),
      axis.ticks=element_blank(),
      panel.grid = element_blank()
    ) +
    coord_sf(datum = NA) +
    scale_fill_gradient(
      low = "#f2f0f7",
      high = "#54278f",
      labels = format_values_call
    )

  if (use_ggiraph == TRUE) {
      # Render girafe plot
      gg <- girafe(ggobj = gg)
      girafe_options(
        x = gg,
        opts_sizing(width = 0.7),
        opts_tooltip(css = tooltip_css),
        opts_hover(css = "border:0; fill:#fff7bc; cursor: crosshair;")
      )
  } else gg


}

#' @export
guess_geo_layer_id <- function(geo_layer) {
  geo_layer_cols <- colnames(geo_layer)
  geo_layer_id <- geo_layer_cols[grepl('^GEOID', geo_layer_cols, ignore.case = T)]

  if (length(geo_layer_id) == 0 | length(geo_layer_id) > 1) {
    stop('Could not guess geo_layer_id from geo_layer. Please specify.')
  } else {
    warning('Using ', geo_layer_id, ' as geo_layer_id.')
    geo_layer_id
  }
}

#' @export
guess_geo_layer_name <- function(geo_layer) {
  geo_layer_cols <- colnames(geo_layer)
  geo_layer_name <- head(geo_layer_cols[grepl('^NAME', geo_layer_cols, ignore.case = T)], 1)

  if (length(geo_layer_name) == 0) {
    geo_layer_name <- suppressWarnings(geo_layer_id)
  }

  if (length(geo_layer_name) == 0) {
    stop('Could not guess geo_layer_name from geo_layer. Please specify.')
  } else {
    warning('Using ', geo_layer_name, ' as geo_layer_name.')
    geo_layer_name
  }
}
