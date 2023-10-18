#' @title Make Interactive Charts
#'
#' @description Make interactive bar charts utilizing \link[ggiraph]{girafe}.
#'
#' @param tbl Analysis table (data.table object).
#' @param x character. Variable to be displayed along x-axis.
#' @param y character. Response variable.
#' @param fill character. Variable to be categorized using interior coloring.
#' @param facet logical. Variable to be categorized by faceted grid.
#' @param interactive logical. Print as girafe object instead of ggplot object?
#' @param order logical. Order by values instead of natural order?
#' @param flip logical. Flip x and y axes?
#' @param flat_print logical. Print as ggplot object instead of girafe object?
#' @param palette character. A color pallete to be used for the fill variable. See pallete argument here \link[ggplot2]{scale_fill_brewer}
#' @param legend logical. Include a legend?
#' @param confidence Confidence level for margin of error calculation. Defaults to 0.90. Set to NULL for standard error.
#' @param girafe_options Optional list of options to pass to \link[ggiraph]{girafe}.
#' @param ... Optional formatting arguments. See \link[surveyvisualize]{format_values}.
#'
#' @import ggiraph
#' @importFrom knitr is_html_output
#'
#' @export
make_chart <- function(tbl, x = NULL, y = NULL, fill = NULL, facet = NULL, interactive = TRUE,
                       order = FALSE, flip = FALSE, palette = 'Set3', variable_labels = TRUE, 
                       label_options = list(wrap = 35, trunc = 100), legend = TRUE, confidence = 0.95, 
                       use_girafe = TRUE, girafe_options = list(), ...) {

  loginfo(paste('Calling chart maker:', format(match.call())))
  
  # Apply margin of error
  tbl <- use_moe(tbl, confidence)

  if(is.null(y)) y <- 'Estimate'

  by <- attr(tbl,'by')
  agg_label <- attr(tbl,'agg_label')
  error <- attr(tbl,'error')
  by_label <- attr(tbl,'by_label')
  prop <- attr(tbl,'prop')
  prop_by <- NULL

  # Define feature and response variables
  if (is.null(by)) {
    value.vars <- c('Estimate','SE','Survey','N')
    response <- colnames(tbl)[colnames(tbl) %in% value.vars]
    by <- colnames(tbl)[!colnames(tbl) %in% response]
  }

  tbl[, (by) := lapply(.SD, gsub, pattern = "'", replacement = "’"), .SDcols = by]

  # If percentage argument is not passed, set the default
  format_arguments <- list(...)
  if(!'percentage' %in% names(format_arguments)) {
    format_arguments <- c(format_arguments, percentage = prop)
  }

  format_chart_values <- function(x) {
    do.call(format_values, c(list(x = x), format_arguments))
  }

  # Coerce all character variables as factors
  tbl[, by] <- lapply(tbl[, ..by], function(x) factor(x, levels = unique(x)))

  group_count <- length(by)
  group_level_count <- sapply(tbl[, ..by], function(x) length(levels(x)))
  groups_sorted <- names(sort(group_level_count))

  choose_group <- function(f) {
    if(is.null(f)) f <- groups_sorted[!groups_sorted %in% c(x, facet, fill)][1]
    return(f)
  }

  if (group_count == 1) {

    x <- choose_group(x)
    if (!is.null(fill)) warning('fill parameter not used with 1 group variable.')
    if (!is.null(facet)) warning('facet parameter not used with 1 group variable.')
    fill <- NULL
    facet <- NULL

  } else if (group_count == 2) {

    if(!is.null(facet) & !is.null(fill)) {
      warning('Cannot specify fill and facet with 2 group variables. Only fill parameter will be used.')
      facet <- NULL
    }
    x <- choose_group(x)
    if (is.null(fill)) fill <- prop_by
    if (is.null(facet)) fill <- choose_group(fill)

  } else if (group_count == 3) {

    if (is.null(facet)) {
      facet <- prop_by
      facet <- choose_group(facet)
    }
    x <- choose_group(x)
    fill <- choose_group(fill)

  } else stop('Cannot construct a chart with more than 3 group variables.')

  # Use variable labels or names
  if (variable_labels == TRUE) {

    x_label <- unlist(by_label[x])
    x_label <- trim_label(x_label, label_options$wrap, label_options$trunc)

    y_label <- agg_label
    y_label <- trim_label(y_label, label_options$wrap, label_options$trunc)

    fill_label <- unlist(by_label[fill])
    fill_label <- trim_label(fill_label, label_options$wrap, label_options$trunc)

  } else {
    x_label <- x
    y_label <- paste(agg_label, agg_var)
    fill_label <- fill
  }

  if(is.null(fill)) {
    fill <- y
    group <- NULL
    config_scale <- scale_fill_continuous(low = '#daadec', high = '#5f416b', labels = format_chart_values)
  } else {
    config_scale <- scale_fill_brewer(palette = palette)
    group <- fill
  }

  # Order by value
  if(order == T) tbl[[x]] <- tbl[, reorder(get(x), Estimate)]

  # Create confidence interval variables
  tbl$CI_max <- tbl[['Estimate']] + tbl[['SE']]
  tbl$CI_min <- tbl[['Estimate']] - tbl[['SE']]
  tbl$CI_min <- as.numeric(ifelse(tbl$CI_min < 0, 0, tbl$CI_min))

  # Create formatted copy of table for the tooltip
  formatted_tbl <- copy(tbl[, .(Estimate, SE)])
  formatted_tbl <- lapply(
    X = formatted_tbl,
    FUN = format_chart_values
  )

  # Create tooltip
  tbl$tooltip <- sprintf('<b>%s</b><br>%s &plusmn; %s',
    paste0(tbl[[x]], sprintf('<br>%s', tbl[,..by][[fill]])),
    formatted_tbl[['Estimate']],
    formatted_tbl[['SE']]
  )

  # Initiate ggplot object
  g <- ggplot(tbl, aes_string(btwrap(x), y, fill = btwrap(fill), group = btwrap(group)))

  # Add ggiraph bar chart interactivity
  if(interactive) {
    g <- g + geom_bar_interactive(
      aes(tooltip = tooltip, data_id = tooltip),
      stat = 'identity',
      position = position_dodge(width = 0.9)
    )
  } else {
    g <- g + geom_bar(stat = 'identity', position = position_dodge(width = 0.9))
  }

  # Add error bars
  g <- g + geom_errorbar(
    aes(ymax = CI_max, ymin = CI_min),
    position = position_dodge(width = 0.9),
    colour = '#d8490b',
    width = 0.25,
    alpha = 0.7
  )

  # Add Facet grid if applicable
  if(!is.null(facet)) g <- g + facet_grid(reformulate(btwrap(facet)), scales = 'free_y')

  # Specify theme
  g <- g + config_scale
  g <- g + labs(x = x_label, y = y_label, fill = fill_label)
  g <- g + theme_minimal()
  g <- g + theme(text = element_text(size = 8))
  g <- g + theme(plot.title = element_text(hjust = 0.5))
  g <- g + theme(strip.text.y = element_text(angle = 0))
  g <- g + theme(axis.text.x = element_text(angle = 50, hjust = 1, vjust = 1))
  g <- g + theme(legend.position = ifelse(legend, 'right', 'none'),  legend.text = element_text(size=8))
  g <- g + scale_y_continuous(labels = format_chart_values)
  g <- g + theme(plot.margin = margin(5, 5, 5, 25))

  if(flip) g <- g + coord_flip()

  if(
    (use_girafe == TRUE & !isTRUE(getOption('knitr.in.progress')))
    | (use_girafe == TRUE & is_html_output())
    ) {
    girafe_options_default <- list(
      ggobj = g,
      #hover_css = 'opacity: 0.5;stroke: #ffec8b; cursor: crosshair;'
      opts_hover(css = "opacity: 0.5; stroke: #ffec8b; cursor: crosshair;")
    )
    do.call(girafe, c(
        girafe_options_default[!names(girafe_options_default) %in% names(girafe_options)],
        girafe_options
      )
    )
  } else g

}

btwrap <- function(x) {
  if (length(x) == 0) return(NULL)
  sprintf('`%s`', x)
}

trim_label <- function(x, wrap_width, trunc_width) {
  wrap_width <- ifelse(is.null(wrap_width), 35, wrap_width)
  trunc_width <- ifelse(is.null(trunc_width), 100, trunc_width)
  regex_pattern <- sprintf('([[:print:]]{%s}[[:alnum:]]+).*', trunc_width)
  x <- sub(regex_pattern, "\\1...", x)
  x <- paste(strwrap(x, width = wrap_width), collapse = "\n")
  return(x)
}
