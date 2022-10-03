#' @title Make HTML Table
#'
#' @description Make formatted tables.
#'
#' @param tbl Analysis table (data.table object).
#' @param row_vars Group variables to be represented in the row position.
#' @param col_vars Group variables to be represented in the column position.
#' @param confidence Confidence level for margin of error calculation. Defaults to 0.90. Set to NULL for standard error.
#' @param variable_labels logical. Use labels for variable names?
#' @param use_viewer logical. If interactive, render markdown table as html to be displayed in viewer pane?
#' @param stat_vars Select the statistic output columns to include or change or rearrange their default order.
#' @param ... Optional formatting arguments. See \link[surveyvisualize]{format_values}.
#' @return An html, pdf, or docx table.
#'
#' @importFrom pander pander pander_return
#' @importFrom data.table copy
#' @importFrom rstudioapi viewer
#' @importFrom rmarkdown render
#' @import logging


#' @export
make_table <- function(tbl, row_vars = NULL, col_vars = NULL, confidence = 0.95, variable_labels = TRUE, use_viewer = TRUE, stat_vars = c("Estimate","SE","Survey","N"), ...) {

  loginfo(paste('Calling table maker:', format(match.call())))
  
	# Rearrange or drop output stats columns as requested
	stat_vars_possible <- c("Estimate","SE","Survey","N")
	input_vars <- names(tbl)[!names(tbl) %in% stat_vars_possible]
	tbl <- tbl[, c(input_vars, stat_vars), with=F]
  
  if (!is.null(row_vars) & !is.null(col_vars)) {
    logerror('Specify either row_vars or col_vars, not both.')
    stop('Please specify either row_vars or col_vars, not both.')
  }
  
  # Grab labels for "group by" variables
  by_labels <- unlist(attr(tbl, 'by_label'))
  
  # Calculate margin of Error
  if (!is.null(confidence) & all(c("Estimate","SE") %in% input_vars)) {
    loginfo('Updating table to use Margin of Error.')
    tbl <- use_moe(tbl, confidence = confidence)
    setnames(tbl, 'SE', attr(tbl, 'moe_col'))
  }

  # Create contingency table
  ftbl <- make_crosstab(tbl, row_vars = row_vars, col_vars = col_vars, ...)
  
  # Grab ftbl row/col var attributes
  row_vars <- attr(ftbl, 'row.vars')
  col_vars <- attr(ftbl, 'col.vars')
  
  # Matrix version of ftbl
  mftbl <- trimws(format(ftbl, quote = F))
  
  # Get variable/value labels
  stat_labels <- unlist(c(col_vars, row_vars)[names(c(col_vars, row_vars)) == ''])
  group_labels <- unlist(c(col_vars, row_vars)[names(c(col_vars, row_vars)) != ''])
  group_var_labels <- names(c(col_vars, row_vars))[names(c(col_vars, row_vars)) != '']
  
  # Get the array indices for each variable/value label
  stat_label_cells <- which(array(mftbl %in% stat_labels, dim = dim(mftbl)), arr.ind = T)
  group_label_cells <- which(array(mftbl %in% group_labels, dim = dim(mftbl)), arr.ind = T)
  group_var_label_cells <- which(array(mftbl %in% group_var_labels, dim = dim(mftbl)), arr.ind = T)
  
  if (variable_labels == TRUE) {
    loginfo('Using variable labels.')
    for (var in names(by_labels)) {
      # Replace row variable names with labels
      names(attr(ftbl, 'row.vars'))[names(row_vars) %in% var] <- by_labels[var]
      # Replace row variable names with labels
      names(attr(ftbl, 'col.vars'))[names(col_vars) %in% var] <- by_labels[var]
    }
  }
  
  if (interactive() & use_viewer == TRUE) {
    loginfo('Using interactive table viewer to render markdown as HTML.')
    view_table(
      x = ftbl,
      split.table = Inf,
      emphasize.strong.cells = rbind(stat_label_cells, group_var_label_cells),
      emphasize.italics.cells = group_label_cells
    )
  } else {
    loginfo('Using pander to output markdown table (for non-interactive use).')
    pander(
      x = ftbl,
      split.table = Inf,
      emphasize.strong.cells = rbind(stat_label_cells, group_var_label_cells),
      emphasize.italics.cells = group_label_cells
    )
  }
  
}

#' @export
make_crosstab <- function(tbl, row_vars = NULL, col_vars = NULL, ...) {

  # Define feature and response variables
  features <- attr(tbl, 'by')
  response <- colnames(tbl)[!colnames(tbl) %in% features]
  
  if (length(features) == 0) {
    xtbl <- t(as.table(t(tbl)))
    row.names(xtbl) <- ''
    return(xtbl)
  }

  # Retain order of the table by setting levels of factors
  tbl[, (features) := lapply(.SD, function(x) factor(x, levels = unique(x))), .SDcols = features]

  # Build the xtabs formula
  rhs <- paste(btwrap(features), collapse = ' + ')
  lhs <- paste0('cbind(', paste(btwrap(response), collapse = ', '), ')')
  form <- as.formula(paste(lhs, rhs, sep = ' ~ '))

  # Create xtabs table
  xtbl <- xtabs(form, tbl, exclude = NULL, na.action=na.pass, drop.unused.levels = T)

  # Override formatting options if specified
  format_values_call <- function(x) {
    do.call(format_values, c(list(x = x), list(...)))
  }

  # Apply numeric formatting
  xtbl <- apply(xtbl, MARGIN = seq_along(dim(xtbl)), FUN = format_values_call)

  # Create ftable
  ftbl <- ftable(xtbl, row.vars = row_vars, col.vars = col_vars)

  return(ftbl)
}

view_table <- function(x, ...) {
  
  ftbl_pandoc <- pander_return(x, ...)

  temp_md <- tempfile(fileext = '.Rmd')
  writeLines(ftbl_pandoc, con = temp_md)
  temp_html <- paste0(basename(temp_md), '.html')
  render(
    input = temp_md, 
    output_file = temp_html,
    quiet = TRUE
  )
  html_view_file <- normalizePath(file.path(tempdir(), temp_html))
  viewer(html_view_file)
  unlink(c(temp_md, temp_html))
  
}

