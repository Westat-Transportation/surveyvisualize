#' @title Make HTML Table
#'
#' @description Make formatted HTML tables utilizing \link[kableExtra]{kableExtra-package}
#'
#' @param tbl Analysis table (data.table object).
#' @param row_vars Group variables to be represented in the row position.
#' @param col_vars Group variables to be represented in the column position.
#' @param title Title of the table
#' @param confidence Confidence level for margin of error calculation. Defaults to 0.90. Set to NULL for standard error.
#' @param ... Optional formatting arguments. See \link[surveyvisualize]{format_values}.
#' @return A kableExtra object.
#'
#' @import kableExtra


#' @export
make_table <- function(tbl, row_vars = NULL, col_vars = NULL, title = NULL, confidence = 0.90, ...) {

  if (!is.null(confidence)) {
    tbl <- use_moe(tbl, confidence = confidence)
    setnames(tbl, 'SE', attr(tbl, 'moe_col'))
  }

  ftbl <- make_crosstab(tbl, row_vars = row_vars, col_vars = col_vars, ...)

  # Get a lists of the row column variables
  row_vars <- attr(ftbl, 'row.vars')
  col_vars <- attr(ftbl, 'col.vars')

  if (length(row_vars) > 2) {
    stop('make_table does not support tables with more than 2 row variables.')
  }

  # Get dimensions
  n_rows <- dim(ftbl)[1]
  n_cols <- dim(ftbl)[2]

  # Create aggregate matrix of ftable
  stat_tbl <- matrix(ftbl, nrow = n_rows, ncol = n_cols)

  # Formatted ftbl - Matrix includeing row/col headers
  fftbl <- trimws(format(ftbl, quote = FALSE, method = 'compact'))

  # Extract column and row names
  col_names <- tail(fftbl[length(col_vars), ], n_cols)
  row_names <- tail(fftbl[, length(row_vars)], n_rows)

  # Assign row and column names
  colnames(stat_tbl) <- col_names
  mftbl <- cbind(row_names, stat_tbl)
  colnames(mftbl)[1] <- paste(names(row_vars)[!names(row_vars) %in% 'Statistic'], collapse = ' / <br>')

  # For complex crosstabulations:
  # Column Groups added above main colnames.
  col_groups <- col_vars[-length(col_vars)]
  # Row groups added for tables with more than one row variable.
  row_groups <- row_vars[-length(row_vars)]

  # Meta data for kable styling
  n_row_header <- length(row_vars) - length(row_groups)
  kable_ncol <- ncol(mftbl) + n_row_header
  n_col_header <- length(unique(col_names))

  # Columns to apply right-border
  border_cols <- seq(n_row_header, kable_ncol - 1, n_col_header)
  # Columns containing statistics
  stat_cols <- 1:n_cols + n_row_header

  # Construct Styled Kable Table
  kbl <- kable(mftbl, escape = F, caption = title) %>%
    kable_styling(
      bootstrap_options = c("hover", "condensed"),
      font_size = 10.5,
      full_width = FALSE,
      position = "center"
    ) %>%
    # Add borders
    column_spec(
      column = border_cols,
      extra_css = 'border-right: 1px dotted lightgrey;'
    ) %>%
    # Make row header bold
    column_spec(
      column = n_row_header,
      extra_css = 'font-weight: bold;'
    ) %>%
    # Align statistics to the right
    column_spec(
      column = stat_cols,
      extra_css = 'text-align: right;'
    ) %>%
    column_spec(
      column = n_row_header,
      width_min = '12em',
      extra_css = 'color: #444444; text-align: center;'
    )

  # Add column groups
  for (i in rev(seq_along(col_groups))) {

    # Get variable details
    var_name <- names(col_groups[i])
    var_labs <- col_groups[[i]]

    # Calculate span
    row <- tail(fftbl[i, ], n_cols)
    n_vals <- sum(row %in% var_labs)
    span <- n_cols / n_vals

    # Create header input
    header <- rep(span, n_vals)
    names(header) <- array(var_labs, length(header))
    header <- c(var_name, header)

    kbl <- add_header_above(kbl, header, line = TRUE)
  }

  # Add row groups
  for (i in rev(seq_along(row_groups))) {

    # Get variable details
    var_name <- names(row_groups[i])
    var_labs <- row_groups[[i]]

    col <- tail(fftbl[, i], n_rows)
    n_vals <- sum(col %in% var_labs)
    span <- n_rows / n_vals

    header <- rep(span, n_vals)
    names(header) <- array(var_labs, length(header))
    group_row_css <- 'border-bottom: 1px solid; background-color: #666; color: #fff;'
    kbl <- group_rows(kbl, index = header, label_row_css = group_row_css)

  }

  return(kbl)
}


#' @export
html_table_to_docx <- function(tbl, output) {
  tmp_file <- tempfile(fileext = '.html')
  suppressWarnings(kableExtra::save_kable(tbl, file = tmp_file, self_contained = T))
  rmarkdown::pandoc_convert(tmp_file, to = 'docx', output = basename(output))
  tmp_docx <- normalizePath(file.path(dirname(tmp_file), basename(output)))
  file.copy(tmp_docx, output, overwrite = TRUE)
  unlink(c(tmp_file, tmp_docx))
}



make_crosstab <- function(tbl, row_vars = NULL, col_vars = NULL, col_level_threshold = 8, ...) {

  value.vars <- c('Estimate','SE','Survey','N')

  # Define feature and response variables
  features <- key(tbl)
  response <- colnames(tbl)[!colnames(tbl) %in% features]

  if (is.null(features)) {
    response <- colnames(tbl)[colnames(tbl) %in% value.vars | grepl('^MOE \\([0-9]+%\\)$', colnames(tbl))]
    features <- colnames(tbl)[!colnames(tbl) %in% response]
  }

  # Retain order of the table by setting levels of factors
  tbl[, (features) := lapply(.SD, function(x) factor(x, levels = unique(x))), .SDcols = features]

  # Build the xtabs formula
  rhs <- paste(btwrap(features), collapse = ' + ')
  lhs <- paste0('cbind(', paste(btwrap(response), collapse = ', '), ')')
  form <- as.formula(paste(lhs, rhs, sep = ' ~ '))

  # Create xtabs table
  xtbl <- xtabs(form, tbl, exclude = NULL, na.action=na.pass, drop.unused.levels = T)

  if(is.null(row_vars) & is.null(col_vars)) {
    # List of xtable dimensions
    xtbl_dim <- lapply(attr(xtbl, "dimnames"), length)

    #If there are less than 4 table dimensions
    if(length(xtbl_dim) < 4) {
      # Dimensions greater than the threshold are the row_vars
      row_vars <- names(xtbl_dim[xtbl_dim > col_level_threshold])
      # If no row_vars greater then the threshold, then the largest dimension is the row_var
      if(length(row_vars) == 0) row_vars <- names(xtbl_dim[order(-unlist(xtbl_dim))])[1]
    } else {
      # With 4 or more table dimensions, the two largest dimensions are the row_vars
      xtbl_dim <- xtbl_dim[names(xtbl_dim) != '']
      row_vars <- names(xtbl_dim[order(-unlist(xtbl_dim))])[1:2]
    }
  }

  if (all(names(dimnames(xtbl))[!names(dimnames(xtbl)) %in% col_vars] %in% '')) {
    names(dimnames(xtbl))[names(dimnames(xtbl)) == ''] <- tail(col_vars, 1)
  } else {
    names(dimnames(xtbl))[names(dimnames(xtbl)) == ''] <- 'Statistic'
  }

  # Override formatting options if specified
  format_values_call <- function(x) {
    do.call(format_values, c(list(x = x), list(...)))
  }

  # Apply numeric formatting
  xtbl <- tapply(X = xtbl, INDEX = expand.grid(dimnames(xtbl)), FUN = format_values_call)

  # Create ftable
  ftbl <- ftable(xtbl, row.vars = row_vars, col.vars = col_vars)

  return(ftbl)
}

