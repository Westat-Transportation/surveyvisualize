.onLoad <- function(libname, pkgname) {

  default_options <- list(
    # ======= Formatting ======= #
    surveyvisualize.format.digits = 2,
    surveyvisualize.format.percentage = FALSE,
    surveyvisualize.format.scientific = FALSE,
    surveyvisualize.format.multiplier = NULL
  )

  op <- options()
  toset <- !(names(default_options) %in% names(op))
  if(any(toset)) options(default_options[toset])

  invisible()
}
