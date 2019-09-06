#' @importFrom data.table copy

#' @export
# use_moe
use_moe <- function(tbl, confidence = 0.90, se_col = 'SE') {

  if (!is.null(confidence)) {

    if (confidence < .5 | confidence >= 1) {
      stop('Confidence level must be between 0.5 and 1.')
    }

    alpha <- 1 - confidence
    standard_score <- qnorm(alpha / 2, lower.tail = F)

    moe_col <- sprintf('MOE (%s%%)', 100 * confidence)
    out_tbl <- copy(tbl)
    out_tbl[, (se_col) := get(se_col) * standard_score]

    # If Standard Error has no decimal places than round the MOE
    if (all(tbl[[se_col]] == as.integer(tbl[[se_col]]), na.rm = T)) {
      out_tbl[, (se_col) := round(get(se_col))]
    }
    # out_tbl[, (se_col) := NULL]
    setattr(out_tbl, 'moe_col', moe_col)
    return(out_tbl[])
  } else {
    # Show standard error
    return(tbl[])
  }

}

