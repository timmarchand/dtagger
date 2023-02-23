#' @title Add split infinitive <SPIN> tag
#' @description Adds split infinitive <SPIN> based on a regex match.
#' @param x A character of tokenized strings with _ST tags.
#' @return A character vector with some split infinitive  <SPIN> tags.
#' @export
dtag_split_infinitives <- function(x){

  split_infin1 <- split_infin2 <- NULL
  x <- data.table(x)

  x[, split_infin1 := d_grepl(x, "\\bto_") &
      d_grepl(shift(x, type="lead", n=1), "_RB|_AMPLIF|_DOWNTON|\\bjust_|\\breally_|\\bmost_|\\bmore_") &
      d_grepl(shift(x, type="lead", n=2), "_V")]

  x[, split_infin2 := d_grepl(x, "\\bto_") &
      d_grepl(shift(x, type="lead", n=1), "_RB|_AMPLIF|_DOWNTON|\\bjust_|\\breally_|\\bmost_|\\bmore_") &
      d_grepl(shift(x, type="lead", n=2), "_RB|_AMPLIF|_DOWNTON") &
      d_grepl(shift(x, type="lead", n=3), "_V")]

  x[split_infin1 == TRUE, x := d_sub(x, "$", " <SPIN>")]
  x[split_infin2 == TRUE, x := d_sub(x, "$", " <SPIN>")]

  return(x$x)
}

