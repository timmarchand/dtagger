#' @title Correction of TO as preposition
#' @description Adds the preposition tag  <PIN> to preposition form of to_TO
#' @param x A character of tokenized strings with ST tags
#' @return A character vector with some preposition tags.
#' @export
#'


dtag_to_prep <- function(x){
  x <- data.table(x)

  x[, to_prep := d_grepl(x, "\\bto_") &
      d_grepl(shift(x, type="lead", n=1), str_flatten("_IN|_CD|_DT|_JJ|_PRPS|_WPS|_NN|_NNP|_PDT|_PRP|_WDT|_WRB", sh["wp"],"|")) ]

  x[to_prep == TRUE, x := d_sub(x, "$", " <PIN>")]

  return(x$x)
}
