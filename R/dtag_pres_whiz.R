#' @title Add present participial WHIZ deletion relative <WZPRES> tag
#' @description Adds present participial WHIZ deletion relative <WZPRES> based on regex matches.
#' @param x A character of tokenized strings with _ST tags.
#' @return A character vector with some present participial WHIZ deletion relative <WZPRES> tags.
#' @export
dtag_pres_whiz <- function(x){
 pres_whiz1 <- NULL

  x <- data.table(x)
  x[, pres_whiz1 := d_grepl(shift(x, type="lag", n=1), "_N") &
  d_grepl(x, "_VBG")]

  x[pres_whiz1 == TRUE, x := d_sub(x, "$", " <WZPRES>")]

  return(x$x)
}

