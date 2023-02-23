#' @title Add past participial WHIZ deletion relative <WZPAST> tag
#' @description Adds past participial WHIZ deletion relative <WZPAST> based on regex matches.
#' @param x A character of tokenized strings with _ST tags.
#' @return A character vector with some past participial WHIZ deletion relative <WZPAST> tags.
#' @export
dtag_past_whiz <- function(x){
 past_whiz1 <- NULL

 x <- data.table(x)
  x[, past_whiz1 := (d_grepl(shift(x, type="lag", n=1), "_N") |
      d_grepl(shift(x, type="lag", n=1), "_QUPR")) &
      d_grepl(x, "_VBN") &
      (d_grepl(shift(x, type="lead", n=1), "<PIN>") |
      d_grepl(shift(x, type="lead", n=1), "_RB") |
      d_grepl(shift(x, type="lead", n=1), sh["be"]))]

  x[past_whiz1 == TRUE, x := d_sub(x, "$", " <WZPAST>")]

  return(x$x)
}
