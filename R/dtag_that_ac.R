#' @title Add THAT as adjectival complement <THAC> tag
#' @description Adds THAT as adjectival complement <THAC> based on a regex match.
#' @param x A character of tokenized strings with _ST tags.
#' @return A character vector with some THAT as adjectival complement <THAC> tags.
#' @export

dtag_that_ac <- function(x){
  that_ac <- NULL

  x <- data.table(x)
  x[, that_ac := d_grepl(x, "\\bthat_") &
      d_grepl(shift(x, type="lag", n=1), "_JJ")]

  x[that_ac == TRUE, x := d_sub(x, "$", " <THAC>")]

  return(x$x)
}
