#' @title Add Demonstratives  <DEMO> tag
#' @description Adds demonstratives <DEMO> based on a regex match.
#' @param x A character of tokenized strings with _ST tags.
#' @return A character vector with some demonstratives <DEMO> tags.
#' @export

dtag_demonstratives <- function(x){
 demonstratives <- NULL

  x <- data.table(x)
  x[, demonstratives := d_grepl(x, "\\bthat_|\\bthis_|\\bthese_|\\bthose_") &
      !d_grepl(x, "<DEMP>|<TOBJ>|<TSUB>|<THAC>|<THVC>|_NULL")]

  x[demonstratives == TRUE, x := d_sub(x, "$", " <DEMO>")]

  return(x$x)
}
