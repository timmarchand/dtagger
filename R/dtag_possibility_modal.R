#' @title Add possibility modal <POMD> tag
#' @description Adds the possibility modal tag <POMD> based on a regex match.
#' @param x A character of tokenized strings with _ST tags.
#' @return A character vector with some possibility modal   <POMD> tags.
#' @export
#'
dtag_possibility_modal <- function(x){
  x <- data.table(x)
  regex <- "(\\bcan|\\bmay|\\bmight|\\bcould|\\bca)_MD"
  x[d_grepl(x, regex), x:=d_sub(x, "$", " <POMD>")]
  return(x$x)
}
