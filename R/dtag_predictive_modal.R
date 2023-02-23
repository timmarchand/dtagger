#' @title Add predictive modal <PRMD> tag
#' @description Adds the predictive modal tag <PRMD> based on a regex match.
#' @param x A character of tokenized strings with _ST tags.
#' @return A character vector with some predictive modal   <PRMD> tags.
#' @export
#'
dtag_predictive_modal <- function(x){
  x <- data.table(x)
  regex <- "(\\bwill|'ll|\\bwo|\\bwould|\\bshall|\\bsha|'d)_MD"
  x[d_grepl(x,  regex), x:=d_sub(x, "$", " <PRMD>")]
  return(x$x)
}
