#' @title Tag analytic negation
#' @description Adds the analytic negation tag  <XX0> based on a regex match.
#' @param x A character of tokenized strings with _ST tags.
#' @return A character vector with some negation <XX0> tags.
#' @export
dtag_negation <- function(x){
  x <- data.table(x)
  regex <- "\\bnot_RB|\\bn't_RB"
  x[d_grepl(x,regex), x:= d_sub(x,  "$", " <XX0>")]
  return(x$x)
}
