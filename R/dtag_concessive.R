#' @title Add concessive subordinator <CONC> tag
#' @description Adds the concessive subordinator tag <CONC> based on a regex match.
#' @details The function matches "although" and both "though" and the shortened
#' form "tho" as concessive subordinators.
#' @param x A character of tokenized strings with _ST tags.
#' @return A character vector with some concessive subordinator  <CONC> tags.
#' @export
#'
dtag_concessive <- function(x){
  x <- data.table(x)
  regex <-"(\\balthough|\\bthough|\\btho)_\\w+"
  x[d_grepl(x, regex), x:=d_sub(x, "$", " <CONC>")]
  return(x$x)
}
