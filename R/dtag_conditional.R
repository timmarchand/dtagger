#' @title Add conditional subordinator <COND> tag
#' @description Adds the conditional subordinator tag <COND> based on a regex match.
#' @param x A character of tokenized strings with _ST tags.
#' @return A character vector with some conditional subordinator  <COND> tags.
#' @export
#'
dtag_conditional <- function(x){
  x <- data.table(x)
  regex <- "(\\bif|\\bunless)_\\w+"
  x[d_grepl(x, regex), x:=d_sub(x, "$", " <COND>")]
  return(x$x)
}
