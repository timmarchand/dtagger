#' @title Add causative <CAUS> tag
#' @description Adds the causative tag <CAUS> based on a regex match.
#' @details The function matches both "because" and the informal "cos" as causatives.
#' @param x A character of tokenized strings with _ST tags.
#' @return A character vector with some causative  <CAUS> tags.
#' @export
#'
dtag_causative <- function(x){
  x <- data.table(x)
  regex <- "(\\bbecause|\\bcos)_\\w+" # cos added for spoken
  x[d_grepl(x, regex), x:=d_sub(x, "_\\w+", "_IN <CAUS>")] # replace cos tag
  return(x$x)
}
