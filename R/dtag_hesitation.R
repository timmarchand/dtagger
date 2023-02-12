#' @title Add hesitation markers
#' @description Add hesitation marker <HSTN> to a vector of tokenized strings
#' @param x A character vector.
#' @param regex A regular expression (default "\\berm?\\b|\\berm?_|\\bum\\b|\\bum_").
#' The regex expression is case insensitive by default.
#' @return A character vector with hesitation tags appended.
#' @export
#'
#' @examples
#' dtag_hesitation(c("I'm", "not", "sure",".", "Um" ,"," ,"no"))
#'
dtag_hesitation <- function(x, regex =  "\\berm?\\b|\\berm?_|\\bum\\b|\\bum_" ){
  regex <- {{regex}}
  x <- data.table(x)
   x[d_grepl(x,regex), x:= d_sub(x, "$", " <HSTN>")]
  return(x$x)
}
