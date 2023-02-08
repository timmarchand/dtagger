#' @title Correct Stanford (ST) possessive tags
#' @description  Correct Stanford (ST) possessive tags by replacing $ with S to make future regex matching easier
#' @param x a character vector of strings with ST tags
#' @return a character vector
#' @import data.table
#' @export
#'
#' @examples
#' x <- c("Their_PRP$", "house_NN", "was_VBD", "big_JJ", "and_CC", "luxurious_JJ", "._.")
#' dtag_possessives(x)

dtag_possessives <- function(x){

  x <- data.table::data.table(x)
  x[d_grepl_case(x, "(_PRP)."), x:= d_sub(x, "(_PRP).", "\\1S")]
  x[d_grepl_case(x, "(_WP)."), x:= d_sub(x, "(_WP).", "\\1S")]
  return(x$x)
}
