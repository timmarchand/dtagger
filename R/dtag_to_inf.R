#' @title Tag infinitive to
#' @description Adds the infinitive to tag  <TO> to all to tokens based on a regex match,
#' @details Adds the infinitive to tag  <TO> to all tokens of the form "to_".
#' The function should be followed by the dtag_to_prep() function to distinguish
#' to when used as a preposition.
#' @param x A character of tokenized strings with _ST tags.
#' @return A character vector with infinitive to tag  <TO> added.
#' @export
dtag_to_inf <- function(x){
  x <- data.table(x)
  x[d_grepl(x, "\\bto_"), x:=d_sub(x, "$", " <TO>")]
  return(x$x)
}

