#' @title Add third person pronoun <TPP3> tag
#' @description Adds the third person pronoun tag <TPP3> based on a regex match.
#' @param x A character of tokenized strings with _ST tags.
#' @return A character vector with some third person pronoun  <TPP3> tags.
#' @export
#'
dtag_third_person_pronoun <- function(x){
  regex <- "(\\bshe|\\bhe|\\bthey|\\bher|\\bhim|\\bthem|\\bhis|\\btheir|\\bimself|\\bherself|\\bthemselves)_\\w+"
  x <- data.table(x)
  x[d_grepl(x, regex), x:=d_sub(x, "$", " <TPP3>")]
  return(x$x)
}
