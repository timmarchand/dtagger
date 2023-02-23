#' @title Add second person pronoun <SPP2> tag
#' @description Adds the second person pronoun <SPP2> based on a regex match.
#' @param x A character of tokenized strings with _ST tags.
#' @return A character vector with some second person pronoun  <SPP2> tags.
#' @export
#'
dtag_second_person_pronoun <- function(x){
  regex <- "(\\byou|\\byour|\\byourself|\\byourselves|\\bthy|\\bthee|\\bthyself|\\bthou)_\\w+"
  x <- data.table(x)
  x[d_grepl(x, regex), x:=d_sub(x, "$", " <SPP2>")]
  return(x$x)
}
