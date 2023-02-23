#' @title Tag nominalisation
#' @description Adds the nominalisation tag  <NOMZ> based on a regex match.
#' @param x A character of tokenized strings with _ST tags.
#' @return A character vector with some nominalisation <NOMZ> tags.
#' @export

dtag_nominalisation <- function(x){
  regex <- "(tion|tions|ment|ments|ness|nesses|ity|ities)_\\w+"
  x <- data.table(x)
  x[str_detect(x, regex), x:=str_replace(x, "$", " <NOMZ>")]
  return(x$x)
}
