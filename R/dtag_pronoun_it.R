#' @title Add it pronoun <PIT> tag
#' @description Adds the it pronoun tag <PIT> based on a regex match.
#' @param x A character of tokenized strings with _ST tags.
#' @return A character vector with some it pronoun  <PIT> tags.
#' @export
#'
dtag_pronoun_it <- function(x){
  regex <- "(\\bit|\\bits|\\bitself)_\\w+"
  x <- data.table(x)
  x[d_grepl(x, regex), x:=d_sub(x, "$", " <PIT>")]
  return(x$x)
}
