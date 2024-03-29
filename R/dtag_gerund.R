#' @title Tag gerunds
#' @description Adds the gerund tag  <GER> based on a regex match.
#' @param x A character of tokenized strings with _ST tags.
#' @return A character vector with some gerund <GER> tags.
#' @export
dtag_gerund <- function(x){
  regex <- "\\b\\w{4,}ings?_NN.?"
  x <- data.table(x)
  x[d_grepl(x, regex) &
      !d_grepl(x, "thing"), x:=d_sub(x, "$", " <GER>") ]
  return(x$x)
}
