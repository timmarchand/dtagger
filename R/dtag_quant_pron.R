#' @title Tag quantifier pronouns
#' @description Adds the quantifier pronoun tag  <QUPR> based on a regex match.
#' @param x A character of tokenized strings with _ST tags.
#' @return A character vector with some quantifier pronoun <QUPR> tags.
#' @export
#'
dtag_quant_pron <- function(x){
  x <- data.table(x)
  regex <- "\\beverybody_|\\bsomebody_|\\banybody_|\\beveryone_|\\bsomeone_|\\banyone_|\\beverything_|\\bsomething_|\\banything_"
  x[d_grepl(x,regex), x:= d_sub(x, "$", " <QUPR>")]
  return(x$x)
}
