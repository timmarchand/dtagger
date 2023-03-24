#' @title Tag indefinite prononouns
#' @description Adds the indefinite pronoun tag  <INPR> based on a regex match.
#' @param x A character of tokenized strings with _ST tags.
#' @return A character vector with some indefinite pronoun <INPR> tags.
#' @export
#'
dtag_ind_pron <- function(x){
  x <- data.table(x)
  regex <- "\\bnobody_|\\bnone_|\\bnothing_|\\bnowhere_"
 # regex <- "\\banybody_|\\banyone_|\\banything_|\\beverybody_|\\beveryone_|\\beverything_|\\bnobody_|\\bnone_|\\bnothing_|\\bnowhere_|\\bsomebody_|\\bsomeone_|\\bsomething_"
  x[d_grepl(x,regex), x:= d_sub(x, "$", " <INPR>")]
  return(x$x)
}
