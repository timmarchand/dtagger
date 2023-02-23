#' @title Add first person pronoun <FPP1> tag
#' @description Adds the first person pronoun tag <FPP1> based on a regex match.
#' @param x A character of tokenized strings with _ST tags.
#' @return A character vector with some first person pronoun  <FPP1> tags.
#' @export
#'
dtag_first_person_pronoun <- function(x){
  regex <- "(\\bI_|\\b[Mm]e_|\\b[Ww]e_|\\b[Uu]s_|\\b[Mm]y_|\\b[Oo]ur_|\\b[Mm]yself_|\\b[Oo]urselves_)\\w+"
  x <- data.table(x)
  x[d_grepl_case(x, regex), x:=d_sub(x, "$", " <FPP1>")]
  return(x$x)
}
