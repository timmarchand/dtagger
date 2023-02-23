#' @title Add past tense <VBD> tag to all verbs in past tense
#' @description Adds the generic past tense tag  <VBD> to all past tenses
#'  based on regex match.
#' @param x A character of tokenized strings with _ST tags.
#' @return A character vector with all past tense verb matches  tagged by <VBD>.
#' @export
dtag_past_tenses <- function(x){
  regex <- "_VBD"
  x <- data.table(x)
  x[d_grepl(x, regex), x:=d_sub(x, "$", " <VBD>")]
  return(x$x)
}

