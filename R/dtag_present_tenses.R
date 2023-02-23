#' @title Add present tense <VPRT> tag to all verbs in present tense
#' @description Adds the generic present tense tag  <VPRT> to all present tenses
#'  based on regex match.
#' @param x A character of tokenized strings with _ST tags.
#' @return A character vector with all present tense verb matches  tagged by <VPRT>.
#' @export
dtag_present_tenses <- function(x){
  regex <- "_VBP|_VBZ"
  x <- data.table(x)
  x[d_grepl(x, regex), x:=d_sub(x, "$", " <VPRT>")]
  return(x$x)
}

