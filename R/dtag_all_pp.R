#' @title Add past participle <VBN> tag to all past participle verb forms
#' @description Adds the generic past participle  tag  <VBN> to allpast participle verb forms
#'  based on regex match.
#' @param x A character of tokenized strings with _ST tags.
#' @return A character vector with all past participle verb matches tagged by <VBN>.
#' @export
dtag_past_tenses <- function(x){
  regex <- "_VBN"
  x <- data.table(x)
  x[d_grepl(x, regex), x:=d_sub(x, "$", " <VBN>")]
  return(x$x)
}

