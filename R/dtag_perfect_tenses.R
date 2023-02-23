#' @title Add perfect form <VBN> tag to all verbs with perfect participle form
#' @description Adds the genericperfect form <VBN> to all perfect verb forms
#'  based on regex match.
#' @param x A character of tokenized strings with _ST tags.
#' @return A character vector with all perfect form matches  tagged by <VBN>.
#' @export
dtag_past_tenses <- function(x){
  regex <- "_VBN"
  x <- data.table(x)
  x[d_grepl(x, regex), x:=d_sub(x, "$", " <VBN>")]
  return(x$x)
}

