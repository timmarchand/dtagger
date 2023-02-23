#' @title Add necessity modal <NEMD> tag
#' @description Adds the necessity modal tag <NEMD> based on a regex match.
#' @param x A character of tokenized strings with _ST tags.
#' @return A character vector with some necessity modal   <NEMD> tags.
#' @export
#'
dtag_necessity_modal <- function(x){
  x <- data.table(x)
  regex <- "(ought|should|must)_MD"
  x[d_grepl(x, regex), x:=d_sub(x, "$", " <NEMD>")]
  return(x$x)
}
