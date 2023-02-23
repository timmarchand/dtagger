#' @title Add contractions <CONT> tag
#' @description Adds the contraction tag <CONT> based on a regex match.
#' @param x A character of tokenized strings with _ST tags.
#' @return A character vector with some contraction <CONT> tags.
#' @export
dtag_contractions <- function(x){
    x <- data.table(x)
  regex <- "'\\w+_V|n't_RB|'ll|'d"
  x[d_grepl(x,regex), x:= d_sub(x[d_grepl(x,regex)],
                                                "$",
                                                " <CONT>")]
  return(x$x)
}
