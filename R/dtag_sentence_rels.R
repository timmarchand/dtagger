#' @title Add sentence relatives <SERE> tag
#' @description Adds the sentence relatives tag <SERE> based on a regex match.
#' @param x A character of tokenized strings with _ST tags.
#' @return A character vector with some sentence relatives <SERE> tags.
#' @export
dtag_sentence_rels <- function(x){

  sentence_rels <- NULL

  x <- data.table(x)

  x[, sentence_rels := str_detect(shift(x, type="lag", n=1), "_\\W") & d_grepl(x,"\\bwhich_")]

  x[sentence_rels == TRUE, x := d_sub(x, "$", " <SERE>")]

  return(x$x)
}
