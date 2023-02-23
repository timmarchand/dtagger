#' @title Add "that" Relative Clauses on Object Position  <TOBJ> tag
#' @description Adds "that" relative clauses on object position <TOBJ> based on regex matches.
#' @param x A character of tokenized strings with _ST tags.
#' @return A character vector with some "that" relative clauses on object Position <TOBJ> tags.
#' @export
dtag_that_obj <- function(x){
that_obj1 <- NULL

  x <- data.table(x)

  x[, that_obj1 := d_grepl(shift(x, type="lag", n=1), "_N") &
      d_grepl(x, "\\bthat_") &
      (d_grepl(shift(x, type="lead", n=1), "_DT|_QUAN|_CD|\\bit_|_JJ|_NNS|_NNP|_PRPS|\\bi_|\\bwe_|\\bhe_|\\bshe_|\\bthey_") |
         (d_grepl(shift(x, type="lead", n=1), "_N")  &
            d_grepl(shift(x, type="lead", n=2), "_POS")))]

  x[that_obj1 == TRUE, x := d_sub(x, "$", " <TOBJ>")]

  return(x$x)

}
