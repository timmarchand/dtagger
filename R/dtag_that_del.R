#' @title Add "That" Deletion <THATD> tag
#' @description Adds "that" deletion <THATD> based on regex matches.
#' @param x A character of tokenized strings with _ST tags.
#' @return A character vector with some "that" deletion <THATD> tags.
#' @export
dtag_that_del <- function(x){
  that_del1 <- that_del2 <- that_del3 <- that_del4 <- NULL

   x <- data.table(x)
  x[, that_del1 := d_grepl(x, str_flatten(sh[c("public" , "private" , "suasive")], "|")) &
      d_grepl(shift(x, type="lead", n=1), "<DEMP>|\\bi_|\\bwe_|\\bhe_|\\bshe_|\\bthey_")]

  x[, that_del2 := d_grepl(x, str_flatten(sh[c("public" , "private" , "suasive")], "|")) &
      d_grepl(shift(x, type="lead", n=1), "_PRP|_N") &
      (d_grepl(shift(x, type="lead", n=2), "_MD|_V") |
      d_grepl(shift(x, type="lead", n=2), str_flatten(sh[c("do","have","be")],"|")))]

  x[, that_del3 := d_grepl(x, str_flatten(sh[c("public" , "private" , "suasive")], "|")) &
      (d_grepl(shift(x, type="lead", n=1), "_PRP|_N") |
      d_grepl(shift(x, type="lead", n=1), "_JJ|_PRED|_RB|_DT|_QUAN|_CD|_PRPS")) &
      d_grepl(shift(x, type="lead", n=2), "_N") &
      (d_grepl(shift(x, type="lead", n=3), "_MD|_V") |
         d_grepl(shift(x, type="lead", n=3), str_flatten(sh[c("do","have","be")],"|")))]

  x[, that_del4 := d_grepl(x, str_flatten(sh[c("public" , "private" , "suasive")], "|")) &
      (d_grepl(shift(x, type="lead", n=1), "_PRP|_N") |
         d_grepl(shift(x, type="lead", n=1), "_JJ|_PRED|_RB|_DT|_QUAN|_CD|_PRPS")) &
      d_grepl(shift(x, type="lead", n=2), "_JJ|_PRED") &
      d_grepl(shift(x, type="lead", n=3), "_N") &
      (d_grepl(shift(x, type="lead", n=4), "_MD|_V") |
      d_grepl(shift(x, type="lead", n=4), str_flatten(sh[c("do","have","be")],"|")))]

  x[that_del1 == TRUE, x := d_sub(x, "$", " <THATD>")]
  x[that_del2 == TRUE, x := d_sub(x, "$", " <THATD>")]
  x[that_del3 == TRUE, x := d_sub(x, "$", " <THATD>")]
  x[that_del4 == TRUE, x := d_sub(x, "$", " <THATD>")]

  return(x$x)
}
