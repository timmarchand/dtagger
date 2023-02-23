#' @title Add perfect aspect <PEAS> tag
#' @description Adds the perfect aspect tag <PEAS> based on a regex match.
#' @param x A character of tokenized strings with _ST tags.
#' @return A character vector with some prefect aspect <PEAS> tags.
#' @export
dtag_perfect_asp <- function(x){

  perfect_asp1 <- perfect_asp2 <- perfect_asp3 <- perfect_asp4 <- NULL


  x <- data.table(x)
  x[, perfect_asp1 := d_grepl(x,sh["have"]) &
      d_grepl(shift(x, type="lead", n=1),"_VBD|_VBN")]

  x[, perfect_asp2 := d_grepl(x,sh["have"]) &
      d_grepl(shift(x, type="lead", n=1),"_RB|_XX0|_NN|_NNP|_PRP") &
      d_grepl(shift(x, type="lead", n=2),"_VBD|_VBN")]

  x[, perfect_asp3 := d_grepl(x,sh["have"]) &
      d_grepl(shift(x, type="lead", n=1),"_RB|_XX0") &
      d_grepl(shift(x, type="lead", n=2),"_RB|_XX0") &
      d_grepl(shift(x, type="lead", n=3),"_VBD|_VBN")]

  x[, perfect_asp4 := d_grepl(x,sh["have"]) &
      d_grepl(shift(x, type="lead", n=1),"_XX0") &
      d_grepl(shift(x, type="lead", n=2),"_NN|_NNP|_PRP") &
      d_grepl(shift(x, type="lead", n=3),"_VBD|_VBN")]

  x[perfect_asp1 == TRUE, x := d_sub(x, "$", " <PEAS>")]
  x[perfect_asp2 == TRUE, x := d_sub(x, "$", " <PEAS>")]
  x[perfect_asp3 == TRUE, x := d_sub(x, "$", " <PEAS>")]
  x[perfect_asp4 == TRUE, x := d_sub(x, "$", " <PEAS>")]

  return(x$x)
}
