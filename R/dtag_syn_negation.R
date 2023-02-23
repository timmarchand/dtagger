#' @title Add synthetic negation <SYNE> tag
#' @description Adds synthetic negation <SYNE> based on a regex match.
#' @param x A character of tokenized strings with _ST tags.
#' @return A character vector with some synthetic negation <SYNE> tags.
#' @export
dtag_syn_negation <- function(x){

  syn_neg1 <- syn_neg2 <- syn_neg3 <- NULL

  x <- data.table(x)
  x[, syn_neg1 := d_grepl(x, "\\bno_") & d_grepl(shift(x, type="lead", n=1), "JJ|PRED|NN|NNP")]
  x[, syn_neg2 := d_grepl(x, "\\bneither_")]
  x[, syn_neg3 := d_grepl(x, "\\bnor_")]

  x[syn_neg1 == TRUE | syn_neg2 == TRUE | syn_neg3 == TRUE, x := d_sub(x, "$", " <SYNE>")]

  return(x$x)
}
