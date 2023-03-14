dtag_pred_adj <- function(x){
  pred_1 <-  pred_2 <-  pred_3 <-  pred_4 <-  pred_5 <- NULL
  x <- data.table(x)
  x[, pred_1 := d_grepl(shift(x, type="lag", n=1), sh["be"]) &
      d_grepl(x, "_JJ") &
      !d_grepl(shift(x, type="lead", n=1), "_JJ|_RB|_NN|_NNP")]
  x[, pred_2 := d_grepl(shift(x, type="lag", n=2), sh["be"]) &
      d_grepl(shift(x, type="lag", n=1), "_RB") &
      d_grepl(x, "_JJ") &
      !d_grepl(shift(x, type="lead", n=1), "_JJ|_RB|_NN|_NNP")]
  x[, pred_3 := d_grepl(shift(x, type="lag", n=2), sh["be"])  &
      d_grepl(shift(x, type="lag", n=1), "<XX0>") &
      d_grepl(x, "_JJ") &
      !d_grepl(shift(x, type="lead", n=1), "_JJ|_RB|_NN|_NNP")]
  x[, pred_4 := d_grepl(shift(x, type="lag", n=3), sh["be"]) &
      d_grepl(shift(x, type="lag", n=2), "<XX0>") &
      d_grepl(shift(x, type="lead", n=1), "_RB") &
      d_grepl(x, "_JJ") &
      !d_grepl(shift(x, type="lead", n=1), "_JJ|_RB|_NN|_NNP")]

  x[pred_1 == TRUE | pred_2 == TRUE | pred_3 == TRUE | pred_4 == TRUE,
    x := d_sub(x, "$", " <PRED>")]
  x[, pred_5 := str_detect(shift(x, type="lag", n=2), "<PRED>") &
      d_grepl(shift(x, type="lag", n=1), "_PHC|_CC") &
      d_grepl(x, "_JJ")]
  x[pred_5 == TRUE, x := d_sub(x, "$", " <PRED>")]

  return(x$x)
}
