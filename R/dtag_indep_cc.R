#' @title Add Independent Clause Coordination <ANDC> tag
#' @description Adds independent clause coordination <ANDC> based on regex matches.
#' @param x A character of tokenized strings with _ST tags.
#' @return A character vector with some independent clause coordination <ANDC> tags.
#' @export

dtag_indep_cc <- function(x){
  andc1 <- andc2 <- andc3 <- andc4 <- NULL

   x <- data.table(x)
  x[, andc1 := d_grepl_case(shift(x, type="lag", n=1), "_\\W") &
      d_grepl(x, "\\band_")]

  x[, andc2 := d_grepl(x, "\\band_") &
      (d_grepl(shift(x, type="lead", n=1), "\\bbecause_|\\balthough_|\\bthough_|\\btho_|\\bif_|\\bunless_|<OSUB>|<DPAR>|<CONJ>")|
         d_grepl(shift(x, type="lead", n=1),str_flatten(sh[c("wp","who")], "|")))]

  x[, andc3 := d_grepl(x, "_,") &
      d_grepl(shift(x, type="lead", n=1), "\\band_") &
      d_grepl(shift(x, type="lead", n=2), "\\bit_|\\bso_|\\bthen_|\\byou_|<DEMP>|\\bi_|\\bwe_|\\bhe_|\\bshe_|\\bthey_")]

  x[, andc4 := d_grepl(shift(x, type="lag", n=1), "_,") &
      d_grepl(x, "\\band_") &
      d_grepl(shift(x, type="lead", n=1), "\\bthere_") &
      d_grepl(shift(x, type="lead", n=2), sh["be"])]

  x[andc1 == TRUE | andc2 == TRUE | andc3 == TRUE | andc4 == TRUE,
    x:= d_sub(x, "(<PHC>)?$", " <ANDC>")]
  return(x$x)
}
