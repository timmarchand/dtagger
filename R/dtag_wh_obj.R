#' @title Add WH Relative Clauses on Object Position <WHOBJ> tag
#' @description Adds WH relative clauses on object position <WHOBJ> based on regex matches.
#' @param x A character of tokenized strings with _ST tags.
#' @return A character vector with some WH relative clauses on object position <WHOBJ> tags.
#' @export
dtag_wh_obj <- function(x){
wh_obj1 <- NULL

  x <- data.table(x)
  x[, wh_obj1 := !d_grepl(shift(x, type="lag", n=3), "\\bask_|\\basks_|\\basked_|\\basking_|\\btell_|\\btells_|\\btold_|\\btelling_") &
      d_grepl(shift(x, type="lag", n=1), "_N") &
      d_grepl(x, sh["wp"]) &
     !d_grepl(shift(x, type="lead", n=1), "_RB|_XX0|_MD|_V") &
     !d_grepl(shift(x, type="lead", n=1), str_flatten(sh[c("do" , "have" , "be")], "|"))]

  x[wh_obj1 == TRUE, x := d_sub(x, "$", " <WHOBJ>")]

  return(x$x)
}
