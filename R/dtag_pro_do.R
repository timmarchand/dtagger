#' @title Add pro verb do <PROD> tag
#' @description Adds the pro verb do tag <PROD> based on a regex match.
#' @param x A character of tokenized strings with _ST tags.
#' @return A character vector with some pro verb do <PROD> tags.
#' @export
dtag_pro_do <- function(x){

  pro_do <- NULL

  x <- data.table(x)
  x[, pro_do :=   !d_grepl_case(shift(x, type="lag", n=1),"(_\\W)") &
      !d_grepl(shift(x, type="lag", n=1), str_flatten(sh[c("wp","who")], "|")) &
      d_grepl(x,sh["do"]) &
      !d_grepl(shift(x, type="lead", n=1),"_V") &
      !d_grepl(shift(x, type="lead", n=1),"<XX0>") &
      !(d_grepl(shift(x, type="lead", n=1),"_RB|<XX0>") | d_grepl(shift(x, type="lead", n=2),"_V")) &
      !(d_grepl(shift(x, type="lead", n=1),"_RB|<XX0>") | d_grepl(shift(x, type="lead", n=1),"_RB") | d_grepl(shift(x, type="lead", n=3),"_V"))]

  x[pro_do == TRUE, x := d_sub(x, "$", " <PROD>")]

  return(x$x)
}
