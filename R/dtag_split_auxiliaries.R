#' @title Add split auxiliary <SPAU> tag
#' @description Adds split auxiliary <SPAU> based on a regex match.
#' @param x A character of tokenized strings with _ST tags.
#' @return A character vector with some split auxiliary  <SPAU> tags.
#' @export
dtag_split_auxiliaries <- function(x){

  split_aux1 <- NULL
    split_aux2 <- NULL
  x <- data.table(x)

  x[, split_aux1 := d_grepl(x,str_c(str_flatten(sh[c("do" , "have" , "be")],"|"),"|_MD")) &
      d_grepl(shift(x, type="lead", n=1), "_RB|\\bjust_|\\breally_|\\bmost_|\\bmore_|<AMP>|<DWNT>") &
      !d_grepl(shift(x, type="lead", n=1), "n't_|not") &
      d_grepl(shift(x, type="lead", n=2), "_V")]

    x[, split_aux2 := d_grepl(x,str_c(str_flatten(sh[c("do" , "have" , "be")],"|"),"|_MD")) &
      d_grepl(shift(x, type="lead", n=1), "_RB|\\bjust_|\\breally_|\\bmost_|\\bmore_|<AMP>|<DWNT>") &
      d_grepl(shift(x, type="lead", n=2), "_RB") &
      d_grepl(shift(x, type="lead", n=3), "_V")]

  x[split_aux1 == TRUE | split_aux2 == TRUE , x := d_sub(x, "$", " <SPAU>")]

  return(x$x)
}
