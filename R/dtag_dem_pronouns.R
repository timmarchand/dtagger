#' @title Add Demonstrative Pronouns <DEMP> tag
#' @description Adds demonstrative pronouns <DEMP> based on regex matches.
#' @param x A character of tokenized strings with _ST tags.
#' @return A character vector with some demonstrative pronouns <DEMP> tags.
#' @export

dtag_dem_pronouns <- function(x){
 dem_pronouns1 <- dem_pronouns2 <- NULL

  x <- data.table(x)
  x[, dem_pronouns1 := d_grepl(x, "\\bthat_|\\bthis_|\\bthese_|\\bthose_") &
      !d_grepl(x, "_NULL") &
      (d_grepl(shift(x, type="lead", n=1), "_V|_MD|_\\W|\\band_") |
         d_grepl(shift(x, type="lead", n=1), str_flatten(sh[c("do","have","be","wp")], "|"))) &
      !d_grepl(x, "<TOBJ>|<TSUB>|<THAC>|<THVC>")]

  x[, dem_pronouns2 := d_grepl(x, "\\bthat_") &
      d_grepl(shift(x, type="lead", n=1), "[i']s_")]

  x[dem_pronouns1 == TRUE, x := d_sub(x, "$", " <DEMP>")]
  x[dem_pronouns2 == TRUE, x := d_sub(x, "$", " <DEMP>")]

  return(x$x)
}
