#' @title Add THAT as verbal complement <THVC> tag
#' @description Adds THAT as verbal complement <THVC> based on regex matches.
#' @param x A character of tokenized strings with _ST tags.
#' @return A character vector with some THAT as verbal complement <THVC> tags.
#' @export
dtag_that_vc <- function(x){
 that_vc1 <- that_vc2 <- that_vc2 <- that_vc2 <- that_vc2 <- NULL

  x <- data.table(x)
  x[, that_vc1 := d_grepl_case(shift(x, type="lag", n=1), "\\band_|\\bnor_|\\bbut_|\\bor_|\\balso_|_\\W") &
      d_grepl(x, "\\bthat_") &
      d_grepl(shift(x, type="lead", n=1), "_DT|<QUAN>|_CD|_PRP|there_|_NNS|_NNP")]

  x[, that_vc2 := d_grepl(shift(x, type="lag", n=1), str_c(str_flatten(sh[c("public" , "private" , "suasive")],"|"),
                                                           "|\\bseem_|\\bseems_|\\bseemed_|\\bseeming_|\\bappear_|\\bappears_|\\bappeared_|\\bappearing_")) &
      d_grepl(x, "\\bthat_") &
      !d_grepl(shift(x, type="lead", n=1), "_V|_MD|\\band_|_\\W") &
      !d_grepl(shift(x, type="lead", n=1), str_flatten(sh[c("do" , "have" , "be")], "|"))]

  x[, that_vc3 := d_grepl(shift(x, type="lag", n=1), str_flatten(sh[c("public" , "private" , "suasive")],"|")) &
      d_grepl(x, "\\bthat_") &
      d_grepl(shift(x, type="lead", n=1), "_N") &
      d_grepl(shift(x, type="lead", n=2), "<PIN>") &
      !d_grepl(shift(x, type="lead", n=3), "_N")]


  x[, that_vc4 := d_grepl(shift(x, type="lag", n=1), str_flatten(sh[c("public" , "private" , "suasive")],"|")) &
      d_grepl(x, "\\bthat_") &
      d_grepl(shift(x, type="lead", n=2), "_N") &
      d_grepl(shift(x, type="lead", n=3), "<PIN>") &
      !d_grepl(shift(x, type="lead", n=4), "_N")]



  x[, that_vc5 := d_grepl(shift(x, type="lag", n=1), str_flatten(sh[c("public" , "private" , "suasive")],"|")) &
      d_grepl(x, "\\bthat_") &
      d_grepl(shift(x, type="lead", n=1), "_N") &
      !d_grepl(shift(x, type="lead", n=2), "_N") &!
      d_grepl(shift(x, type="lead", n=3), "_N") &
      !d_grepl(shift(x, type="lead", n=4), "_N") &
      d_grepl(shift(x, type="lead", n=5), "<PIN>")]

  x[that_vc1 == TRUE | that_vc2 == TRUE | that_vc2 == TRUE | that_vc2 == TRUE | that_vc2 == TRUE,
    x := d_sub(x, "$", " <THVC>")]

  return(x$x)
}
