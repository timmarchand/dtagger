#' @title Add BE as main verb <BEMA> tag
#' @description Adds the BE as main verb tag <BEMA> based on a regex match.
#' @param x A character of tokenized strings with _ST tags.
#' @return A character vector with some BE as main verb <BEMA> tags.
#' @export
dtag_be_main <- function(x){

  be_main1 <- be_main2 <- NULL

    x <- data.table(x)

    x[, be_main1 := !d_grepl(shift(x, type="lag", n=2), "_EX") &
        !d_grepl(shift(x, type="lag", n=1), "_EX") &
        d_grepl(x,sh["be"]) &
        d_grepl(shift(x, type="lead", n=1),"_CD|_DT|_PDT|_PRPS|_PRP|_JJ|_PRED|<PIN>|_QUAN")]

    x[, be_main2 := !d_grepl(shift(x, type="lag", n=2), "_EX") &
        !d_grepl(shift(x, type="lag", n=1), "_EX") &
        d_grepl(x,sh["be"]) &
        d_grepl(shift(x, type="lead", n=1),"_RB|_XX0") &
        d_grepl(shift(x, type="lead", n=2),"_CD|_DT|_PDT|_PRPS|_PRP|_JJ|_PRED|<PIN>|_QUAN")]

    x[be_main1 == TRUE, x := d_sub(x, "$", " <BEMA>")]
    x[be_main2 == TRUE, x := d_sub(x, "$", " <BEMA>")]

    return(x$x)
  }
