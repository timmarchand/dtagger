#' @title Add emphatic <EMPH> tag
#' @description Adds the emphatic tag <EMPH> based on a regex match.
#' @details In case of multiword units such as "a lot", the second token has
#' its _ST tag replaced with _NULL
#' @param x A character of tokenized strings with _ST tags.
#' @return A character vector with some emphatic <EMPH> tags.
#' @export
#
dtag_emphatics <- function(x){
emphatics1  <- emphatics2  <- emphatics3  <- null3 <- NULL
  x <- data.table(x)

  x[, emphatics1 := d_grepl(x, "\\bjust_|\\breally_|\\bmost_|\\bmore_")]

  x[, emphatics2 := d_grepl(x, "\\breal_") & d_grepl(shift(x, type="lead", n=1), "_JJ") |
      d_grepl(x, "\\bso_") & d_grepl(shift(x, type="lead", n=1), "_JJ") |
      d_grepl(x,sh["do"]) & d_grepl(shift(x, type="lead", n=1), "_V")]

  x[, emphatics3 := d_grepl(x, "\\bfor_") & d_grepl(shift(x, type="lead", n=1), "\\bsure_") |
      d_grepl(x, "\\ba_") & d_grepl(shift(x, type="lead", n=1), "\\blot_") |
      d_grepl(x, "\\bsuch_") & d_grepl(shift(x, type="lead", n=1), "\\ba_")]

  x[, null3 := d_grepl(shift(x, type="lag", n=1), "\\bfor_") & d_grepl(x, "\\bsure_") |
      d_grepl(shift(x, type="lag", n=1), "\\ba_") & d_grepl(x, "\\blot_") |
      d_grepl(shift(x, type="lag", n=1), "\\bsuch_") & d_grepl(x, "\\ba_")]

  x[emphatics1 == TRUE | emphatics2 == TRUE | emphatics3 == TRUE,
    x := d_sub(x, "(<PRED>)?$", " <EMPH>")]

  x[null3 == TRUE, x := d_sub(x, "_\\w+", "_NULL")]

  return(x$x)
}
