#' @title Add discourse particles <DPART> tag
#' @description Adds the discourse particles tag <DPART> based on a regex match.
#' @param x A character of tokenized strings with _ST tags.
#' @return A character vector with some discourse particles <DPART> tags.
#' @export
dtag_disc_part <- function(x){
 disc_part <- NULL

  x <- data.table(x)
  x[, disc_part := str_detect(shift(x, type="lag", n=1), "_\\W") &
      d_grepl(x, "\\bwell_|\\bnow_|\\banyhow_|\\banyways_")]

  x[disc_part == TRUE, x := d_sub(x, "$", " <DPAR>")]

  return(x$x)
}
