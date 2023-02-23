#' @title Add past participial <PASTP> tag
#' @description Adds past participial <PASTP> based on regex matches.
#' @param x A character of tokenized strings with _ST tags.
#' @return A character vector with some past participial <PASTP> tags.
#' @export
dtag_past_part <- function(x){
pastp1 <- NULL

  x <- data.table(x)
  x[, pastp1 := d_grepl(x, "_VBN") &
      (is.na(shift(x, type="lag", n=1)) |
         str_detect(shift(x, type="lag", n=1), "_\\W")) &
      (d_grepl(shift(x, type="lead", n=1), "<PIN>") |
         d_grepl(shift(x, type="lead", n=1), "_RB"))]

  x[pastp1 == TRUE, x := d_sub(x, "$", " <PASTP>")]

  return(x$x)
}
