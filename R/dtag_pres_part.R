#' @title Add present participial <PRESP> tag
#' @description Adds present participial <PRESP> based on regex matches.
#' @param x A character of tokenized strings with _ST tags.
#' @return A character vector with some present participial <PRESP> tags.
#' @export
dtag_pres_part <- function(x){
presp1 <- NULL

  x <- data.table(x)
  x[, presp1 :=  d_grepl(x, "_VBG") &
      (is.na(shift(x, type="lag", n=1)) |
      str_detect(shift(x, type="lag", n=1), "_\\W")) &
      (d_grepl(shift(x, type="lead", n=1), "<PIN>") |
         d_grepl(shift(x, type="lead", n=1), "_DT") |
         d_grepl(shift(x, type="lead", n=1), "_QUAN") |
         d_grepl(shift(x, type="lead", n=1), "_CD") |
         d_grepl(shift(x, type="lead", n=1), sh["wp"]) |
         d_grepl(shift(x, type="lead", n=1), "_WPS") |
         d_grepl(shift(x, type="lead", n=1), sh["who"]) |
         d_grepl(shift(x, type="lead", n=1), "_PRP") |
         d_grepl(shift(x, type="lead", n=1), "_RB"))]

  x[presp1 == TRUE, x := d_sub(x, "$", " <PRESP>")]

  return(x$x)
}
