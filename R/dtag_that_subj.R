#' @title Add "that" Relative Clauses on Subject Position  <TSUB> tag
#' @description Adds "that" relative clauses on subject position <TSUB> based on regex matches.
#' @param x A character of tokenized strings with _ST tags.
#' @return A character vector with some "that" relative clauses on subject position <TSUB> tags.
#' @export
dtag_that_subj <- function(x){
  that_sub1 <- that_sub2 <- that_sub3 <- NULL

  x <- data.table(x)
  x[, that_subj1 := d_grepl(shift(x, type="lag", n=1), "_N") &
      str_detect(x, "\\bthat_") &
      (d_grepl(shift(x, type="lead", n=1), "_MD") |
         d_grepl(shift(x, type="lead", n=1), str_flatten(sh[c("do" , "have" , "be")], "|")) |
         d_grepl(shift(x, type="lead", n=1), "_V"))]

  x[, that_subj2 := d_grepl(shift(x, type="lag", n=1), "_N") &
      str_detect(x, "\\bthat_") &
      d_grepl(shift(x, type="lead", n=1), "_RB|_XX0") &
      (d_grepl(shift(x, type="lead", n=2), "_MD") |
         d_grepl(shift(x, type="lead", n=2), str_flatten(sh[c("do" , "have" , "be")], "|")))]

  x[, that_subj3 := d_grepl(shift(x, type="lag", n=1), "_N") &
      str_detect(x, "\\bthat_") &
      d_grepl(shift(x, type="lead", n=1), "_RB|_XX0") &
      d_grepl(shift(x, type="lead", n=2), "_RB|_XX0") &
      (d_grepl(shift(x, type="lead", n=3), "_MD") |
         d_grepl(shift(x, type="lead", n=3), str_flatten(sh[c("do" , "have" , "be")], "|")) |
         d_grepl(shift(x, type="lead", n=3), "_V"))]

  x[that_subj1 == TRUE, x := d_sub(x, "$", " <TSUB>")]
  x[that_subj2 == TRUE, x := d_sub(x, "$", " <TSUB>")]
  x[that_subj3 == TRUE, x := d_sub(x, "$", " <TSUB>")]

  return(x$x)
}
