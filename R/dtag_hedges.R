#' @title Add hedges <HDG> tag
#' @description Adds the hedges <HDG> based on a regex match.
#' @details In case of multiword units such as "more or less", the second and third
#' token has its _ST tag replaced with _NULL.
#' @param x A character of tokenized strings with _ST tags.
#' @return A character vector with some emphatic <EMPH> tags.
#' @export
dtag_hedges <- function(x){
hedges1 <- hedges2 <- hedges3 <- hedges4 <- hedges5 <- NULL
null2 <- null3 <- null4 <- null4a <- null5 <- NULL

  x <- data.table(x)

  x[, hedges1 := d_grepl(x, "\\bmaybe_")]

  x[, hedges2 := d_grepl(x, "\\bat_") &
      d_grepl(shift(x, type="lead", n=1), "\\babout_")]
  x[, null2 := d_grepl(shift(x, type = "lag", n=1), "\\bat_") &
      d_grepl(x, "\\babout_")]

  x[, hedges3 := d_grepl(x, "\\bsomething_") &
      d_grepl(shift(x, type="lead", n=1), "\\blike_")]
  x[, null3 := d_grepl(shift(x, type = "lag", n=1), "\\bsomething_") &
      d_grepl(x, "\\blike_")]

  x[, hedges4 := d_grepl(x, "\\bmore_") &
      d_grepl(shift(x, type="lead", n=1), "\\bor_") &
      d_grepl(shift(x, type="lead", n=2), "\\bless_")]
  x[, null4 := d_grepl(shift(x, type = "lag", n=1), "\\bmore_") &
      d_grepl(x, "\\bor_") &
      d_grepl(shift(x, type="lead", n=1), "\\bless_")]
  x[, null4a := d_grepl(shift(x, type = "lag", n=2), "\\bmore_") &
      d_grepl(shift(x, type="lag", n=1), "\\bor_") &
      d_grepl(x, "\\bless_")]

  x[, hedges5 := !d_grepl(shift(x, type="lag", n=1), str_flatten(str_c("_DT|_QUAN|_CD|_JJ|_PRED|_PRPS","|",sh["who"]))) &
      d_grepl(x, "\\bsort_|\\bkind_") &
      d_grepl(shift(x, type="lead", n=1), "\\bof_")]
  x[, null5 := !d_grepl(shift(x, type="lag", n=2), str_flatten(str_c("_DT|_QUAN|_CD|_JJ|_PRED|_PRPS","|",sh["who"])))  &
      d_grepl(shift(x, type="lag", n=1), "\\bsort_|\\bkind_") &
      d_grepl(x, "\\bof_")]


  x[hedges1 == TRUE | hedges2 == TRUE | hedges3 == TRUE | hedges4 == TRUE | hedges5 == TRUE,
    x := d_sub(x, "$", " <HDG>")]

  x[null2 == TRUE | null3 == TRUE | null4 == TRUE | null4a == TRUE | null5 == TRUE,
    x := d_sub(x, "_\\w+( <PIN>)?", "_NULL")] # remove preposition tag in case of sort/kind of

  return(x$x)

}
