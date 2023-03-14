#' @title Add WH Relative Clauses on Subject Position <WHSUB> tag
#' @description Adds WH relative clauses on subject position <WHSUB> based on regex matches.
#' @param x A character of tokenized strings with _ST tags.
#' @return A character vector with some WH relative clauses on subject Position <WHSUB> tags.
#' @export
dtag_wh_subj <- function(x){
 what_subj1 <- what_subj2 <- what_subj3 <- NULL

  x <- data.table(x)
  x[, what_subj1 := !d_grepl(shift(x, type="lag", n=3), "\\bask_|\\basks_|\\basked_|\\basking_|\\btell_|\\btells_|\\btold_|\\btelling_") &
      d_grepl(shift(x, type="lag", n=1), "_N") &
      d_grepl(x,   sh["wp"]) &
      d_grepl(shift(x, type="lead", n=1),str_c(str_flatten(sh[c("do" , "have" , "be")],"|"),"|_MD"))]

  x[, what_subj2 := !d_grepl(shift(x, type="lag", n=3), "\\bask_|\\basks_|\\basked_|\\basking_|\\btell_|\\btells_|\\btold_|\\btelling_") &
      d_grepl(shift(x, type="lag", n=1), "_N") &
      d_grepl(x,   sh["wp"]) &
      d_grepl(shift(x, type="lead", n=1), "_RB") &
      d_grepl(shift(x, type="lead", n=2),str_c(str_flatten(sh[c("do" , "have" , "be")],"|"),"|_MD"))]

  x[, what_subj3 := !d_grepl(shift(x, type="lag", n=3), "\\bask_|\\basks_|\\basked_|\\basking_|\\btell_|\\btells_|\\btold_|\\btelling_") &
      d_grepl(shift(x, type="lag", n=1), "_N") &
      d_grepl(x,   sh["wp"]) &
      d_grepl(shift(x, type="lead", n=1), "_RB") &
      d_grepl(shift(x, type="lead", n=2), "_RB") &
      d_grepl(shift(x, type="lead", n=3),str_c(str_flatten(sh[c("do" , "have" , "be")],"|"),"|_MD"))]


  x[what_subj1 == TRUE | what_subj2 == TRUE | what_subj3 == TRUE,
    x := d_sub(x, "$", " <WHSUB>")]

  return(x$x)
}
