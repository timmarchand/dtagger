#' Tag Adverbial Subordinating Conjunctions
#'
#' This function tags adverbial subordinating conjunctions in text.
#'
#' @param x A character vector of strings that have been tokenized and tagged with ST tags
#'
#' @return A character vector with adverbial subordinating conjunctions <OSUB> tags added.
#'
#' @examples
#' dtag_adverbial_subords(c("Since_RB",  "he_PRP" , "left_VBD"))
#'
#' @export
#'
dtag_adverbial_subords <- function(x){
adverbial1 <- adverbial2 <- adverbial3 <- adverbial4 <- adverbial5 <- NULL
null2 <- null3 <- null4 <- null5 <- null5a <- NULL
  x <- data.table(x)
  x[, adverbial1 := d_grepl(x, "\\bsince_|\\bwhile_|\\bwhilst_|\\bwhereupon_|\\bwhereas_|\\bwhereby_")]
  x[, adverbial2 := d_grepl(x, "\\bsuch_") & d_grepl(shift(x, type="lead", n=1), "that_")]
  x[,null2 := d_grepl(shift(x, type = "lag", n=1), "\\bsuch_") & d_grepl(x, "that_")]


  x[, adverbial3 := d_grepl(x, "\\binasmuch_\\bforasmuch_\\binsofar_\\binsomuch_") &
      d_grepl(shift(x, type="lead", n=1), "as_")]
  x[, null3 := d_grepl(shift(x, type = "lag", n=1), "\\binasmuch_\\bforasmuch_\\binsofar_\\binsomuch_") &
      d_grepl(x, "as_")]


  x[, adverbial4 := d_grepl(x, "so_") &
      d_grepl(shift(x, type="lead", n=1), "that_") &
      !d_grepl(shift(x, type="lead", n=2), "_NN|_NNP|_JJ")]
  x[, null4 := d_grepl(shift(x, type = "lag", n = 1), "so_") &
      d_grepl(x, "that_") &
      !d_grepl(shift(x, type="lead", n=1), "_NN|_NNP|_JJ")]

  x[, adverbial5 := d_grepl(x, "\\bas_") &
      d_grepl(shift(x, type="lead", n=1), "\\blong_|\\bsoon_") &
      d_grepl(shift(x, type="lead", n=2), "\\bas_")]
  x[, null5 := d_grepl(shift(x, type= "lag", n=1), "\\bas_") &
      d_grepl(x, "\\blong_|\\bsoon_") &
      d_grepl(shift(x, type="lead", n=1), "\\bas_")]
  x[, null5a := d_grepl(shift(x, type= "lag", n=2), "\\bas_") &
      d_grepl(shift(x, type= "lag", n=1), "\\blong_|\\bsoon_") &
      d_grepl(x, "\\bas_")]

  x[adverbial1 == TRUE | adverbial2 == TRUE | adverbial3 == TRUE | adverbial4 == TRUE | adverbial5 == TRUE,
    x := d_sub(x, "$", " <OSUB>")]
  x[null2 == TRUE | null3 == TRUE | null4 == TRUE | null5 == TRUE | null5a == TRUE,
    x := d_sub(x, "_\\w+", "_NULL")]
  return(x$x)
}
