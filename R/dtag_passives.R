#' @title Add passive tags <PASS> or <BYPA>
#'
#' @description This function tags passives in text that has been previously _ST
#' tagged and tokenized.
#' @details The function adds the <PASS> tag to the relevant token based on regex and
#' after certain conditions are met.
#'
#' When the by argument is set to TRUE, the function replaces the <PASS> tag with <BYPA> if a by token
#' is found within 1 to 3 tokens of the tagged passive.
#'
#' @param x A character vector of strings that have been tokenized and tagged with _ST tags.
#' @param by Logical. If TRUE (default), the function replaces the <PASS> tag with <BYPA>
#' in cases of passives with "by".
#' @return A character vector with passive <PASS> or <BYPA> tags added.
#' @export
utils::globalVariables(c("by"))
dtag_passives <- function(x, by = TRUE){
passives1 <- passives2 <- passives3 <- passives4 <- passives5 <- NULL
bypassives1 <- bypassives2 <- bypassives3 <-  NULL

  x <- data.table(x)

  x[, passives1 := d_grepl(x, sh["be"]) &
      d_grepl(shift(x, type="lead", n=1), "_VBD|_VBN")]

  x[, passives2 := d_grepl(x, sh["be"]) &
      d_grepl(shift(x, type="lead", n=1), "_RB|_XX0") &
      d_grepl(shift(x, type="lead", n=2), "_VBD|_VBN")]

  x[, passives3 := d_grepl(x, sh["be"]) &
      d_grepl(shift(x, type="lead", n=1), "_NN|_NNP|_PRP") &
      d_grepl(shift(x, type="lead", n=2), "_VBD|_VBN")]

  x[, passives4 := d_grepl(x, sh["be"]) &
      d_grepl(shift(x, type="lead", n=1), "_RB|_XX0") &
      d_grepl(shift(x, type="lead", n=2), "_RB|_XX0") &
      d_grepl(shift(x, type="lead", n=3), "_VBD|_VBN")]

  x[, passives5 := d_grepl(x, sh["be"]) &
      d_grepl(shift(x, type="lead", n=1), "_RB|_XX0") &
      d_grepl(shift(x, type="lead", n=2), "_NN|_NNP|_PRP") &
      d_grepl(shift(x, type="lead", n=3), "_VBD|_VBN")]

  x[passives1 == TRUE | passives2 == TRUE | passives3 == TRUE | passives4 == TRUE | passives5 == TRUE, x := d_sub(x, "$", " <PASS>")]

  if(by){  x[, bypassives1 := d_grepl(x, "<PASS>") &
               d_grepl(shift(x, type="lead", n=2), "\\bby_")]

    x[, bypassives2 := d_grepl(x, "<PASS>") &
        d_grepl(shift(x, type="lead", n=3), "\\bby_")]

    x[, bypassives3 := d_grepl(x, "<PASS>") &
        d_grepl(shift(x, type="lead", n=4), "\\bby_")]

    x[bypassives1 == TRUE | bypassives2 == TRUE | bypassives3 == TRUE, x := d_sub(x, "<PASS>", "<BYPA>")]

    return(x$x)
  }

  return(x$x)
}
