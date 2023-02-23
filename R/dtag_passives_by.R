#' @title Add passive tags <BYPA>
#' @description This function tags by passives in text that has been previously _ST
#' tagged, tokenized and tagged by the  dtag_passives() function.
#' @details The function replaces the <PASS> tag with <BYPA> if a "by" token
#' is found within 1 to 3 tokens of the tagged passive.
#'
#' Usually there is no need to run this function unless the \code{dtag_passives(by = FALSE)}
#' has been run first.
#'
#' @param x A character vector of strings that have been tokenized and tagged with _ST tags.
#' @return A character vector with passive <BYPA> tags added.
#' @export
dtag_passives_by <- function(x){

    if(any(str_detect(x, "<PASS>")) == FALSE) stop('No PASSIVE tags found. Try running dtag_passives() first.\nThe default setting will tag BY- passives as well')

    x <- data.table(x)



  x[, bypassives1 := d_grepl(x, "<PASS>") &
      d_grepl(shift(x, type="lead", n=2), "\\bby_")]

  x[, bypassives2 := d_grepl(x, "<PASS>") &
      d_grepl(shift(x, type="lead", n=3), "\\bby_")]

  x[, bypassives3 := d_grepl(x, "<PASS>") &
      d_grepl(shift(x, type="lead", n=4), "\\bby_")]

  x[bypassives1 == TRUE | bypassives2 == TRUE | bypassives3 == TRUE, x := d_sub(x, "<PASS>", "<BYPA>")]

  return(x$x)
}
