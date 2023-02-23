#' @title Add stranded preposition <STPR> tag
#' @description Adds stranded preposition  <STPR> based on a regex match.
#' @param x A character of tokenized strings with _ST tags.
#' @return A character vector with some stranded preposition  <STPR> tags.
#' @export
dtag_str_prepositions <- function(x){
  str_prepositions <- NULL
   x <- data.table(x)

    x[, str_prepositions := d_grepl(x, "<PIN>") & !d_grepl(x, "\\bbesides") &
        d_grepl(shift(x, type="lead", n=1), "_[\\,.]")]

    x[str_prepositions == TRUE, x := d_sub(x,"$"," <STPR>")]

    return(x$x)
  }
