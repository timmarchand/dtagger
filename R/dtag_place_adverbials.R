#' @title Add place adverbial <PLACE> tag
#' @description Adds place adverbial <PLACE> based on a regex match.
#' @param x A character of tokenized strings with _ST tags.
#' @return A character vector with some place adverbial <PLACE> tags.
#' @export
dtag_place_adverbials <- function(x){
place_adverbials <- NULL

  x <- data.table(x)
  x[, place_adverbials := d_grepl(x,sh["place"]) &
      !d_grepl(x, "_NN$|_NNP")]

  x[place_adverbials == TRUE, x := d_sub(x, "$", " <PLACE>")]

  return(x$x)
}
