#' @title Add phrasal coordination <PHC> tag
#' @description Adds the phrasal coordination tag <PHC> based on a regex match.
#' @details The <PHC> tag is added in case of "and" with certain conditions
#' such as:
#' * between two adverb tags
#' * between two adjective tags
#' * between two noun tags
#' @param x A character of tokenized strings with _ST tags.
#' @return A character vector with some phrasal coordination <PHC> tags.
#' @export
#
dtag_phrasal_coord <- function(x){
phrasal_coord1 <- phrasal_coord2 <- phrasal_coord3 <- phrasal_coord4 <- NULL


  x <- data.table(x)
  x[, phrasal_coord1 := d_grepl(x,"\\band_") & d_grepl(shift(x, type="lag", n=1),"_RB") & d_grepl(shift(x, type="lead", n=1),"_RB")]
  x[, phrasal_coord2 := d_grepl(x,"\\band_") & d_grepl(shift(x, type="lag", n=1),"_JJ") & d_grepl(shift(x, type="lead", n=1),"_JJ")]
  x[, phrasal_coord3 := d_grepl(x,"\\band_") & d_grepl(shift(x, type="lag", n=1),"_V") & d_grepl(shift(x, type="lead", n=1),"_V")]
  x[, phrasal_coord4 := d_grepl(x,"\\band_") & d_grepl(shift(x, type="lag", n=1),"_NN") & d_grepl(shift(x, type="lead", n=1),"_NN")]

  x[phrasal_coord1 == TRUE | phrasal_coord2 == TRUE | phrasal_coord3 == TRUE | phrasal_coord4 == TRUE,
    x := d_sub(x, "$", " <PHC>")]

  return(x$x)
}
