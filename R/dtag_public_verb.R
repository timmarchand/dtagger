#' @title Add public verb <PUBV> tag
#' @description Adds the public verb tag <PUBV> based on a regex match.
#' @details For a list of the public verbs matched, see the shorthand character vector
#' named public \code{sh["public"]}
#' @param x A character of tokenized strings with _ST tags.
#' @return A character vector with some public verb <PUBV> tags.
#' @export
#'
dtag_public_verb <- function(x){
    x <- data.table(x)
  x[d_grepl(x,sh["public"]), x:= d_sub(x[d_grepl(x,sh["public"])],
                                                "$",
                                                " <PUBV>")]
  return(x$x)
}
