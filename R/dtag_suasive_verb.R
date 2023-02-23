#' @title Add suasive verb <SUAV> tag
#' @description Adds the suasive verb tag <SUAV> based on a regex match.
#' @details For a list of the suasive verbs matched, see the shorthand character vector
#' named suasive \code{sh["suasive"]}
#' @param x A character of tokenized strings with _ST tags.
#' @return A character vector with some suasive verb <SUAV> tags.
#' @export
#'
dtag_suasive_verb <- function(x){
    x <- data.table(x)
  x[d_grepl(x,sh["suasive"]), x:= d_sub(x[d_grepl(x,sh["suasive"])],
                                                "$",
                                                " <SUAV>")]
  return(x$x)
}
