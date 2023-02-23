#' @title Add private verb <PRIV> tag
#' @description Adds the private verb tag <PRIV> based on a regex match.
#' @details For a list of the private verbs matched, see the shorthand character vector
#' named private \code{sh["private"]}
#' @param x A character of tokenized strings with _ST tags.
#' @return A character vector with some private verb <PRIV> tags.
#' @export
#'
dtag_private_verb <- function(x){
    x <- data.table(x)
  x[d_grepl(x, sh["private"]), x:= d_sub(x[d_grepl(x, sh["private"])],
                                                "$",
                                                " <PRIV>")]
  return(x$x)
}
