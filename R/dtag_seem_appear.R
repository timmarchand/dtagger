#' @title Add seem / appear verb <SMP> tag
#' @description Adds the seem / appear  verb tag <SMP> based on a regex match.
#' @param x A character of tokenized strings with _ST tags.
#' @return A character vector with some seem / appear  verb <SMP> tags.
#' @export
#'
dtag_seem_appear <- function(x){
    x <- data.table(x)
  regex <- "(\\bseem|\\bseems|\\bseemed|\\bseeming|\\bappear|\\bappears|\\bappeared|\\bappearing)_V"
  x[d_grepl(x,regex), x:= d_sub(x[d_grepl(x,regex)],
                                                "$",
                                                " <SMP>")]
  return(x$x)
}
