#' @title Tag quantifiers
#' @description Adds the quantifier tag  <QUAN> based on a regex match.
#' @param x A character of tokenized strings with _ST tags.
#' @return A character vector with some quantifier <QUAN> tags.
#' @export
#'
dtag_quantifiers <- function(x){
  x <- data.table(x)
   regex <- "\\beach_|\\ball_|\\bevery_|\\bmany_|\\bmuch_|\\bfew_|\\bseveral_|\\bsome_|\\bany_"
  x[d_grepl(x,regex), x:= d_sub(x, "$", " <QUAN>")]
  return(x$x)
}

