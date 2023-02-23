#' @title Tag existential there
#' @description Adds the existential there tag  <EX> based on a regex match.
#' @param x A character of tokenized strings with _ST tags.
#' @return A character vector with some existential there <EX> tags.
#' @export
dtag_ex_there <- function(x){
  regex <- "_EX"
  x <- data.table(x)
  x[d_grepl(x, regex), x:=d_sub(x, "$", " <EX>")]
  return(x$x)
}

