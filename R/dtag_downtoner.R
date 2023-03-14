#' @title Tag downtoners
#' @description Adds the downtoner tag  <DWNT> based on a regex match.
#' @param x A character of tokenized strings with _ST tags.
#' @return A character vector with some downtoner <DWNT> tags.
#' @export
dtag_downtoner <- function(x){
  regex <-"\\balmost_|\\bbarely_|\\bhardly_|\\bmerely_|\\bmildly_|\\bnearly_|\\bonly_|\\bpartially_|\\bpartly_|\\bpractically_|\\bscarcely_|\\bslightly_|\\bsomewhat_"
   x <- data.table(x)
   x[d_grepl(x, regex), x:= d_sub(x, "$", " <DWNT>")]
  return(x$x)
}

