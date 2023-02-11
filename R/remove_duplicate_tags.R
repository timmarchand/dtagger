#' Removes duplicated tags
#'
#' @param x A vector of tags
#'
#' @return A vector of tags with duplicates removed
#'
#' @export
remove_duplicated_tags <- function(x){
  x <- data.table(x)
  x[, x:=lapply(x, tstrsplit, " ")][,
      x:=lapply(x, unique)][,
     x:=lapply(x, paste, collapse = " ")]
  unlist(x$x)
}
