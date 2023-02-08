#' @title Substitute Patterns in a Vector
#'
#' @description Substitute patterns in a vector `x` with a given `replacement`,  using base::sub for speed, by stringr format for convenience
#'
#' @param x A vector.
#' @param pattern The pattern to look for.
#' @param replacement The replacement string.
#' @param perl Logical value. If `TRUE`, pattern is a Perl-compatible regular expression. If `FALSE`, pattern is a POSIX basic regular expression.
#' @param ... Further arguments passed to \code{\link[base]{sub}}.
#'
#' @return A vector with patterns replaced.
#'
#' @examples
#' d_sub(x = c("abc", "def", "ghi"), pattern = "[a-z]{3}", replacement = "0")
#'
#' @export
#'
d_sub <- function(x,pattern, replacement, perl = TRUE, ...){
  sub(pattern, replacement, x, perl = TRUE, ignore.case = FALSE)
}
