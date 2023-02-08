#' @title Pattern matching in strings
#' @description Match given pattern in provided strings using grepl for speed, by stringr format for convenience
#' @param x A character vector where pattern matching is done
#' @param pattern The pattern to match
#' @param ... Additional arguments to pass to grepl()
#' @return A logical vector
#' @export
#'
#' @examples
#' x <- c("ABC", "abC", "Bcd")
#' d_grepl(x, "bc")
#'
#' d_grepl_case(x, "bc")

d_grepl_case <- function(x, pattern, ...){
  base::grepl({{pattern}}, {{x}}, ignore.case = FALSE, perl = TRUE)
}
