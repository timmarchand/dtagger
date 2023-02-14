#' Log Midpipe
#'
#' A function to interrupt a pipe to print an expression before continuing with the pipe.
#'
#' @param x A data object.
#' @param expr An expression to be evaluated on the data object.
#'
#' @return The data object after the expression is evaluated.
#'
#'
#'
log_midpipe <- function(x, expr) {
  result <- x
  expr
  result
}
