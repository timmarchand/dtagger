#' Find the mode of a given vector
#'
#' @description This function takes a vector and finds the mode (most frequent value)
#' @param x A vector
#' @return The mode of the vector
#' @export
#' @examples
#' find_mode(c(1,2,2,2,3,4,4))
#' # 2
find_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
