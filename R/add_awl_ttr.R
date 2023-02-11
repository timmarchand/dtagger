#' @title Compute Average Word Length and Type-Token Ratio
#'
#' @description Computes the Average Word Length (AWL) and Type-Token Ratio (TTR) of a vector of words.
#'
#' @param vec The vector of words for which to compute AWL and TTR.
#' @param ttr The number of words to consider for TTR calculation. Default is 400.
#' @importFrom stringr str_extract
#' @importFrom tibble tibble
#' @importFrom magrittr "%>%"
#' @return A tibble containing the AWL and TTR values.
#'
#' @examples
#'
#' add_awl_ttr(c("Hello world", "This is R"))
#'
#' @export

add_awl_ttr <- function(vec, ttr = 400){
words <- vec %>%
stringr::str_extract("\\w+(?=_)") %>%
  .[!is.na(.)]

if(ttr > length(words)){ ttr <- length(words)}

tibble::tibble(AWL = (nchar(words) %>% sum) / length(words),
       TTR = (words[1:{{ttr}}] %>% unique %>% length) / ttr )
}
