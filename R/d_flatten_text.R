#' Flatten Text
#'
#' This function takes a vector of strings and flattens them into a single text.
#' @details A wrapper for stringr::str_flatten() with the additional function of
#' removing floating spaces between a set of punctuation marks [.,;:!?] and tokens.
#' @param x A character vector.
#'
#' @return A character vector where the elements are a single string.
#' @importFrom stringr str_flatten
#' @importFrom stringr str_replace_all
#' @importFrom purrr map_chr
#' @export
#' @examples
#' text <- c("This" , "is" , "," , "or" , "at" , "least" , "should" , "be" , "," ,
#' "a" , "suitable" , "example" , "." , "Is" , "it", "?")
#' d_flatten_text(text)
d_flatten_text <- function(x){
   x <- stringr::str_flatten(x, " ") %>%
    purrr::map_chr(~stringr::str_replace_all(.x, "\\s([.,;:!?])", "\\1"))
   return(x)
}
