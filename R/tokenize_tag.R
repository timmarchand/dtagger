#' Tokenize tag
#'
#' Split a character vector based on a tag. The function tokenizes MDA
#' tagged texts by splitting on each space not followed by an <MDA> tag.
#' It will also work on _ST tags by default.
#'
#' @param vec A character vector.
#' @param regex The regular expression used for splitting the vector. Use the default
#' setting for <MDA> tags.
#' @return A list of character vectors split by tag.
#' @importFrom stringr str_split_1
#' @importFrom purrr map
#'
#' @export
#'
tokenize_tag <- function(vec, regex = "\\s(?!<\\w+>)"){
  purr::map(vec, ~stringr::str_split_1(.x, {{regex}}))
}
