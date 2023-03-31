#' Replace multiple text tokens in strings
#'
#' Replace multiple text tokens with new text in provided string.
#'
#' The function
#'
#' @param text A string vector to replace tokens in.
#' @param tbl A two-column tibble with the `token` to replace and its
#'     corresponding `new` value.
#' @param add_breaks A logical indicating whether to add `\\b` (word boundary) to token.
#' @export
#' @importFrom dplyr distinct filter mutate
#' @importFrom tibble deframe
#' @importFrom purrr map_chr
#' @return A character vector of strings with replaced tokens.
#'
#' @examples
#' # Replace multiple tokens
#' text <- c("Hello world!", "Goodbye world!")
#' tbl <- data.frame(token = c("Hello", "world"), new = c("Hi", "universe"))
#' multi_replace_all(text, tbl)
#' #> [1] "Hi universe!" "Goodbye universe!"
#'
multi_replace_all <- function(text, tbl, add_breaks = TRUE){
  replace_vec <-
    {{tbl}}[,c("token", "new")] %>%
     dplyr::mutate(add_breaks = {{add_breaks}},
       token = if_else(add_breaks,paste0("\\b",token,"\\b"),token)) %>%
     select(-add_breaks) %>%
     dplyr::distinct() %>%
     dplyr::filter(1 != 2) %>%
     tibble::deframe()

  purrr::map_chr({{text}}, ~stringr::str_replace_all(.x, replace_vec), .progress = "Replacing matches")
}

