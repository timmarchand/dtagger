#' Find Non-ASCII Characters in Text
#'
#' This function finds non-ASCII characters in a given text column of a data frame. It uses
#' the `quick_conc` function for finding non-ASCII characters. The results can optionally be sorted
#' by the non-ASCII characters.
#'
#' @param tbl A data frame that contains the text data.
#' @param id A character string indicating the name of the identifier column in `tbl` (default is NULL).
#' @param text A character string indicating the name of the text column in `tbl` (default is "text").
#' @param sort_by_chr A logical indicating whether to sort the results by the non-ASCII characters (default is FALSE).
#' @param ... Arguments to pass on to `quick_conc()`. For example, you can extend the
#' resulting search window witb the argument `n = 10`,
#'
#' @return A tibble with identified non-ASCII characters. Each row represents an instance of a non-ASCII
#' character. If `id` is not NULL, the tibble also includes the identifier for each instance.
#' If `sort_by_chr` is TRUE, the tibble is sorted by the non-ASCII characters.
#'
#' @examples
#' \dontrun{
#' data <- tibble(id = 1:2,
#' text = c("This is a text with a non-ASCII character: é.", "Another text without."))
#' find_non_ascii(data, id = "id")
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom purrr map
#' @importFrom tidyr unnest
#' @importFrom dplyr transmute mutate arrange
#' @importFrom stringr str_extract
#'
#' @export

find_non_ascii <- function(tbl, id = NULL, text = "text", sort_by_chr = FALSE, ...){

if(is.null(id)){result <- tbl %>%
    transmute(
              non_ascii = map(.data[[text]], ~quick_conc(.x, index = "[^ -~]+", tokenize = TRUE))) %>%
     unnest(non_ascii) %>%
  mutate(non_ascii = str_extract(match, "[^ -~]+"), .before = everything())
}

  if(!is.null(id)){
  result <- tbl %>%
    transmute(.data[[id]],
              non_ascii = map(.data[[text]], ~quick_conc(.x, index = "[^ -~]+", tokenize = TRUE))) %>%
     unnest(non_ascii) %>%
  mutate(non_ascii = str_extract(match, "[^ -~]+"), .before = everything())
  }

  if(sort_by_chr){result <- result %>%
     mutate(case = row_number(), .by = non_ascii) %>%
  arrange(non_ascii)
  }

  return(result)

}
