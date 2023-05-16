#' Identify Irregular Spellings and Suggest Corrections to a Table of Text
#'
#' This function identifies spelling mistakes in a given text column of a data frame, suggests corrections,
#' and adds them as new columns. It uses the `hunspell` package for spell checking and providing suggestions.
#' The output can be exported to a spreadsheet for easier editing.
#' @param tbl A data frame that contains the text data.
#' @param x A character string indicating the name of the text column in `tbl` (default is "text").
#' @param window An integer specifying the context window size for `quick_conc` function (default is 5).
#' @param dict A character string indicating the dictionary to be used by `hunspell` (default is "en_US").
#' @param change_to A character string specifying the column name ("alt_01" or "typo") to use for the `change_to` column (default is NULL).
#'
#' @return A tibble with identified spelling mistakes and corresponding suggested corrections.
#' Each row represents an instance of a spelling mistake, and each cell in the 'alt' column
#' represents a comma-separated list of spelling suggestions.
#'
#' If `change_to` is specified, an additional column 'change_to' is included with
#' either the original incorrect spelling  (in case of `change_to = "typo"`), or the function's
#' best guess at the correct spelling (when `change_to = "alt_01"`).
#'
#' This can be useful when exporting to a spreadsheet app for editing.
#'
#' @examples
#' \dontrun{
#' data <- tibble(text = c("This is a smaple text.", "Another txt with a typo."))
#' add_spell_suggestions(data)
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom purrr map map_chr map2
#' @importFrom hunspell hunspell hunspell_suggest
#' @importFrom tidyr unnest
#' @importFrom dplyr select mutate distinct
#' @importFrom splitstackshape cSplit
#'
#' @export

add_spell_suggestions <- function(tbl, x = "text", window = 5, dict = "en_US" , change_to = NULL){

  dict <- {{dict}}
  n <- window

tbl <-   tbl %>%
    mutate(typo = map(.data[[x]], ~hunspell::hunspell(.x, dict = dict) %>% unlist, .progress = "(1/3) Finding typos"),
           alt = map(typo,
                  ~hunspell::hunspell_suggest(.x, dict = dict) %>%
                    map_chr(~paste(.x, collapse = ",")), .progress = "(2/3) Generating suggestions")) %>%
    unnest(c(typo,alt), keep_empty = TRUE) %>%
  mutate(conc = map2(.x = text, .y =  typo,  ~quick_conc(.x, .y, tokenize = TRUE, n = n), .progress = "(3/3) Finding context"), .before = text ) %>%
  unnest(conc) %>%
  select(-c(case, token_id, text)) %>%
  splitstackshape::cSplit("alt",",", type.convert = FALSE) %>%
  tibble() %>%
    distinct()

if(is.null(change_to)){return(tbl)}

if(change_to == "alt_01" | change_to == "typo" ){tbl <- tbl %>%
    mutate(change_to = .data[[change_to]], .before = typo)}

return(tbl)
}
