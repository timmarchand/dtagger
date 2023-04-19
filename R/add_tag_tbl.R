#' Add a Table of ST, MDA and Other UD Tags to Text
#'
#' The `add_tag_tbl` function is a wrapper around the `add_st_tags` function that also adds MDA tags to the text.
#' It processes and annotates the input text using the full Universal Dependencies (UD) model with the udpipe package, then adds MDA tags to the resulting output.
#'
#' @param text A character vector of input text to be processed.
#' @param ... Additional arguments to be passed to the `add_st_tags()` function.
#'
#' @return A tibble with the original text annotated with ST tags and MDA tags. The output columns include `doc_id`, `st`, and `mda`.
#'
#' @examples
#' \dontrun{
#' # Load the required udpipe model
#' init_udpipe_model()
#'
#'
#' # Process text with the add_tag_tbl function
#' text <- "This is a sample sentence."
#' result <- add_tag_tbl(text)
#' }
#' @export

add_tag_tbl <- function(text, ...){

    add_st_tags(x = text, skip_parse = FALSE) %>%
    nest(data = -doc_id) %>%
    mutate(mda = (map(data, ~ add_mda_tags(.x$st)))) %>%
    unnest(-doc_id) %>%
    relocate(mda, .after = st)
}
