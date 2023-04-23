#' Add a Table of ST, MDA and Other UD Tags
#'
#' The `add_tag_tbl` function is a wrapper around the `add_st_tags` function that also adds MDA tags to the text.
#' It processes and annotates the input text using the full Universal Dependencies (UD) model with the udpipe package, then adds MDA tags to the resulting output.
#'
#' @param x A character vector of input text to be processed.
#' @param ... Additional arguments to be passed to the `add_st_tags()` function.
#'
#' @return A tibble with the original text annotated with ST tags and MDA tags. The output columns includes
#' id columns  (`doc_id`, `paragraph_id`, `sentence_id` etc.) udpipe output (`token`, `upos`, `xpos`, `dep_rel`, etc)
#' and both `st` and `mda` tags.
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

add_tag_tbl <- function(x, ...){

    add_st_tags(x = x, skip_parse = FALSE, ...) %>%
    nest(data = -doc_id) %>%
    mutate(mda = (map(data, ~ add_mda_tags(.x$st)))) %>%
    unnest(-doc_id) %>%
    relocate(mda, .after = st)
}
