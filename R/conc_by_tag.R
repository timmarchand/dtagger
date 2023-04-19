#' @title Concordance Text By Tag Choices
#'
#' @description
#' This function produces concordance lines of text from `data` by finding up to
#' two tag matches in tokenized text.
#'
#' @details
#' The purpose of this function is to allow fine-grained concordance searches of tagged text.
#' The input should be a dataframe with a column for tokens in tokenized form,
#' and separate columns for tags, document and corpus details.
#'
#' Typically, the function can be used with output from \code{udpipe::udpipe_annotate}
#' and \code{dtagger::dtag_tbl}, \code{dtagger::dtag_directory} or \code{dtagger::add_tag_tbl} functions.
#'
#' The concordancer can take up to two tag inputs, for example matching all `upos == "ADJ"`
#' tags and `dep_rel == "amod"` tags, and seing the resulting key words in context.
#'
#' @param data A relational data frame containing the text to concordance.
#' The data frame is expected to have one column of tokens, in tokenized form, at least one
#' column of the corresponding tags, and identifying details such as corpus, doc_id etc.
#' @param what The name of the column containing the text to concatenate. Default is "token".
#' @param tag The name of the column containing the tags to match. Default is "mda".
#' @param match The tag to match within the `tag` column. The match can take regex, so you can use
#' anchoring characters (^ and $) for specific searches.
#' @param cols The names of the columns to include in the output. It may be useful to include some
#' extra reference columns (such as doc_id), or other tags for more fine-grained filtering.
#' @param tag2 The name of the second column containing the tags to match (optional).
#' @param match2 The second tag to match within the `tag2` column (optional).
#' @param ... Additional arguments to be passed onto \code{dtagger::quick_conc}.
#'
#' For example:
#' pass on the `separated = TRUE` argument, to enable sorting search result by
#' adjacent tokens to the left and right
#'
#' pass on the `n = 3 `argument, to limit the search window to 3 tokens either side of the match.
#'
#'
#'
#' @return A tibble containing:
#' * case - a case number for the match found.
#' * left - objects immediately adjacent (up to n) to the left of the matched node,
#' as defined by the `what` argument (default is token).
#' In case of `separated = TRUE`, the left  are separated into left(n):left1
#' * match - the matched search item, as defined by the `match` argument.
#' * right - tokens immediately adjacent (up to n) to the right of the matched node,
#' as defined by the `what` argument (default is token).
#' In case of `separated = TRUE`, the right tokens are separated into right1:right(n).
#' * index - the index row position of matched result from the input data frame.
#' * other cols - as defined by the `tag`, `tag2` and `cols` arguments.
#' @examples
#' \dontrun{
#' # Load the required udpipe model
#' Load the required udpipe model
#' init_udpipe_model()
#'
#' # Process text with the add_tag_tbl function (assuming add_mda_tags function is defined)
#' text <- c(doc1 = "This is a simple sentence with a specific keyword.",
#'           doc2 = "Is this one more complex or simpler?")
#' data <- add_tag_tbl(text)
#'
#' # Run conc_by_tag function with specified tags and matches
#' conc_by_tag(
#'   data,
#'   what = "token",
#'   tag = "xpos",
#'   match = "^JJ$",
#'   cols = c("doc_id", "lemma"),
#'   tag2 = "dep_rel",
#'   match2 = "^amod$"
#' )
#' conc_by_tag(
#'   data,
#'   what = "token",
#'   tag = "xpos",
#'   match = "JJ",
#'   cols = c("doc_id", "dep_rel"),
#'   separated = TRUE,
#'   n = 3
#' )
#' }
#' @export
conc_by_tag <- function(data, what = "token", tag = "mda", match, cols = NULL,
                           tag2 = NULL, match2 = NULL, ...){


 cols <- c("index", {{tag}}, {{tag2}},  {{cols}})

 what <- data %>% pull({{what}})

 if(is.null(tag2)){result  <-  data %>%
  mutate(index = row_number(), .before = everything()) %>%
  filter(str_detect(!!sym(tag),{{match}})) %>%
       select(all_of(cols))} else{

      result  <-  data %>%
  mutate(index = row_number(), .before = everything()) %>%
  filter(str_detect(!!sym(tag),{{match}}), str_detect(!!sym(tag2), {{match2}})) %>%
       select(all_of(cols)) }



 indices <- result %>%
  pull(index)

quick_conc(what, indices, ...) %>%
   bind_cols(result) %>%
    select(-token_id)

  }
