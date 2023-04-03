#' @title Concordance text by choice of tag
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
#' and \code{dtagger::dtag_tbl} od \code{dtagger::dtag_directory} functions.
#'
#' The concordancer can take up to two tag inputs, for example matching all `upos == "ADJ"`
#' tags and `dep_rel == "amod"` tags, and seing the resulting key words in context.
#'
#' @param data A relational data frame containing the text to concordance.
#' The data frame is expected to have one column of tokens, in tokenized form, at least one
#' column of the corresponding tags, and identifying details such as corpus, doc_id etc.
#' @param what The name of the column containing the text to concatenate. Default is "token".
#' @param tag The name of the column containing the tags to match. Default is "mda_tags".
#' @param match The tag to match within the `tag` column.
#' @param cols The names of the columns to include in the output. By default, these will include
#' corpus, doc_id, sentence, but need to be changed if the inputting df has different columns.
#' @param tag2 The name of the second column containing the tags to match (optional).
#' @param match2 The second tag to match within the `tag2` column (optional).
#' @param ... Additional arguments to be passed onto \code{dtagger::quick_conc}.
#'
#' @return A data frame containing the `cols` plus an extra column with the
#' concatenated text.
#' @examples
#' \dontrun{
#' # create two example vectors:
#' # vec1 tagged with ewt udpipe English model
#' vec1 <- c("This_DT", "is_VBZ", "a_DT", "test_NN", "sentence_NN", "with_IN",
#' "tags_NNS", "._.")
#'
#' vec2 tagged with line udpipe English model
#'
#' vec2 <- c("This_DEM-SG", "is_PRES", "a_IND-SG", "test_SG-NOM", "sentence_SG-NOM",
#' NA, "tags_PL-NOM", "._Period")
#'
#' # find tags in vec1 that are missing in vec2
#' missing_tags(vec1, vec2, regex = "DT")

#' # find tags in vec2 that are missing in vec1
#' missing_tags(vec2, vec1, regex = "Period")
#' # find tags in vec1 that are missing in vec2 but with different tag searches
#' missing_tags(vec1, vec2, regex1 = "DT", regex2 = "IND-SG")
#'
#' @export
conc_by_tag <- function(data, what = "token", tag = "mda", match, cols = c("corpus", "doc_id", "sentence"),
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
