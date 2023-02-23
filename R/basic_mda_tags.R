#' @title Basic MDA Tags
#' @description Append basic <MDA> tags to a tokenized vector of _ST tagged strings.
#' @details This group of dtag functions are best applied before other dtags as subsequent ones may depend on them.
#' The functions match various case insensitive regex patterns and append the relevant
#' <MDA> tag to the end of the respective token in the vector.
#' @param x character vector of _ST tagged text, tokenized.
#' @return x character vector with <MDA> tags appended.
#' @examples
#' \dontrun{
#' # Generate some text
#' text <- "Anyone using this text should find some examples among these words.
#' Despite not each pattern being referenced, several are among the words,
#' and enough for somebody to get the gist."
#' # Add Stanford tags to text
#' text <- add_st_tags(text)
#' # dtag_negation
#' dtag_negation(text)
#' # dtag_prepostions
#' dtag_prepositions(text)
#' # dtag_ind_pron
#' dtag_ind_pron(text)
#' # dtag_quantifiers
#' dtag_quantifiers(text)
#' # dtag_quant_pron
#' dtag_quant_pron(text)}



