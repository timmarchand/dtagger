#' Dimension Tags by Regex Patterns
#' @description Append <MDA> tags after pattern matching with regex.
#' @details This group of dtag functions are best applied after other dtags have been processed.
#' The functions match various case insensitive regex patterns and append the relevant
#' <MDA> tag to the end of the respective token in the vector.
#'
#' @param x character vector of _ST tagged text, tokenized.
#' @return x character vector with <MDA> tags appended.
#' @examples
#'  \dontrun{
#' # Generate some text
#' text <- "This text is merely a quick exemplification of some dtag options.
#' Because it is short, you shouldn't expect very much."
#' # Add Stanford tags to text
#' text <- add_st_tags(text)
#' # dtag_amplifier
#' dtag_amplifier(text)
#' # dtag_downtoner
#' dtag_downtoner(text)
#' # dtag_nominalistation
#' dtag_nominalisation(text)
#' # dtag_all_adjectives
#' dtag_all_adjectives(text)
#' # dtag_second_person_pronoun
#' dtag_second_person_pronoun(text)
#' # dtag_pronoun_it
#' dtag_pronoun_it(text)
#' # dtag_necessity modal
#' dtag_necessity modal(text)
#' # dtag_contractions
#' dtag_contractions(text)}
