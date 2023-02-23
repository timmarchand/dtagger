#' @title Complex MDA Tags
#' @description Append complex <MDA> tags to a tokenized vector of _ST tagged strings.
#' @details This group of dtag functions are best applied after the Basic  MDA tags as they may rely on them.
#' The functions match various case insensitive regex patterns and append the relevant
#' <MDA> tag to the end of the respective token in the vector.
#' @param x character vector of _ST tagged text, tokenized.
#' @return x character vector with <MDA> tags appended.
#' @examples
#' \dontrun{
#' # Generate some text
#' text <- "This example is short and sweet. This means that not all the complex
#' tags will have been included, which is why this is really only a guide and it should
#' be used with that in mind. Otherwise, I think you will be disappointed."
#' # Load udpipe model into the global environment for _ST tagging
#' init_udpipe_model()
#' # Add Stanford tags to text
#' text <- add_st_tags(text)
#' # dtag_passives
#' dtag_passives(text)
#' # dtag_that_del
#' dtag_that_del(text)
#' # dtag_demonstratives
#' dtag_demonstratives(text)
#' # dtag_dem_pronouns
#' dtag_dem_pronouns(text)
#' # dtag_sentence_rels
#' dtag_sentence_rels(text)
#' # dtag_emphatics
#' dtag_emphatics(text)
#' #dtag_be_main
#' dtag_be_main(text)
#' #dtag_perfect_asp
#' dtag_perfect_asp(text)
#' # #dtag_pred_adj
#' dtag_pred_adj(text)}
