#' @title Add <MDA> Tags
#'
#' @description Adds <MDA> tags to a given text. The text must already include _ST tags and tokenized.
#'
#' @param x character string
#' @param mda_hesitation logical. If \code{TRUE}, hesitation markers are extracted from the text.
#' Experimental feature - should hesitation markers be excluded before tagging?
#' Regex for the hesitation markers is the same as the default for \code{dtag_hesitation},
#' but can be set using the regex argument. See \code{\link{dtag_hesitation}} for details.
#' @param progress logical. If \code{TRUE}, a progress message is printed
#' @param ... Additional arguments to pass on.
#' @import tibble
#' @import dplyr
#' @import purrr
#' @import stringr
#' @return character string with <MDA> tags added
#' @export
#' @examples
#' \dontrun{
#' # Simple example:
#' text <- c("I_PPSS", "have_VB", "a_DET", "dog_NN")
#' add_mda_tags(text)
#' # More detailed example:
#' # Generate some text
#' text <- "This example is short and sweet. This means that not all the will have
#' been included, which is why this is really only a guide. It should
#' be used with that in mind. Otherwise, I think it may lead to disappointment."
#' # Load udpipe model into the global environment for _ST tagging
#' init_udpipe_model()
#' # Add Stanford tags to text
#' text <- add_st_tags(text)
#' # Add <MDA> tags
#' add_mda_tags(text)}
#' @export
#'
add_mda_tags <- function(x, mda_hesitation = TRUE, progress = FALSE,...){
if(progress){message("\nAdding MDA tags to ", x)}
 # # tag and extract mda_hesitation markers
if(mda_hesitation){
 mda_hesitations_extracted <-
   dtag_hesitation(x) %>%
    enframe() %>%
    group_split(mda_hesitation = str_detect(value, "HSTN")) %>%
    map(~select(.x, -mda_hesitation))

x <- mda_hesitations_extracted[[1]] %>% deframe
nms <- mda_hesitations_extracted[[1]] %>% pull(name)

if(length(mda_hesitations_extracted) < 2){mda_hesitations_extracted[2] <- list(c())}
}


x <- data.table(x)

result <-
  x %>%
  # Basic MDA tags
    dtag_possessives() %>%
    dtag_to_prep() %>%
    dtag_negation() %>%
    dtag_prepositions() %>%
    dtag_ind_pron() %>%
    dtag_quantifiers() %>%
    dtag_quant_pron() %>%
  # Complex MDA tags
    dtag_adverbial_subords() %>%
    dtag_conjuncts() %>%
    dtag_pred_adj() %>%
    dtag_emphatics() %>%
    dtag_phrasal_coord() %>%
    dtag_pro_do() %>%
    dtag_wh_questions() %>%
    dtag_sentence_rels() %>%
    dtag_perfect_asp() %>%
    dtag_passives() %>%
    dtag_be_main() %>%
    dtag_wh_clauses() %>%
    dtag_pp_rel_clauses() %>%
    dtag_str_prepositions() %>%
    dtag_split_infinitives() %>%
    dtag_split_auxiliaries() %>%
    dtag_time_adverbials() %>% # overlap with DPAR
    dtag_place_adverbials() %>% # upstairs?
    dtag_that_vc()  %>%
    dtag_that_ac() %>%
    dtag_pres_part() %>%
    dtag_past_part() %>%
    dtag_past_whiz() %>%
    dtag_pres_whiz() %>%
    dtag_that_subj() %>%
    dtag_that_obj() %>%
    dtag_wh_subj() %>%
    dtag_wh_obj() %>%
    dtag_hedges() %>%
    dtag_disc_part() %>%
    dtag_dem_pronouns () %>%
    dtag_demonstratives() %>%
    dtag_that_del() %>%
    dtag_indep_cc() %>%
  # Regex match MDA tags
    dtag_amplifier() %>%
    dtag_downtoner() %>%
    dtag_nominalisation() %>%
    dtag_gerund() %>%
    dtag_all_nouns() %>%
    dtag_all_adjectives() %>%
    dtag_all_adverbs() %>%
    dtag_present_tenses() %>%
    dtag_first_person_pronoun() %>%
    dtag_second_person_pronoun() %>%
    dtag_third_person_pronoun() %>%
    dtag_pronoun_it() %>%
    dtag_causative() %>%
    dtag_concessive() %>%
    dtag_conditional() %>%
    dtag_possibility_modal() %>%
    dtag_necessity_modal() %>%
    dtag_predictive_modal() %>%
    dtag_public_verb() %>%
    dtag_private_verb() %>%
    dtag_suasive_verb() %>%
    dtag_seem_appear() %>%
    dtag_contractions() %>%
   unlist(x) %>%
   remove_duplicated_tags()


if(mda_hesitation){
   result <- result %>%
     enframe() %>%
     mutate(name = nms) %>% # replace name with original index position
   bind_rows(mda_hesitations_extracted[[2]]) %>%
   arrange(name) %>%
   pull(value) }

return(result)

}
