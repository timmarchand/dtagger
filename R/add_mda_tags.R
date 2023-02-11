#' @title Add MDA Tags
#'
#' @description Adds MDA tags to a given text. The text must already include ST tags and tokenized.
#'
#' @param x character string
#' @param hesitation logical. If \code{TRUE}, hesitation markers are extracted from the text
#' @param progress logical. If \code{TRUE}, a progress message is printed
#' @import data.table
#' @import tibble
#' @import dplyr
#' @import purrr
#' @import stringr
#' @return character string
#'
#' @examples
#' text <- c("I_PPSS", "have_VB", "a_DET", "dog_NN")
#' add_mda_tags(text)
#'
#' @export
#'
add_mda_tags <- function(x, hesitation = TRUE, progress = FALSE){
if(progress){message("\nAdding MDA tags to ", x)}
 # # tag and extract hesitation markers
if(hesitation){
 hesitations_extracted <-
   dtag_hesitation(x) %>%
    enframe() %>%
    group_split(hesitation = str_detect(value, "HSTN")) %>%
    map(~select(.x, -hesitation))

x <- hesitations_extracted[[1]] %>% deframe
nms <- hesitations_extracted[[1]] %>% pull(name)

if(length(hesitations_extracted) < 2){hesitations_extracted[2] <- list(c())}
}


x <- data.table(x)

result <-
  x %>%
    dtag_possessives() %>%
    dtag_to_prep() %>%
    dtag_negation() %>%
    dtag_prepositions() %>%
    dtag_ind_pron() %>%
    dtag_quantifiers() %>%
    dtag_quant_pron() %>%
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


if(hesitation){
   result <- result %>%
     enframe() %>%
     mutate(name = nms) %>% # replace name with original index position
   bind_rows(hesitations_extracted[[2]]) %>%
   arrange(name) %>%
   pull(value) }

return(result)

}
