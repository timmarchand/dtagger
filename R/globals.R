#' @importFrom  utils globalVariables

globs <-
c(".","...", "<<-", "andc1", "andc2", "andc3", "andc4", "be_main1", "be_main2",
"biber_base", "biber_mean", "biber_sd", "by", "bypassives1", "bypassives2",
"bypassives3", "case","closest_text_type", "conjuncts1","conjuncts2", "conjuncts3","conjuncts4",
"conjuncts5" ,"conjuncts6", "conjuncts7","conjuncts8", "conjuncts9",
"conjuncts10", "corpus", "dem_pronouns1", "dem_pronouns2",
"demonstratives", "detail", "dimension", "dimension_tags", "dimension_scores" , "disc_part", "doc_id",
"docvar2", "dscore", "emphatics1", "emphatics2", "emphatics3",
"feature", "hedges1", "hedges2", "hedges3", "hedges4", "hedges5",
"left", "log_midpipe", "mda_tags", "name", "null2", "null3",
"null4", "null4a", "null5", "null6", "null7", "null8",  "null8a",
"null9", "null9a","null10", "null10a", "null10b", "Other", "passives1",
"passives2", "passives3", "passives4", "passives5", "past_whiz1", "pastp1",
"perfect_asp1", "perfect_asp2", "perfect_asp3", "perfect_asp4",
"phrasal_coord1", "phrasal_coord2", "phrasal_coord3", "phrasal_coord4",
"pivot_wider", "place_adverbials", "pp_rel_clauses", "pred_1",
"pred_2", "pred_3", "pred_4", "pred_5", "pres_whiz1", "presp1",
"pro_do", "right", "sentence_rels", "separate", "sh", "split_aux1",
"split_infin1", "split_infin2", "st_tags", "str_prepositions",
"syn_neg1", "syn_neg2", "syn_neg3", "tagged", "tagged_text",
"text", "that_ac", "that_del1", "that_del2", "that_del3", "that_del4",
"that_obj1", "that_sub1", "that_sub2", "that_sub3", "that_subj1",
"that_subj2", "that_subj3", "that_vc1", "that_vc2", "that_vc3",
"that_vc4", "that_vc5", "time_adv1", "time_adv2", "to_prep",
"token", "tokenized",  "token_id", "udmodel", "unnest", "value", "wh_clauses",
"wh_obj1", "wh_questions", "what_subj1", "what_subj2", "what_subj3",
"wordcount", "xpos", "zscore")



utils::globalVariables(globs)
