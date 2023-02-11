#' @title complexTagging
#' @param x character vector of ST tagged text, tokenized.
#'
#'
#'
#' @describeIn complexTagging tag  adverbial subordinators
dtag_adverbial_subords <- function(x){
adverbial1 <- adverbial2 <- adverbial3 <- adverbial4 <- adverbial5 <- NULL
null2 <- null3 <- null4 <- null5 <- null5a <- NULL
  x <- data.table(x)
  x[, adverbial1 := d_grepl(x, "\\bsince_|\\bwhile_|\\bwhilst_|\\bwhereupon_|\\bwhereas_|\\bwhereby_")]
  x[, adverbial2 := d_grepl(x, "\\bsuch_") & d_grepl(shift(x, type="lead", n=1), "that_")]
  x[,null2 := d_grepl(shift(x, type = "lag", n=1), "\\bsuch_") & d_grepl(x, "that_")]


  x[, adverbial3 := d_grepl(x, "\\binasmuch_\\bforasmuch_\\binsofar_\\binsomuch_") &
      d_grepl(shift(x, type="lead", n=1), "as_")]
  x[, null3 := d_grepl(shift(x, type = "lag", n=1), "\\binasmuch_\\bforasmuch_\\binsofar_\\binsomuch_") &
      d_grepl(x, "as_")]


  x[, adverbial4 := d_grepl(x, "so_") &
      d_grepl(shift(x, type="lead", n=1), "that_") &
      !d_grepl(shift(x, type="lead", n=2), "_NN|_NNP|_JJ")]
  x[, null4 := d_grepl(shift(x, type = "lag", n = 1), "so_") &
      d_grepl(x, "that_") &
      !d_grepl(shift(x, type="lead", n=1), "_NN|_NNP|_JJ")]

  x[, adverbial5 := d_grepl(x, "\\bas_") &
      d_grepl(shift(x, type="lead", n=1), "\\blong_|\\bsoon_") &
      d_grepl(shift(x, type="lead", n=2), "\\bas_")]
  x[, null5 := d_grepl(shift(x, type= "lag", n=1), "\\bas_") &
      d_grepl(x, "\\blong_|\\bsoon_") &
      d_grepl(shift(x, type="lead", n=1), "\\bas_")]
  x[, null5a := d_grepl(shift(x, type= "lag", n=2), "\\bas_") &
      d_grepl(shift(x, type= "lag", n=1), "\\blong_|\\bsoon_") &
      d_grepl(x, "\\bas_")]

  x[adverbial1 == TRUE | adverbial2 == TRUE | adverbial3 == TRUE | adverbial4 == TRUE | adverbial5 == TRUE,
    x := d_sub(x, "$", " <OSUB>")]
  x[null2 == TRUE | null3 == TRUE | null4 == TRUE | null5 == TRUE | null5a == TRUE,
    x := d_sub(x, "_\\w+", "_NULL")]
  return(x$x)
}

#' @describeIn complexTagging tag conjuncts
dtag_conjuncts <- function(x){
conjuncts1 <- conjuncts2 <- conjuncts3 <- conjuncts4 <- conjuncts5 <- NULL
conjuncts6 <- conjuncts7 <- conjuncts8 <- conjuncts9 <- conjuncts10 <- NULL
null1 <- null2 <- null3 <- null4 <- null5 <- null6 <- null7  <- NULL
null8 <- null8a <- null9<- null9a <- null10 <- null10a <- null10b   <- NULL

 x <- data.table(x)

  x[, conjuncts1 := d_grepl(x, "\\belse_|\\baltogether_|\\brather_") &
      str_detect(shift(x, type="lag", n=1), "_\\W")]
  x[, null1 := d_grepl(shift(x, type = "lag", n=1), "\\belse_|\\baltogether_|\\brather_") &
      str_detect(x, "_\\W")]

  x[, conjuncts2 := d_grepl(x, "\\balternatively_|\\bconsequently_|\\bconversely_|\\beg_|\\be\\.g\\._|\\bfurthermore_|\\bhence_|\\bhowever_|\\bi\\.e\\._|\\binstead_|\\blikewise_|\\bmoreover_|\\bnamely_|\\bnevertheless_|\\bnonetheless_|\\bnotwithstanding_|\\botherwise_|\\bsimilarly_|\\btherefore_|\\bthus_|\\bviz\\.") &
      str_detect(shift(x, type="lag", n=1), "_\\W")]
  x[, null2 := d_grepl(shift(x, type = "lag", n=1), "\\balternatively_|\\bconsequently_|\\bconversely_|\\beg_|\\be\\.g\\._|\\bfurthermore_|\\bhence_|\\bhowever_|\\bi\\.e\\._|\\binstead_|\\blikewise_|\\bmoreover_|\\bnamely_|\\bnevertheless_|\\bnonetheless_|\\bnotwithstanding_|\\botherwise_|\\bsimilarly_|\\btherefore_|\\bthus_|\\bviz\\.") &
      str_detect(x, "_\\W")]

  x[, conjuncts3 := d_grepl(x, "\\bin_") &
      d_grepl(shift(x, type="lead", n=1), "\\bcomparison_|\\bcontrast_|\\bparticular_|\\baddition_|\\bconclusion_|\\bconsequence_|\\bsum_|\\bsummary_")]
  x[, null3 := d_grepl(shift(x, type= "lag", n=1), "\\bin_") &
      d_grepl(x, "\\bcomparison_|\\bcontrast_|\\bparticular_|\\baddition_|\\bconclusion_|\\bconsequence_|\\bsum_|\\bsummary_")]

  x[, conjuncts4 := d_grepl(x, "\\bfor_") &
      d_grepl(shift(x, type="lead", n=1), "\\bexample_|\\binstance_")]
  x[, null4 := d_grepl(shift(x, type= "lag", n=1), "\\bfor_") &
      d_grepl(x, "\\bexample_|\\binstance_")]

  x[, conjuncts5 := d_grepl(x, "\\binstead_") &
      d_grepl(shift(x, type="lead", n=1), "\\bof_")]
  x[, null5 := d_grepl(shift(x, type= "lag", n=1), "\\binstead_") &
      d_grepl(x, "\\bof_")]

  x[, conjuncts6 := d_grepl(x, "\\bby_") &
      d_grepl(shift(x, type="lead", n=1), "\\bcontrast_|\\bcomparison_")]
  x[, null6 := d_grepl(shift(x, type = "lag", n=1), "\\bby_") &
      d_grepl(x, "\\bcontrast_|\\bcomparison_")]

  x[, conjuncts7 := d_grepl(x, "\\bin_") &
      d_grepl(shift(x, type="lead", n=1), "\\bany_") &
      d_grepl(shift(x, type="lead", n=2), "\\bevent_|\\bcase_")]
  x[, null7 := d_grepl(shift(x, type = "lag", n=1), "\\bin_") &
      d_grepl(x, "\\bany_") &
      d_grepl(shift(x, type="lead", n=1), "\\bevent_|\\bcase_")]

  x[, conjuncts8 := d_grepl(x, "\\bin_") &
      d_grepl(shift(x, type="lead", n=1), "\\bother_") &
      d_grepl(shift(x, type="lead", n=2), "\\bwords_")]
  x[, null8 := d_grepl(shift(x, type = "lag", n=1), "\\bin_") &
      d_grepl(x, "\\bother_") &
      d_grepl(shift(x, type="lead", n=1), "\\bwords_")]
  x[, null8a := d_grepl(shift(x, type = "lag", n=2), "\\bin_") &
      d_grepl(shift(x, type="lag", n=1), "\\bother_") &
      d_grepl(x, "\\bwords_")]

  x[, conjuncts9 := d_grepl(x, "\\bas_") &
      d_grepl(shift(x, type="lead", n=1), "\\ba_") &
      d_grepl(shift(x, type="lead", n=2), "\\bresult_|\\bconsequence_")]
  x[, null9 := d_grepl(shift(x, type = "lag", n=1), "\\bas_") &
      d_grepl(x, "\\ba_") &
      d_grepl(shift(x, type="lead", n=1), "\\bresult_|\\bconsequence_")]
  x[, null9a := d_grepl(shift(x, type = "lag", n=2), "\\bas_") &
      d_grepl(shift(x, type = "lag", n=1), "\\ba_") &
      d_grepl(x, "\\bresult_|\\bconsequence_")]

  x[, conjuncts10 := d_grepl(x, "\\bon_") &
      d_grepl(shift(x, type = "lead", n=1), "\\bthe_") &
      d_grepl(shift(x, type = "lead", n=2), "\\bother_") &
      d_grepl(shift(x, type = "lead", n=3), "\\bhand_")]
  x[, null10 := d_grepl(shift(x, type = "lag", n=1), "\\bon_") &
      d_grepl(x, "\\bthe_") &
      d_grepl(shift(x, type = "lead", n=1), "\\bother_") &
      d_grepl(shift(x, type = "lead", n=2), "\\bhand_")]
  x[, null10a := d_grepl(shift(x, type = "lag", n=2), "\\bon_") &
      d_grepl(shift(x, type = "lag", n=1), "\\bthe_") &
      d_grepl(x, "\\bother_") &
      d_grepl(shift(x, type = "lead", n=1), "\\bhand_")]
  x[, null10b := d_grepl(shift(x, type = "lag", n=3), "\\bon_") &
      d_grepl(shift(x, type = "lag", n=2), "\\bthe_") &
      d_grepl(shift(x, type = "lag", n=1), "\\bother_") &
      d_grepl(x, "\\bhand_")]

  x[conjuncts1 == TRUE | conjuncts2 == TRUE | conjuncts3 == TRUE | conjuncts4 == TRUE |
    conjuncts5 == TRUE | conjuncts6 == TRUE | conjuncts7 == TRUE | conjuncts8 == TRUE |
    conjuncts9 == TRUE | conjuncts10 == TRUE,
    x := d_sub(x, "$", " <CONJ>")]
  x[null1 == TRUE | null2 == TRUE | null3 == TRUE | null4 == TRUE | null4 == TRUE |
    null5 == TRUE | null6 == TRUE | null7 == TRUE | null8 == TRUE | null8a == TRUE |
    null9 == TRUE | null9a == TRUE | null10 == TRUE | null10a == TRUE | null10b == TRUE,
    x := d_sub(x, "_\\w+", "_NULL")]

  return(x$x)
  }

#' @describeIn complexTagging tag predicative adjectives

dtag_pred_adj <- function(x){
  pred_1 <-  pred_2 <-  pred_3 <-  pred_4 <-  pred_5 <- NULL
  x <- data.table(x)
  x[, pred_1 := d_grepl(shift(x, type="lag", n=1), sh["be"]) &
      d_grepl(x, "_JJ") &
      !d_grepl(shift(x, type="lead", n=1), "_JJ|_RB|_NN|_NNP")]
  x[, pred_2 := d_grepl(shift(x, type="lag", n=2), sh["be"]) &
      d_grepl(shift(x, type="lag", n=1), "_RB") &
      d_grepl(x, "_JJ") &
      !d_grepl(shift(x, type="lead", n=1), "_JJ|_RB|_NN|_NNP")]
  x[, pred_3 := d_grepl(shift(x, type="lag", n=2), sh["be"])  &
      d_grepl(shift(x, type="lag", n=1), "_XX0") &
      d_grepl(x, "_JJ") &
      !d_grepl(shift(x, type="lead", n=1), "_JJ|_RB|_NN|_NNP")]
  x[, pred_4 := d_grepl(shift(x, type="lag", n=3), sh["be"]) &
      d_grepl(shift(x, type="lag", n=2), "_XX0") &
      d_grepl(shift(x, type="lead", n=1), "_RB") &
      d_grepl(x, "_JJ") &
      !d_grepl(shift(x, type="lead", n=1), "_JJ|_RB|_NN|_NNP")]

  x[pred_1 == TRUE | pred_2 == TRUE | pred_3 == TRUE | pred_4 == TRUE,
    x := d_sub(x, "$", " <PRED>")]
  x[, pred_5 := str_detect(shift(x, type="lag", n=2), "<PRED>") &
      d_grepl(shift(x, type="lag", n=1), "_PHC|_CC") &
      d_grepl(x, "_JJ")]
  x[pred_5 == TRUE, x := d_sub(x, "$", " <PRED>")]

  return(x$x)
}

#' @describeIn complexTagging tag emphatics
dtag_emphatics <- function(x){
emphatics1  <- emphatics2  <- emphatics3  <- null3 <- NULL
  x <- data.table(x)

  x[, emphatics1 := d_grepl(x, "\\bjust_|\\breally_|\\bmost_|\\bmore_")]

  x[, emphatics2 := d_grepl(x, "\\breal_") & d_grepl(shift(x, type="lead", n=1), "_JJ") |
      d_grepl(x, "\\bso_") & d_grepl(shift(x, type="lead", n=1), "_JJ") |
      d_grepl(x,sh["do"]) & d_grepl(shift(x, type="lead", n=1), "_V")]

  x[, emphatics3 := d_grepl(x, "\\bfor_") & d_grepl(shift(x, type="lead", n=1), "\\bsure_") |
      d_grepl(x, "\\ba_") & d_grepl(shift(x, type="lead", n=1), "\\blot_") |
      d_grepl(x, "\\bsuch_") & d_grepl(shift(x, type="lead", n=1), "\\ba_")]

  x[, null3 := d_grepl(shift(x, type="lag", n=1), "\\bfor_") & d_grepl(x, "\\bsure_") |
      d_grepl(shift(x, type="lag", n=1), "\\ba_") & d_grepl(x, "\\blot_") |
      d_grepl(shift(x, type="lag", n=1), "\\bsuch_") & d_grepl(x, "\\ba_")]

  x[emphatics1 == TRUE | emphatics2 == TRUE | emphatics3 == TRUE,
    x := d_sub(x, "(<PRED>)?$", " <EMPH>")]

  x[null3 == TRUE, x := d_sub(x, "_\\w+", "_NULL")]

  return(x$x)
}



#' @describeIn complexTagging tag phrasal "and" coordination
dtag_phrasal_coord <- function(x){

phrasal_coord1 <- phrasal_coord2 <- phrasal_coord3 <- phrasal_coord4 <- NULL


  x <- data.table(x)
  x[, phrasal_coord1 := d_grepl(x,"\\band_") & d_grepl(shift(x, type="lag", n=1),"_RB") & d_grepl(shift(x, type="lead", n=1),"_RB")]
  x[, phrasal_coord2 := d_grepl(x,"\\band_") & d_grepl(shift(x, type="lag", n=1),"_JJ|_PRED") & d_grepl(shift(x, type="lead", n=1),"_JJ|_PRED")]
  x[, phrasal_coord3 := d_grepl(x,"\\band_") & d_grepl(shift(x, type="lag", n=1),"_V") & d_grepl(shift(x, type="lead", n=1),"_V")]
  x[, phrasal_coord4 := d_grepl(x,"\\band_") & d_grepl(shift(x, type="lag", n=1),"_NN") & d_grepl(shift(x, type="lead", n=1),"_NN")]

  x[phrasal_coord1 == TRUE | phrasal_coord2 == TRUE | phrasal_coord3 == TRUE | phrasal_coord4 == TRUE,
    x := d_sub(x, "$", " <PHC>")]

  return(x$x)
}


#' @describeIn complexTagging tag pro-verb do
dtag_pro_do <- function(x){

  pro_do <- NULL

  x <- data.table(x)
  x[, pro_do :=   !d_grepl_case(shift(x, type="lag", n=1),"(_\\W)") &
      !d_grepl(shift(x, type="lag", n=1), str_flatten(sh[c("wp","who")], "|")) &
      d_grepl(x,sh["do"]) &
      !d_grepl(shift(x, type="lead", n=1),"_V") &
      !d_grepl(shift(x, type="lead", n=1),"_XX0") &
      !(d_grepl(shift(x, type="lead", n=1),"_RB|_XX0") | d_grepl(shift(x, type="lead", n=2),"_V")) &
      !(d_grepl(shift(x, type="lead", n=1),"_RB|_XX0") | d_grepl(shift(x, type="lead", n=1),"_RB") | d_grepl(shift(x, type="lead", n=3),"_V"))]

  x[pro_do == TRUE, x := d_sub(x, "$", " <PROD>")]

  return(x$x)
}


#' @describeIn complexTagging tag WH questions
dtag_wh_questions <- function(x){
wh_questions <- NULL

  x <- data.table(x)
  x[, wh_questions := d_grepl_case(shift(x, type="lag", n=1), "_\\W|\\b[Ss]o_RB|\\b[Aa]nd_") &
      d_grepl(x, sh["who"]) &
  !d_grepl(x,"\\bhowever_|\\bwhatever_") &
  d_grepl(shift(x, type="lead", n=1),str_c("_MD|", str_flatten(sh[c("have","be","do")], "|")))]
  x[wh_questions == TRUE, x := d_sub(x, "$", " <WHQU>")]


  return(x$x)
}

#' @describeIn complexTagging tag sentence relatives
dtag_sentence_rels <- function(x){

  sentence_rels <- NULL

  x <- data.table(x)

  x[, sentence_rels := str_detect(shift(x, type="lag", n=1), "_\\W") & d_grepl(x,"\\bwhich_")]

  x[sentence_rels == TRUE, x := d_sub(x, "$", " <SERE>")]

  return(x$x)
}


#' @describeIn complexTagging tag perfect aspects
dtag_perfect_asp <- function(x){

  perfect_asp1 <- perfect_asp2 <- perfect_asp3 <- perfect_asp4 <- NULL


  x <- data.table(x)
  x[, perfect_asp1 := d_grepl(x,sh["have"]) &
      d_grepl(shift(x, type="lead", n=1),"_VBD|_VBN")]

  x[, perfect_asp2 := d_grepl(x,sh["have"]) &
      d_grepl(shift(x, type="lead", n=1),"_RB|_XX0|_NN|_NNP|_PRP") &
      d_grepl(shift(x, type="lead", n=2),"_VBD|_VBN")]

  x[, perfect_asp3 := d_grepl(x,sh["have"]) &
      d_grepl(shift(x, type="lead", n=1),"_RB|_XX0") &
      d_grepl(shift(x, type="lead", n=2),"_RB|_XX0") &
      d_grepl(shift(x, type="lead", n=3),"_VBD|_VBN")]

  x[, perfect_asp4 := d_grepl(x,sh["have"]) &
      d_grepl(shift(x, type="lead", n=1),"_XX0") &
      d_grepl(shift(x, type="lead", n=2),"_NN|_NNP|_PRP") &
      d_grepl(shift(x, type="lead", n=3),"_VBD|_VBN")]

  x[perfect_asp1 == TRUE, x := d_sub(x, "$", " <PEAS>")]
  x[perfect_asp2 == TRUE, x := d_sub(x, "$", " <PEAS>")]
  x[perfect_asp3 == TRUE, x := d_sub(x, "$", " <PEAS>")]
  x[perfect_asp4 == TRUE, x := d_sub(x, "$", " <PEAS>")]

  return(x$x)
}

#' @describeIn complexTagging tag passives
dtag_passives <- function(x, by = TRUE){
passives1 <- passives2 <- passives3 <- passives4 <- passives5 <- NULL
bypassives1 <- bypassives2 <- bypassives3 <-  NULL

  x <- data.table(x)

  x[, passives1 := d_grepl(x, sh["be"]) &
      d_grepl(shift(x, type="lead", n=1), "_VBD|_VBN")]

  x[, passives2 := d_grepl(x, sh["be"]) &
      d_grepl(shift(x, type="lead", n=1), "_RB|_XX0") &
      d_grepl(shift(x, type="lead", n=2), "_VBD|_VBN")]

  x[, passives3 := d_grepl(x, sh["be"]) &
      d_grepl(shift(x, type="lead", n=1), "_NN|_NNP|_PRP") &
      d_grepl(shift(x, type="lead", n=2), "_VBD|_VBN")]

  x[, passives4 := d_grepl(x, sh["be"]) &
      d_grepl(shift(x, type="lead", n=1), "_RB|_XX0") &
      d_grepl(shift(x, type="lead", n=2), "_RB|_XX0") &
      d_grepl(shift(x, type="lead", n=3), "_VBD|_VBN")]

  x[, passives5 := d_grepl(x, sh["be"]) &
      d_grepl(shift(x, type="lead", n=1), "_RB|_XX0") &
      d_grepl(shift(x, type="lead", n=2), "_NN|_NNP|_PRP") &
      d_grepl(shift(x, type="lead", n=3), "_VBD|_VBN")]

  x[passives1 == TRUE | passives2 == TRUE | passives3 == TRUE | passives4 == TRUE | passives5 == TRUE, x := d_sub(x, "$", " <PASS>")]

  if(by){  x[, bypassives1 := d_grepl(x, "<PASS>") &
               d_grepl(shift(x, type="lead", n=2), "\\bby_")]

    x[, bypassives2 := d_grepl(x, "<PASS>") &
        d_grepl(shift(x, type="lead", n=3), "\\bby_")]

    x[, bypassives3 := d_grepl(x, "<PASS>") &
        d_grepl(shift(x, type="lead", n=4), "\\bby_")]

    x[bypassives1 == TRUE | bypassives2 == TRUE | bypassives3 == TRUE, x := d_sub(x, "<PASS>", "<BYPA>")]

    return(x$x)
  }

  return(x$x)
}

#' @describeIn complexTagging tag passives with by
dtag_passives_by <- function(x){

    if(any(str_detect(x, "<PASS>")) == FALSE) stop('No PASSIVE tags found. Try running dtag_passives() first.\nThe default setting will tag BY- passives as well')

    x <- data.table(x)



  x[, bypassives1 := d_grepl(x, "<PASS>") &
      d_grepl(shift(x, type="lead", n=2), "\\bby_")]

  x[, bypassives2 := d_grepl(x, "<PASS>") &
      d_grepl(shift(x, type="lead", n=3), "\\bby_")]

  x[, bypassives3 := d_grepl(x, "<PASS>") &
      d_grepl(shift(x, type="lead", n=4), "\\bby_")]

  x[bypassives1 == TRUE | bypassives2 == TRUE | bypassives3 == TRUE, x := d_sub(x, "<PASS>", "<BYPA>")]

  return(x$x)
}

#' @describeIn complexTagging tag be as main verb
dtag_be_main <- function(x){

  be_main1 <- be_main2 <- NULL

    x <- data.table(x)

    x[, be_main1 := !d_grepl(shift(x, type="lag", n=2), "_EX") &
        !d_grepl(shift(x, type="lag", n=1), "_EX") &
        d_grepl(x,sh["be"]) &
        d_grepl(shift(x, type="lead", n=1),"_CD|_DT|_PDT|_PRPS|_PRP|_JJ|_PRED|<PIN>|_QUAN")]

    x[, be_main2 := !d_grepl(shift(x, type="lag", n=2), "_EX") &
        !d_grepl(shift(x, type="lag", n=1), "_EX") &
        d_grepl(x,sh["be"]) &
        d_grepl(shift(x, type="lead", n=1),"_RB|_XX0") &
        d_grepl(shift(x, type="lead", n=2),"_CD|_DT|_PDT|_PRPS|_PRP|_JJ|_PRED|<PIN>|_QUAN")]

    x[be_main1 == TRUE, x := d_sub(x, "$", " <BEMA>")]
    x[be_main2 == TRUE, x := d_sub(x, "$", " <BEMA>")]

    return(x$x)
  }

#' @describeIn complexTagging tag WH clauses
dtag_wh_clauses <- function(x){
  wh_clauses <- NULL

    x <- data.table(x)
    x[, wh_clauses := d_grepl(shift(x, type="lag", n=1),str_flatten(sh[c("public" , "private" , "suasive")],"|")) &
        d_grepl(x,str_flatten(sh[c("wp" , "who")],"|")) &
        any(!d_grepl(shift(x, type="lead", n=1),"_MD") |
        !d_grepl(shift(x, type="lead", n=1), str_flatten(sh[c("do" , "have" , "be")],"|")))]

    x[wh_clauses == TRUE, x := d_sub(x, "$", " <WHCL>")]

    return(x$x)
}

#' @describeIn complexTagging pied piper relative clauses
dtag_pp_rel_clauses <- function(x){
  pp_rel_clauses <- NULL
  x <- data.table(x)

  x[,pp_rel_clauses := d_grepl(x, "<PIN>") &
  d_grepl(shift(x, type="lead", n=1), "\\bwho_|\\bwhom_|\\bwhose_|\\bwhich_")]

  x[pp_rel_clauses == TRUE, x := d_sub(x,"$"," <PIRE>")]
}


#' @describeIn complexTagging tag stranded Prepositions
dtag_str_prepositions <- function(x){
  str_prepositions <- NULL
   x <- data.table(x)

    x[, str_prepositions := d_grepl(x, "<PIN>") & !d_grepl(x, "\\bbesides") &
        d_grepl(shift(x, type="lead", n=1), "_[\\,.]")]

    x[str_prepositions == TRUE, x := d_sub(x,"$"," <STPR>")]

    return(x$x)
  }

#' @describeIn complexTagging tag Split Infinitive
dtag_split_infinitives <- function(x){

  split_infin1 <- split_infin2 <- NULL
  x <- data.table(x)

  x[, split_infin1 := d_grepl(x, "\\bto_") &
      d_grepl(shift(x, type="lead", n=1), "_RB|_AMPLIF|_DOWNTON|\\bjust_|\\breally_|\\bmost_|\\bmore_") &
      d_grepl(shift(x, type="lead", n=2), "_V")]

  x[, split_infin2 := d_grepl(x, "\\bto_") &
      d_grepl(shift(x, type="lead", n=1), "_RB|_AMPLIF|_DOWNTON|\\bjust_|\\breally_|\\bmost_|\\bmore_") &
      d_grepl(shift(x, type="lead", n=2), "_RB|_AMPLIF|_DOWNTON") &
      d_grepl(shift(x, type="lead", n=3), "_V")]

  x[split_infin1 == TRUE, x := d_sub(x, "$", " <SPIN>")]
  x[split_infin2 == TRUE, x := d_sub(x, "$", " <SPIN>")]

  return(x$x)
}


#' @describeIn complexTagging tag Split Auxiliaries
dtag_split_auxiliaries <- function(x){

  split_aux1 <- NULL
  x <- data.table(x)

  x[, split_aux1 := d_grepl(x,str_c(str_flatten(sh[c("do" , "have" , "be")],"|"),"|_MD")) &
      d_grepl(shift(x, type="lead", n=1), "_RB|\\bjust_|\\breally_|\\bmost_|\\bmore_|<AMP>|<DWNT>") &
      !d_grepl(shift(x, type="lead", n=1), "n't_|not") &
      d_grepl(shift(x, type="lead", n=2), "_V")]

  x[split_aux1 == TRUE, x := d_sub(x, "$", " <SPAU>")]

  return(x$x)
}


#' @describeIn complexTagging tag Synthetic Negation
dtag_syn_negation <- function(x){

  syn_neg1 <- syn_neg2 <- syn_neg3 <- NULL

  x <- data.table(x)
  x[, syn_neg1 := d_grepl(x, "\\bno_") & d_grepl(shift(x, type="lead", n=1), "JJ|PRED|NN|NNP")]
  x[, syn_neg2 := d_grepl(x, "\\bneither_")]
  x[, syn_neg3 := d_grepl(x, "\\bnor_")]

  x[syn_neg1 == TRUE | syn_neg2 == TRUE | syn_neg3 == TRUE, x := d_sub(x, "$", " <SYNE>")]

  return(x$x)
}


#' @describeIn complexTagging tag Time Adverbials
dtag_time_adverbials <- function(x){
time_adv1 <- time_adv2 <- NULL

  x <- data.table(x)
  x[, time_adv1 := d_grepl(x, "\\bafterwards_|\\bagain_|\\bearlier_|\\bearly_|\\beventually_|\\bformerly_|\\bimmediately_|\\binitially_|\\binstantly_|\\blate_|\\blately_|\\blater_|\\bmomentarily_|\\bnow_|\\bnowadays_|\\bonce_|\\boriginally_|\\bpresently_|\\bpreviously_|\\brecently_|\\bshortly_|\\bsimultaneously_|\\bsubsequently_|\\btoday_|\\bto-day_|\\btomorrow_|\\bto-morrow_|\\btonight_|\\bto-night_|\\byesterday_")]
  x[, time_adv2 := d_grepl(x, "\\bsoon_") & !d_grepl(shift(x, type="lag", n=1), "\\bas_")]

  x[time_adv1 == TRUE | time_adv2 == TRUE, x := d_sub(x, "$", " <TIME>")]

  return(x$x)
}


#' @describeIn complexTagging tag Place Adverbials
dtag_place_adverbials <- function(x){
place_adverbials <- NULL

  x <- data.table(x)
  x[, place_adverbials := d_grepl(x, sh["place"]) &
      !d_grepl(x, "_NN$|_NNP")]

  x[place_adverbials == TRUE, x := d_sub(x, "$", " <PLACE>")]

  return(x$x)
}

#' @describeIn complexTagging tag 'that' verb complements
dtag_that_vc <- function(x){
 that_vc1 <- that_vc2 <- that_vc2 <- that_vc2 <- that_vc2 <- NULL

  x <- data.table(x)
  x[, that_vc1 := str_detect(shift(x, type="lag", n=1), "\\band_|\\bnor_|\\bbut_|\\bor_|\\balso_|_\\W") &
      d_grepl(x, "\\bthat_") &
      d_grepl(shift(x, type="lead", n=1), "_DT|_QUAN|_CD|_PRP|there_|_NNS|_NNP")]

  x[, that_vc2 := d_grepl(shift(x, type="lag", n=1), str_flatten(sh[c("public" , "private" , "suasive","SMP")],"|")) &
      d_grepl(x, "\\bthat_") &
      !d_grepl(shift(x, type="lead", n=1), "_V|_MD|\\band_|_\\W") &
      !d_grepl(shift(x, type="lead", n=1), str_flatten(sh[c("do" , "have" , "be")], "|"))]

  x[, that_vc3 := d_grepl(shift(x, type="lag", n=1), str_flatten(sh[c("public" , "private" , "suasive")],"|")) &
      d_grepl(x, "\\bthat_") &
      d_grepl(shift(x, type="lead", n=1), "_N") &
      d_grepl(shift(x, type="lead", n=2), "<PIN>") &
      !d_grepl(shift(x, type="lead", n=3), "_N")]


  x[, that_vc4 := d_grepl(shift(x, type="lag", n=1), str_flatten(sh[c("public" , "private" , "suasive")],"|")) &
      d_grepl(x, "\\bthat_") &
      d_grepl(shift(x, type="lead", n=2), "_N") &
      d_grepl(shift(x, type="lead", n=3), "<PIN>") &
      !d_grepl(shift(x, type="lead", n=4), "_N")]



  x[, that_vc5 := d_grepl(shift(x, type="lag", n=1), str_flatten(sh[c("public" , "private" , "suasive")],"|")) &
      d_grepl(x, "\\bthat_") &
      d_grepl(shift(x, type="lead", n=1), "_N") &
      !d_grepl(shift(x, type="lead", n=2), "_N") &!
      d_grepl(shift(x, type="lead", n=3), "_N") &
      !d_grepl(shift(x, type="lead", n=4), "_N") &
      d_grepl(shift(x, type="lead", n=5), "<PIN>")]

  x[that_vc1 == TRUE | that_vc2 == TRUE | that_vc2 == TRUE | that_vc2 == TRUE | that_vc2 == TRUE,
    x := d_sub(x, "$", " <THVC>")]

  return(x$x)
}

#' @describeIn complexTagging tag 'that' adjective complements

dtag_that_ac <- function(x){
  that_ac <- NULL

  x <- data.table(x)
  x[, that_ac := d_grepl(x, "\\bthat_") &
      d_grepl(shift(x, type="lag", n=1), "_JJ")]

  x[that_ac == TRUE, x := d_sub(x, "$", " <THAC>")]

  return(x$x)
}

#' @describeIn complexTagging tag Present Participial Clauses
dtag_pres_part <- function(x){
presp1 <- NULL

  x <- data.table(x)
  x[, presp1 :=  d_grepl(x, "_VBG") &
      (is.na(shift(x, type="lag", n=1)) |
      str_detect(shift(x, type="lag", n=1), "_\\W")) &
      (d_grepl(shift(x, type="lead", n=1), "<PIN>") |
         d_grepl(shift(x, type="lead", n=1), "_DT") |
         d_grepl(shift(x, type="lead", n=1), "_QUAN") |
         d_grepl(shift(x, type="lead", n=1), "_CD") |
         d_grepl(shift(x, type="lead", n=1), sh["wp"]) |
         d_grepl(shift(x, type="lead", n=1), "_WPS") |
         d_grepl(shift(x, type="lead", n=1), sh["who"]) |
         d_grepl(shift(x, type="lead", n=1), "_PRP") |
         d_grepl(shift(x, type="lead", n=1), "_RB"))]

  x[presp1 == TRUE, x := d_sub(x, "$", " <PRESP>")]

  return(x$x)
}

#' @describeIn complexTagging tag Past Participial Clauses
dtag_past_part <- function(x){
pastp1 <- NULL

  x <- data.table(x)
  x[, pastp1 := d_grepl(x, "_VBN") &
      (is.na(shift(x, type="lag", n=1)) |
         str_detect(shift(x, type="lag", n=1), "_\\W")) &
      (d_grepl(shift(x, type="lead", n=1), "<PIN>") |
         d_grepl(shift(x, type="lead", n=1), "_RB"))]

  x[pastp1 == TRUE, x := d_sub(x, "$", " <PASTP>")]

  return(x$x)
}

#' @describeIn complexTagging tag Past Participial WHIZ Deletion Relatives
dtag_past_whiz <- function(x){
 past_whiz1 <- NULL

 x <- data.table(x)
  x[, past_whiz1 := (d_grepl(shift(x, type="lag", n=1), "_N") |
      d_grepl(shift(x, type="lag", n=1), "_QUPR")) &
      d_grepl(x, "_VBN") &
      (d_grepl(shift(x, type="lead", n=1), "<PIN>") |
      d_grepl(shift(x, type="lead", n=1), "_RB") |
      d_grepl(shift(x, type="lead", n=1), sh["be"]))]

  x[past_whiz1 == TRUE, x := d_sub(x, "$", " <WZPAST>")]

  return(x$x)
}

#' @describeIn complexTagging tag Present Participial WHIZ Deletion Relatives
dtag_pres_whiz <- function(x){
 pres_whiz1 <- NULL

  x <- data.table(x)
  x[, pres_whiz1 := d_grepl(shift(x, type="lag", n=1), "_N") &
  d_grepl(x, "_VBG")]

  x[pres_whiz1 == TRUE, x := d_sub(x, "$", " <WZPRES>")]

  return(x$x)
}



#' @describeIn complexTagging tag "that" Relative Clauses on Subject Position
dtag_that_subj <- function(x){
  that_sub1 <- that_sub2 <- that_sub3 <- NULL

  x <- data.table(x)
  x[, that_subj1 := d_grepl(shift(x, type="lag", n=1), "_N") &
      str_detect(x, "\\bthat_") &
      (d_grepl(shift(x, type="lead", n=1), "_MD") |
         d_grepl(shift(x, type="lead", n=1), str_flatten(sh[c("do" , "have" , "be")], "|")) |
         d_grepl(shift(x, type="lead", n=1), "_V"))]

  x[, that_subj2 := d_grepl(shift(x, type="lag", n=1), "_N") &
      str_detect(x, "\\bthat_") &
      d_grepl(shift(x, type="lead", n=1), "_RB|_XX0") &
      (d_grepl(shift(x, type="lead", n=2), "_MD") |
         d_grepl(shift(x, type="lead", n=2), str_flatten(sh[c("do" , "have" , "be")], "|")))]

  x[, that_subj3 := d_grepl(shift(x, type="lag", n=1), "_N") &
      str_detect(x, "\\bthat_") &
      d_grepl(shift(x, type="lead", n=1), "_RB|_XX0") &
      d_grepl(shift(x, type="lead", n=2), "_RB|_XX0") &
      (d_grepl(shift(x, type="lead", n=3), "_MD") |
         d_grepl(shift(x, type="lead", n=3), str_flatten(sh[c("do" , "have" , "be")], "|")) |
         d_grepl(shift(x, type="lead", n=3), "_V"))]

  x[that_subj1 == TRUE, x := d_sub(x, "$", " <TSUB>")]
  x[that_subj2 == TRUE, x := d_sub(x, "$", " <TSUB>")]
  x[that_subj3 == TRUE, x := d_sub(x, "$", " <TSUB>")]

  return(x$x)
}


#' @describeIn complexTagging tag "that" Relative Clauses on Object Position
dtag_that_obj <- function(x){
that_obj1 <- NULL

  x <- data.table(x)

  x[, that_obj1 := d_grepl(shift(x, type="lag", n=1), "_N") &
      d_grepl(x, "\\bthat_") &
      (d_grepl(shift(x, type="lead", n=1), "_DT|_QUAN|_CD|\\bit_|_JJ|_NNS|_NNP|_PRPS|\\bi_|\\bwe_|\\bhe_|\\bshe_|\\bthey_") |
         (d_grepl(shift(x, type="lead", n=1), "_N")  &
            d_grepl(shift(x, type="lead", n=2), "_POS")))]

  x[that_obj1 == TRUE, x := d_sub(x, "_\\w+", "_TOBJ")]

  return(x$x)

}


#' @describeIn complexTagging tag WH Relative Clauses on Subject Position
dtag_wh_subj <- function(x){
 that_subj1 <- that_subj2 <- that_subj3 <- NULL

  x <- data.table(x)
  x[, that_subj1 := !d_grepl(shift(x, type="lag", n=3), "\\bask_|\\basks_|\\basked_|\\basking_|\\btell_|\\btells_|\\btold_|\\btelling_") &
      d_grepl(shift(x, type="lag", n=1), "_N") &
      d_grepl(x, "\\bthat_") &
      (d_grepl(shift(x, type="lead", n=1), "_DT|_QUAN|_CD|\\bit_|_JJ|_NNS|_NNP|_PRPS|\\bi_|\\bwe_|\\bhe_|\\bshe_|\\bthey_") |
         (d_grepl(shift(x, type="lead", n=1), "_N") &
            d_grepl(shift(x, type="lead", n=2), "_POS")))]

  x[, that_subj2 := !d_grepl(shift(x, type="lag", n=3), "\\bask_|\\basks_|\\basked_|\\basking_|\\btell_|\\btells_|\\btold_|\\btelling_") &
      d_grepl(shift(x, type="lag", n=1), "_N") &
      d_grepl(x, "\\bthat_") &
      d_grepl(shift(x, type="lead", n=1), "_RB|_XX0") &
      (d_grepl(shift(x, type="lead", n=2), "_MD|_V") |
       d_grepl(shift(x, type="lead", n=2), str_flatten(sh[c("do" , "have" , "be")], "|")))]

  x[, that_subj3 := !d_grepl(shift(x, type="lag", n=3), "\\bask_|\\basks_|\\basked_|\\basking_|\\btell_|\\btells_|\\btold_|\\btelling_") &
      d_grepl(shift(x, type="lag", n=1), "_N") &
      d_grepl(x, "\\bthat_") &
      d_grepl(shift(x, type="lead", n=1), "_RB|_XX0") &
      d_grepl(shift(x, type="lead", n=2), "_RB|_XX0") &
      (d_grepl(shift(x, type="lead", n=3), "_MD|_V") |
      d_grepl(shift(x, type="lead", n=3), str_flatten(sh[c("do" , "have" , "be")], "|")))]

  x[that_subj1 == TRUE | that_subj2 == TRUE | that_subj3 == TRUE,
    x := d_sub(x, "$", " <WHSUB>")]

  return(x$x)
}


#' @describeIn complexTagging tag WH Relative Clauses on Object Position
dtag_wh_obj <- function(x){
wh_obj1 <- NULL

  x <- data.table(x)
  x[, wh_obj1 := !d_grepl(shift(x, type="lag", n=3), "\\bask_|\\basks_|\\basked_|\\basking_|\\btell_|\\btells_|\\btold_|\\btelling_") &
      d_grepl(shift(x, type="lag", n=1), "_N") &
      d_grepl(x, sh["wp"]) &
     !d_grepl(shift(x, type="lead", n=1), "_RB|_XX0|_MD|_V") &
     !d_grepl(shift(x, type="lead", n=1), str_flatten(sh[c("do" , "have" , "be")], "|"))]

  x[wh_obj1 == TRUE, x := d_sub(x, "$", " <WHOBJ>")]

  return(x$x)
}

#' @describeIn complexTagging tag Hedges
dtag_hedges <- function(x){
hedges1 <- hedges2 <- hedges3 <- hedges4 <- hedges5 <- NULL
null2 <- null3 <- null4 <- null4a <- null5 <- NULL

  x <- data.table(x)

  x[, hedges1 := d_grepl(x, "\\bmaybe_")]

  x[, hedges2 := d_grepl(x, "\\bat_") &
      d_grepl(shift(x, type="lead", n=1), "\\babout_")]
  x[, null2 := d_grepl(shift(x, type = "lag", n=1), "\\bat_") &
      d_grepl(x, "\\babout_")]

  x[, hedges3 := d_grepl(x, "\\bsomething_") &
      d_grepl(shift(x, type="lead", n=1), "\\blike_")]
  x[, null3 := d_grepl(shift(x, type = "lag", n=1), "\\bsomething_") &
      d_grepl(x, "\\blike_")]

  x[, hedges4 := d_grepl(x, "\\bmore_") &
      d_grepl(shift(x, type="lead", n=1), "\\bor_") &
      d_grepl(shift(x, type="lead", n=2), "\\bless_")]
  x[, null4 := d_grepl(shift(x, type = "lag", n=1), "\\bmore_") &
      d_grepl(x, "\\bor_") &
      d_grepl(shift(x, type="lead", n=1), "\\bless_")]
  x[, null4a := d_grepl(shift(x, type = "lag", n=2), "\\bmore_") &
      d_grepl(shift(x, type="lag", n=1), "\\bor_") &
      d_grepl(x, "\\bless_")]

  x[, hedges5 := !d_grepl(shift(x, type="lag", n=1), str_flatten(str_c("_DT|_QUAN|_CD|_JJ|_PRED|_PRPS","|",sh["who"]))) &
      d_grepl(x, "\\bsort_|\\bkind_") &
      d_grepl(shift(x, type="lead", n=1), "\\bof_")]
  x[, null5 := !d_grepl(shift(x, type="lag", n=2), str_flatten(str_c("_DT|_QUAN|_CD|_JJ|_PRED|_PRPS","|",sh["who"])))  &
      d_grepl(shift(x, type="lag", n=1), "\\bsort_|\\bkind_") &
      d_grepl(x, "\\bof_")]


  x[hedges1 == TRUE | hedges2 == TRUE | hedges3 == TRUE | hedges4 == TRUE | hedges5 == TRUE,
    x := d_sub(x, "$", " <HDG>")]

  x[null2 == TRUE | null3 == TRUE | null4 == TRUE | null4a == TRUE | null5 == TRUE,
    x := d_sub(x, "_\\w+( <PIN>)?", "_NULL")] # remove preposition tag in case of sort/kind of

  return(x$x)

}

#' @describeIn complexTagging tag Discourse Particles
dtag_disc_part<- function(x){
 disc_part <- NULL

  x <- data.table(x)
  x[, disc_part := str_detect(shift(x, type="lag", n=1), "_\\W") &
      d_grepl(x, "\\bwell_|\\bnow_|\\banyhow_|\\banyways_")]

  x[disc_part == TRUE, x := d_sub(x, "$", " <DPAR>")]

  return(x$x)
}

#' @describeIn complexTagging tag Demonstrative Pronouns
dtag_dem_pronouns <- function(x){
 dem_pronouns1 <- dem_pronouns2 <- NULL

  x <- data.table(x)
  x[, dem_pronouns1 := d_grepl(x, "\\bthat_|\\bthis_|\\bthese_|\\bthose_") &
      !d_grepl(x, "_NULL") &
      (d_grepl(shift(x, type="lead", n=1), "_V|_MD|_\\W|\\band_") |
         d_grepl(shift(x, type="lead", n=1), str_flatten(sh[c("do","have","be","wp")], "|"))) &
      !d_grepl(x, "<TOBJ>|<TSUB>|<THAC>|<THVC>")]

  x[, dem_pronouns2 := d_grepl(x, "\\bthat_") &
      d_grepl(shift(x, type="lead", n=1), "[i']s_")]

  x[dem_pronouns1 == TRUE, x := d_sub(x, "$", " <DEMP>")]
  x[dem_pronouns2 == TRUE, x := d_sub(x, "$", " <DEMP>")]

  return(x$x)
}

#' @describeIn complexTagging tag Demonstratives
dtag_demonstratives <- function(x){
 demonstratives <- NULL

  x <- data.table(x)
  x[, demonstratives := d_grepl(x, "\\bthat_|\\bthis_|\\bthese_|\\bthose_") &
      !d_grepl(x, "<DEMP>|<TOBJ>|<TSUB>|<THAC>|<THVC>|_NULL")]

  x[demonstratives == TRUE, x := d_sub(x, "$", " <DEMO>")]

  return(x$x)
}

#' @describeIn complexTagging tag Subordinator-That Deletion
dtag_that_del <- function(x){
  that_del1 <- that_del2 <- that_del3 <- that_del4 <- NULL

   x <- data.table(x)
  x[, that_del1 := d_grepl(x, str_flatten(sh[c("public" , "private" , "suasive")], "|")) &
      d_grepl(shift(x, type="lead", n=1), "<DEMP>|\\bi_|\\bwe_|\\bhe_|\\bshe_|\\bthey_")]

  x[, that_del2 := d_grepl(x, str_flatten(sh[c("public" , "private" , "suasive")], "|")) &
      d_grepl(shift(x, type="lead", n=1), "_PRP|_N") &
      (d_grepl(shift(x, type="lead", n=2), "_MD|_V") |
      d_grepl(shift(x, type="lead", n=2), str_flatten(sh[c("do","have","be")],"|")))]

  x[, that_del3 := d_grepl(x, str_flatten(sh[c("public" , "private" , "suasive")], "|")) &
      (d_grepl(shift(x, type="lead", n=1), "_PRP|_N") |
      d_grepl(shift(x, type="lead", n=1), "_JJ|_PRED|_RB|_DT|_QUAN|_CD|_PRPS") &
      d_grepl(shift(x, type="lead", n=2), "_N")) &
      (d_grepl(shift(x, type="lead", n=3), "_MD|_V") |
         d_grepl(shift(x, type="lead", n=3), str_flatten(sh[c("do","have","be")],"|")))]

  x[, that_del4 := d_grepl(x, str_flatten(sh[c("public" , "private" , "suasive")], "|")) &
      (d_grepl(shift(x, type="lead", n=1), "_PRP|_N") |
         d_grepl(shift(x, type="lead", n=1), "_JJ|_PRED|_RB|_DT|_QUAN|_CD|_PRPS") &
      d_grepl(shift(x, type="lead", n=2), "_JJ|_PRED")) &
      d_grepl(shift(x, type="lead", n=3), "_N") &
      (d_grepl(shift(x, type="lead", n=4), "_MD|_V") |
      d_grepl(shift(x, type="lead", n=4), str_flatten(sh[c("do","have","be")],"|")))]

  x[that_del1 == TRUE, x := d_sub(x, "$", " <THATD>")]
  x[that_del2 == TRUE, x := d_sub(x, "$", " <THATD>")]
  x[that_del3 == TRUE, x := d_sub(x, "$", " <THATD>")]
  x[that_del4 == TRUE, x := d_sub(x, "$", " <THATD>")]

  return(x$x)
}


#' @describeIn complexTagging tag Independent Clause Coordination
dtag_indep_cc <- function(x){
  andc1 <- andc2 <- andc3 <- andc4 <- NULL

   x <- data.table(x)
  x[, andc1 := d_grepl_case(shift(x, type="lag", n=1), "_\\W") &
      d_grepl(x, "\\band_")]

  x[, andc2 := d_grepl(x, "\\band_") &
      (d_grepl(shift(x, type="lead", n=1), "\\bbecause_|\\balthough_|\\bthough_|\\btho_|\\bif_|\\bunless_|<OSUB>|<DPAR>|<CONJ>")|
         d_grepl(shift(x, type="lead", n=1),str_flatten(sh[c("wp","who")], "|")))]

  x[, andc3 := d_grepl(x, "_,") &
      d_grepl(shift(x, type="lead", n=1), "\\band_") &
      d_grepl(shift(x, type="lead", n=2), "\\bit_|\\bso_|\\bthen_|\\byou_|_DEMP|\\bi_|\\bwe_|\\bhe_|\\bshe_|\\bthey_")]

  x[, andc4 := d_grepl(shift(x, type="lag", n=1), "_,") &
      d_grepl(x, "\\band_") &
      d_grepl(shift(x, type="lead", n=1), "\\bthere_") &
      d_grepl(shift(x, type="lead", n=2), sh["be"])]

  x[andc1 == TRUE | andc2 == TRUE | andc3 == TRUE | andc4 == TRUE,
    x:= d_sub(x, "$", " <ANDC>")]
  return(x$x)
}
