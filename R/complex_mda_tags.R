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

#' @describeIn Complex MDA Tags tag predicative adjectives
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

#' @describeIn Complex MDA Tags tag emphatics
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



#' @describeIn Complex MDA Tags tag phrasal "and" coordination
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


#' @describeIn Complex MDA Tags tag pro-verb do
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


#' @describeIn Complex MDA Tags tag WH questions
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

#' @describeIn Complex MDA Tags tag sentence relatives
dtag_sentence_rels <- function(x){

  sentence_rels <- NULL

  x <- data.table(x)

  x[, sentence_rels := str_detect(shift(x, type="lag", n=1), "_\\W") & d_grepl(x,"\\bwhich_")]

  x[sentence_rels == TRUE, x := d_sub(x, "$", " <SERE>")]

  return(x$x)
}


#' @describeIn Complex MDA Tags tag perfect aspects
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

#' @describeIn Complex MDA Tags tag passives
utils::globalVariables(c("by"))
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

#' @describeIn Complex MDA Tags tag passives with by
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

#' @describeIn Complex MDA Tags tag be as main verb
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

#' @describeIn Complex MDA Tags tag WH clauses
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

#' @describeIn Complex MDA Tags pied piper relative clauses
dtag_pp_rel_clauses <- function(x){
  pp_rel_clauses <- NULL
  x <- data.table(x)

  x[,pp_rel_clauses := d_grepl(x, "<PIN>") &
  d_grepl(shift(x, type="lead", n=1), "\\bwho_|\\bwhom_|\\bwhose_|\\bwhich_")]

  x[pp_rel_clauses == TRUE, x := d_sub(x,"$"," <PIRE>")]
}


#' @describeIn Complex MDA Tags tag stranded Prepositions
dtag_str_prepositions <- function(x){
  str_prepositions <- NULL
   x <- data.table(x)

    x[, str_prepositions := d_grepl(x, "<PIN>") & !d_grepl(x, "\\bbesides") &
        d_grepl(shift(x, type="lead", n=1), "_[\\,.]")]

    x[str_prepositions == TRUE, x := d_sub(x,"$"," <STPR>")]

    return(x$x)
  }

#' @describeIn Complex MDA Tags tag Split Infinitive
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


#' @describeIn Complex MDA Tags tag Split Auxiliaries
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


#' @describeIn Complex MDA Tags tag Synthetic Negation
dtag_syn_negation <- function(x){

  syn_neg1 <- syn_neg2 <- syn_neg3 <- NULL

  x <- data.table(x)
  x[, syn_neg1 := d_grepl(x, "\\bno_") & d_grepl(shift(x, type="lead", n=1), "JJ|PRED|NN|NNP")]
  x[, syn_neg2 := d_grepl(x, "\\bneither_")]
  x[, syn_neg3 := d_grepl(x, "\\bnor_")]

  x[syn_neg1 == TRUE | syn_neg2 == TRUE | syn_neg3 == TRUE, x := d_sub(x, "$", " <SYNE>")]

  return(x$x)
}


#' @describeIn Complex MDA Tags tag Time Adverbials
dtag_time_adverbials <- function(x){
time_adv1 <- time_adv2 <- NULL

  x <- data.table(x)
  x[, time_adv1 := d_grepl(x, "\\bafterwards_|\\bagain_|\\bearlier_|\\bearly_|\\beventually_|\\bformerly_|\\bimmediately_|\\binitially_|\\binstantly_|\\blate_|\\blately_|\\blater_|\\bmomentarily_|\\bnow_|\\bnowadays_|\\bonce_|\\boriginally_|\\bpresently_|\\bpreviously_|\\brecently_|\\bshortly_|\\bsimultaneously_|\\bsubsequently_|\\btoday_|\\bto-day_|\\btomorrow_|\\bto-morrow_|\\btonight_|\\bto-night_|\\byesterday_")]
  x[, time_adv2 := d_grepl(x, "\\bsoon_") & !d_grepl(shift(x, type="lag", n=1), "\\bas_")]

  x[time_adv1 == TRUE | time_adv2 == TRUE, x := d_sub(x, "$", " <TIME>")]

  return(x$x)
}


#' @describeIn Complex MDA Tags tag Place Adverbials
dtag_place_adverbials <- function(x){
place_adverbials <- NULL

  x <- data.table(x)
  x[, place_adverbials := d_grepl(x, sh["place"]) &
      !d_grepl(x, "_NN$|_NNP")]

  x[place_adverbials == TRUE, x := d_sub(x, "$", " <PLACE>")]

  return(x$x)
}

#' @describeIn Complex MDA Tags tag 'that' verb complements
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

#' @describeIn Complex MDA Tags tag 'that' adjective complements

dtag_that_ac <- function(x){
  that_ac <- NULL

  x <- data.table(x)
  x[, that_ac := d_grepl(x, "\\bthat_") &
      d_grepl(shift(x, type="lag", n=1), "_JJ")]

  x[that_ac == TRUE, x := d_sub(x, "$", " <THAC>")]

  return(x$x)
}

#' @describeIn Complex MDA Tags tag Present Participial Clauses
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

#' @describeIn Complex MDA Tags tag Past Participial Clauses
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

#' @describeIn Complex MDA Tags tag Past Participial WHIZ Deletion Relatives
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

#' @describeIn Complex MDA Tags tag Present Participial WHIZ Deletion Relatives
dtag_pres_whiz <- function(x){
 pres_whiz1 <- NULL

  x <- data.table(x)
  x[, pres_whiz1 := d_grepl(shift(x, type="lag", n=1), "_N") &
  d_grepl(x, "_VBG")]

  x[pres_whiz1 == TRUE, x := d_sub(x, "$", " <WZPRES>")]

  return(x$x)
}



#' @describeIn Complex MDA Tags tag "that" Relative Clauses on Subject Position
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


#' @describeIn Complex MDA Tags tag "that" Relative Clauses on Object Position
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


#' @describeIn Complex MDA Tags tag WH Relative Clauses on Subject Position
dtag_wh_subj <- function(x){
 what_subj1 <- what_subj2 <- what_subj3 <- NULL

  x <- data.table(x)
  x[, what_subj1 := !d_grepl(shift(x, type="lag", n=3), "\\bask_|\\basks_|\\basked_|\\basking_|\\btell_|\\btells_|\\btold_|\\btelling_") &
      d_grepl(shift(x, type="lag", n=1), "_N") &
      d_grepl(x, "\\bthat_") &
      (d_grepl(shift(x, type="lead", n=1), "_DT|_QUAN|_CD|\\bit_|_JJ|_NNS|_NNP|_PRPS|\\bi_|\\bwe_|\\bhe_|\\bshe_|\\bthey_") |
         (d_grepl(shift(x, type="lead", n=1), "_N") &
            d_grepl(shift(x, type="lead", n=2), "_POS")))]

  x[, what_subj2 := !d_grepl(shift(x, type="lag", n=3), "\\bask_|\\basks_|\\basked_|\\basking_|\\btell_|\\btells_|\\btold_|\\btelling_") &
      d_grepl(shift(x, type="lag", n=1), "_N") &
      d_grepl(x, "\\bthat_") &
      d_grepl(shift(x, type="lead", n=1), "_RB|_XX0") &
      (d_grepl(shift(x, type="lead", n=2), "_MD|_V") |
       d_grepl(shift(x, type="lead", n=2), str_flatten(sh[c("do" , "have" , "be")], "|")))]

  x[, what_subj3 := !d_grepl(shift(x, type="lag", n=3), "\\bask_|\\basks_|\\basked_|\\basking_|\\btell_|\\btells_|\\btold_|\\btelling_") &
      d_grepl(shift(x, type="lag", n=1), "_N") &
      d_grepl(x, "\\bthat_") &
      d_grepl(shift(x, type="lead", n=1), "_RB|_XX0") &
      d_grepl(shift(x, type="lead", n=2), "_RB|_XX0") &
      (d_grepl(shift(x, type="lead", n=3), "_MD|_V") |
      d_grepl(shift(x, type="lead", n=3), str_flatten(sh[c("do" , "have" , "be")], "|")))]

  x[what_subj1 == TRUE | what_subj2 == TRUE | what_subj3 == TRUE,
    x := d_sub(x, "$", " <WHSUB>")]

  return(x$x)
}


#' @describeIn Complex MDA Tags tag WH Relative Clauses on Object Position
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

#' @describeIn Complex MDA Tags tag Hedges
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

#' @describeIn Complex MDA Tags tag Discourse Particles
dtag_disc_part<- function(x){
 disc_part <- NULL

  x <- data.table(x)
  x[, disc_part := str_detect(shift(x, type="lag", n=1), "_\\W") &
      d_grepl(x, "\\bwell_|\\bnow_|\\banyhow_|\\banyways_")]

  x[disc_part == TRUE, x := d_sub(x, "$", " <DPAR>")]

  return(x$x)
}

#' @describeIn Complex MDA Tags tag Demonstrative Pronouns
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

#' @describeIn Complex MDA Tags tag Demonstratives
dtag_demonstratives <- function(x){
 demonstratives <- NULL

  x <- data.table(x)
  x[, demonstratives := d_grepl(x, "\\bthat_|\\bthis_|\\bthese_|\\bthose_") &
      !d_grepl(x, "<DEMP>|<TOBJ>|<TSUB>|<THAC>|<THVC>|_NULL")]

  x[demonstratives == TRUE, x := d_sub(x, "$", " <DEMO>")]

  return(x$x)
}

#' @describeIn Complex MDA Tags tag Subordinator-That Deletion
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


#' @describeIn Complex MDA Tags tag Independent Clause Coordination
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
