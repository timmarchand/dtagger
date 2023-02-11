#' groupedTagging
#' @param x character vector of ST tagged text, tokenized.
#'
#' @describeIn groupedTagging tag amplifiers
dtag_amplifier <- function(x){
  regex <- "\\b[Aa]bsolutely|\\b[Aa]ltogether|\\b[Cc]ompletely|\\b[Ee]normously|\\b[Ee]ntirely|\\b[Ee]xtremely|\\b[Ff]ully|\\b[Gg]reatly|\\b[Hh]ighly|\\b[Ii]ntensely|\\b[Pp]erfectly|\\b[Ss]trongly|\\b[Tt]horoughly|\\b[Tt]otally|\\b[Uu]tterly|\\b[Vv]ery_\\w+"
  x <- data.table(x)
  x[d_grepl(x, regex), x:= d_sub(x, "$", " <AMP>")]
  return(x$x)
}

#' @describeIn groupedTagging tags downtoners
dtag_downtoner <- function(x){
  regex <-" \\balmost_|\\bbarely_|\\bhardly_|\\bmerely_|\\bmildly_|\\bnearly_|\\bonly_|\\bpartially_|\\bpartly_|\\bpractically_|\\bscarcely_|\\bslightly_|\\bsomewhat_\\w+"
   x <- data.table(x)
   x[d_grepl(x, regex), x:= d_sub(x, "$", " <DWNT>")]
  return(x$x)
}

#' @describeIn groupedTagging tags nominalisations
dtag_nominalisation <- function(x){
  regex <- "(tion|tions|ment|ments|ness|nesses|ity|ities)_\\w+"
  x <- data.table(x)
  x[str_detect(x, regex), x:=str_replace(x, "$", " <NOMZ>")]
  return(x$x)
}
#' @describeIn groupedTagging tags gerunds
dtag_gerund <- function(x){
  regex <- "([^(th)]{4,}ing|\\w{4,}ings)_NN.?"
  x <- data.table(x)
  x[d_grepl(x, regex), x:=d_sub(x, "$", " <GER>")]
  return(x$x)
}

#' @describeIn groupedTagging tags for all nouns
dtag_all_nouns <- function(x){
  regex <- "(_NN|_NNS|_NNP|_NNPS)"
  x <- data.table(x)
  x[d_grepl(x, regex), x:=d_sub(x, "$", " <NN>")]
  return(x$x)
}


#' @describeIn groupedTagging tags for all attributive adjectives
dtag_all_adjectives <- function(x){
  regex <- "(_JJ.?)"
  x <- data.table(x)
  x[d_grepl(x, regex) & !d_grepl(x, "<PRED>") & !d_grepl(x, "<EMPH>")
    & !d_grepl(x, "<QUAN>") & !d_grepl(x, "<DWNT>"),
    x:=d_sub(x, "$", " <JJ>")]
  return(x$x)
}

#' @describeIn groupedTagging tags for all adverbs
dtag_all_adverbs <- function(x){
  regex <- "(_RB.?|_WRB)"
  x <- data.table(x)
  x[d_grepl(x, regex) & !d_grepl(x, "<OSUB>") & !d_grepl(x, "<EMPH>") &
      !d_grepl(x, "<xx0>") & !d_grepl(x, "<AMP>") & !d_grepl(x, "<DWNT>") &
      !d_grepl(x, "<DPAR>") &  !d_grepl(x, "<QUAN>") & !d_grepl(x, "<PLACE>") &
      !d_grepl(x, "<TIME>"),
    x:=d_sub(x, "$", " <RB>")]
  return(x$x)
}

#' @describeIn groupedTagging tags for all present tenses
dtag_present_tenses <- function(x){
  regex <- "_VBP|_VBZ"
  x <- data.table(x)
  x[d_grepl(x, regex), x:=d_sub(x, "$", " <VPRT>")]
  return(x$x)
}

#' @describeIn groupedTagging tags first person pronouns
dtag_first_person_pronoun <- function(x){
  regex <- "(\\bI_|\\b[Mm]e_|\\b[Ww]e_|\\b[Uu]s_|\\b[Mm]y_|\\b[Oo]ur_|\\b[Mm]yself_|\\b[Oo]urselves_)\\w+"
  x <- data.table(x)
  x[d_grepl_case(x, regex), x:=d_sub(x, "$", " <FPP1>")]
  return(x$x)
}

#' @describeIn groupedTagging tags second person pronouns
dtag_second_person_pronoun <- function(x){
  regex <- "(\\byou|\\byour|\\byourself|\\byourselves|\\bthy|\\bthee|\\bthyself|\\bthou)_\\w+"
  x <- data.table(x)
  x[d_grepl(x, regex), x:=d_sub(x, "$", " <SPP2>")]
  return(x$x)
}

#' @describeIn groupedTagging tags third person pronouns
dtag_third_person_pronoun <- function(x){
  regex <- "(\\bshe|\\bhe|\\bthey|\\bher|\\bhim|\\bthem|\\bhis|\\btheir|\\bimself|\\bherself|\\bthemselves)_\\w+"
  x <- data.table(x)
  x[d_grepl(x, regex), x:=d_sub(x, "$", " <TPP3>")]
  return(x$x)
}

#' @describeIn groupedTagging tags pronoun it
dtag_pronoun_it <- function(x){
  regex <- "(\\bit|\\bits|\\bitself)_\\w+"
  x <- data.table(x)
  x[d_grepl(x, regex), x:=d_sub(x, "$", " <PIT>")]
  return(x$x)
}

#' @describeIn groupedTagging tags causative because
dtag_causative <- function(x){
  x <- data.table(x)
  regex <- "(\\bbecause|\\bcos)_\\w+" # cos added for spoken
  x[d_grepl(x, regex), x:=d_sub(x, "_\\w+", "_IN <CAUS>")] # replace cos tags
  return(x$x)
}

#' @describeIn groupedTagging tags concessive subordinators
dtag_concessive <- function(x){
  x <- data.table(x)
  regex <-"(\\balthough|\\bthough|\\btho)_\\w+"
  x[d_grepl(x, regex), x:=d_sub(x, "$", " <CONC>")]
  return(x$x)
}

#' @describeIn groupedTagging tags conditional subordinators
dtag_conditional <- function(x){
  x <- data.table(x)
  regex <- "(\\bif|\\bunless)_\\w+"
  x[d_grepl(x, regex), x:=d_sub(x, "$", " <COND>")]
  return(x$x)
}

#' @describeIn groupedTagging tags possibility modals
dtag_possibility_modal <- function(x){
  x <- data.table(x)
  regex <- "(\\bcan|\\bmay|\\bmight|\\bcould|\\bca)_MD"
  x[d_grepl(x, regex), x:=d_sub(x, "$", " <POMD>")]
  return(x$x)
}

#' @describeIn groupedTagging tags necessity modals
dtag_necessity_modal <- function(x){
  x <- data.table(x)
  regex <- "(ought|should|must)_MD"
  x[d_grepl(x, regex), x:=d_sub(x, "$", " <NEMD>")]
  return(x$x)
}

#' @describeIn groupedTagging tags predictive modals
dtag_predictive_modal <- function(x){
  x <- data.table(x)
  regex <- "(\\bwill|'ll|\\bwo|\\bwould|\\bshall|\\bsha|'d)_MD"
  x[d_grepl(x,  regex), x:=d_sub(x, "$", " <PRMD>")]
  return(x$x)
}

#' @describeIn groupedTagging tags public verbs
dtag_public_verb <- function(x){
    x <- data.table(x)
  x[d_grepl(x,sh["public"]), x:= d_sub(x[d_grepl(x,sh["public"])],
                                                "$",
                                                " <PUBV>")]
  return(x$x)
}

#' @describeIn groupedTagging tags private verbs
dtag_private_verb <- function(x){
    x <- data.table(x)
  x[d_grepl(x, sh["private"]), x:= d_sub(x[d_grepl(x, sh["private"])],
                                                "$",
                                                " <PRIV>")]
  return(x$x)
}


#' @describeIn groupedTagging tags suasive verbs
dtag_suasive_verb <- function(x){
    x <- data.table(x)
  x[d_grepl(x,sh["suasive"]), x:= d_sub(x[d_grepl(x,sh["suasive"])],
                                                "$",
                                                " <SUAV>")]
  return(x$x)
}


#' @describeIn groupedTagging tags seem/appear
dtag_seem_appear <- function(x){
    x <- data.table(x)
  regex <- "(\\bseem|\\bseems|\\bseemed|\\bseeming|\\bappear|\\bappears|\\bappeared|\\bappearing)_V"
  x[d_grepl(x,regex), x:= d_sub(x[d_grepl(x,regex)],
                                                "$",
                                                " <SMP>")]
  return(x$x)
}


#' @describeIn groupedTagging tags contractions
dtag_contractions <- function(x){
    x <- data.table(x)
  regex <- "'\\w+_V|n't_RB|'ll|'d"
  x[d_grepl(x,regex), x:= d_sub(x[d_grepl(x,regex)],
                                                "$",
                                                " <CONT>")]
  return(x$x)
}
