#' @title basicTagging
#'
#' A collection of functions to add basic <MDA> tags to a tokenized vector of ST- tagged strings
#'
#' This collection of tags do SOMETHING
#' #' basicTagging
#' @param x character vector of ST tagged text tokenized
#'
#'
#'

#' @describeIn basicTagging tag negation
dtag_negation <- function(x){
  x <- data.table(x)
  regex <- "\\bnot_RB|\\bn't_RB"
  x[d_grepl(x,regex), x:= d_sub(x,  "$", " <XX0>")]
  return(x$x)
}

#' @describeIn basicTagging tag preopositions
dtag_prepositions <- function(x){
  x <- data.table(x)
  # regex "\\bagainst_|\\bamid_|\\bamidst_|\\bamong_|\\bamongst_|\\bat_|\\bbesides_|\\bbetween_|\\bby_|\\bdespite_|\\bduring_|\\bexcept_|\\bfor_|\\bfrom_|\\bin_|\\binto_|\\bminus_|\\bnotwithstanding_|\\bof_|\\boff_|\\bon_|\\bonto_|\\bopposite_|\\bout_|\\bper_|\\bplus_|\\bpro_|\\bthan_|\\bthrough_|\\bthroughout_|\\bthru_|\\btoward_|\\btowards_|\\bupon_|\\bversus_|\\bvia_|\\bwith_|\\bwithin_|\\bwithout_"
  x[d_grepl(x,sh["preposition"]), x:= d_sub(x, "$", " <PIN>")]
  return(x$x)
}
#' @describeIn basicTagging tag indefenite pronouns
dtag_ind_pron <- function(x){
  x <- data.table(x)
  regex <- "\\banybody_|\\banyone_|\\banything_|\\beverybody_|\\beveryone_|\\beverything_|\\bnobody_|\\bnone_|\\bnothing_|\\bnowhere_|\\bsomebody_|\\bsomeone_|\\bsomething_"
  x[d_grepl(x,regex), x:= d_sub(x, "$", " <INPR>")]
  return(x$x)
}

#' @describeIn basicTagging tag quantifiers
dtag_quantifiers <- function(x){
  x <- data.table(x)
  # regex <- "\\beach_|\\ball_|\\bevery_|\\bmany_|\\bmuch_|\\bfew_|\\bseveral_|\\bsome_|\\bany_"
  x[d_grepl(x,sh["quantifiers"]), x:= d_sub(x, "$", " <QUAN>")]
  return(x$x)
}

#' @describeIn basicTagging tag quantifier pronouns
dtag_quant_pron <- function(x){
  x <- data.table(x)
  regex <- "\\beverybody_|\\bsomebody_|\\banybody_|\\beveryone_|\\bsomeone_|\\banyone_|\\beverything_|\\bsomething_|\\banything_"
  x[d_grepl(x,regex), x:= d_sub(x, "$", " <QUPR>")]
  return(x$x)
}
