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
#' @describeIn Basic MDA Tags tag negation
dtag_negation <- function(x){
  x <- data.table(x)
  regex <- "\\bnot_RB|\\bn't_RB"
  x[d_grepl(x,regex), x:= d_sub(x,  "$", " <XX0>")]
  return(x$x)
}

#' @describeIn Basic MDA Tags tag preopositions
dtag_prepositions <- function(x){
  x <- data.table(x)
  # regex "\\bagainst_|\\bamid_|\\bamidst_|\\bamong_|\\bamongst_|\\bat_|\\bbesides_|\\bbetween_|\\bby_|\\bdespite_|\\bduring_|\\bexcept_|\\bfor_|\\bfrom_|\\bin_|\\binto_|\\bminus_|\\bnotwithstanding_|\\bof_|\\boff_|\\bon_|\\bonto_|\\bopposite_|\\bout_|\\bper_|\\bplus_|\\bpro_|\\bthan_|\\bthrough_|\\bthroughout_|\\bthru_|\\btoward_|\\btowards_|\\bupon_|\\bversus_|\\bvia_|\\bwith_|\\bwithin_|\\bwithout_"
  x[d_grepl(x,sh["preposition"]), x:= d_sub(x, "$", " <PIN>")]
  return(x$x)
}
#' @describeIn Basic MDA Tags tag indefenite pronouns
dtag_ind_pron <- function(x){
  x <- data.table(x)
  regex <- "\\banybody_|\\banyone_|\\banything_|\\beverybody_|\\beveryone_|\\beverything_|\\bnobody_|\\bnone_|\\bnothing_|\\bnowhere_|\\bsomebody_|\\bsomeone_|\\bsomething_"
  x[d_grepl(x,regex), x:= d_sub(x, "$", " <INPR>")]
  return(x$x)
}

#' @describeIn Basic MDA Tags tag quantifiers
dtag_quantifiers <- function(x){
  x <- data.table(x)
   regex <- "\\beach_|\\ball_|\\bevery_|\\bmany_|\\bmuch_|\\bfew_|\\bseveral_|\\bsome_|\\bany_"
  x[d_grepl(x,regex), x:= d_sub(x, "$", " <QUAN>")]
  return(x$x)
}

#' @describeIn Basic MDA Tags tag quantifier pronouns
dtag_quant_pron <- function(x){
  x <- data.table(x)
  regex <- "\\beverybody_|\\bsomebody_|\\banybody_|\\beveryone_|\\bsomeone_|\\banyone_|\\beverything_|\\bsomething_|\\banything_"
  x[d_grepl(x,regex), x:= d_sub(x, "$", " <QUPR>")]
  return(x$x)
}
