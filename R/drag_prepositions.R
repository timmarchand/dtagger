#' @title Add preposition <PIN> tag
#' @description Adds the preposition tag  <PIN> based on a regex match.
#' @param x A character of tokenized strings with _ST tags.
#' @return A character vector with some preposition <PIN> tags.
#' @export
#'
dtag_prepositions <- function(x){
  x <- data.table(x)
  # regex "\\bagainst_|\\bamid_|\\bamidst_|\\bamong_|\\bamongst_|\\bat_|\\bbesides_|\\bbetween_|\\bby_|\\bdespite_|\\bduring_|\\bexcept_|\\bfor_|\\bfrom_|\\bin_|\\binto_|\\bminus_|\\bnotwithstanding_|\\bof_|\\boff_|\\bon_|\\bonto_|\\bopposite_|\\bout_|\\bper_|\\bplus_|\\bpro_|\\bthan_|\\bthrough_|\\bthroughout_|\\bthru_|\\btoward_|\\btowards_|\\bupon_|\\bversus_|\\bvia_|\\bwith_|\\bwithin_|\\bwithout_"
  x[d_grepl(x,sh["preposition"]), x:= d_sub(x, "$", " <PIN>")]
  return(x$x)
}
