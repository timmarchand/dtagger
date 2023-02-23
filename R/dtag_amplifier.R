#' @title Tag amplifiers
#' @description Adds the amplifier tag  <AMP> based on a regex match.
#' @param x A character of tokenized strings with _ST tags.
#' @return A character vector with some amplifier <AMP> tags.
#' @export
#'
dtag_amplifier <- function(x){
  regex <- "\\b[Aa]bsolutely|\\b[Aa]ltogether|\\b[Cc]ompletely|\\b[Ee]normously|\\b[Ee]ntirely|\\b[Ee]xtremely|\\b[Ff]ully|\\b[Gg]reatly|\\b[Hh]ighly|\\b[Ii]ntensely|\\b[Pp]erfectly|\\b[Ss]trongly|\\b[Tt]horoughly|\\b[Tt]otally|\\b[Uu]tterly|\\b[Vv]ery_\\w+"
  x <- data.table(x)
  x[d_grepl(x, regex), x:= d_sub(x, "$", " <AMP>")]
  return(x$x)
}

