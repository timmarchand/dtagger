#' @title Add noun <NN> tag to all nouns
#' @description Adds the generic noun tag  <NN> to all nouns based on regex match.
#' @param x A character of tokenized strings with _ST tags.
#' @return A character vector with  all nouns tagged by <NN>.
#' @export

dtag_all_nouns <- function(x){
  regex <- "(_NN|_NNS|_NNP|_NNPS)"
  x <- data.table(x)
  x[d_grepl(x, regex)
    & !d_grepl(x,"<GER>|<NOMZ>|<QUPR>|<INPR>|<TIME>|<PLACE>)"), x:=d_sub(x, "$", " <NN>")]
  return(x$x)
}

