#' @title Add adjective <JJ> tag to all attributive adjectives
#' @description Adds the generic adjective tag  <JJ> to all attributive adjectives
#'  based on regex match.
#' @param x A character of tokenized strings with _ST tags.
#' @return A character vector with all attributive adjectives tagged by <JJ>.
#' @export
dtag_all_adjectives <- function(x){
  regex <- "(_JJ.?)"
  x <- data.table(x)
  x[d_grepl(x, regex) & !d_grepl(x, "<PRED>") & !d_grepl(x, "<EMPH>")
    & !d_grepl(x, "<QUAN>") & !d_grepl(x, "<DWNT>"),
    x:=d_sub(x, "$", " <JJ>")]
  return(x$x)
}
