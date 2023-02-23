#' @title Add adverb <RB> tag to all adverbs
#' @description Adds the generic adverb tag  <RB> to all adverbs not included in
#' other categories.
#' @details The functions adds the <RB> tag to all adverbs except the following:
#' * Amplifiers <AMP>
#' * Discourse particles <DPAR>
#' * Downtoners <DWNT>
#' * Emphatis <EMPH>
#' * Subordinating conjunctions <OSUB>
#' * Place adverbials <PLACE>
#' * Quantifiers <QUAN>
#' * Time adverbials <TIME>
#' * Not <XX0>
#' @param x A character of tokenized strings with _ST tags.
#' @return A character vector with some adverbs tagged by <RB>.
#' @export
dtag_all_adverbs <- function(x){
  regex <- "(_RB.?|_WRB)"
  x <- data.table(x)
  x[d_grepl(x, regex) & !d_grepl(x, "<OSUB>") & !d_grepl(x, "<EMPH>") &
      !d_grepl(x, "<XX0>") & !d_grepl(x, "<AMP>") & !d_grepl(x, "<DWNT>") &
      !d_grepl(x, "<DPAR>") &  !d_grepl(x, "<QUAN>") & !d_grepl(x, "<PLACE>") &
      !d_grepl(x, "<TIME>"),
    x:=d_sub(x, "$", " <RB>")]
  return(x$x)
}
