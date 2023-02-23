#' @title Add WH- question <WHQU> tag
#' @description Adds the WH- question tag <WHQU> based on a regex match.
#' @param x A character of tokenized strings with _ST tags.
#' @return A character vector with some WH- question <WHQU> tags.
#' @export
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
