#' @title Add pied piper relative clause <PIRE> tag
#' @description Adds the pied piper relative clausetag <PIRE> based on a regex match.
#' @param x A character of tokenized strings with _ST tags.
#' @return A character vector with some pied piper relative clause <PIRE> tags.
#' @export
dtag_pp_rel_clauses <- function(x){
  pp_rel_clauses <- NULL
  x <- data.table(x)

  x[,pp_rel_clauses := d_grepl(x, "<PIN>") &
  d_grepl(shift(x, type="lead", n=1), "\\bwho_|\\bwhom_|\\bwhose_|\\bwhich_")]

  x[pp_rel_clauses == TRUE, x := d_sub(x,"$"," <PIRE>")]
}
