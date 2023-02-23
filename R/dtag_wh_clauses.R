#' @title Add Wh- clause <WHCL> tag
#' @description Adds the Wh- clause tag <WHCL> based on a regex match.
#' @param x A character of tokenized strings with _ST tags.
#' @return A character vector with some Wh- clause <WHCL> tags.
#' @export
dtag_wh_clauses <- function(x){
  wh_clauses <- NULL

    x <- data.table(x)
    x[, wh_clauses := d_grepl(shift(x, type="lag", n=1),str_flatten(sh[c("public" , "private" , "suasive")],"|")) &
        d_grepl(x,str_flatten(sh[c("wp" , "who")],"|")) &
        any(!d_grepl(shift(x, type="lead", n=1),"_MD") |
        !d_grepl(shift(x, type="lead", n=1), str_flatten(sh[c("do" , "have" , "be")],"|")))]

    x[wh_clauses == TRUE, x := d_sub(x, "$", " <WHCL>")]

    return(x$x)
}
