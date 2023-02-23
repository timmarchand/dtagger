#' @title Add time adverbial <TIME> tag
#' @description Adds time adverbial <TIME> based on a regex match.
#' @param x A character of tokenized strings with _ST tags.
#' @return A character vector with some time adverbial <TIME> tags.
#' @export
dtag_time_adverbials <- function(x){
time_adv1 <- time_adv2 <- NULL

  x <- data.table(x)
  x[, time_adv1 := d_grepl(x, "\\bafterwards_|\\bagain_|\\bearlier_|\\bearly_|\\beventually_|\\bformerly_|\\bimmediately_|\\binitially_|\\binstantly_|\\blate_|\\blately_|\\blater_|\\bmomentarily_|\\bnow_|\\bnowadays_|\\bonce_|\\boriginally_|\\bpresently_|\\bpreviously_|\\brecently_|\\bshortly_|\\bsimultaneously_|\\bsubsequently_|\\btoday_|\\bto-day_|\\btomorrow_|\\bto-morrow_|\\btonight_|\\bto-night_|\\byesterday_")]
  x[, time_adv2 := d_grepl(x, "\\bsoon_") & !d_grepl(shift(x, type="lag", n=1), "\\bas_")]

  x[time_adv1 == TRUE | time_adv2 == TRUE, x := d_sub(x, "$", " <TIME>")]

  return(x$x)
}
