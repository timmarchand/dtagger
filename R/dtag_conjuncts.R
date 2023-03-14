#' @title Tag conjuncts <CONJ>
#'
#' @description This function tags conjuncts in text that has been previously _ST tagged and tokenized.
#' @details The function adds the <CONJ> tag to the relevant token.
#'
#' In case of conjuncts that are part of a multi-word expression (e.g. in_IN addition_NN),
#' the function adds the <CONJ> tag to the first token, and a _NULL tag to the second
#' (e.g. "in_IN <CONJ>" , "addition_NULL").
#'
#' @param x A character vector of strings that have been tokenized and tagged with _ST tags.
#'
#' @return A character vector with adverbial subordinating conjunctions <CONJ> tags added.
#'
#' @examples
#' \dontrun{ x <- c("In_IN", "addition_NN", ",_,", "the_DT", "project_NN",
#' "went_VBD", "way_NN", "over_RB", "budget_NN", "._.")
#' dtag_conjuncts(x)
#' }
#' @export
dtag_conjuncts <- function(x){

 x <- data.table(x)

  x[, conjuncts1 := d_grepl(x, "\\belse_|\\baltogether_|\\brather_") &
      str_detect(shift(x, type="lag", n=1), "_\\W")]

  x[, conjuncts2 := d_grepl(x, "\\balternatively_|\\bconsequently_|\\bconversely_|\\beg_|\\be\\.g\\._|\\bfurthermore_|\\bhence_|\\bhowever_|\\bi\\.e\\._|\\binstead_|\\blikewise_|\\bmoreover_|\\bnamely_|\\bnevertheless_|\\bnonetheless_|\\bnotwithstanding_|\\botherwise_|\\bsimilarly_|\\btherefore_|\\bthus_|\\bviz\\.")

  x[, conjuncts3 := d_grepl(x, "\\bin_") &
      d_grepl(shift(x, type="lead", n=1), "\\bcomparison_|\\bcontrast_|\\bparticular_|\\baddition_|\\bconclusion_|\\bconsequence_|\\bsum_|\\bsummary_")]
  x[, null3 := d_grepl(shift(x, type= "lag", n=1), "\\bin_") &
      d_grepl(x, "\\bcomparison_|\\bcontrast_|\\bparticular_|\\baddition_|\\bconclusion_|\\bconsequence_|\\bsum_|\\bsummary_")]

  x[, conjuncts4 := d_grepl(x, "\\bfor_") &
      d_grepl(shift(x, type="lead", n=1), "\\bexample_|\\binstance_")]
  x[, null4 := d_grepl(shift(x, type= "lag", n=1), "\\bfor_") &
      d_grepl(x, "\\bexample_|\\binstance_")]

  x[, conjuncts5 := d_grepl(x, "\\binstead_") &
      d_grepl(shift(x, type="lead", n=1), "\\bof_")]
  x[, null5 := d_grepl(shift(x, type= "lag", n=1), "\\binstead_") &
      d_grepl(x, "\\bof_")]

  x[, conjuncts6 := d_grepl(x, "\\bby_") &
      d_grepl(shift(x, type="lead", n=1), "\\bcontrast_|\\bcomparison_")]
  x[, null6 := d_grepl(shift(x, type = "lag", n=1), "\\bby_") &
      d_grepl(x, "\\bcontrast_|\\bcomparison_")]

  x[, conjuncts7 := d_grepl(x, "\\bin_") &
      d_grepl(shift(x, type="lead", n=1), "\\bany_") &
      d_grepl(shift(x, type="lead", n=2), "\\bevent_|\\bcase_")]
  x[, null7 := d_grepl(shift(x, type = "lag", n=1), "\\bin_") &
      d_grepl(x, "\\bany_") &
      d_grepl(shift(x, type="lead", n=1), "\\bevent_|\\bcase_")]

  x[, conjuncts8 := d_grepl(x, "\\bin_") &
      d_grepl(shift(x, type="lead", n=1), "\\bother_") &
      d_grepl(shift(x, type="lead", n=2), "\\bwords_")]
  x[, null8 := d_grepl(shift(x, type = "lag", n=1), "\\bin_") &
      d_grepl(x, "\\bother_") &
      d_grepl(shift(x, type="lead", n=1), "\\bwords_")]
  x[, null8a := d_grepl(shift(x, type = "lag", n=2), "\\bin_") &
      d_grepl(shift(x, type="lag", n=1), "\\bother_") &
      d_grepl(x, "\\bwords_")]

  x[, conjuncts9 := d_grepl(x, "\\bas_") &
      d_grepl(shift(x, type="lead", n=1), "\\ba_") &
      d_grepl(shift(x, type="lead", n=2), "\\bresult_|\\bconsequence_")]
  x[, null9 := d_grepl(shift(x, type = "lag", n=1), "\\bas_") &
      d_grepl(x, "\\ba_") &
      d_grepl(shift(x, type="lead", n=1), "\\bresult_|\\bconsequence_")]
  x[, null9a := d_grepl(shift(x, type = "lag", n=2), "\\bas_") &
      d_grepl(shift(x, type = "lag", n=1), "\\ba_") &
      d_grepl(x, "\\bresult_|\\bconsequence_")]

  x[, conjuncts10 := d_grepl(x, "\\bon_") &
      d_grepl(shift(x, type = "lead", n=1), "\\bthe_") &
      d_grepl(shift(x, type = "lead", n=2), "\\bother_") &
      d_grepl(shift(x, type = "lead", n=3), "\\bhand_")]
  x[, null10 := d_grepl(shift(x, type = "lag", n=1), "\\bon_") &
      d_grepl(x, "\\bthe_") &
      d_grepl(shift(x, type = "lead", n=1), "\\bother_") &
      d_grepl(shift(x, type = "lead", n=2), "\\bhand_")]
  x[, null10a := d_grepl(shift(x, type = "lag", n=2), "\\bon_") &
      d_grepl(shift(x, type = "lag", n=1), "\\bthe_") &
      d_grepl(x, "\\bother_") &
      d_grepl(shift(x, type = "lead", n=1), "\\bhand_")]
  x[, null10b := d_grepl(shift(x, type = "lag", n=3), "\\bon_") &
      d_grepl(shift(x, type = "lag", n=2), "\\bthe_") &
      d_grepl(shift(x, type = "lag", n=1), "\\bother_") &
      d_grepl(x, "\\bhand_")]

  x[conjuncts1 == TRUE | conjuncts2 == TRUE | conjuncts3 == TRUE | conjuncts4 == TRUE |
    conjuncts5 == TRUE | conjuncts6 == TRUE | conjuncts7 == TRUE | conjuncts8 == TRUE |
    conjuncts9 == TRUE | conjuncts10 == TRUE,
    x := d_sub(x, "$", " <CONJ>")]
  x[null3 == TRUE | null4 == TRUE | null4 == TRUE |   null5 == TRUE | null6 == TRUE |
    null7 == TRUE | null8 == TRUE | null8a == TRUE |  null9 == TRUE | null9a == TRUE |
    null10 == TRUE | null10a == TRUE | null10b == TRUE,
    x := d_sub(x, "_\\w+", "_NULL")]

  return(x$x)
  }
