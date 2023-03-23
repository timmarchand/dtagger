#' Compare Tags
#'
#' This function compares the tags between two vectors of strings.
#' @details This function is used to compare the tagging results om two texts.
#' The texts should be reduced to two vectors in tokenised form. The function
#' produces concordance lines of the tags as chosen by regex or index position.
#' Both inputs should be exactly the same length.
#' @param vec1 A character vector of tagged text in tokenised form used for reference.
#' @param vec2 A character vector of tagged text in tokenised form to compare against vec1.
#' @param regex A regular expression of the tag to be checked in both vectors.
#' @param regex1 The regular expression of the tag to search in vector 1.
#' @param regex2 The regular expression of the tag to search in vector 2.
#' @param ... Additional arguments to be passed on to the \code{quick_conc} output.
#' @return A tibble showing the \code{quick_conc} concordance of the tags in
#' vector 1 displaid line by line in comparison to vector 2.
#' @export
compare_tags <- function(vec1, vec2, regex = NULL, regex1 = NULL, regex2 = NULL, ...){

input1 <- deparse(substitute(vec1))
input2 <- deparse(substitute(vec2))

if(!is.null(regex)){
   result1 <- quick_conc(vec1, {{regex}}) %>%
              mutate(input := input1, .before = case)

   result2<- quick_conc(vec2, {{regex}}) %>%
              mutate(input := input2, .before = case)

 result <-     bind_rows(result1,result2) %>%
              arrange(token_id)
}

if(is.null(regex)){
     result1 <- quick_conc(vec1, {{regex1}}) %>%
              mutate(input := input1, .before = case)

   result2<- quick_conc(vec2, {{regex2}}) %>%
              mutate(input := input2, .before = case)

 result <-     bind_rows(result1,result2) %>%
              arrange(token_id)

}

return(result)

}

