#' Find Missing Tags
#'
#' This function finds the tags missing from one vector and present in another.
#' @details This function is used to compare the tagging results on the same text.
#' The texts should be reduced to two vectors in tokenised form. The function
#' produces concordance lines of where tags in vector one are missing from the same
#' index position in vector two. Both inputs should be exactly the same length.
#' @param vec1 A character vector of tagged text in tokenised form used for reference.
#' @param vec2 A character vector of tagged text in tokenised form to compare against vec1.
#' @param regex A regular expression of the tag to be checked in both vectors.
#' @param regex1 The regular expression of the tag to search in vector 1.
#' @param regex2 The regular expression of the tag to search in vector 2.
#' @param ... Additional arguments to be passed on to the \code{quick_conc} output.
#' @return A tibble showing the \code{quick_conc} concordance of the missing tags in
#' vector 2 in comparison to vector 1.
#' @export
missing_tags <- function(vec1, vec2, regex = NULL,  regex1 = NULL,  regex2 = NULL, ...){

input1 <- deparse(substitute(vec1))
input2 <- deparse(substitute(vec2))

      if(!is.null(regex)){
      index1 <- str_which(vec1, {{regex}})
      index2 <- str_which(vec2, {{regex}})

     missing1 <-  setdiff(index1,index2) %>% quick_conc(vec1, .) %>%
       mutate(input := input1, .before = case)

     missing2 <-  setdiff(index1, index2) %>% quick_conc(vec2, .)%>%
       mutate(input := input2, .before = case)


   result <-   bind_rows(missing1,missing2) %>%
       arrange(token_id)
      }

      if(is.null(regex)){
      index1 <- str_which(vec1, {{regex1}})
      index2 <- str_which(vec2, {{regex2}})

     missing1 <-  setdiff(index1,index2) %>% quick_conc(vec1, .) %>%
       mutate(input := input1, .before = case)

     missing2 <-  setdiff(index1, index2) %>% quick_conc(vec2, .)%>%
       mutate(input := input2, .before = case)


   result <-   bind_rows(missing1,missing2) %>%
       arrange(token_id)
      }
return(result)
}
