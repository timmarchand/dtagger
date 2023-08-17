#' Tokenize strings
#'
#' This function tokenizes a given string based on the Penn Treebank rules and
#' performs additional modifications to handle clitics (e.g., "n't", "'ll") and
#' periods (trailing full stops) in the text.
#'
#' @param x A character vector containing the text you wish to tokenize.
#' @param flatten If 'TRUE', the tokeniser flattens the result into a single string,
#' with white spaces separating tokenised items.
#' @details The function is a wrapper for the tokenizers::tokenize_ptb() function,
#' which generally works well but tends to co-join as single tokens sentence ending
#' full stops, and quotations starting with single quotation marks (apostrophes).
#'
#' It can be used as a tokenizer to prepare text for the `add_st_tags()` function,
#' in which case you should use `flatten = TRUE` argument, in combination with
#'  `add_st_tags(tokenizer = "horizontal")`:
#'
#' `dtag_tokenize(text, flatten = TRUE) |> add_st_tags(tokenizer = "horizontal")`
#'
#' Note the `add_st_tags()` function itself can also tokenize the text using the udmodel, but
#' this tends to separate hyphenated words, which might not be the desired result.
#'
#' Check the examples to compare
#'
#'
#' @return A character vector containing the tokenized text.
#'
#' @importFrom dplyr filter
#' @importFrom dplyr arrange
#' @importFrom dplyr select
#' @importFrom dplyr anti_join
#' @importFrom dplyr bind_rows
#' @importFrom tibble rowid_to_column
#' @importFrom tibble tibble
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr separate_longer_delim
#' @importFrom stringr str_detect
#' @importFrom stringr str_extract
#' @importFrom stringr str_flatten
#' @importFrom tokenizers tokenize_ptb
#'
#' @examples
#' text <- "I was given a part-time job to prepare this 'example'. I'd say it isn't conclusive..."
#' dtag_tokenize(text)
#'
#' # Compare the 'add_st_tags()' output:
#'\dontrun{
#' add_st_tags(text)dtag_tokenize(text) |> add_st_tags()
#' dtag_tokenize(text) |> add_st_tags(tokenizer = "horizontal")
#' dtag_tokenize(text, flatten = TRUE) |> add_st_tags(tokenizer = "horizontal")}
#' @export

dtag_tokenize <- function(x, flatten = FALSE){

  clitics <- c("n't", "N'T", "'ll", "'LL", "'m", "'M", "'d", "'D", "'s", "'S", "'re", "'RE", "'ve", "'VE", "''") %>%
    str_flatten("|")

  x <- tokenizers::tokenize_ptb(x) %>%
     unlist()

  df <- tibble(token = x) %>%
  rowid_to_column()



df_apos <- df %>%
   filter(str_detect(token, "'"), !str_detect(token, clitics)) %>%
  ## add @@ to all single quotation marks
  mutate(token = str_replace_all(token, "'", "@@'@@")) %>%
  ## separate longer on @@
  separate_longer_delim(token, "@@") %>%
  ## remove empty rows
  filter(token != "")



df <- df %>%
  ## remove df_apos rows from df
  anti_join(df_apos, by = join_by(rowid)) %>%
  ## join with df_apos
bind_rows(df_apos) %>%
  arrange(rowid)


 df_periods <- df %>%
  filter(str_detect(token, "[^\\.]+\\.$")) %>%
  transmute(rowid, word = str_extract(token, "\\S+(?=\\.$)"),
         dot = ".") %>%
  pivot_longer(cols = c(word, dot), values_to = "token", values_drop_na = TRUE) %>%
   select(-name)

 result <-  df %>%
  anti_join(df_periods, by = join_by(rowid)) %>%
bind_rows(df_periods) %>%
  arrange(rowid) %>%
   pull(token)


 if(flatten){result <- str_flatten(result, " ")}

 return(result)
}
