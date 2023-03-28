#' Run Tukey HSD on Dimension scores
#'
#' This function performs Tukey HSD to a data frame of dimension scores where more
#' than one corpus have been anaylsed. If using the result of \code{dtag_tbl},
#' the data frame needs to be changed to a long format, e.g. by using the
#' \code{pivot_longer(tbl , contains("Dimension"), names_to = "dimension",
#' names_transform = list(dimension = as.factor))}, with an extra column to identify
#' the source corpus.
#'
#' @param df A data frame of dimension scores in long format, with a column for
#' identifying the input corpus.
#' @param dimension The dimension for the Tukey HSD test.
#' @param corpus The corpus for the Tukey HSD test.
#' @return A data frame with the Tukey HSD test results.
#' @export
d_Tukey_hsd <- function(df, dimension = "dimension", corpus = "corpus" ){

  dimension <-  {{dimension}}
  corpus <- {{corpus}}

  df %>%
  group_by(dimension) %>%
  nest() %>%
  mutate(model = map(data, ~lm(value ~ corpus, .x))) %>%
  summarise(aov = map(model, ~stats::aov(.x)),
         Tukey = map(aov, ~ stats::TukeyHSD(.x))) %>%
    mutate(Tukey = map(Tukey, ~broom::tidy(.x))) %>%
    select(dimension, Tukey) %>%
  unnest(Tukey)%>%
    select(-term)
}
