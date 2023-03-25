#' @title Add closest text type
#'
#' @description Adds a column for the closest text type based on the Euclidean distances of the five dimensions to each type
#'
#' @param data A data frame with a corpus or doc_id column, and columns for Dimension scores Dimension1 ~ Dimension5.
#' The data frame needs to be in a list for the purrr::map function to work.
#' @param by The column name by which the dimensions are to be grouped.
#'
#' @return A data frame with closest_text_type column added
#' @export
#' @examples
#'
#' # Create a data frame with the five dimensions
#' data <- data.frame(
#'   corpus = c(1, 1, 2, 2),
#'   doc_id = c(1, 2, 1, 2),
#'   Dimension1 = c(50, 60, 40, 10),
#'   Dimension2 = c(0, 0, 0, 0),
#'   Dimension3 = c(-4, -6, -4, -4),
#'   Dimension4 = c(3, 1, 5, 7),
#'   Dimension5 = c(-2, -2, -2, -2)
#' )
#'
add_closest_text_type <- function(data, by = c("doc_id","corpus")){
  map({{data}}, ~.x %>%
mutate("Intimate interpersonal interaction" = sqrt(((Dimension1- 45) ^ 2) + ((Dimension2- -1) ^ 2) + ((Dimension3- -6) ^ 2) + ((Dimension4- 1) ^ 2) + ((Dimension5- -4) ^ 2)),
  "Informational interaction" = sqrt(((Dimension1- 30) ^ 2) + ((Dimension2- -1) ^ 2) + ((Dimension3- -4) ^ 2) + ((Dimension4- 1) ^ 2) + ((Dimension5- -3) ^ 2)),
  "Scientific exposition" = sqrt(((Dimension1- -15) ^ 2) + ((Dimension2- -2.5) ^ 2) + ((Dimension3- 4) ^ 2) + ((Dimension4- -2) ^ 2) + ((Dimension5- 9) ^ 2)),
  "Learned exposition" = sqrt(((Dimension1- -20) ^ 2) + ((Dimension2- -2) ^ 2) + ((Dimension3- 5) ^ 2) + ((Dimension4- -3) ^ 2) + ((Dimension5- 2) ^ 2)),
  "Imaginative narrative" = sqrt(((Dimension1- 5) ^ 2) + ((Dimension2- 7) ^ 2) + ((Dimension3- -4) ^ 2) + ((Dimension4- 1) ^ 2) + ((Dimension5- -2) ^ 2)),
  "General narrative exposition" = sqrt(((Dimension1- -10) ^ 2) + ((Dimension2- 2) ^ 2) + ((Dimension3- 0) ^ 2) + ((Dimension4- -1) ^ 2) + ((Dimension5- 0) ^ 2)),
  "Situated reportage" = sqrt(((Dimension1- 0) ^ 2) + ((Dimension2- -3) ^ 2) + ((Dimension3- -13) ^ 2) + ((Dimension4- -4.5) ^ 2) + ((Dimension5- -3) ^ 2)),
  "Involved persuasion" = sqrt(((Dimension1- 5) ^ 2) + ((Dimension2- -2) ^ 2 + ((Dimension3- 2) ^ 2) + ((Dimension4- -4) ^ 2) + ((Dimension5- -1) ^ 2)))) %>%
 pivot_longer(cols = contains(" "), names_to = "closest_text_type") %>%
  slice(which.min(value), .by = {{by}})%>%
  select(-value))
}
