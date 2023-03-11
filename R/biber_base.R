#' @title Biber Base
#'
#' @description  Baseline statistics for calculating Dimension scores based on Biber (1988).
#' @details The data lists all the features used (plus some others), and includes the mean and sd values for these features as a base for the calculation of Z-scores.
#'
#' @format A tibble with 72 rows and 6 variables:
#' \itemize{
#' \item(dimension){Dimension to which each feature belongs. Features not used in the original study are categorised as Others}
#' \item(feature){Tagged features in the text and also Average Word Length <AWL> and Type Token Ratio <TTR>}
#' \item(detail){Simple description of the feature}
#' \item(biber_mean){Mean value for the feature found in Biber 1988}
#' \item(biber_sd){Standard deviation of the feature found in Biber 1988}
#' \item(loading){Whether the feature has a positive or negative loading in the MDA}
#' }
"biber_base"
