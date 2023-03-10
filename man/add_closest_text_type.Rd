% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_closest_text_type.R
\name{add_closest_text_type}
\alias{add_closest_text_type}
\title{Add closest text type}
\usage{
add_closest_text_type(data, by = c("doc_id", "corpus"))
}
\arguments{
\item{data}{A data frame with a corpus or doc_id column, and columns for Dimension scores Dimension1 ~ Dimension5.
The data frame needs to be in a list for the purrr::map function to work.}

\item{by}{The column name by which the dimensions are to be grouped.}
}
\value{
A data frame with closest_text_type column added
}
\description{
Adds a column for the closest text type based on the Euclidean distances of the five dimensions to each type
}
\examples{

# Create a data frame with the five dimensions
data <- data.frame(
  corpus = c(1, 1, 2, 2),
  doc_id = c(1, 2, 1, 2),
  Dimension1 = c(50, 60, 40, 10),
  Dimension2 = c(0, 0, 0, 0),
  Dimension3 = c(-4, -6, -4, -4),
  Dimension4 = c(3, 1, 5, 7),
  Dimension5 = c(-2, -2, -2, -2)
)

}
