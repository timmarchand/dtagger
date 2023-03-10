% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/d_grepl_case.R
\name{d_grepl_case}
\alias{d_grepl_case}
\title{Pattern matching in strings}
\usage{
d_grepl_case(x, pattern, ...)
}
\arguments{
\item{x}{A character vector where pattern matching is done}

\item{pattern}{The pattern to match}

\item{...}{Additional arguments to pass to grepl()}
}
\value{
A logical vector
}
\description{
Match given pattern in provided strings using grepl for speed, by stringr format for convenience
}
\examples{
x <- c("ABC", "abC", "Bcd")
d_grepl(x, "bc")

d_grepl_case(x, "bc")
}
