% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dtag_split_infinitives.R
\name{dtag_split_infinitives}
\alias{dtag_split_infinitives}
\title{Add split infinitive <SPIN> tag}
\usage{
dtag_split_infinitives(x)
}
\arguments{
\item{x}{A character of tokenized strings with _ST tags.}
}
\value{
A character vector with some split infinitive  <SPIN> tags.
}
\description{
Adds split infinitive <SPIN> based on a regex match.
}
