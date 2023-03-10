% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dtag_ind_pron.R
\name{dtag_ind_pron}
\alias{dtag_ind_pron}
\title{Tag indefinite prononouns}
\usage{
dtag_ind_pron(x)
}
\arguments{
\item{x}{A character of tokenized strings with _ST tags.}
}
\value{
A character vector with some indefinite pronoun <INPR> tags.
}
\description{
Adds the indefinite pronoun tag  <INPR> based on a regex match.
}
