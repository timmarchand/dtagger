% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dtag_amplifier.R
\name{dtag_amplifier}
\alias{dtag_amplifier}
\title{Tag amplifiers}
\usage{
dtag_amplifier(x)
}
\arguments{
\item{x}{A character of tokenized strings with _ST tags.}
}
\value{
A character vector with some amplifier <AMP> tags.
}
\description{
Adds the amplifier tag  <AMP> based on a regex match.
}
