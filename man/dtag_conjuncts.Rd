% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dtag_conjuncts.R
\name{dtag_conjuncts}
\alias{dtag_conjuncts}
\title{Tag conjuncts <CONJ>}
\usage{
dtag_conjuncts(x)
}
\arguments{
\item{x}{A character vector of strings that have been tokenized and tagged with _ST tags.}
}
\value{
A character vector with adverbial subordinating conjunctions <CONJ> tags added.
}
\description{
This function tags conjuncts in text that has been previously _ST tagged and tokenized.
}
\details{
The function adds the <CONJ> tag to the relevant token.

In case of conjuncts that are part of a multi-word expression (e.g. in_IN addition_NN),
the function adds the <CONJ> tag to the first token, and a _NULL tag to the second
(e.g. "in_IN <CONJ>" , "addition_NULL").
}
\examples{
\dontrun{ x <- c("In_IN", "addition_NN", ",_,", "the_DT", "project_NN",
"went_VBD", "way_NN", "over_RB", "budget_NN", "._.")
dtag_conjuncts(x)
}
}
