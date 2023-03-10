% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dtag_directory.R
\name{dtag_directory}
\alias{dtag_directory}
\title{dtag_directory}
\usage{
dtag_directory(path, n = NULL, ST = FALSE, ...)
}
\arguments{
\item{path}{A character string denoting the folder containing the target folders (at any level).}

\item{n}{An optional argument denoting the maximum number of text files to be analyzed.}

\item{ST}{Logical argument denoting whether the text files have _ST tags included already.}

\item{...}{Additional arguments to be passed on.}
}
\value{
A list of data frames containing:

\bold{corpus_dimension_scores}
\itemize{
\item corpus - name of corpus folder
\item corpus_text_type - closest text type for average corpus dimensions
\item most_common_text_type - the mode of the closest text type for the documents within the corpus folder
\item Dimension scores - calculated scores for Dimension1 ~ Dimension6
}

\bold{document_dimension_scores}
\itemize{
\item corpus - name of corpus folder
\item doc_id - name of text file
\item Dimension scores - calculated scores for Dimension1 ~ Dimension6
\item closest_text_type - closest matching text type for each doc_id, based on Biber 1989
\item dimension_tags
\item dimension - Dimension1 ~ Dimension6 from Biber 1988 for each feature
\item feature - the <MDA> tag or AWL or TTR
\item detail - brief description of the feature
\item count - number of times the feature is counted in text
\item value - in case of <MDA> tag, normailsed frequency per 100 tokens
\item z-score - value scaled to the biber_mean and biber_sd
\item d-score - same as z-score, but with the sign of negative dimension features reversed
\item biber_mean and biber_sd for each feature, based on Biber 1988
}

\bold{tokenized_tags}
\itemize{
\item corpus - name of corpus folder
\item doc_id - name of text file
\item st_tags - text tokenized on each _ST tag
\item mda_tags - text tokenized on each <MDA> tag
}

\bold{texts}
\itemize{
\item corpus - name of corpus folder
\item doc_id - name of text file
\item raw_text - untagged, flattened text for each doc_id
\item tagged_text - flattened text with _ST and <MDA> tags for each doc_id
\item wordcount - number of non-punctuation tokens found in text
}
}
\description{
Tag a directory of folders of plain text files with <MDA> tags
}
\details{
The target texts to be tagged should be placed in a directory of folders
with $$ prefixed on the folder names. The function will then read in any text
files from the target folders, and retrieve the folder names as the "corpus" variable.

If the texts have already been tagged with Stanford _ST tags, choose the option \code{ST = TRUE}.

Otherwise, the function add_st_tags() will run over the texts,
for which it is necessary to have a udpipe model loaded. See \code{\link{add_st_tags}} for details.

The function then adds multidimensional analysis <MDA> tags, and calculates Dimension scores
based on the Biber 1988 standard.

The function returns a list of tibbles including the tagged texts, individual and
corpus-level scores for each dimension of the text and word counts.
}
\examples{
\dontrun{
dtag_directory("path_to_directory")}
}
\references{
\enumerate{
\item Biber, D. (1988). Variation across Speech and Writing. Cambridge: Cambridge University Press. doi:10.1017/CBO9780511621024
\item Biber, D. (1989). A typology of English texts. , 27(1), 3-44. https://doi.org/10.1515/ling.1989.27.1.3
}
}
