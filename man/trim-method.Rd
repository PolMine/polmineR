% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trim.R, R/utils.R
\docType{methods}
\name{trim}
\alias{trim}
\alias{trim-method}
\alias{trim,TermDocumentMatrix-method}
\alias{trim,DocumentTermMatrix-method}
\alias{punctuation}
\title{Trim an object.}
\format{
An object of class \code{character} of length 13.
}
\usage{
trim(.Object, ...)

\S4method{trim}{TermDocumentMatrix}(
  .Object,
  terms_to_drop,
  docs_to_keep,
  min_count,
  min_doc_length,
  verbose = TRUE,
  ...
)

\S4method{trim}{DocumentTermMatrix}(
  .Object,
  terms_to_drop,
  docs_to_keep,
  min_count,
  min_doc_length,
  verbose = TRUE,
  ...
)

punctuation
}
\arguments{
\item{.Object}{The object to be trimmed}

\item{...}{further arguments}

\item{terms_to_drop}{A \code{character} vector with terms to exclude from matrix
(terms used as stopwords).}

\item{docs_to_keep}{A \code{character} vector with documents to keep.}

\item{min_count}{A \code{numeric} value with a minimum value of total term
frequency across documents to exclude rare terms from matrix.}

\item{min_doc_length}{A \code{numeric} value with minimum total of the summed-up
occurrence of tokens in a document. Exclude documents below this value and
filter out short documents. Note that the \code{min_doc_length} filter is applied
before filtering for \code{min_count} and \code{terms_to_keep}, and that these filters
will reduce document lengths.}

\item{verbose}{A \code{logical} value, whether to output progress messages.}
}
\description{
Method to trim and adjust objects by applying thresholds, minimum frequencies
etc. It can be applied to \code{context}, \code{features}, \code{context}, \code{partition} and
\code{partition_bundle} objects.
}
\examples{
use("RcppCWB", corpus = "REUTERS")
dtm <- corpus("REUTERS") \%>\%
  split(s_attribute = "id") \%>\%
  as.DocumentTermMatrix(p_attribute = "word", verbose = FALSE)
trim(dtm, min_doc_length = 100)
}
\author{
Andreas Blaette
}
\keyword{datasets}
