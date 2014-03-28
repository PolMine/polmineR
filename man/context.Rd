\name{context}
\alias{context}
\title{Analyze context of a node word}
\usage{
context(query, partition, pAttribute = NULL, leftContext = NULL,
  rightContext = NULL, minSignificance = NULL, posFilter = NULL,
  filterType = NULL, verbose = TRUE, multicore = NULL)
}
\arguments{
  \item{query}{query, which may by a multi-word unit}

  \item{partition}{a partition object}

  \item{pAttribute}{p-attribute of the query}

  \item{leftContext}{no of tokens and to the left of the
  node word}

  \item{rightContext}{no of tokens to the right of the node
  word}

  \item{minSignificance}{minimum log-likelihood value}

  \item{posFilter}{character vector with the POS tags to be
  included - may not be empty!!}

  \item{filterType}{either "include" or "exclude"}

  \item{verbose}{report progress, defaults to TRUE}

  \item{multicore}{speed it up or not}
}
\description{
Retrieve the concordances of a token and calculate the
log-likelihood test for collocates
}
\details{
For formulating the query, CPQ syntax may be used (see
examples).
}
\examples{
\dontrun{
p <- partition(list(text_type="speech"), "PLPRBTTXT")
a <- context('"Integration"', p)
}
}
\author{
Andreas Blaette
}

