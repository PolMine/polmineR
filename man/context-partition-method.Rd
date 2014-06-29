\docType{methods}
\name{context,partition-method}
\alias{as.TermContextMatrix,contextCluster-method}
\alias{as.matrix,contextCluster-method}
\alias{context}
\alias{context,contextCluster-method}
\alias{context,partition-method}
\alias{context,partitionCluster-method}
\title{Analyze context of a node word}
\usage{
\S4method{context}{partition}(object, query, pAttribute = "useControls",
  leftContext = 0, rightContext = 0, minSignificance = 0,
  posFilter = c(), filterType = "exclude", stopwords = c(),
  statisticalTest = "LL", verbose = TRUE)
}
\arguments{
  \item{object}{a partition or a partitionCluster object}

  \item{query}{query, which may by a character vector or a
  cqpQuery object}

  \item{pAttribute}{p-attribute of the query}

  \item{leftContext}{no of tokens and to the left of the
  node word}

  \item{rightContext}{no of tokens to the right of the node
  word}

  \item{minSignificance}{minimum log-likelihood value}

  \item{posFilter}{character vector with the POS tags to be
  included - may not be empty!!}

  \item{filterType}{either "include" or "exclude"}

  \item{stopwords}{exclude a query hit from analysis if
  stopword(s) is/are in context}

  \item{statisticalTest}{either "LL" (default) or "pmi"}

  \item{verbose}{report progress, defaults to TRUE}
}
\value{
depending on whether a partition or a partitionCluster
serves as input, the return will be a context object, or a
contextCluster object
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

