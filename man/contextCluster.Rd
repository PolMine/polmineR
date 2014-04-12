\name{contextCluster}
\alias{as.TermContextMatrix,contextCluster-method}
\alias{as.matrix,contextCluster-method}
\alias{contextCluster}
\title{Compute a set of context analyses}
\usage{
contextCluster(query, partitionCluster, pAttribute = "useControls",
  leftContext = 0, rightContext = 0, minSignificance = -1,
  posFilter = "useControls", filterType = "useControls", verbose = TRUE)
}
\arguments{
  \item{query}{either a character vector or a cqpQuery}

  \item{partitionCluster}{a partitionCluster object}

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
}
\description{
Compute a set of context analyses
}

