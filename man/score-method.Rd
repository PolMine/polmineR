\docType{methods}
\name{score}
\alias{score}
\alias{score,partitionCluster-method}
\alias{score-method}
\title{score partitions in a cluster}
\usage{
\S4method{score}{partitionCluster}(object, token, method, pAttribute,
  rel = FALSE, tfMethod = FALSE)
}
\arguments{
  \item{object}{a partitionCluster object}

  \item{token}{a character vector supplying tokens to be
  looked up}

  \item{method}{either "additive" or "vsm"}

  \item{pAttribute}{the p-attribute the analysis is based
  on}

  \item{rel}{logical, whether to use relative frequencies}

  \item{tfMethod}{what method to use for term frequencies}
}
\value{
a named numeric vector (names are the labels of the
partitions in the cluster)
}
\description{
Assign scores to the partitions in a partitionCluster
object.
}

