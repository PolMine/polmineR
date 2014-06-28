\docType{methods}
\name{score}
\alias{score}
\alias{score,partitionCluster-method}
\alias{score-method}
\title{score partitions in a cluster}
\usage{
\S4method{score}{partitionCluster}(object, query, method)
}
\arguments{
  \item{object}{a partitionCluster object}

  \item{query}{query a character vector supplying tokens to
  be looked up}

  \item{method}{either "addedFreq" or "SVM"}
}
\value{
a named numeric vector (names are the labels of the
partitions in the cluster)
}
\description{
Assign scores to the partitions in a partitionCluster
object.
}

