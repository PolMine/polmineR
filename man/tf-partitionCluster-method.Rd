\docType{methods}
\name{tf,partitionCluster-method}
\alias{tf,partitionCluster-method}
\title{get term frequencies from a partitionCluster object}
\usage{
\S4method{tf}{partitionCluster}(object, token, pAttribute = c(),
  rel = FALSE, method = "in")
}
\arguments{
  \item{object}{a partitionCluster object}

  \item{token}{one or several tokens}

  \item{pAttribute}{the p-attribute you want to get}

  \item{rel}{logical, defaults to FALSE}

  \item{method}{either "in" or "grep"}
}
\value{
a data frame (partitions in the rows, terms in the columns)
}
\description{
Obtain term frequencies from a \code{partitionCluster}. You
get a data frame with the partitions in the object in the
rows and the different terms in the columns. It needs to be
specified whether relative or absolute frequencies are
retrieved, and what s-attribute will be queried.
}

