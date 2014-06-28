\docType{methods}
\name{enrich}
\alias{enrich}
\alias{enrich-method}
\title{enrich an object}
\usage{
enrich(object, ...)
}
\arguments{
  \item{object}{a partition, partitionCluster or keyness
  object}

  \item{...}{further parameters}
}
\description{
Method to fill slots of a partition, partitionCluster or
keyness object that have not been set up previously. See
\code{method?enrich("partition")} for applying the method
on partition or partitionCluster objects, or
\code{method?enrich("keyness")}).
}

