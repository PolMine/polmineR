\docType{methods}
\name{trim,partitionCluster-method}
\alias{trim,partitionCluster-method}
\title{trim partitionCluster Object}
\usage{
\S4method{trim}{partitionCluster}(object, pAttribute, minFrequency = 0,
  posFilter = c(), drop = NULL, minSize = 0, ...)
}
\arguments{
  \item{object}{a partitionCluster object}

  \item{pAttribute}{character vector: "word", "lemma", or
  both}

  \item{minFrequency}{minimum frequency of tokens}

  \item{posFilter}{pos to keep}

  \item{drop}{partitionObjects you want to drop, specified
  either by number or by label}

  \item{minSize}{a minimum size for the partitions to be
  kept}

  \item{...}{further arguments (unused)}
}
\value{
partitionCluster
}
\description{
Apply the trim method to all partitions in a
partitionCluster. In addition, partitions can be dropped.
}

