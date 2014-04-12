\name{adjust}
\alias{adjust}
\title{perform addPos and further adjustments for all partitions in a cluster}
\usage{
adjust(cluster, pos = c(""), posFilter = c(), minFrequency = 0)
}
\arguments{
  \item{cluster}{a partitionCluster object}

  \item{pos}{character vector: "word", "lemma", or both}

  \item{posFilter}{pos to keep}

  \item{minFrequency}{minimum frequency of tokens}
}
\description{
perform addPos and further adjustments for all partitions
in a cluster
}

