\docType{class}
\name{partitionCluster-class}
\alias{as.DocumentTermMatrix.partitionCluster}
\alias{as.TermDocumentMatrix.partitionCluster}
\alias{as.matrix.partitionCluster}
\alias{partitionCluster-class}
\alias{partitionMerge,partitionCluster-method}
\alias{partitionMerge.partitionCluster}
\alias{print.partitionCluster}
\title{S3 partitionCluster class}
\description{
it's simply a list with partition (class) objects
}
\section{Slots}{
  \describe{ \item{\code{partition}:}{Object of class
  \code{"partition"} } }
}

\section{Methods}{
  \describe{ \item{print}{get essential information about
  the partitionCluster}
  \item{partitionMerge}{(partitionCluster, label) merge the
  partitions in a partitionCluster into one partition}
  \item{as.DocumentTermMatrix}{(x, pAttribute)}
  \item{as.TermDocumentMatrix}{(x, pAttribute)}
  \item{as.matrix}{(x, pAttribute)} }
}

