\docType{class}
\name{partitionCluster-class}
\alias{+,partitionCluster,ANY-method}
\alias{+,partitionCluster,partition-method}
\alias{+,partitionCluster,partitionCluster-method}
\alias{+,partitionCluster-method}
\alias{[,partitionCluster,ANY,ANY,ANY-method}
\alias{[,partitionCluster-method}
\alias{[[,partitionCluster-method}
\alias{addPos,partitionCluster-method}
\alias{as.DocumentTermMatrix,partitionCluster-method}
\alias{as.TermDocumentMatrix,partitionCluster-method}
\alias{as.matrix,partitionCluster-method}
\alias{as.partitionCluster,list-method}
\alias{as.sparseMatrix}
\alias{as.sparseMatrix,partitionCluster-Method}
\alias{enrich,partitionCluster-method}
\alias{merge,partitionCluster-method}
\alias{names,partitionCluster-method}
\alias{partitionCluster-class}
\alias{show,partitionCluster-method}
\alias{summary,partitionCluster-method}
\alias{tf,partitionCluster-method}
\alias{trim}
\alias{trim,partitionCluster-method}
\alias{trim-method}
\title{partitionCluster class}
\description{
A cluster of partition objects.
}
\section{Slots}{
  \describe{ \item{\code{partitions}:}{Object of class
  \code{"list"} the partitions making up the cluster }
  \item{\code{corpus}:}{Object of class \code{"character"}
  the CWB corpus the partition is based on }
  \item{\code{sAttributesFixed}:}{Object of class
  \code{"list"} fixed sAttributes }
  \item{\code{encoding}:}{Object of class
  \code{"character"} encoding of the corpus }
  \item{\code{explanation}:}{Object of class
  \code{"character"} an explanation of the partition }
  \item{\code{xml}:}{Object of class \code{"character"}
  whether the xml is flat or nested } }
}

\section{Methods}{
  \describe{ \item{show}{\code{signature(object =
  "partitionCluster")}: Display essential information }
  \item{addPos}{\code{signature(object="partitionCluster")}:
  add list with most frequent pos for a token }
  \item{tf}{\code{signature(object="partitionCluster")}:
  get term frequencies }
  \item{trim}{\code{signature(object="partitionCluster")}:
  trim a partitionCluster object } \item{[}{get frequency
  of a query} \item{[[}{get a partition within the cluster}
  \item{+}{\code{signature(object = "partitionCluster")}:
  combine two partitionClusters into a new one } }
}
\author{
Andreas Blaette
}

