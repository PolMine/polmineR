\docType{class}
\name{partition-class}
\alias{[,partition,ANY,ANY,ANY-method}
\alias{[,partition-method}
\alias{[[,partition,ANY,ANY,ANY-method}
\alias{[[,partition-method}
\alias{addPos}
\alias{addPos,partition-method}
\alias{partition-class}
\alias{sAttributes}
\alias{sAttributes,partition-method}
\alias{show,partition-method}
\title{partition class}
\description{
In the 'driller'-package, the class 'partition' is the
basis for most other operations.
}
\section{Slots}{
  \describe{ \item{\code{label}:}{Object of class
  \code{"character"} a label that may be useful }
  \item{\code{corpus}:}{Object of class \code{"character"}
  the CWB corpus the partition is based on }
  \item{\code{encoding}:}{Object of class
  \code{"character"} encoding of the corpus }
  \item{\code{sattributes}:}{Object of class \code{"list"}
  s-attributes specifying the partition }
  \item{\code{explanation}:}{Object of class
  \code{"character"} an explanation of the partition }
  \item{\code{cpos}:}{Object of class \code{"matrix"}
  corpus positions } \item{\code{pos}:}{Object of class
  \code{"list"} with tables "abs", "rel" and "max"}
  \item{\code{size}:}{Object of class \code{"numeric"}
  total size of the partition }
  \item{\code{metadata}:}{Object of class \code{"list"}
  metadata information } \item{\code{strucs}:}{Object of
  class \code{"numeric"} the strucs defining the partition
  } \item{\code{tf}:}{Object of class \code{"list"} term
  frequencies } \item{\code{xml}:}{Object of class
  \code{"character"} whether the xml is flat or nested }
  \item{\code{sAttributeStrucs}:}{Object of class
  \code{"character"} the base node } }
}

\section{Methods}{
  \describe{ \item{show}{\code{signature(object =
  "partition")}: Display essential information }
  \item{sAttributes}{output of s-attributes in a partition}
  \item{addPos}{\code{signature(object="partition")}: add
  list with most frequent pos for a token } \item{[}{get
  concordance output} \item{[[}{to be written} }
}
\author{
Andreas Blaette
}

