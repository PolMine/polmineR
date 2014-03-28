\docType{class}
\name{concordances-class}
\alias{[,concordances,ANY,ANY,ANY-method}
\alias{[,concordances-method}
\alias{concordances-class}
\alias{show,concordances-method}
\title{concordances (S4 class)}
\description{
S4 class for organizing information for concordance output
}
\section{Slots}{
  \describe{ \item{\code{metadata}:}{Object of class
  \code{"character"} keeping the sAttributes of the
  metadata that are to be displayed }
  \item{\code{table}:}{Object of class \code{"data.frame"}
  a table with the relevant information for kwic output }
  \item{\code{collocate}:}{Object of class
  \code{"character"} collocate, if applicable }
  \item{\code{encoding}:}{Object of class
  \code{"character"} encoding of the corpus } }
}

\section{Methods}{
  \describe{ \item{[}{indexing for seeing only some
  concordances} \item{show}{get kwic output} }
}

