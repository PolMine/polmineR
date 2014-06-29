\docType{class}
\name{keyness-class}
\alias{addPos,keyness-method}
\alias{as.matrix,keynessCluster-method}
\alias{keyness}
\alias{keyness,partitionCluster-method}
\alias{keyness-class}
\alias{show,keyness-method}
\alias{summary,keyness-method}
\title{S4 class for comparing corpora}
\description{
to keep results from a keyness analysis
}
\section{Objects from the class}{
  keyness objects are returned by the function call
  \code{keyness}
}

\section{Slots}{
  \describe{ \item{\code{corpus}:}{Object of class
  \code{"character"} ~~ } \item{\code{pattribute}:}{Object
  of class \code{"character"} ~~ }
  \item{\code{encoding}:}{Object of class
  \code{"character"} ~~ } \item{\code{corpus}:}{Object of
  class \code{"character"} ~~ } \item{\code{stat}:}{Object
  of class \code{"data.frame"} ~~ }
  \item{\code{statisticalTest}:}{Object of class
  \code{"character"} statisticalTest used }
  \item{\code{statisticalSummary}:}{Object of class
  \code{"data.frame"} statistical summary } }
}

\section{Methods}{
  \describe{ \item{summary}{\code{signature(object =
  "keyness")}: Display essential information }
  \item{addPos}{\code{signature(object = "keyness")}: add
  POS attribute to statistics table } }
}
\author{
Andreas Blaette
}

