\docType{methods}
\name{speeches}
\alias{speeches}
\alias{speeches,partition-method}
\alias{speeches-method}
\title{Split partition into speeches}
\usage{
\S4method{speeches}{partition}(object, sAttribute, gap = 500,
  exclude = NULL, addMeta = NULL, mc = FALSE, verbose = TRUE)
}
\arguments{
  \item{object}{a partition object}

  \item{sAttribute}{the s-attribute that provides the
  speaker names}

  \item{gap}{number of tokens between strucs to identify
  speeches}

  \item{exclude}{names of speakers to exclude}

  \item{addMeta}{whether to add metainformation to
  partition label, if wanted, provide s-attribute with date
  as character vector}

  \item{mc}{whether to use multicore, defaults to FALSE}

  \item{verbose}{logical, defaults to TRUE}
}
\value{
a partitionCluster object
}
\description{
This method is designed for corpora from the PolMine
corpora of plenary protocols.
}

