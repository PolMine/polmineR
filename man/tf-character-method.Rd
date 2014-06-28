\docType{methods}
\name{tf,character-method}
\alias{tf,character-method}
\title{get term frequencies from a corpus}
\usage{
\S4method{tf}{character}(object, token, pAttribute, method = "in")
}
\arguments{
  \item{object}{a character string specifying a CWB corpus}

  \item{token}{one or several tokens}

  \item{pAttribute}{the p-attribute you want to get}

  \item{method}{either "in" or "cqp" (cqp method is not yet
  implemented)}
}
\value{
a data frame (partitions in the rows, terms in the columns)
}
\description{
Obtain term frequencies from a CWB corpus. You get a data
frame with the partitions in the object in the rows and the
different terms in the columns.
}
\examples{
tf("PLPRTXT", c("machen", "Integration"), "word")
}

