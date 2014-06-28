\name{dispersion}
\alias{dispersion}
\title{Dispersion of a query or multiple queries}
\usage{
dispersion(partition, query, dim, pAttribute = drillingControls$pAttribute)
}
\arguments{
  \item{partition}{a partition object that will be queried}

  \item{query}{a character vector containing one or
  multiple queries}

  \item{dim}{a character vector of length 1 or 2 providing
  the sAttributes}

  \item{pAttribute}{the p-attribute that will be looked up,
  typically 'word' or 'lemma'}
}
\value{
depends on the input, as this is a wrapper function
}
\description{
The function returns the frequencies of a query or a
multiple queries in sub-partitions defined by one or two
dimensions. This is a wrapper function, so the output will
depend on the number of queries and dimensions provided.
Note that metadata need to be set up for the partition.
}
\examples{
\dontrun{
bt <- partition("PLPRBTTXT", tf=FALSE) # term frequencies are not needed
dispersion(plpr, "Vorsorge", "text_year")
}
}
\author{
Andreas Blaette
}

