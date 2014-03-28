\name{collocations}
\alias{collocations}
\title{calculate all collocations in a partition}
\usage{
collocations(partitionObject, pAttribute = "word", window = 5,
  filter = TRUE, posFilter = c("ADJA", "NN"), multicore = FALSE)
}
\arguments{
  \item{partitionObject}{a partition object}

  \item{pAttribute}{p-attribute, typically "word" or
  "token"}

  \item{window}{no of tokens to the left and to the right}

  \item{filter}{defaults to TRUE}

  \item{posFilter}{what POS to keep}

  \item{multicore}{whether to use multicore}
}
\value{
a data frame
}
\description{
the result is meant to serve as a result for an analysis of
collocation graphs
}
\author{
Andreas Blaette
}

