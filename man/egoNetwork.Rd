\name{egoNetwork}
\alias{egoNetwork}
\title{Prepare data for an ego-network}
\usage{
egoNetwork(Partition, node, degrees, pAttribute, leftContext, rightContext,
  minSignificance, posFilter)
}
\arguments{
  \item{Partition}{a partition object}

  \item{node}{query, which may by a multi-word unit}

  \item{degrees}{the degrees of the resulting egoNetwork}

  \item{pAttribute}{pattribute of the query}

  \item{leftContext}{no of tokens and to the left of the
  node word}

  \item{rightContext}{no of tokens to the right of the node
  word}

  \item{minSignificance}{minimum log-likelihood value}

  \item{posFilter}{character vector with the POS tags to be
  included - may not be empty!!}
}
\value{
a data frame that can be turned into an igraph object with
graph.data.frame (see example)
}
\description{
For a node word, collocates of n degrees are calculated
}
\details{
The function returns a data frame that can be converted
into an igraph object easily. This conversion is not part
of the function to keep number of dependencies of the
package low.
}
\examples{
\dontrun{
 nw <- partition(list(text_year="2005", text_type="speech"), corpus="PLPRNWHTM")
 net <- egoNetwork(nw, "Integration", 1, "word", 5,5, 3.84, "NN")
 g <- graph.data.frame(net[,c(1,2,3,7)])
 tklplot(g)
 }
}
\author{
Andreas Blaette
}

