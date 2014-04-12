\docType{methods}
\name{mergeCols,crosstab-method}
\alias{mergeCols}
\alias{mergeCols,crosstab-method}
\alias{mergeCols-crosstab-method}
\title{merge two columns in a crosstab object}
\usage{
\S4method{mergeCols}{crosstab}(object, colnameOld1, colnameOld2, colnameNew)
}
\arguments{
  \item{object}{the partition object}

  \item{colnameOld1}{the colname of the first column to be
  merged}

  \item{colnameOld2}{the colname of the second column to be
  merged}

  \item{colnameNew}{the colname of the merged column}
}
\value{
the returned crosstab object has a matrix with partition
sizes, absoute query frequencies and relative query
frequencies, just as the input
}
\description{
The columns with absolute frequencies (partition size,
frequencies for a query) are merged, and the relative
frequencies are recalculated.
}
\author{
Andreas Blaette
}

