\docType{methods}
\name{filterCols,crosstab-method}
\alias{filterCols}
\alias{filterCols,crosstab-method}
\alias{filterCols-crosstab-method}
\title{drop columns from a crosstab object}
\usage{
\S4method{filterCols}{crosstab}(object, filter, what = "drop")
}
\arguments{
  \item{object}{the crosstab object}

  \item{crosstab}{the crosstab object to be reworked}

  \item{filter}{a character vector with colnames}

  \item{what}{if "drop", cols is used as a stoplist, if
  "keep", itis a list with the columns to be kept}
}
\value{
you get a crosstab object with partition size, absolute and
relative frequencies
}
\description{
columns indicated in a character vector are either dropped
or maintained, depending on whether the vector is used as a
stoplist or a list of columns to be kept
}
\author{
Andreas Blaette
}

