\docType{methods}
\name{mergeColsRegex,crosstab-method}
\alias{mergeColsRegex}
\alias{mergeColsRegex,crosstab-method}
\title{merge columns that match a regex}
\usage{
\S4method{mergeColsRegex}{crosstab}(object, regex, colname.new)
}
\arguments{
  \item{object}{the partition object}

  \item{regex}{a regular expression}

  \item{colname.new}{the colname of the merged column}
}
\value{
a crosstab object has a matrix with partition sizes,
absoute query frequencies and relative query frequencies,
just as the input
}
\description{
Merge columns of a crosstab object that match a regular
expression
}

