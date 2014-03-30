\name{frequencies}
\alias{frequencies}
\title{Frequency breakdown of the variation of query results}
\usage{
frequencies(query, Partition, pAttribute = NULL)
}
\arguments{
  \item{query}{a query, CQP syntax may be used}

  \item{Partition}{a partition object}

  \item{pAttribute}{pAttribute}
}
\description{
If one query is supplied, the function returns a frequency
breakdown of the results of the query. If several queries
are supplied, frequencies for the individual queries are
retrieved.
}

