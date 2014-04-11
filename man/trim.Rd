\name{trim}
\alias{trim}
\title{trim and filter a context object}
\usage{
trim(object, minSignificance = 0, minFrequency = 0, maxRank = 0,
  posFilter = NULL, tokenFilter = NULL)
}
\arguments{
  \item{object}{a context object to be filtered}

  \item{minSignificance}{minimum significance level}

  \item{minFrequency}{the minimum frequency}

  \item{maxRank}{maximum rank}

  \item{posFilter}{exclude words with a POS tag not in this
  list}

  \item{tokenFilter}{tokens to exclude from table}
}
\description{
Trim
}
\details{
Maybe it would be more efficient to use the subset
function.-
}
\author{
Andreas Blaette
}

