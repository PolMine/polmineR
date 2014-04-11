\name{combineCollocates}
\alias{combineCollocates}
\title{combine statistics}
\usage{
combineCollocates(x, y, maxRank = 0, minFrequency = 0,
  tokenFilter = c("FOO"), pearson = FALSE)
}
\arguments{
  \item{x}{context object 1}

  \item{y}{context object 2}

  \item{maxRank}{a cutoff rank}

  \item{minFrequency}{a minimum frequency}

  \item{tokenFilter}{keep rows only for tokens given by a
  character vector}

  \item{pearson}{set to TRUE if pearsons rho shall be
  calculated, defaults to FALSE}
}
\value{
a combined data frame
}
\description{
Merge statistics for two contextual analyses, calculate
pearson rank correlation (experimental)
}
\details{
The function is intended to facilitate the exploration of
the variation of a term. Setting the max.rank needs to be
handled with care. This analysis makes only sense for
dyachronic comparisons with roughly equal frequencies.
}
\author{
Andreas Blaette
}

