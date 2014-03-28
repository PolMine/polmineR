\name{combineCollocates}
\alias{combineCollocates}
\title{combine statistics}
\usage{
combineCollocates(x, y, max.rank = 0, min.frequency = 0,
  token.filter = c("FOO"), pearson = FALSE)
}
\arguments{
  \item{x}{context object 1}

  \item{y}{context object 2}

  \item{max.rank}{a cutoff rank}

  \item{min.frequency}{a minimum frequency}

  \item{token.filter}{keep rows only for tokens given by a
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

