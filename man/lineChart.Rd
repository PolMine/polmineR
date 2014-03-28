\name{lineChart}
\alias{lineChart}
\title{Line chart}
\usage{
lineChart(tab, xlab = "", ylab = "", main = "", rowsParties = "TRUE",
  ymax = NA)
}
\arguments{
  \item{tab}{a table to be presented}

  \item{xlab}{xlab (cp. plot)}

  \item{ylab}{ylab (cp. plot)}

  \item{main}{main title (cp.plot)}

  \item{rowsParties}{logical, either 'TRUE' or 'FALSE'}

  \item{ymax}{maximum y value for chart}
}
\value{
output is a plot
}
\description{
Draw a line chart
}
\details{
Suitable for time series data. If time series data is
provided, dates should be in columns
}
\author{
Andreas Blaette
}

