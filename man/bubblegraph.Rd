\name{bubblegraph}
\alias{bubblegraph}
\title{bubblegraph visualisation}
\usage{
bubblegraph(ctab, rex = 1)
}
\arguments{
  \item{ctab}{a crosstabulation (a slot of a crosstab
  object, see example)}

  \item{rex}{a radius expansion factor to control the
  bubble size}
}
\value{
a nice plot, hopefully
}
\description{
visualise the distribution of counts/frequencies in a
crosstab object with a bubblegraph
}
\examples{
\dontrun{
bt <- partition(sAttributes=list(text_date="*"), method="grep", corpus="PLPRBTTXT", encoding="latin1")
dist <- distribution(bt, "Politik", c("text_date", "text_party"))
bubblegraph(dist@total$rel)
}
}

