\name{kwic}
\alias{kwic}
\title{KWIC output}
\usage{
kwic(ctxt, metadata = drillingControls$kwicMetadata, collocate = c())
}
\arguments{
  \item{ctxt}{a context object}

  \item{metadata}{character vector with the metadata
  included in output}

  \item{collocate}{limit output to a concordances
  containing a specific collocate}
}
\value{
a concordances object
}
\description{
Based on a context object, you get concordances, i.e. the
context of a keyword
}
\details{
This functiongives you quite some flexibility to adjust the
output to your needs. Use drillingControls to adjust
output.
}
\author{
Andreas Blaette
}

