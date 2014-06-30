\docType{methods}
\name{tf,partition-method}
\alias{tf,partition-method}
\title{get term frequencies from a partition object}
\usage{
\S4method{tf}{partition}(object, token, method = "in", pAttribute = NULL)
}
\arguments{
  \item{object}{a partition object}

  \item{token}{a character vector (one or multiple terms to
  be looked up)}

  \item{method}{either "in", "grep" or "cqp" (defaults to
  "in")}

  \item{pAttribute}{if NULL, the pAttributes available in
  the partition object will be reported}
}
\value{
a data frame
}
\description{
Obtain term frequencies for one or several terms from a
partition object.
}
\details{
The term frequencies are retrieved for all the p-attributes
that are available in the partition object. To get the
total frequency of the variations of a term, use the
"grep"-method.
}
\examples{
# generate a partition for testing
test <- partition("PLPRBTTXT", list(text_date=".*"), tf=c("word", "lemma"), method="grep")
tf(test, "Wir") # get frequencies for one token
tf(test, c("Wir", "lassen", "uns")) # get frequencies for multiple tokens
tf(test, c("Zuwander.*", "Integration.*"), method="grep") # get frequencies using "grep"-method
}

