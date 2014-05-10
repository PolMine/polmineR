\name{zoom}
\alias{zoom}
\title{zoom into a partition}
\usage{
zoom(Partition, sAttribute, label = c(""), method = "in", tf = c("word",
  "lemma"), verbose = TRUE)
}
\arguments{
  \item{Partition}{a partition object}

  \item{sAttribute}{a list supplying further sAttributes}

  \item{label}{a label for the new partition}

  \item{method}{either "in" or "grep"}

  \item{tf}{character vector, pAttributes for which term
  frequencies shall be retrieved}

  \item{verbose}{logical, show progress report or not
  (defaults to TRUE)}
}
\description{
add a further specification of a s-attribute to an existing
partition
}

