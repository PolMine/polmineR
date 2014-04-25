\name{partitionCluster}
\alias{partitionCluster}
\title{Generate a list of partitions}
\usage{
partitionCluster(corpus, sAttributesFixed, sAttributeVar,
  sAttributeVarValues = c(), prefixLabels = c(""), encoding = NULL,
  pAttributes = c("word", "lemma"), metadata = "defined",
  sAttributesMetadata = c(), method = "grep", xml = "flat")
}
\arguments{
  \item{corpus}{the CWB corpus to be used}

  \item{sAttributesFixed}{a list with the definition of a
  partition that shall be prepared}

  \item{sAttributeVar}{character vector indicating the
  s-attribute to be variabel}

  \item{sAttributeVarValues}{character vector}

  \item{encoding}{encoding of the corpus, if not provided,
  encoding provided in the registry file will be used}

  \item{pAttributes}{the pAttributes for which term
  frequencies shall be retrieved}

  \item{metadata}{logical, whether to set up metadata}

  \item{sAttributesMetadata}{the metadata to include in the
  setup of the metadata table (set to NULL, if only
  metadata values are wanted)}

  \item{method}{either 'grep' or 'in'}

  \item{xml}{either 'flat' (default) or 'nested'}

  \item{prefixLabels}{a character vector that will serve as
  a prefix for partition labels}
}
\value{
a S3 class 'partitionCluster', which is a list with
partition objects
}
\description{
A list of partition objects with fixed s-attributes and one
variable s-attribute is generated
}
\details{
If sAttributeVarValues is not given, all values for
sAttributeVar in the partition defined by sAttributesFixed
will be retrived and used for defining the partitions.
While generally S4 methods are used in the driller package,
the return is a S3 method. The reasons is that the number
of partitions kept in the cluster is not known before the
initialization. Setting multicore to TRUE will speed up
things. Error handling is less benevolent, risk of
overheating, no verbose output.
}
\author{
Andreas Blaette
}

