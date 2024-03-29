% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/S4classes.R, R/corpus.R, R/partition_bundle.R,
%   R/split.R
\docType{class}
\name{subcorpus_bundle-class}
\alias{subcorpus_bundle-class}
\alias{show,subcorpus_bundle-method}
\alias{merge,subcorpus_bundle-method}
\alias{merge,subcorpus-method}
\alias{split,subcorpus-method}
\alias{split,corpus-method}
\alias{split,subcorpus_bundle-method}
\title{Bundled subcorpora}
\usage{
\S4method{show}{subcorpus_bundle}(object)

\S4method{merge}{subcorpus_bundle}(x, name = "", verbose = FALSE)

\S4method{merge}{subcorpus}(x, y, ...)

\S4method{split}{subcorpus}(
  x,
  s_attribute,
  values,
  prefix = "",
  mc = getOption("polmineR.mc"),
  verbose = TRUE,
  progress = FALSE,
  type = get_type(x)
)

\S4method{split}{corpus}(
  x,
  s_attribute,
  values,
  prefix = "",
  mc = getOption("polmineR.mc"),
  verbose = TRUE,
  progress = FALSE,
  type = get_type(x),
  xml = "flat"
)

\S4method{split}{subcorpus_bundle}(
  x,
  s_attribute,
  prefix = "",
  progress = TRUE,
  mc = getOption("polmineR.mc")
)
}
\arguments{
\item{object}{An object of class \code{subcorpus_bundle}.}

\item{x}{A \code{corpus}, \code{subcorpus}, or \code{subcorpus_bundle}
object.}

\item{name}{The name of the new \code{subcorpus} object.}

\item{verbose}{Logical, whether to provide progress information.}

\item{y}{A \code{subcorpus} to be merged with \code{x}.}

\item{...}{Further \code{subcorpus} objects to be merged with \code{x} and \code{y}.}

\item{s_attribute}{The s-attribute to vary.}

\item{values}{Either a \code{character} vector with values used for splitting, or
a \code{logical} value: If \code{TRUE}, changes of s-attribute values will be the
basis for generating subcorpora. If \code{FALSE}, a new subcorpus is generated
for every struc of the s-attribute. If missing (default), \code{TRUE}/\code{FALSE} is
assigned depending on whether \code{s-attribute} has values, or not.}

\item{prefix}{A character vector that will be attached as a prefix to partition names.}

\item{mc}{Logical, whether to use multicore parallelization.}

\item{progress}{Logical, whether to show progress bar.}

\item{type}{The type of \code{partition} to generate.}

\item{xml}{A \code{logical} value.}
}
\description{
A \code{subcorpus_bundle} object combines a set of
\code{subcorpus} objects in a \code{list} in the the slot \code{objects}.
The class inherits from the \code{partition_bundle} and the \code{bundle}
class. Typically, a \code{subcorpus_bundle} is generated by applying the
\code{split}-method on a \code{corpus} or \code{subcorpus}.
}
\details{
Applying the \code{split}-method to a \code{subcorpus_bundle}-object
will iterate through the subcorpus, and apply \code{split} on each
\code{subcorpus} object in the bundle, splitting it up by the s-attribute
provided by the argument \code{s_attribute}. The return value is a
\code{subcorpus_bundle}, the names of which will be the names of the
incoming \code{partition_bundle} concatenated with the s-attribute values
used for splitting. The argument \code{prefix} can be used to achieve a
more descriptive name.
}
\examples{
corpus("REUTERS") \%>\% split(s_attribute = "id") \%>\% summary()

# Merge multiple subcorpus objects
a <- corpus("GERMAPARLMINI") \%>\% subset(date == "2009-10-27")
b <- corpus("GERMAPARLMINI") \%>\% subset(date == "2009-10-28")
c <- corpus("GERMAPARLMINI") \%>\% subset(date == "2009-11-10")
y <- merge(a, b, c)
s_attributes(y, "date")
sc <- subset("GERMAPARLMINI", date == "2009-11-11")
b <- split(sc, s_attribute = "speaker")

p <- partition("GERMAPARLMINI", date = "2009-11-11")
y <- partition_bundle(p, s_attribute = "speaker")
gparl <- corpus("GERMAPARLMINI")
b <- split(gparl, s_attribute = "date")
# split up objects in partition_bundle by using partition_bundle-method
use("polmineR")
y <- corpus("GERMAPARLMINI") \%>\%
  split(s_attribute = "date") \%>\%
  split(s_attribute = "speaker")

summary(y)
}
