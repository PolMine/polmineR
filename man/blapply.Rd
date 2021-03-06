% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/blapply.R
\name{blapply}
\alias{blapply}
\alias{blapply,list-method}
\alias{blapply,vector-method}
\alias{blapply,bundle-method}
\title{apply a function over a list or bundle}
\usage{
blapply(x, ...)

\S4method{blapply}{list}(x, f, mc = TRUE, progress = TRUE, verbose = FALSE, ...)

\S4method{blapply}{vector}(x, f, mc = FALSE, progress = TRUE, verbose = FALSE, ...)

\S4method{blapply}{bundle}(x, f, mc = FALSE, progress = TRUE, verbose = FALSE, ...)
}
\arguments{
\item{x}{a list or a bundle object}

\item{...}{further parameters}

\item{f}{a function that can be applied to each object contained in the
bundle, note that it should swallow the parameters mc, verbose and progress
(use ... to catch these params )}

\item{mc}{logical, whether to use multicore - if TRUE, the number of cores
will be taken from the polmineR-options}

\item{progress}{logical, whether to display progress bar}

\item{verbose}{logical, whether to print intermediate messages}
}
\description{
Very similar to lapply, but applicable to bundle-objects, in particular.
The purpose of the method is to supply a uniform und convenient parallel
backend for the polmineR package. In particular, progress bars are supported
(the naming of the method is derived from bla bla).
}
\examples{
use("polmineR")
bt <- partition("GERMAPARLMINI", date = ".*", regex=TRUE)
speeches <- as.speeches(bt, s_attribute_date = "date", s_attribute_name = "speaker")
foo <- blapply(speeches, function(x, ...) slot(x, "cpos"))
}
