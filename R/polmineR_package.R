#' @include generics.R 
NULL


#' @importFrom graphics dotchart grid par plot points rect text
#' @importFrom stats formula ftable setNames xtabs
#' @importFrom utils View browseURL download.file setTxtProgressBar txtProgressBar untar write.csv
#' @importFrom tm TermDocumentMatrix DocumentTermMatrix
#' @importFrom tm as.TermDocumentMatrix as.DocumentTermMatrix
NULL

#' @importFrom Matrix rowSums colSums
NULL

#' polmineR-package
#' 
#' Machinery for mining CWB corpora
#'
#' The package provides functions for basic text statistics for corpora 
#' that are managed by the Corpus Workbench (CWB). A core feature is to generate
#' subcorpora/partitions based on metadata. The package is also meant to serve
#' as an interface between the CWB and R-packages implementing more
#' sophisticated statistical procedures (e.g. lsa, lda, topicmodels) or
#' providing further functionality for text mining (e.g. tm).
#'   
#' Any analysis using this package will usually start with setting up a 
#' subcorpus/partition (with \code{partition}). A set of partitions can be
#' generated with \code{partitionBundle}. Once a partition or a set of partitions
#' has been set up, core functions are \code{context} and
#' \code{compare}. Based on a partition bundle, a
#' term-document matrix (class 'TermDocumentMatrix' from the tm package) can be
#' generated (with \code{as.TermDocumentMatrix}). This opens the door to the wealth of
#' statistical methods implemented in R.
#' @author Andreas Blaette (andreas.blaette@@uni-due.de)
#' @references http://polmine.sowi.uni-due.de
#' @keywords package
#' @docType package
#' @rdname polmineR-package
#' @name polmineR-package
NULL


#' generic methods defined in the polmineR-package
#' 
#' The package defines a set of generic functions. This doc file
#' only provides an overview. Please consult the documentation
#' of the classes to learn which methods can be applied to a class
#' of a certain type.
#' 
#' @rdname polmineR-generics
#' @param x object
#' @param value value
#' @name polmineR-generics
NULL


# setOldClass("dfmSparse") # class defined in quanteda-package
setOldClass("igraph")
setOldClass("html")





#' @export polmineR
#' @rdname polmineR-package
polmineR <- function(){
  # shiny::runApp("/Users/blaette/Lab/github/polmineR/inst/shiny/gui")
  shiny::runApp(system.file("shiny", "gui", package="polmineR"))
}