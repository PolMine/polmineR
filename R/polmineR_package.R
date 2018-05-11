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
#' Tools for mining CWB corpora.
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
#' has been set up, core functions are \code{cooccurrences} and
#' \code{features}. Based on a partition bundle, a
#' term-document matrix (class 'TermDocumentMatrix' from the tm package) can be
#' generated (with \code{as.TermDocumentMatrix}). This opens the door to the wealth of
#' statistical methods implemented in R.
#' 
#' When the package is loaded and attached, the package will look for a file name 'polmineR.conf'
#' in a directory defined by the environment variable 'POLMINER_DIR'. It will take general 
#' settings for polmineR from that file. Second, templates are restored.
#' 
#' @author Andreas Blaette (andreas.blaette@@uni-due.de)
#' @keywords package
#' @docType package
#' @rdname polmineR-package
#' @name polmineR-package
#' @references 
#' Jockers, Matthew L. (2014): \emph{Text Analysis with R for Students of Literature}.
#' Cham et al: Springer.
#' 
#' Baker, Paul (2006): \emph{Using Corpora in Discourse Analysis}. London: continuum.
#' @export polmineR
#' @importFrom RcppCWB cqp_initialize cqp_is_initialized
#' @importFrom parallel detectCores
polmineR <- function(){
  if (requireNamespace("shiny", quietly = TRUE)){
    shiny::runApp(system.file("shiny", package = "polmineR"))
  } else {
    stop("package 'shiny' required but not installed")
  }
}


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



