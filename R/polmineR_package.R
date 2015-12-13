# roxygen2::roxygenise(package.dir="/Users/blaette/Lab/github/polmineR", roclets=c("rd", "namespace"), clean=T)
# R CMD build polmineR --no-build-vignettes
# R CMD check polmineR_0.4.58.tar.gz --no-build-vignettes --no-examples --no-vignettes
NULL


#' @include generics.R session_class.R
NULL

#' @importFrom rcqp cqi_cpos2id cqi_cpos2str cqi_cpos2id cqi_cpos2struc
#' @importFrom rcqp cqi_regex2id
#' @importFrom rcqp cqi_id2str cqi_str2id cqi_id2freq cqi_id2cpos
#' @importFrom rcqp cqi_lexicon_size cqi_list_corpora cqi_attribute_size cqi_attributes
#' @importFrom rcqp cqi_struc2str cqi_struc2cpos
#' @importFrom rcqp cqi_query cqi_dump_subcorpus
#' @importFrom plyr join
NULL

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
#' @name polmineR-generics
NULL


setOldClass("TermDocumentMatrix")
setOldClass("DocumentTermMatrix")
setOldClass("igraph")
setOldClass("html")


.onLoad <- function (lib, pkg) {
  session@defaultRegistry <- Sys.getenv("CORPUS_REGISTRY")
}
