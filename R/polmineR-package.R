#' driller-package
#' 
#' Machinery for mining CWB corpora
#'
#' The driller-package provides functions for basic text statistics for corpora 
#' that are managed by the Corpus Workbench (CWB). A core feature is to generate
#' subcorpora/partitions based on metadata. The package is also meant to serve
#' as an interface between the CWB and R-packages implementing more
#' sophisticated statistical procedures (e.g. lsa, lda, topicmodels) or
#' providing further functionality for text mining (e.g. tm).
#'   
#' Any analysis using this package will usually start with setting up a 
#' subcorpus/partition (with \code{partition}). A set of partitions can be
#' generated with \code{partitionCluster}. Once a partition or a set of partitions
#' has been set up, core functions are \code{context} and
#' \code{keyness}. Based on a partition cluster, a
#' term-document matrix (class 'TermDocumentMatrix' from the tm package) can be
#' generated (with \code{as.TermDocumentMatrix}). This opens the door to the wealth of
#' statistical methods implemented in R.
#' @author Andreas Blaette (andreas.blaette@@uni-due.de)
#' @references http://polmine.sowi.uni-due.de
#' @keywords package
#' @docType package
#' @rdname driller-package
#' @name driller-package
NULL


#' colors associated with German parties
#' 
#' included in the package for line chart graphs
#' 
#' @name colorsParties
#' @docType data
#' @format a simple list object
#' @keywords datasets
#' data(colorsParties)
NULL

setOldClass("TermDocumentMatrix")

