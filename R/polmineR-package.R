#' @include generics.R
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
#' @rdname polmineR-package
#' @name polmineR-package
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
setOldClass("igraph")
setOldClass("html")
