#' @include textstat_class.R partition_class.R polmineR_package.R
NULL

#' S4 context class
#' 
#' class to organize information of context analysis
#' 
#' @slot call Object of class \code{"character"} the call that generated the object
#' @slot partition Object of class \code{"character"} the partition the analysis is based on
#' @slot partitionSize  Object of class \code{"numeric"} the size of the partition
#' @slot leftContext  Object of class \code{"numeric"} number of tokens to the right
#' @slot rightContext  Object of class \code{"numeric"} number of tokens to the left
#' @slot pAttribute  Object of class \code{"character"} p-attribute of the query
#' @slot corpus  Object of class \code{"character"} the CWB corpus used
#' @slot stat  Object of class \code{"data.frame"} statistics of the analysis
#' @slot encoding  Object of class \code{"character"} encoding of the corpus
#' @slot posFilter  Object of class \code{"character"} part-of-speech tags filtered
#' @slot method  Object of class \code{"character"} statistical test(s) used
#' @slot cutoff  Object of class \code{"list"} cutoff levels that have been applied
#' @slot svg Object of class \code{"character"} - valid XML with svg representation
#' @aliases cooccurrencesReshaped-class
#' @rdname cooccurrencesReshaped-class
#' @docType class
#' @exportClass cooccurrencesReshaped
setClass("cooccurrencesReshaped",
         representation(
           call="character",
           partition="character",
           partitionSize="numeric",
           leftContext="numeric",
           rightContext="numeric",
           pAttribute="character",
           corpus="character",
           stat="data.frame",
           encoding="character",
           posFilter="character",
           method="character",
           cutoff="list",
           svg="character"
         ),
         contains=c("cooccurrences", "textstat")
)

#' Methods for manipulating cooccurrencesReshaped-class-objects
#' 
#' @param x cooccurrences for a corpus of interest
#' @param y cooccurrences for a reference corpus
#' @rdname cooccurrencesReshaped
#' @aliases cooccurrencesReshaped merge,cooccurrencesReshaped-method
#' @name cooccurrencesReshaped
NULL
