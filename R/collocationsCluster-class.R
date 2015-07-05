#' @include collocations-class.R bundle-class.R
NULL

#' S4 collocationsCluster class
#' 
#' class to organize information of multiple context analyses
#' 
#' @slot objects Object of class \code{"list"} a list of context objects
#'     
#' @param x a collocationsCluster object
#' @param col the column to extract
#' @param directed to be explained
#' @param rel to be explained, too
#' @param mc logical, whether to use multicore
#' @name collocationsCluster-class
#' @aliases collocationsCluster
#' @docType class
#' @exportClass kwic
#' @rdname collocationsCluster-class
setClass("collocationsCluster",
         slots=c(
           objects="list",
           pAttribute="character",
           encoding="character",
           corpus="character"
         ),
         contains=c("bundle")
)

