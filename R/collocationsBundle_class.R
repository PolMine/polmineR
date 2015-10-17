#' @include collocations_class.R bundle_class.R
NULL

#' S4 collocationsBundle class
#' 
#' class to organize information of multiple context analyses
#' 
#' @slot objects Object of class \code{"list"} a list of context objects
#'     
#' @param x a collocationsBundle object
#' @param col the column to extract
#' @param directed to be explained
#' @param rel to be explained, too
#' @param mc logical, whether to use multicore
#' @name collocationsBundle-class
#' @aliases collocationsBundle
#' @docType class
#' @exportClass kwic
#' @rdname collocationsBundle-class
setClass("collocationsBundle",
         slots=c(
           objects="list",
           pAttribute="character",
           encoding="character",
           corpus="character"
         ),
         contains=c("bundle")
)

