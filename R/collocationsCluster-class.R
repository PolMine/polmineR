#' @include collocations-class.R bundle-class.R
NULL

#' S4 collocationsCluster class
#' 
#' class to organize information of multiple context analyses
#' 
#' @slot objects Object of class \code{"list"} a list of context objects
#'     
#' @name collocationsCluster-class
#' @aliases collocationsCluster as.TermDocumentMatrix,collocationsCluster-method
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

