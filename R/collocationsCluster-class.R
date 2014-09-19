#' @include collocations-class.R
NULL

#' S4 collocationsCluster class
#' 
#' class to organize information of multiple context analyses
#' 
#' @section Slots:
#'   \describe{
#'     \item{\code{contexts}:}{Object of class \code{"list"} a list of context objects }
#'   }
#'     
#' @name collocationsCluster-class
#' @aliases collocationsCluster as.TermDocumentMatrix,collocationsCluster-method
#' @docType class
#' @exportClass kwic
#' @rdname collocationsCluster-class
setClass("collocationsCluster",
         representation(
           collocations="list",
           pAttribute="character",
           encoding="character",
           corpus="character"
         )
)

