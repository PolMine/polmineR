#' @include context-class.R bundle-class.R
NULL

#' S4 contextCluster class
#' 
#' class to organize information of multiple context analyses
#' 
#' @slot objects Object of class \code{"list"} a list of context objects
#'
#' @section Methods:
#'   \describe{
#'     \item{show}{output of core information}
#'     \item{summary}{core statistical information}
#'     \item{[}{specific collocates}
#'     \item{[[}{specific collocates}
#'    }
#'     
#' @name contextCluster-class
#' @aliases show,contextCluster-method summary,contextCluster-method [,contextCluster-method [,contextCluster,ANY,ANY,ANY-method [[,contextCluster-method as.TermContextCluster,contextCluster-method as.TermContextMatrix
#' @docType class
#' @exportClass kwic
#' @rdname contextCluster-class
setClass("contextCluster",
         representation(
           objects="list",
           query="character",
           pAttribute="character"
         ),
         contains=c("bundle")
)

