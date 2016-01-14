#' @include context_class.R bundle_class.R
NULL

#' S4 contextBundle class
#' 
#' class to organize information of multiple context analyses
#' 
#' @slot objects Object of class \code{"list"} a list of context objects
#'
#' @section Methods:
#'   \describe{
#'     \item{show}{output of core information}
#'     \item{summary}{core statistical information}
#'     \item{[}{specific cooccurrences}
#'     \item{[[}{specific cooccurrences}
#'    }
#'     
#' @name contextBundle-class
#' @aliases show,contextBundle-method summary,contextBundle-method [,contextBundle-method [,contextBundle,ANY,ANY,ANY-method [[,contextBundle-method as.TermContextBundle,contextBundle-method
#' @docType class
#' @exportClass kwic
#' @rdname contextBundle-class
setClass("contextBundle",
         representation(
           objects="list",
           query="character",
           pAttribute="character"
         ),
         contains=c("bundle")
)

