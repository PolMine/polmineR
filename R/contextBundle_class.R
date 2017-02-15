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

#' @docType methods
#' @noRd
setMethod("summary", "contextBundle", function(object, top=3){
  partitionSizes <- unlist(lapply(object@objects, function(x) x@partitionSize))
  counts <- unlist(lapply(object@objects, function(x) x@frequency))
  overview <- data.frame(
    count=counts,
    freq=round(counts/partitionSizes*100000,2)
  )
  overview <- cbind(overview, t(data.frame(lapply(object@objects, function(x) .statisticalSummary(x)$no))))
  colnames(overview)[3:6] <- criticalValue <- c(">10.83", ">7.88", ">6.63", ">3.84")
  overview <- cbind(overview, t(data.frame(lapply(object@objects, function(x) rownames(x@stat)[1:top]))))
  overview
})

#' @docType methods
#' @noRd
setMethod("show", "contextBundle", function(object){
  summary(object)
})


