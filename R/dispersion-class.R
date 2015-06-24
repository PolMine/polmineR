#' dispersion class
#' 
#' class to organize results from dispersion analysis
#' 
#' @slot abs Object of class \code{"character"} the call that generated the object
#' @slot rel Object of class \code{"character"} the partition the analysis is based on
#' @slot subcorpusSize  Object of class \code{"numeric"} the size of the partition
#' @exportClass dispersion
setClass("dispersion",
         representation(
           call="character",
           abs="matrix",
           rel="matrix",
           subsets="matrix"
         )
)

#' @exportMethod rel
setGeneric("rel", function(x) standardGeneric("rel"))
setMethod("rel", "dispersion", function(x) x$rel)
