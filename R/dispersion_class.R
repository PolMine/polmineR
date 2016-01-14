#' dispersion class
#' 
#' class to organize results from dispersion analysis
#' 
#' @slot call call that generated this object
#' @slot count Object of class \code{"character"} the call that generated the object
#' @slot freq Object of class \code{"character"} the partition the analysis is based on
#' @slot dim dimensions of the object
#' @slot query the query / the queries that were used
#' @slot sizes  Object of class \code{"numeric"} the size of the partition
#' @param x a dispersion object
#' @param .Object a dispersion object
#' @aliases freq
#' @exportClass dispersion
#' @rdname dispersion-class
setClass("dispersion",
         representation(
           call="character",
           count="matrix",
           freq="matrix",
           dim="character",
           query="character",
           sizes="matrix"
         )
)

