#' @include textstat_class.R features_class.R
NULL

#' Context class.
#' 
#' Class to organize information of context analysis.
#' 
#' @details Objects of the class \code{context} include a \code{data.table} in the
#' slot \code{cpos}. The \code{data.table} will at least include the columns "hit_no",
#' "cpos" and "position".
#' 
#' @slot query Object of class \code{"character"}, the query/node examined
#' @slot count Object of class \code{"numeric"} number of hits
#' @slot partition Object of class \code{"partition"}, the partition the context object is based on
#' @slot partitionSize Object of class \code{"numeric"} the size of the partition
#' @slot left Object of class \code{"numeric"} number of tokens to the left
#' @slot right Object of class \code{"numeric"} number of tokens to the right
#' @slot size Object of class \code{"numeric"} number of tokens in the right and left context
#' @slot sAttribute Object of class \code{"character"} s-attribute
#' @slot pAttribute Object of class \code{"character"} p-attribute of the query
#' @slot corpus Object of class \code{"character"} the CWB corpus used
#' @slot stat Object of class \code{"data.table"} statistics of the analysis
#' @slot encoding Object of class \code{"character"} encoding of the corpus
#' @slot cpos Object of class \code{"list"} corpus positions of the hits
#' @slot method Object of class \code{"character"} statistical test used
#' @slot call Object of class \code{"character"} call that generated the object
#'     
#' @param .Object object
#' @param x a context object
#' @param size integer indicating sample size
#' @param object a context object
#' @param progress logical, whether to show progress bar
#' @aliases context_class show,context-method [,context-method [,context,ANY,ANY,ANY-method
#'   [[,context-method summary,context-method head,context-method
#'   as.DataTables,context-method
#' @docType class
#' @rdname context-class
#' @exportClass context
setClass("context",
         slots = c(
           query = "character",
           count = "numeric",
           partition = "partition",
           partitionSize = "numeric",
           left = "numeric",
           right = "numeric",
           size = "numeric",
           sAttribute = "character",
           cpos = "data.table",
           call = "character"
         ),
         contains = c("features", "textstat")
)

#' @rdname context-class
setMethod("sample", "context", function(x, size){
  hits_unique <- unique(x@cpos[["hit_no"]])
  if (size > length(hits_unique)){
    warning("argument size exceeds number of hits, returning original object")
    return(x)
  }
  x@cpos <- x@cpos[which(x@cpos[["hit_no"]] %in% sample(hits_unique, size = size))]
  x@count <- size
  x@size <- length(which(x@cpos[["position"]] != 0))
  x
})