#' @include polmineR_package.R textstat_class.R
NULL

# this file includes the partition class, the constructor function 'partition'
# for generating the partition class, and the helper functions used 
# by the constructur


#' partition class
#' 
#' @slot name Object of class \code{"character"} a name that may be useful 
#' @slot corpus Object of class \code{"character"} the CWB corpus the partition is based on 
#' @slot encoding Object of class \code{"character"} encoding of the corpus 
#' @slot sAttributes Object of class \code{"list"} s-attributes specifying the partition 
#' @slot explanation Object of class \code{"character"} an explanation of the partition 
#' @slot cpos Object of class \code{"matrix"} corpus positions
#' @slot annotations Object of class \code{"list"}
#' @slot pos Object of class \code{"list"} with tables "abs", "rel" and "max"
#' @slot size Object of class \code{"numeric"} total size of the partition 
#' @slot metadata Object of class \code{"data.frame"} metadata information 
#' @slot strucs Object of class \code{"numeric"} the strucs defining the partition 
#' @slot pAttribute Object of class \code{"character"} indicating the pAttribute of the
#' count in slot tf
#' @slot xml Object of class \code{"character"} whether the xml is flat or nested 
#' @slot sAttributeStrucs Object of class \code{"character"} the base node 
#' @slot call Object of class \code{"character"} the call that generated the partition 
#' @param .Object a partition object
#' @param x a partition object
#' @param cpos ...
#' @param meta ...
#' @param i index
#' @param cutoff maximum number of tokens to decode
#' @param ... further parameters
#' @param value value
#' @param object a partition object
#' @param dim don't know
#' @param verbose logical, whether to be verbose
#' @aliases partition-class show,partition-method [,partition,ANY,ANY,ANY-method 
#'   [,partition-method as.partitionBundle 
#'   as.partitionBundle,partition-method export export,partition-method split
#'   dissect
#' @rdname partition-class
#' @name partition-class
#' @exportClass partition
#' @docType class
#' @author Andreas Blaette
setClass(
  "partition",
  representation(
    name = "character", 
    corpus = "character",
    encoding = "character",
    sAttributes = "list",
    explanation = "character",
    cpos = "matrix",
    pos = "list",
    annotations = "list",
    size = "numeric",
    metadata = "data.frame",
    strucs = "numeric",
    stat = "data.table",
    pAttribute = "character",
    xml = "character",
    sAttributeStrucs = "character",
    call = "character"
  ),
  contains = c("textstat")
)


#' @rdname partition-class
setClass("plprPartition", contains="partition")

#' @rdname partition-class
setClass("pressPartition", contains="partition")

setAs(from = "partition", to = "plprPartition", function(from){
  y <- new("plprPartition")
  for (z in slotNames(from)) slot(y, z) <- slot(from, z)
  y
})