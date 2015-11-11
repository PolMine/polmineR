#' S4 textstat class
#' 
#' superclass for keyness and context class
#' 
#' @slot pAttribute Object of class \code{"character"} p-attribute of the query
#' @slot corpus Object of class \code{"character"}
#' @slot stat Object of class \code{"data.frame"} statistics of the analysis
#' @slot encoding Object of class \code{"character"} encoding of the corpus
#' @slot cutoff Object of class \code{"list"} cutoff levels that have been applied
#' @param .Object an object
#' @param x an object
#' @param ... further parameters
#' @aliases as.data.frame,textstat-method show,textstat-method
#'   dim,textstat-method
#'   colnames,textstat-method rownames,textstat-method names,textstat-method
#'   as.DataTables,textstat-method
#' @docType class
#' @exportClass textstat
setClass("textstat",
         representation(
           corpus="character",
           pAttribute="character",
           encoding="character",
           stat="data.table",
           cutoff="list"
         )
)
