#' S4 textstat class
#' 
#' superclass for keyness and conctext class
#' 
#' @section Slots: \describe{ \item{\code{pAttribute}:}{Object of class
#'   \code{"character"} p-attribute of the query } \item{\code{corpus}:}{Object
#'   of class \code{"character"} the CWB corpus used } 
#'   \item{\code{stat}:}{Object of class \code{"data.frame"} statistics of the
#'   analysis } \item{\code{encoding}:}{Object of class \code{"character"}
#'   encoding of the corpus } \item{\code{cutoff}:}{Object of class
#'   \code{"list"} cutoff levels that have been applied } }
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
         representation(corpus="character",
                        pAttribute="character",
                        encoding="character",
                        stat="data.frame",
                        cutoff="list"
         )
)
