#' partition class
#' 
#' Basic class for for almost anything in the driller package.
#' 
#' @section Slots:
#' \describe{
#'  \item{\code{label}:}{Object of class \code{"character"} a label that may be useful }
#'  \item{\code{corpus}:}{Object of class \code{"character"} the CWB corpus the partition is based on }
#'  \item{\code{encoding}:}{Object of class \code{"character"} encoding of the corpus }
#'  \item{\code{sAttributes}:}{Object of class \code{"list"} s-attributes specifying the partition }
#'  \item{\code{explanation}:}{Object of class \code{"character"} an explanation of the partition }
#'  \item{\code{cpos}:}{Object of class \code{"matrix"} corpus positions }
#'  \item{\code{pos}:}{Object of class \code{"list"} with tables "abs", "rel" and "max"}
#'  \item{\code{size}:}{Object of class \code{"numeric"} total size of the partition }
#'  \item{\code{metadata}:}{Object of class \code{"list"} metadata information }
#'  \item{\code{strucs}:}{Object of class \code{"numeric"} the strucs defining the partition }
#'  \item{\code{tf}:}{Object of class \code{"list"} term frequencies }
#'  \item{\code{xml}:}{Object of class \code{"character"} whether the xml is flat or nested }
#'  \item{\code{sAttributeStrucs}:}{Object of class \code{"character"} the base node }
#' }
#' 
#' @section Methods:
#'   \describe{
#'    \item{show}{\code{signature(object = "partition")}: Display essential information }
#'    \item{addPos}{\code{signature(object="partition")}: add list with most frequent pos for a token }
#'    \item{trim}{\code{signature(object="partition")}: trim a partition object }
#'    \item{tf}{\code{signature(object="partition")}: get term frequencies }
#'    \item{as.partitionCluster}{\code{signature(object="partition")}: transform a partition object into a partitionCluster (to add further objects) }
#'    \item{[}{get frequency of a query}
#'    \item{html}{transform partition to html}
#'    }
#' 
#' @aliases partition-class show,partition-method [,partition,ANY,ANY,ANY-method 
#'   [,partition-method as.partitionCluster addPos,partition-method
#'   as.partitionCluster,partition-method export export,partition-method split
#' @rdname partition-class
#' @name partition-class
#' @exportClass partition
#' @docType class
#' @author Andreas Blaette
setClass("partition",
         representation(label="character", 
                        corpus="character",
                        encoding="character",
                        sAttributes="list",
                        explanation="character",
                        cpos="matrix",
                        pos="list",
                        size="numeric",
                        metadata="list",
                        strucs="numeric",
                        tf="list",
                        xml="character",
                        sAttributeStrucs="character"
         )
)
