#' partitionCluster class
#' 
#' A cluster of partition objects.
#' 
#' @section Slots: \describe{ \item{\code{partitions}:}{Object of class 
#'   \code{"list"} the partitions making up the cluster } 
#'   \item{\code{corpus}:}{Object of class \code{"character"} the CWB corpus the
#'   partition is based on } \item{\code{sAttributesFixed}:}{Object of class 
#'   \code{"list"} fixed sAttributes } \item{\code{encoding}:}{Object of class 
#'   \code{"character"} encoding of the corpus } 
#'   \item{\code{explanation}:}{Object of class \code{"character"} an 
#'   explanation of the partition } \item{\code{xml}:}{Object of class 
#'   \code{"character"} whether the xml is flat or nested } }
#'   
#' @section Methods: \describe{ \item{show}{\code{signature(object = 
#'   "partitionCluster")}: Display essential information } 
#'   \item{addPos}{\code{signature(object="partitionCluster")}: add list with 
#'   most frequent pos for a token } 
#'   \item{tf}{\code{signature(object="partitionCluster")}: get term frequencies
#'   } \item{trim}{\code{signature(object="partitionCluster")}: trim a 
#'   partitionCluster object } \item{[}{get frequency of a query} \item{[[}{get 
#'   a partition within the cluster} \item{+}{\code{signature(object = 
#'   "partitionCluster")}: combine two partitionClusters into a new one } 
#'   \item{html}{bla}
#'   \item{as.sparseMatrix}{bla}  
#'   }
#'   
#' @aliases partitionCluster-class show,partitionCluster-method 
#'   [,partitionCluster-method [[,partitionCluster-method
#'   as.DocumentTermMatrix,partitionCluster-method 
#'   as.matrix,partitionCluster-method 
#'   as.TermDocumentMatrix,partitionCluster-method merge,partitionCluster-method
#'   as.sparseMatrix,partitionCluster-Method as.sparseMatrix 
#'   +,partitionCluster-method names,partitionCluster-method 
#'   summary,partitionCluster-method +,partitionCluster,ANY-method
#'   [,partitionCluster,ANY,ANY,ANY-method +,partitionCluster,partition-method 
#'   +,partitionCluster,partitionCluster-method as.partitionCluster,list-method 
#'   as.sparseMatrix,partitionCluster-method as.TermDocumentMatrix as.DocumentTermMatrix
#'   addPos,partitionCluster-method barplot,partitionCluster-method
#'   as.sparseMatrix,TermDocumentMatrix-method
#' @rdname partitionCluster-class
#' @name partitionCluster-class
#' @exportClass partitionCluster
#' @docType class
#' @author Andreas Blaette
setClass("partitionCluster",
         representation(partitions="list", 
                        corpus="character",
                        sAttributesFixed="list",
                        encoding="character",
                        explanation="character",
                        xml="character"
         )
)

