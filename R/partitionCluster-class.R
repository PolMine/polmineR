#' @include partition-class.R bundle-class.R
NULL

#' partitionCluster class
#' 
#' A cluster of partition objects. 
#' 
#' @slot objects Object of class \code{"list"} the partitions making up the cluster
#' @slot corpus Object of class \code{"character"} the CWB corpus the partition is based on
#' @slot sAttributesFixed Object of class \code{"list"} fixed sAttributes
#' @slot encoding Object of class \code{"character"} encoding of the corpus
#' @slot explanation Object of class \code{"character"} an explanation of the partition
#' @slot xml Object of class \code{"character"} whether the xml is flat or nested
#' @slot call Object of class \code{"character"} the call that generated the partitionCluster
#' @aliases partitionCluster-class
#'   [,partitionCluster-method [[,partitionCluster-method
#'   as.matrix,partitionCluster-method 
#'   merge,partitionCluster-method
#'   as.sparseMatrix,partitionCluster-Method as.sparseMatrix 
#'   +,partitionCluster-method names,partitionCluster-method 
#'   summary,partitionCluster-method +,partitionCluster,ANY-method
#'   [,partitionCluster,ANY,ANY,ANY-method +,partitionCluster,partition-method 
#'   +,partitionCluster,partitionCluster-method as.partitionCluster,list-method 
#'   as.sparseMatrix,partitionCluster-method as.DocumentTermMatrix
#'   barplot,partitionCluster-method
#'   as.sparseMatrix,TermDocumentMatrix-method
#' @param x a partitionCluster object
#' @param object a partitionCluster object
#' @param i integer index
#' @param sAttribute the s-attribute to use
#' @rdname partitionCluster-class
#' @name partitionCluster-class
#' @exportClass partitionCluster
#' @docType class
#' @author Andreas Blaette
setClass("partitionCluster",
         representation(objects="list", 
                        corpus="character",
                        sAttributesFixed="list",
                        encoding="character",
                        explanation="character",
                        xml="character",
                        call="character"
         ),
         contains=c("bundle")
)

