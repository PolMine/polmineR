#' @include partition_class.R bundle_class.R
NULL

#' partitionBundle class
#' 
#' A bundle of partition objects. 
#' 
#' @slot objects Object of class \code{"list"} the partitions making up the bundle
#' @slot corpus Object of class \code{"character"} the CWB corpus the partition is based on
#' @slot sAttributesFixed Object of class \code{"list"} fixed sAttributes
#' @slot encoding Object of class \code{"character"} encoding of the corpus
#' @slot explanation Object of class \code{"character"} an explanation of the partition
#' @slot xml Object of class \code{"character"} whether the xml is flat or nested
#' @slot call Object of class \code{"character"} the call that generated the partitionBundle
#' @aliases partitionBundle-class
#'   [,partitionBundle-method [[,partitionBundle-method
#'   as.matrix,partitionBundle-method 
#'   merge,partitionBundle-method
#'   as.sparseMatrix,partitionBundle-Method as.sparseMatrix 
#'   +,partitionBundle-method names,partitionBundle-method 
#'   summary,partitionBundle-method +,partitionBundle,ANY-method
#'   [,partitionBundle,ANY,ANY,ANY-method +,partitionBundle,partition-method 
#'   +,partitionBundle,partitionBundle-method as.partitionBundle,list-method 
#'   as.sparseMatrix,partitionBundle-method
#'   barplot,partitionBundle-method
#'   as.sparseMatrix,TermDocumentMatrix-method
#' @param x a partitionBundle object
#' @param .Object a partitionBundle object
#' @param object a partitionBundle object
#' @param i integer index
#' @param sAttribute the s-attribute to use
#' @param height height
#' @param ... further parameters
#' @rdname partitionBundle-class
#' @name partitionBundle-class
#' @exportClass partitionBundle
#' @author Andreas Blaette
setClass("partitionBundle",
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

