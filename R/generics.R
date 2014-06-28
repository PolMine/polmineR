#' trim an object
#' 
#' Use this method to trim 'context', 'keyness', 'context', partition' and 'partitionCluster'
#' objects. See respective documentation available through \code{method?trim("context")},
#' \code{method?trim("keyness")}, \code{method?trim("partition")},
#' \code{method?trim("partitionCluster")}, \code{method?trim("crosstab")}.
#' 
#' @param object the object to be trimmed
#' @param ... further parameters
#' @author Andreas Blaette
#' @docType methods
#' @aliases trim trim-method
#' @rdname trim-method
setGeneric("trim", function(object, ...){standardGeneric("trim")})

#' add pos information
#' 
#' Add information on part-of-speech tags of tokens to tokens. The method is 
#' available for objects of the classes 'partition', 'partitionCluster' and
#' 'keyness' respectively. 
#' \code{method?addPos("partitionCluster")}, \code{method?addPos("keyness")}).
#' @param object either a partition, a partitionCluster or a keyness object
#' @param ... further arguments
#' @return the original, enhanced object
#' @noRd
#' @docType methods
setGeneric("addPos", function(object,...){standardGeneric("addPos")})

setGeneric("keyness", function(x, ...){standardGeneric("keyness")})

# documented with meta,partition-method
setGeneric("meta", function(object, ...){standardGeneric("meta")})

#' contextual analysis
#' 
#' statistical analysis of the context of a token
#' 
#' The method can be applied to partition or partitionCluster class objects.
#' 
#' @param object a partition or partitionCluster object
#' @param ... further arguments
#' @noRd
#' @exportMethod context
setGeneric("context", function(object, ...){standardGeneric("context")})


#' get term frequencies
#' 
#' Method to obtain term frequencies for one or multiple terms or queries.
#' The method can be applied to partition or partitionCluster class objects.
#' If object is a character string, frequencies for a whole corpus are returned.
#' Please see the respective documentation for details 
#' (\code{method?tf("partition")}, \code{method?tf("partitionCluster")} or
#' \code{method?tf("character")}).
#' 
#' @param object either a partition or a partitionCluster object
#' @param ... further parameters
#' @aliases tf tf-method
#' @rdname tf-method
setGeneric("tf", function(object, ...){standardGeneric("tf")})

setGeneric("mail", function(object, ...){standardGeneric("mail")})

setGeneric("sAttributes", function(object,...){standardGeneric("sAttributes")})

#' enrich an object
#' 
#' Method to fill slots of a partition, partitionCluster or keyness object that 
#' have not been set up previously. See  \code{method?enrich("partition")} for 
#' applying the method on partition or partitionCluster objects, or 
#' \code{method?enrich("keyness")}).
#' 
#' @param object a partition, partitionCluster or keyness object
#' @param ... further parameters
#' @aliases enrich enrich-method
#' @docType methods
#' @rdname enrich-method
setGeneric("enrich", function(object, ...){standardGeneric("enrich")})

setGeneric("html", function(object, ...){standardGeneric("html")})

setGeneric("as.sparseMatrix", function(x,...){standardGeneric("as.sparseMatrix")})

setGeneric("as.partitionCluster", function(object,...){standardGeneric("as.partitionCluster")})

setGeneric("as.TermContextMatrix", function(x, col, ...) {standardGeneric("as.TermContextMatrix")})

setGeneric("score", function(object, ...){standardGeneric("score")})
