#' @include bundle_class.R
NULL

#' keynessBundle-class
#' 
#' The class inherits from the bundle class.
#' 
#' @param x object
#' @param col the column to extract
#' @param rmBlank remove blank rows
#' @param verbose logical, whether to be verbose
#' @slot objects an object of class \code{list}
#' @rdname keynessBundle-class
setClass("keynessBundle",
         slots=c(objects="list"),
         contains=c("bundle")
)

