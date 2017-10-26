#' @include polmineR_package.R textstat_class.R
NULL


#' Count class.
#' 
#' S4 class to organize counts. The classes \code{polmineR} and
#' \code{ngrams} inherit from the class.
#' 
#' @slot stat Object of class \code{data.table}
#' @slot corpus Object of class \code{character} the CWB corpus the partition is based on 
#' @slot encoding Object of class \code{character} encoding of the corpus 
#' @slot name Object of class \code{character}, a name for the object
#' @slot size Object of class \code{integer}, the size of the partition or
#'   corpus the count is based upon
#' @rdname count_class
#' @name count_class
#' @exportClass count
#' @docType class
#' @author Andreas Blaette
#' @aliases count-class
#' @seealso The \code{count}-class inherits from the \code{\link{textstat-class}}
setClass("count", representation = list(size = "integer"), contains = "textstat")
