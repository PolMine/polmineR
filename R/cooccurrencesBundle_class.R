#' @include cooccurrences_class.R bundle_class.R
NULL

#' S4 cooccurrencesBundle class
#' 
#' class to organize information of multiple context analyses
#' 
#' @slot objects Object of class \code{"list"} a list of context objects
#'     
#' @param x a cooccurrencesBundle object
#' @param col the column to extract
#' @param directed to be explained
#' @param rel to be explained, too
#' @param mc logical, whether to use multicore
#' @name cooccurrencesBundle-class
#' @aliases cooccurrencesBundle
#' @docType class
#' @exportClass kwic
#' @rdname cooccurrencesBundle-class
setClass("cooccurrencesBundle",
         slots=c(
           objects="list",
           pAttribute="character",
           encoding="character",
           corpus="character"
         ),
         contains=c("bundle")
)

