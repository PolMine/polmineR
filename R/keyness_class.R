#' @include textstat_class.R
NULL


#' S4 class for comparing corpora
#' 
#' to keep results from a keyness analysis
#' 
#' @slot corpus Object of class \code{"character"} 
#' @slot pAttribute Object of class \code{"character"} 
#' @slot encoding Object of class \code{"character"}  
#' @slot corpus Object of class \code{"character"}  
#' @slot stat Object of class \code{"data.frame"} 
#' @slot sizeCoi Object of class \code{"numeric"} 
#' @slot sizeRef Object of class \code{"numeric"} 
#' @slot included Object of class \code{"logical"} whether corpus of interest is included in reference corpus
#' @slot minFrequency Object of class \code{"logical"} minimum frequency
#' @slot statisticalTest Object of class \code{"character"} statisticalTest used
#' @slot digits Object of class \code{"data.frame"} number of digits
#' @slot cutoff Object of class \code{"list"} cutoff levels that have been applied
#' @slot call Object of class \code{"character"} the call that generated the object
#'  @section Methods:
#'   \describe{
#'    \item{summary}{\code{signature(object = "keyness")}: Display essential information }
#'    \item{addPos}{\code{signature(object = "keyness")}: add POS attribute to statistics table }
#'    \item{as.data.frame}{\code{signature(object = "keyness")}: get the statistics table}
#'    }
#' @rdname keyness-class
#' @name keyness-class
#' @aliases keyness keyness-class summary,keyness-method
#'   show,keyness-method as.matrix,keynessBundle-method
#'   [[,keynessBundle-method enrich,keynessBundle-method
#'   summary,keynessBundle-method
#'   ll,keyness-method as.igraph,keyness-method
#' @docType class
#' @exportClass keyness
#' @author Andreas Blaette
setClass("keyness",
         representation(
           corpus="character",
           pAttribute="character",
           encoding="character",
           stat="data.table",
           sizeCoi="numeric",
           sizeRef="numeric",
           statisticalTest="character",
           included="logical",
           minFrequency="numeric",
           digits="list",
           cutoff="list",
           call="character"
         ),
         contains=c("textstat")
)

#' @rdname keyness-class
#' @exportClass keynessCooccurrences
setClass("keynessCooccurrences", contains=c("keyness", "textstat"))


