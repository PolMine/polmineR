#' @include textstat-class.R
NULL


#' S4 class for comparing corpora
#' 
#' to keep results from a keyness analysis
#' 
#' @section Objects from the class:
#' keyness objects are returned by the function call \code{keyness}
#'   
#' @section Slots:
#' \describe{
#'   \item{\code{corpus}:}{Object of class \code{"character"} ~~ }
#'   \item{\code{pAttribute}:}{Object of class \code{"character"} ~~ }
#'   \item{\code{encoding}:}{Object of class \code{"character"} ~~ } 
#'   \item{\code{corpus}:}{Object of class \code{"character"} ~~ } 
#'   \item{\code{stat}:}{Object of class \code{"data.frame"} ~~ }
#'   \item{\code{sizeCoi}:}{Object of class \code{"numeric"} ~~ }
#'   \item{\code{sizeRef}:}{Object of class \code{"numeric"} ~~ }
#'   \item{\code{included}:}{Object of class \code{"logical"} whether corpus of interest is included in reference corpus }
#'   \item{\code{minFrequency}:}{Object of class \code{"logical"} minimum frequency }
#'   \item{\code{statisticalTest}:}{Object of class \code{"character"} statisticalTest used }
#'   \item{\code{digits}:}{Object of class \code{"data.frame"} number of digits }
#'   \item{\code{cutoff}:}{Object of class \code{"list"} cutoff levels that have been applied }
#'   \item{\code{call}:}{Object of class \code{"character"} the call that generated the object }
#'   }
#'  @section Methods:
#'   \describe{
#'    \item{summary}{\code{signature(object = "keyness")}: Display essential information }
#'    \item{addPos}{\code{signature(object = "keyness")}: add POS attribute to statistics table }
#'    \item{as.data.frame}{\code{signature(object = "keyness")}: get the statistics table}
#'    }
#' @rdname keyness-class
#' @name keyness-class
#' @aliases keyness keyness-class summary,keyness-method
#'   show,keyness-method as.matrix,keynessCluster-method
#'   [[,keynessCluster-method enrich,keynessCluster-method
#'   summary,keynessCluster-method
#'   ll,keyness-method as.igraph,keyness-method
#' @docType class
#' @exportClass keyness
#' @author Andreas Blaette
setClass("keyness",
         representation(corpus="character",
                        pAttribute="character",
                        encoding="character",
                        stat="data.frame",
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
#' @exportClass keynessCollocations
setClass("keynessCollocations",
         representation(corpus="character",
                        pAttribute="character",
                        encoding="character",
                        stat="data.frame",
                        sizeCoi="numeric",
                        sizeRef="numeric",
                        statisticalTest="character",
                        included="logical",
                        minFrequency="numeric",
                        digits="list",
                        cutoff="list",
                        call="character"
         ),
         contains=c("keyness", "textstat")
)

