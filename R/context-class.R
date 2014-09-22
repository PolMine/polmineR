#' @include textstat-class.R
NULL

#' S4 context class
#' 
#' class to organize information of context analysis
#' 
#' @section Slots:
#'   \describe{
#'     \item{\code{query}:}{Object of class \code{"character"} node examined }
#'     \item{\code{frequency}:}{Object of class \code{"numeric"} number of hits }
#'     \item{\code{partition}:}{Object of class \code{"character"} the partition the analysis is based on }
#'     \item{\code{partitionSize}:}{Object of class \code{"numeric"} the size of the partition }
#'     \item{\code{leftContext}:}{Object of class \code{"numeric"} number of tokens to the right }
#'     \item{\code{rightContext}:}{Object of class \code{"numeric"} number of tokens to the left }
#'     \item{\code{size}:}{Object of class \code{"numeric"} number of tokens in the right and left context }
#'     \item{\code{pAttribute}:}{Object of class \code{"character"} p-attribute of the query }
#'     \item{\code{corpus}:}{Object of class \code{"character"} the CWB corpus used }
#'     \item{\code{stat}:}{Object of class \code{"data.frame"} statistics of the analysis }
#'     \item{\code{encoding}:}{Object of class \code{"character"} encoding of the corpus }
#'     \item{\code{posFilter}:}{Object of class \code{"character"} part-of-speech tags filtered}
#'     \item{\code{cpos}:}{Object of class \code{"list"} corpus positions of the hits }
#'     \item{\code{statisticalTest}:}{Object of class \code{"character"} statistical test used }
#'     \item{\code{cutoff}:}{Object of class \code{"list"} cutoff levels that have been applied }
#'     \item{\code{call}:}{Object of class \code{"character"} call that generated the object }
#'   }
#' @section Methods:
#'   \describe{
#'     \item{show}{output of core information}
#'     \item{summary}{core statistical information}
#'     \item{[}{index the object}
#'     \item{[[}{specific collocates}
#'     \item{trim}{trim the object}
#'     \item{as.data.frame}{get the statistics table}
#'    }
#' @aliases show,context-method [,context-method [,context,ANY,ANY,ANY-method
#'   [[,context-method summary,context-method head,context-method ll,context-method
#' @docType class
#' @exportClass context
setClass("context",
         representation(query="character",
                        frequency="numeric",
                        partition="character",
                        partitionSize="numeric",
                        leftContext="numeric",
                        rightContext="numeric",
                        size="numeric",
                        pAttribute="character",
                        corpus="character",
                        stat="data.frame",
                        encoding="character",
                        posFilter="character",
                        cpos="list",
                        statisticalTest="character",
                        cutoff="list",
                        call="character"
         ),
         contains=c("textstat")
)
