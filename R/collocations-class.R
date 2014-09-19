#' @include textstat-class.R partition-class.R
NULL

#' S4 context class
#' 
#' class to organize information of context analysis
#' 
#' @section Slots:
#'   \describe{
#'     \item{\code{partition}:}{Object of class \code{"character"} the partition the analysis is based on }
#'     \item{\code{partitionSize}:}{Object of class \code{"numeric"} the size of the partition }
#'     \item{\code{leftContext}:}{Object of class \code{"numeric"} number of tokens to the right }
#'     \item{\code{rightContext}:}{Object of class \code{"numeric"} number of tokens to the left }
#'     \item{\code{pAttribute}:}{Object of class \code{"character"} p-attribute of the query }
#'     \item{\code{corpus}:}{Object of class \code{"character"} the CWB corpus used }
#'     \item{\code{stat}:}{Object of class \code{"data.frame"} statistics of the analysis }
#'     \item{\code{encoding}:}{Object of class \code{"character"} encoding of the corpus }
#'     \item{\code{posFilter}:}{Object of class \code{"character"} part-of-speech tags filtered}
#'     \item{\code{method}:}{Object of class \code{"character"} statistical test(s) used }
#'   }
#' @aliases collocations-class ll,collocations-method [,collocations-method [,collocations,ANY,ANY,ANY-method show,collocations-method summary,collocations-method
#' @docType class
#' @exportClass collocations
setClass("collocations",
         representation(partition="character",
                        partitionSize="numeric",
                        leftContext="numeric",
                        rightContext="numeric",
                        pAttribute="character",
                        corpus="character",
                        stat="data.frame",
                        encoding="character",
                        posFilter="character",
                        method="character"
         ),
         contains=c("textstat")
)
