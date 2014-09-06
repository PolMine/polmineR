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
#'   \item{\code{pattribute}:}{Object of class \code{"character"} ~~ }
#'   \item{\code{encoding}:}{Object of class \code{"character"} ~~ } 
#'   \item{\code{corpus}:}{Object of class \code{"character"} ~~ } 
#'   \item{\code{stat}:}{Object of class \code{"data.frame"} ~~ } 
#'   \item{\code{statisticalTest}:}{Object of class \code{"character"} statisticalTest used }
#'   \item{\code{statisticalSummary}:}{Object of class \code{"data.frame"} statistical summary }
#'   }
#'  @section Methods:
#'   \describe{
#'    \item{summary}{\code{signature(object = "keyness")}: Display essential information }
#'    \item{addPos}{\code{signature(object = "keyness")}: add POS attribute to statistics table }
#'    \item{as.data.frame}{\code{signature(object = "keyness")}: get the statistics table}
#'    }
#' @rdname keyness-class
#' @name keyness-class
#' @aliases keyness keyness-class keynessCluster-class summary,keyness-method
#'   show,keyness-method as.matrix,keynessCluster-method
#'   keyness,partitionCluster-method [[,keynessCluster-method enrich,keynessCluster-method
#'   trim,keynessCluster-method summary,keynessCluster-method
#' @docType class
#' @exportClass keyness
#' @author Andreas Blaette
setClass("keyness",
         representation(corpus="character",
                        pattribute="character",
                        encoding="character",
                        stat="data.frame",
                        statisticalTest="character",
                        statisticalSummary="data.frame"
         )
)
