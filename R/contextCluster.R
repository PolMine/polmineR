#' @include methods.R context.R
NULL

#' S4 contextCluster class
#' 
#' class to organize information of multiple context analyses
#' 
#' @section Slots:
#'   \describe{
#'     \item{\code{contexts}:}{Object of class \code{"list"} a list of context objects }
#'   }
#' @section Methods:
#'   \describe{
#'     \item{show}{output of core information}
#'     \item{summary}{core statistical information}
#'     \item{[}{specific collocates}
#'     \item{[[}{specific collocates}
#'    }
#'     
#' @name contextCluster-class
#' @aliases show,contextCluster-method summary,contextCluster-method [,contextCluster-method [,contextCluster,ANY,ANY,ANY-method [[,contextCluster-method as.TermContextCluster,contextCluster-method as.TermContextMatrix
#' @docType class
#' @exportClass kwic
#' @rdname contextCluster-class
setClass("contextCluster",
         representation(
           contexts="list",
           query="character",
           pAttribute="character"
         )
)



setMethod("[[", "contextCluster", function(x,i){
  return(x@contexts[[i]])
})

setMethod("[", "contextCluster", function(x,i){
  tf <- unlist(lapply(x, function(x) x@stat[i,"freqObs"]))
  cat("be aware of bugs\n")
})

#' @exportMethod summary
#' @noRd
setMethod('summary', 'contextCluster',
          function(object) {
            cat("\n** ContextCluster object - general information: **\n")
            cat(sprintf("%-20s", "Node:"), object@query, "\n")
            cat(sprintf("%-20s", "P-Attribute:"), object@pAttribute, "\n")
            cat("\n** Statistical summary: **\n")
            yet("yet to be implemented\n")      
          }
)


#' @exportMethod summary
#' @noRd
setMethod('show', 'context', function(object) {
            return(summary(object))
})





#' Turn a context cluster into a matrix
#' 
#' Method based on the tm package.
#' @param x a contextCluster object (S3 class)
#' @param col the to be used
#' @param ... furhter arguments
#' @method as.matrix contextCluster
#' @return a matrix
#' @author Andreas Blaette
#' @exportMethod as.matrix
#' @noRd
setMethod("as.matrix", "contextCluster", function(x, col, ...) {
  slamStyle <- as.TermContextMatrix(x, col)
  mat <- as.matrix(slamStyle)
  mat <- mat[which(rowSums(mat)>0),]
  mat
})

setMethod("summary", "contextCluster", function(object, top=3){
  partitionSizes=unlist(lapply(object@contexts, function(x) x@partitionSize))
  tfAbs=unlist(lapply(object@contexts, function(x) x@frequency))
  overview <- data.frame(
    tfAbs=tfAbs,
    tfRel=round(tfAbs/partitionSizes*100000,2)
    )
  overview <- cbind(overview, t(data.frame(lapply(object@contexts, function(x) .statisticalSummary(x)$no))))
  colnames(overview)[3:6] <- criticalValue <- c(">10.83", ">7.88", ">6.63", ">3.84")
  overview <- cbind(overview, t(data.frame(lapply(object@contexts, function(x) rownames(x@stat)[1:top]))))
  overview
})

setMethod("show", "contextCluster", function(object){
  summary(object)
})