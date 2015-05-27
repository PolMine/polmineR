#' @include contextCluster-class.R
NULL



#' @docType methods
setMethod("[", "contextCluster", function(x,i){
  tf <- unlist(lapply(x, function(x) x@stat[i,"freqObs"]))
  cat("be aware of bugs\n")
})

#' @exportMethod summary
#' @docType methods
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
#' @docType methods
#' @noRd
setMethod("as.matrix", "contextCluster", function(x, col, ...) {
  slamStyle <- as.TermContextMatrix(x, col)
  mat <- as.matrix(slamStyle)
  mat <- mat[which(rowSums(mat)>0),]
  mat
})

#' @docType methods
#' @noRd
setMethod("summary", "contextCluster", function(object, top=3){
  partitionSizes=unlist(lapply(object@objects, function(x) x@partitionSize))
  tfAbs=unlist(lapply(object@objects, function(x) x@frequency))
  overview <- data.frame(
    tfAbs=tfAbs,
    tfRel=round(tfAbs/partitionSizes*100000,2)
    )
  overview <- cbind(overview, t(data.frame(lapply(object@objects, function(x) .statisticalSummary(x)$no))))
  colnames(overview)[3:6] <- criticalValue <- c(">10.83", ">7.88", ">6.63", ">3.84")
  overview <- cbind(overview, t(data.frame(lapply(object@objects, function(x) rownames(x@stat)[1:top]))))
  overview
})

#' @docType methods
#' @noRd
setMethod("show", "contextCluster", function(object){
  summary(object)
})