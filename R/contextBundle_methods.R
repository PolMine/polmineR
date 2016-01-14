#' @include contextBundle_class.R
NULL



#' @docType methods
setMethod("[", "contextBundle", function(x,i){
  count <- unlist(lapply(x, function(x) x@stat[i,"freqObs"]))
  cat("be aware of bugs\n")
})

#' @exportMethod summary
#' @docType methods
#' @noRd
setMethod('summary', 'contextBundle',
          function(object) {
            cat("\n** ContextBundle object - general information: **\n")
            cat(sprintf("%-20s", "Node:"), object@query, "\n")
            cat(sprintf("%-20s", "P-Attribute:"), object@pAttribute, "\n")
            cat("\n** Statistical summary: **\n")
            yet("yet to be implemented\n")      
          }
)






#' Turn a context bundle into a matrix
#' 
#' Method based on the tm package.
#' @param x a contextBundle object (S3 class)
#' @param col the to be used
#' @param ... furhter arguments
#' @method as.matrix contextBundle
#' @return a matrix
#' @author Andreas Blaette
#' @exportMethod as.matrix
#' @docType methods
#' @noRd
setMethod("as.matrix", "contextBundle", function(x, col, ...) {
  slamStyle <- as.TermDocumentMatrix(x, col)
  mat <- as.matrix(slamStyle)
  mat <- mat[which(rowSums(mat)>0),]
  mat
})

#' @docType methods
#' @noRd
setMethod("summary", "contextBundle", function(object, top=3){
  partitionSizes <- unlist(lapply(object@objects, function(x) x@partitionSize))
  counts <- unlist(lapply(object@objects, function(x) x@frequency))
  overview <- data.frame(
    count=counts,
    freq=round(counts/partitionSizes*100000,2)
    )
  overview <- cbind(overview, t(data.frame(lapply(object@objects, function(x) .statisticalSummary(x)$no))))
  colnames(overview)[3:6] <- criticalValue <- c(">10.83", ">7.88", ">6.63", ">3.84")
  overview <- cbind(overview, t(data.frame(lapply(object@objects, function(x) rownames(x@stat)[1:top]))))
  overview
})

#' @docType methods
#' @noRd
setMethod("show", "contextBundle", function(object){
  summary(object)
})
