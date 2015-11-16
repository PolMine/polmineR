#' browse an object using View()
#' 
#' @exportMethod view
#' @rdname view
#' @name view
#' @param .Object an object
#' @param ... further parameters
setGeneric("view", function(.Object, ...) standardGeneric("view"))

#' @rdname kwic-class
#' @param .Object a kwic object
setMethod("view", "kwic", function(.Object){
  tableToView <- .Object@table
  View(tableToView)
})

#' @rdname textstat-class
setMethod("view", "textstat", function(.Object){
  .Object <- round(.Object, digits=2)
  View(.Object@stat)
})

# setMethod("view", "partition", function(.Object){
#   View(.Object@stat)
# })

setMethod("view", "context", function(.Object){
  DT <- .Object@stat
  if (length(.Object@pAttribute) == 1){
    tokenCols <- "token"
  } else {
    tokenCols <- .Object@pAttribute
  }
  what <- c(
    "rank", tokenCols,
    "countCoi", "countCorpus", "expCoi", "expCorpus",
    .Object@statisticalTest
    )
  View(DT[, what, with=FALSE])
})


setMethod(view, "cooccurrencesReshaped", function(.Object){
  .Object <- round(.Object, digits=2)
  colsToView <- c("a", "b", "tf_ab", "tf_a", "tf_b", "ll_a2b", "ll_b2a")
  View(.Object@stat[, colsToView, with=FALSE])
  
})