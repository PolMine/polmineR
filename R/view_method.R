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
  .Object <- round(.Object, digits=3)
  View(.Object@stat)
})

# setMethod("view", "partition", function(.Object){
#   View(.Object@stat)
# })

#' @rdname context-class
setMethod("view", "context", function(.Object){
  .Object <- round(.Object, 2)
  whatToView <- c(
    paste("rank", .Object@method, sep="_"),
    .Object@pAttribute,
    "count_window", "count_partition", "exp_window",
    .Object@method
    )
  View(.Object@stat[, whatToView, with=FALSE], title=.Object@query)
})

#' @exportMethod view
#' @rdname comp-class
setMethod("view", "comp", function(.Object){
  .Object <- round(.Object, 2)
  whatToView <- c(
    paste("rank", .Object@method, sep="_"),
    .Object@pAttribute,
    "count_coi", "count_ref", "exp_coi",
    .Object@method
  )
  View(.Object@stat[, whatToView, with=FALSE], title="comp")
})

#' @rdname cooccurrences-class
setMethod(view, "cooccurrencesReshaped", function(.Object){
  .Object <- round(.Object, digits=2)
  colsToView <- c("a", "b", "count_ab", "count_a", "count_b", "ll_a2b", "ll_b2a")
  View(.Object@stat[, colsToView, with=FALSE])
})
