#' browse an object using View()
#' 
#' @exportMethod view
#' @rdname view
#' @name view
#' @param .Object an object
#' @param ... further parameters
setGeneric("view", function(.Object, ...) standardGeneric("view"))

#' @rdname partition-class
setMethod("view", "partition", function(.Object){
  tableToView <- .Object@stat
  get("View", envir = .GlobalEnv)(tableToView)
})


#' @rdname partition-class
setMethod("view", "cooccurrences", function(.Object){
  tableToView <- .Object@stat
  get("View", envir = .GlobalEnv)(tableToView)
})



#' @rdname kwic-class
#' @param .Object a kwic object
setMethod("view", "kwic", function(.Object){
  tableToView <- .Object@table
  get("View", envir = .GlobalEnv)(tableToView)
})

#' @rdname textstat-class
setMethod("view", "textstat", function(.Object){
  .Object <- round(.Object, digits = 3)
  get("View", envir = .GlobalEnv)(.Object@stat)
})


#' @rdname context-class
setMethod("view", "context", function(.Object){
  .Object <- round(.Object, 2)
  whatToView <- c(
    paste("rank", .Object@method, sep="_"),
    .Object@pAttribute,
    "count_window", "count_partition", "exp_window",
    .Object@method
    )
  get("View", envir = .GlobalEnv)(.Object@stat[, whatToView, with=FALSE], title=.Object@query)
})

#' @exportMethod view
#' @rdname features-class
setMethod("view", "features", function(.Object){
  .Object <- round(.Object, 2)
  whatToView <- c(
    paste("rank", .Object@method, sep="_"),
    .Object@pAttribute,
    "count_coi", "count_ref", "exp_coi",
    .Object@method
  )
  get("View", envir = .GlobalEnv)(.Object@stat[, whatToView, with=FALSE], title = "features")
})

#' @rdname cooccurrences-class
setMethod(view, "cooccurrencesReshaped", function(.Object){
  .Object <- round(.Object, digits=2)
  colsToView <- c("a", "b", "count_ab", "count_a", "count_b", "ll_a2b", "ll_b2a")
  get("View", envir = .GlobalEnv)(.Object@stat[, colsToView, with=FALSE])
})
