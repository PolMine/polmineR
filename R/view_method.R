#' browse an object using View()
#' 
#' @exportMethod view
#' @rdname view
#' @name view
#' @param .Object an object
#' @param ... further parameters
setGeneric("view", function(.Object, ...) standardGeneric("view"))

#' @rdname partition_class
setMethod("view", "partition", function(.Object){
  tableToView <- .Object@stat
  get("View", envir = .GlobalEnv)(tableToView)
})


#' @rdname partition_class
setMethod("view", "cooccurrences", function(.Object){
  if ("count_partition" %in% colnames(.Object))
    attr(.Object@stat[["count_partition"]], "label") <- "observed in partition"
  if ("count_window" %in% colnames(.Object))
    attr(.Object@stat[["count_window"]], "label") <- "observed in window"
  if ("exp_window" %in% colnames(.Object))
    attr(.Object@stat[["exp_window"]], "label") <- "expected in window"
  if ("exp_partition" %in% colnames(.Object))
    attr(.Object@stat[["exp_partition"]], "label") <- "expected in partition"
  if ("ll" %in% colnames(.Object)) attr(.Object@stat[["ll"]], "label") <- "log likelihood"
  if ("rank_ll" %in% colnames(.Object)) attr(.Object@stat[["rank_ll"]], "label") <- "rank"
  get("View", envir = .GlobalEnv)(.Object@stat)
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
