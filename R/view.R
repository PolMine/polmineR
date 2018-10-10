#' @include S4classes.R
NULL

#' Inspect object using View().
#' 
#' @exportMethod view
#' @rdname view
#' @name view
#' @param .Object an object
#' @param ... further parameters
setGeneric("view", function(.Object, ...) standardGeneric("view"))


#' @rdname cooccurrences-class
setMethod("view", "cooccurrences", function(.Object){
  dt <- format(.Object)
  get("View", envir = .GlobalEnv)(dt)
})



#' @rdname kwic-class
#' @param .Object A \code{kwic} class object.
setMethod("view", "kwic", function(.Object){
  tableToView <- .Object@table
  get("View", envir = .GlobalEnv)(tableToView)
})

#' @rdname textstat-class
setMethod("view", "textstat", function(.Object){
  dt <- format(.Object)
  get("View", envir = .GlobalEnv)(dt, title = "textstat")
})


#' @exportMethod view
#' @rdname features-class
setMethod("view", "features", function(.Object){
  dt <- format(.Object)
  get("View", envir = .GlobalEnv)(dt, title = "features")
})

#' @rdname cooccurrences-class
setMethod(view, "cooccurrences_reshaped", function(.Object){
  .Object <- round(.Object, digits = 2)
  colsToView <- c("a", "b", "count_ab", "count_a", "count_b", "ll_a2b", "ll_b2a")
  get("View", envir = .GlobalEnv)(.Object@stat[, colsToView, with = FALSE])
})
