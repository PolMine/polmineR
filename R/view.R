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
  table_to_view <- .Object@stat
  get("View", envir = .GlobalEnv)(table_to_view)
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
setMethod("view", "cooccurrences_reshaped", function(.Object){
  .Object <- round(.Object, digits = 2)
  colsToView <- c("a", "b", "count_ab", "count_a", "count_b", "ll_a2b", "ll_b2a")
  get("View", envir = .GlobalEnv)(.Object@stat[, colsToView, with = FALSE])
})

setMethod("view", "kwic", function(.Object){
  warning(
    "Using the 'view()'-method to inspect an object of the 'kwic'-class works, ",
    "but the default method for kwic results is the 'show()'-method. ",
    "You may see the column 'match_id', which is used internally, and raw html ",
    "to highlight terms in the context of query matches."
  )
  callNextMethod()
})
