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
  View(.Object@stat)
})