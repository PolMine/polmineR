#' browse an object using View()
#' 
#' @exportMethod view
#' @rdname view
#' @name view
#' @param .Object an object
setGeneric("view", function(.Object, ...) standardGeneric("view"))

setMethod("view", "kwic", function(.Object){
  View(.Object@table)
})

setMethod("view", "textstat", function(.Object){
  View(.Object@stat)
})