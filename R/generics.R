#' @exportMethod pAttribute
#' @exportMethod pAttribute<-
setGeneric("pAttribute", function(object, ...) standardGeneric("pAttribute"))
setGeneric("pAttribute<-", function(object, value) standardGeneric("pAttribute<-"))


#' display in browser
#' 
#' bla
#' @param object what is to be displayed
#' @param meta metainformation to be displayed
#' @param ... further parameters
#' @rdname browse
#' @name browse
#' @exportMethod browse
setGeneric("browse", function(object, ...) standardGeneric("browse"))


setGeneric("label", function(object, ...) standardGeneric("label"))

setGeneric("label<-", function(object, value) standardGeneric("label<-"))
