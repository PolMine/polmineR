#' @exportMethod pAttribute
#' @exportMethod pAttribute<-
setGeneric("pAttribute", function(object, ...) standardGeneric("pAttribute"))
setGeneric("pAttribute<-", function(object, value) standardGeneric("pAttribute<-"))

#' @exportMethod dissect
setGeneric("dissect", function(object, ...) standardGeneric("dissect"))

#' @exportMethod name
#' @rdname polmineR-generics
setGeneric("name", function(x) standardGeneric("name"))

#' @exportMethod name<-
#' @rdname polmineR-generics
setGeneric("name<-", function(x, value) standardGeneric("name<-"))


