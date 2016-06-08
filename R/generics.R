#' get pAttribute
#'
#' @param pAttribute the pAttribute to get
#' @exportMethod pAttribute
#' @rdname pAttribute-method
setGeneric("pAttribute", function(object) standardGeneric("pAttribute"))

#' @exportMethod dissect
setGeneric("dissect", function(object, ...) standardGeneric("dissect"))

#' @exportMethod name
#' @rdname polmineR-generics
setGeneric("name", function(x) standardGeneric("name"))

#' @exportMethod name<-
#' @rdname polmineR-generics
setGeneric("name<-", function(x, value) standardGeneric("name<-"))


