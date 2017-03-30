#' Assign and get labels.
#' 
#' @param x object
#' @param value length (character vector, length 1)
#' @rdname label_method
setGeneric("label", function(x) standardGeneric("label"))

#' @rdname label_method
setGeneric("label<-", function(x, value) standardGeneric("label<-"))
