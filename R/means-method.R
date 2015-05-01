#' @exportMethod means
setGeneric("means", function(.Object, ...) standardGeneric("means"))

setMethod("means", "DocumentTermMatrix", function(.Object, dim=1){
  if (dim == 1){
    meansVector <- tapply(.Object$v, .Object$j, mean)
    names(meansVector) <- .Object$dimnames[["Terms"]]    
  } else if (dim == 2){
    meansVector <- tapply(.Object$v, .Object$i, mean)
    names(meansVector) <- .Object$dimnames[["Documents"]]        
  }
  meansVector
})

