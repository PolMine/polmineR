#' @exportMethod means
setGeneric("means", function(.Object, ...) standardGeneric("means"))

#' @importFrom slam row_sums col_sums
setMethod("means", "DocumentTermMatrix", function(.Object, dim=1){
  if (dim == 1){
    meansVector <- tapply(.Object$v, .Object$j, mean)
    which(col_sums(.Object) != 0)
    names(meansVector) <- .Object$dimnames[["Terms"]][which(col_sums(.Object) != 0)]
  } else if (dim == 2){
    meansVector <- tapply(.Object$v, .Object$i, mean)
    names(meansVector) <- .Object$dimnames[["Documents"]][which(row_sums(.Object) != 0)]
  }
  meansVector
})

