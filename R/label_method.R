#' Assign and get labels.
#' 
#' @param x object
#' @param value length (character vector, length 1)
#' @param n label index
#' @param ... further parameters
#' @rdname label_method
#' @exportMethod label
setGeneric("label", function(x, ...) standardGeneric("label"))

#' @rdname label_method
setGeneric("label<-", function(x, value) standardGeneric("label<-"))

#' @importFrom utils menu
#' @rdname label_method
setMethod("label", "kwic", function(x, n = NULL){
  if (is.null(n)) n <- 1:length(x)
  for (i in n){
    show(x[i])
    user <- utils::menu(choices = c(x@labels$choices))
    if (as.integer(user) == 0) break
    x@labels$labels[i] <- x@labels$choices[as.integer(user)]
  }
})

