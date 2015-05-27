#' @include textstat-class.R
NULL


#' @exportMethod as.data.frame
setMethod("as.data.frame", "textstat", function(x, ...) x@stat )

#' @exportMethod head
setMethod("head", "textstat", function(x, ...) head(x@stat, ...) )

#' @exportMethod tail
setMethod("tail", "textstat", function(x, ...) tail(x@stat, ...) )

#' @exportMethod dim
setMethod("dim", "textstat", function(x) dim(x@stat))

#' exportMethod nrow
setMethod("nrow", "textstat", function(x) nrow(x@stat))

#' @exportMethod colnames
setMethod("colnames", "textstat", function(x) colnames(x@stat))

#' @exportMethod rownames
setMethod("rownames", "textstat", function(x) rownames(x@stat))

#' @exportMethod names
setMethod("names", "textstat", function(x) rownames(x@stat))
