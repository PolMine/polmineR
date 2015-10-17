#' @include textstat_class.R
NULL


#' @exportMethod as.data.frame
setMethod("as.data.frame", "textstat", function(x, ...) x@stat )

#' @exportMethod head
#' @rdname textstat-class
setMethod("head", "textstat", function(x, ...) head(x@stat, ...) )

#' @exportMethod tail
#' @rdname textstat-class
setMethod("tail", "textstat", function(x, ...) tail(x@stat, ...) )

#' @exportMethod dim
setMethod("dim", "textstat", function(x) dim(x@stat))

#' exportMethod nrow
#' @param x textstat object
setMethod("nrow", "textstat", function(x) nrow(x@stat))

#' @exportMethod colnames
setMethod("colnames", "textstat", function(x) colnames(x@stat))

#' @exportMethod rownames
setMethod("rownames", "textstat", function(x) rownames(x@stat))

#' @exportMethod names
setMethod("names", "textstat", function(x) rownames(x@stat))
