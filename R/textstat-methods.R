#' @include textstat-class.R
NULL

setMethod("as.data.frame", "textstat", function(x, ...) x@stat )

setMethod("head", "textstat", function(x, ...) head(x@stat, ...) )
setMethod("tail", "textstat", function(x, ...) tail(x@stat, ...) )

setMethod("dim", "textstat", function(x) dim(x@stat))
setMethod("nrow", "textstat", function(x) nrow(x@stat))
setMethod("colnames", "textstat", function(x) colnames(x@stat))
setMethod("rownames", "textstat", function(x) rownames(x@stat))
