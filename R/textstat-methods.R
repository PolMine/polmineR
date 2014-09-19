#' @include textstat-class.R
NULL

setMethod("as.data.frame", "textstat", function(x, ...) x@stat )

setMethod("head", "textstat", function(x, ...) head(x@stat, ...) )

setMethod("tail", "textstat", function(x, ...) tail(x@stat, ...) )
