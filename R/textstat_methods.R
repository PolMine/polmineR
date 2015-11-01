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

#' @exportMethod nrow
#' @param x textstat object
#' @rdname textstat-class
setMethod("nrow", "textstat", function(x) nrow(x@stat))

#' @param digits
#' @rdname textstat-class
#' @exportMethod round
setMethod("round", "textstat", function(x, digits=2){
  columnClasses <- sapply(x@stat, function(column) is(column)[1])
  numericColumns <- which(columnClasses == "numeric")
  for (i in numericColumns) x@stat[,i] <- round(x@stat[,i], digits)
  x
})

#' @exportMethod colnames
setMethod("colnames", "textstat", function(x) colnames(x@stat))

#' @exportMethod rownames
setMethod("rownames", "textstat", function(x) rownames(x@stat))

#' @exportMethod names
setMethod("names", "textstat", function(x) rownames(x@stat))
