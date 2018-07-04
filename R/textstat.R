#' @include S4classes.R p_attributes.R
NULL


#' @exportMethod head
#' @rdname textstat-class
setMethod("head", "textstat", function(x, ...) head(x@stat, ...) )

#' @exportMethod tail
#' @rdname textstat-class
setMethod("tail", "textstat", function(x, ...) tail(x@stat, ...) )

#' @exportMethod dim
#' @rdname textstat-class
setMethod("dim", "textstat", function(x) dim(x@stat))

#' @exportMethod nrow
#' @param x textstat object
#' @rdname textstat-class
setMethod("nrow", "textstat", function(x) nrow(x@stat))

#' @param digits no of digits
#' @rdname textstat-class
#' @exportMethod round
setMethod("round", "textstat", function(x, digits=2){
  columnClasses <- sapply(x@stat, function(column) is(column)[1])
  numericColumns <- which(columnClasses == "numeric")
  for (i in numericColumns) x@stat[, colnames(x@stat)[i] := round(x@stat[[i]], digits)]
  x
})

#' @exportMethod colnames
#' @rdname textstat-class
setMethod("colnames", "textstat", function(x) colnames(x@stat))

#' @exportMethod names
setMethod("names", "textstat", function(x) x@name)

#' @exportMethod sort
#' @rdname textstat-class
setMethod("sort", "textstat", function(x, by, decreasing=TRUE){
  setkeyv(x@stat, cols = by)
  setorderv(x@stat, cols = by, order=ifelse(decreasing == TRUE, -1, 1), na.last=TRUE)
  return(x)
})

#' @rdname textstat-class
#' @exportMethod as.bundle
setGeneric("as.bundle", function(object, ...) standardGeneric("as.bundle"))

setMethod("as.bundle", "textstat", function(object){
  new(
    paste(is(object)[1], "_bundle", sep = ""),
    objects = setNames(list(object), object@name),
    corpus = object@corpus,
    encoding = object@encoding
  )
})



#' @exportMethod +
#' @docType methods
#' @rdname textstat-class
setMethod("+", signature(e1 = "textstat", e2 = "textstat"), function(e1, e2){
  if (e1@corpus != e2@corpus) warning("Please be careful - partition is from a different CWB corpus")
  retval <- as.bundle(e1)
  retval@objects[[length(retval@objects) + 1L]] <- e2
  names(retval@objects)[length(retval@objects)] <- e2@name
  retval
})

#' @exportMethod subset
#' @rdname textstat-class
setMethod("subset", "textstat", function(x, ...){
  x@stat <- subset(copy(x@stat), ...)
  x
})

#' @exportMethod as.data.table
#' @rdname textstat-class
setMethod("as.data.table", "textstat", function(x) x@stat)

#' @exportMethod as.data.frame
#' @rdname textstat-class
setMethod("as.data.frame", "textstat", function(x) as.data.frame(x@stat) )


#' @exportMethod p_attributes
#' @param object a textstat object
#' @rdname textstat-class
setMethod("p_attributes", "textstat", function(.Object) .Object@p_attribute)

#' @exportMethod [[
#' @rdname textstat-class
setMethod("[[", "textstat", function(x, i){
  if (nrow(x@stat) == 0){
    warning("indexing is pointless because data.table is empty")
  }
  x@stat[[i]]
})

#' @exportMethod [[
#' @rdname textstat-class
setMethod("[", "textstat", function(x, i, j){
  if (nrow(x@stat) == 0){
    warning("indexing is pointless because data.table is empty")
  }
  if (missing(j)){
    x@stat <- copy(x@stat[i = i])
  } else {
    x@stat <- copy(x@stat[i = i, j = j, with = FALSE])
  }
  x
})

