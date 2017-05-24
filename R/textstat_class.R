#' @include generics.R
NULL

#' S4 textstat class
#' 
#' Superclass for features, context, and partition class.
#' 
#' Objects derived from the \code{textstat} class can be indexed with simple
#' square brackets ("[") to get rows specified by an numeric/integer vector,
#' and with double square brackets ("[[") to get specific columns from the 
#' \code{data.table} in the slot \code{stat}.
#' 
#' @slot pAttribute Object of class \code{"character"} p-attribute of the query
#' @slot corpus Object of class \code{"character"}
#' @slot stat Object of class \code{"data.frame"} statistics of the analysis
#' @slot name name of the object
#' @slot encoding Object of class \code{"character"} encoding of the corpus
#' @param .Object an object
#' @param object an object
#' @param by by
#' @param decreasing logical
#' @param e1 object 1
#' @param e2 object 2
#' @param i vector to index data.table in stat-slot
#' @param j vector to index data.table in stat-slot
#' @param ... further parameters
#' @aliases as.data.frame,textstat-method show,textstat-method
#'   dim,textstat-method
#'   colnames,textstat-method rownames,textstat-method names,textstat-method
#'   as.DataTables,textstat-method
#' @docType class
#' @exportClass textstat
#' @examples
#' \dontrun{
#' use("polmineR.sampleCorpus")
#' P <- partition("PLPRBTTXT", text_year = "2009", pAttribute = "word")
#' y <- cooccurrences(P, query = "Arbeit")
#' y[1:25]
#' y[,c("word", "ll")]
#' y[1:25, "word"]
#' y[1:25][["word"]]
#' y[which(y[["word"]] %in% c("Arbeit", "Sozial"))]
#' y[ y[["word"]] %in% c("Arbeit", "Sozial") ]
#' }
setClass("textstat",
         representation(
           corpus = "character",
           pAttribute = "character",
           encoding = "character",
           stat = "data.table",
           name = "character"
         )
)


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
    paste(is(object)[1], "Bundle", sep=""),
    objects=setNames(list(object), object@name),
    corpus=object@corpus,
    encoding=object@encoding
  )
})



#' @exportMethod +
#' @docType methods
#' @rdname textstat-class
setMethod("+", signature(e1="textstat", e2="textstat"), function(e1, e2){
  if (e1@corpus != e2@corpus) warning("Please be careful - partition is from a different CWB corpus")
  newBundle <- as.bundle(e1)
  newBundle@objects[[length(newBundle@objects)+1]] <- e2
  names(newBundle@objects)[length(newBundle@objects)] <- e2@name
  newBundle
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


#' @exportMethod pAttribute
#' @param object a textstat object
#' @rdname pAttribute-method
setMethod("pAttribute", "textstat", function(object) object@pAttribute)

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

