#' get corpus
#' 
#' Calling \code{corpus()} will return the corpora available. If object 
#' is a partition or partitionBundle-object, the corpus the respective
#' object is derived from is returned.
#' @param object the object
#' @exportMethod corpus
#' @rdname corpus-method
setGeneric("corpus", function(object) standardGeneric("corpus"))


#' @rdname corpus-method
setMethod("corpus", "partition", function(object) object@corpus)


#' @exportMethod corpus
#' @rdname corpus-method
setMethod("corpus", "bundle", function(object){
  unique(sapply(object@objects, function(x) x@corpus))
})

#' @rdname corpus-method
setMethod("corpus", "missing", function(){
  corpora <- CQI$list_corpora()
  data.frame(
    corpus=corpora,
    size=unname(sapply(
      setNames(corpora, corpora),
      function(x){
        CQI$attribute_size(x, CQI$attributes(x, "p")[1])
      })),
    stringsAsFactors = FALSE
  )
})