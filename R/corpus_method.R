#' Get corpus.
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
  if (nchar(Sys.getenv("CORPUS_REGISTRY")) > 1 && is(CQI) == "CQI.rcqp"){
    corpora <- CQI$list_corpora()
    y <- data.frame(
      corpus = corpora,
      size = unname(sapply(corpora,function(x) CQI$attribute_size(x, CQI$attributes(x, "p")[1], type = "p"))),
      template = unname(sapply(corpora, function(x) x %in% names(getOption("polmineR.templates")))),
      stringsAsFactors = FALSE
    )
  } else {
    y <- data.frame(corpus = character(), size = integer())
  }
  y

})