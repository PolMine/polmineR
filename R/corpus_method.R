#' @exportMethod corpus
setGeneric("corpus", function(object, ...) standardGeneric("corpus"))


#' @rdname partition-class
setMethod("corpus", "partition", function(object) object@corpus)


#' @exportMethod corpus
#' @rdname bundle-class
setMethod("corpus", "bundle", function(object){
  unique(sapply(object@objects, function(x) x@corpus))
})

setMethod("corpus", "missing", function(){
  corpora <- rcqp::cqi_list_corpora()
  data.frame(
    corpus=corpora,
    size=unname(sapply(
      setNames(corpora, corpora),
      function(x){
        rcqp::cqi_attribute_size(paste(x, rcqp::cqi_attributes(x, "p")[1], sep="."))
      })),
    stringsAsFactors = FALSE
  )
})