#' Get corpus/partition type.
#' 
#' To generate fulltext output, different templates can be used with a behavior
#' that depends on the type of a corpus. `get_type` will return the type of
#' corpus if it is a specialized one, or `NULL`.
#' 
#' When generating a `partition`, the corpus type will be prefixed to the class
#' that is generated (separated by underscore). If the corpus type is not
#' `NULL`, a class inheriting from the `partition`-class is instantiated. Note
#' that at this time, only `plpr_partition` and `press_partition` is
#' implemented.
#' 
#' 
#' @param .Object A `partition`, `partition_bundle`, `Corpus` object, or a
#'   length-one character vector indicating a CWB corpus.
#' @rdname get_type
#' @exportMethod get_type
#' @aliases get_type,subcorpus-method
#' @examples
#' use("polmineR")
#' 
#' get_type("GERMAPARLMINI")
#' 
#' p <- partition("GERMAPARLMINI", date = "2009-10-28")
#' get_type(p)
#' is(p)
#' 
#' pb <- partition_bundle("GERMAPARLMINI", s_attribute = "date")
#' get_type(pb)
#' 
#' get_type("REUTERS") # returns NULL - no specialized corpus
setGeneric("get_type", function(.Object) standardGeneric("get_type"))


#' @rdname get_type
setMethod("get_type", "corpus", function(.Object){
  get_type(.Object@corpus)
})


#' @rdname get_type
setMethod("get_type", "character", function(.Object){
  stopifnot(length(.Object) == 1)
  corpus_properties <- registry_get_properties(.Object)
  if ("type" %in% names(corpus_properties)) corpus_properties[["type"]] else NULL
})

#' @rdname get_type
setMethod("get_type", "partition_bundle", function(.Object){
  corpus <- unique(unlist(lapply(.Object@objects, function(x) x@corpus)))
  type <- unique(unlist(lapply(corpus, function(x) get_type(x))))
  if (length(type) > 1L)
    warning("cannot determine type, partitions derived from more than one corpus")
  type
})

#' @rdname get_type
setMethod("get_type", "subcorpus_bundle", function(.Object){
  type <- unlist(unique(lapply(.Object@objects, function(x) x@type)))
  if (length(get_type) > 1L)
    warning("cannot determine type, partitions derived from more than one corpus")
  if (is.na(type)) return(NULL)
  type
})

