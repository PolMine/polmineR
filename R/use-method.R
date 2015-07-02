#' use corpus
#' 
#' Use a cwb corpus shipped in a package.
#' 
#' @param .Object the object to be used
#' @exportMethod use
#' @aliases use use,character-method
#' @rdname use-method
#' @name use
setGeneric("use", function(corpus) standardGeneric("use"))

#' @rdname use-method
setMethod("use", "character", function(corpus){
  if (object == "sampleCorpus"){
    path <- file.path(
      system.file("corpora", package="polmineR.sampleCorpus"),
      "registry"
    )
    Sys.setenv(CORPUS_REGISTRY=path)
  }
})