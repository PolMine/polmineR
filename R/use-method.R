#' use corpus
#' 
#' Use a cwb corpus shipped in a package.
#' 
#' @param pkg the object to be used
#' @exportMethod use
#' @aliases use use,character-method
#' @rdname use-method
#' @name use
setGeneric("use", function(pkg) standardGeneric("use"))

#' @rdname use-method
setMethod("use", "character", function(pkg){
    registryDir <- system.file("extdata", "cwb", "registry", package=pkg)
    resetRegistry(registryDir)
})

#' @rdname session
setMethod("use", "missing", function(){
  registryDir <- session@defaultRegistry
  resetRegistry(registryDir)
})