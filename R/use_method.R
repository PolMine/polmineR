#' Use packaged corpus.
#' 
#' Use a cwb corpus shipped in a package or return to default registry.
#' 
#' @param pkg the package with a cwb corpus that shall be used (defaults to NULL, to return to default registry)
#' (will reset original registry)
#' @return the function returns invisibly the registry that was previously set
#' @export use
#' @rdname use
#' @name use
use <- function(pkg = NULL){
  if (pkg %in% unname(installed.packages()[,"Package"])) {
    registryDir <- system.file("extdata", "cwb", "registry", package = pkg)
    if (dir.exists(registryDir)){
      previousRegistry <- resetRegistry(registryDir)
      setTemplate()
    } else {
      stop("pkg exists, but is not a standardized package")
    }
  } else if (is.null(pkg)){
    previousRegistry <- resetRegistry(getOption("polmineR_default_registry"))
  } else {
    registryDir <- system.file("extdata", "cwb", "registry", package = pkg)
    previousRegistry <- resetRegistry(registryDir)
  }
  invisible(previousRegistry)
}  
