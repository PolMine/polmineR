#' use corpus
#' 
#' Use a cwb corpus shipped in a package or return to default registry.
#' 
#' @param pkg the package with a cwb corpus that shall be used, defaults to "default"
#' (will reset original registry)
#' @export use
#' @rdname use
#' @name use
use <- function(pkg="default"){
  pkgSub <- as.character(substitute(pkg))
  if (exists(pkgSub) == FALSE && pkgSub != "default") {
    registryDir <- system.file("extdata", "cwb", "registry", package=pkgSub)
    previousRegistry <- resetRegistry(registryDir)
  } else if (pkg=="default"){
    previousRegistry <- resetRegistry(getOption("polmineR_default_registry"))
  } else {
    registryDir <- system.file("extdata", "cwb", "registry", package=pkg)
    previousRegistry <- resetRegistry(registryDir)
  }
  previousRegistry
}  
