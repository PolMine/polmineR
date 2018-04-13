#' Use a packaged corpus.
#' 
#' Use a CWB corpus shipped in a package, or reset registry directory.
#' 
#' If \code{pkg} is the name of a data package with a CWB indexed corpus,
#' the function will reset the CORPUS_REGISTRY environment variable and restart
#' \code{rcqp} to point to the registry directory in the package.
#' 
#' If \code{pkg} is NULL (default), calling \code{use} will 
#' reset the registry directory to the directory defined by \code{dir}
#' (defaults to the option \code{polmineR.defaultRegistry}, to return to the registry
#' that was used when loading \code{polmineR}).
#' 
#' @param pkg package with a CWB indexed corpus to use (defaults to NULL)
#' @param lib.loc a character vector with path names of \code{R} libraries
#' @param dir a registry directory, defaults to \code{getOption("polmineR.defaultRegistry")}
#' @param verbose logical, whether to output status messages
#' @return the function returns invisibly the registry that was previously set
#' @export use
#' @rdname use
#' @name use
#' @seealso the worker to reset the registry is \code{resetRegistry}
use <- function(pkg = NULL, lib.loc = .libPaths(), dir = getOption("polmineR.defaultRegistry"), verbose = TRUE){
  if (!is.null(pkg)){
    if (pkg %in% unname(installed.packages(lib.loc = lib.loc)[,"Package"])) {
      registryDir <- system.file("extdata", "cwb", "registry", package = pkg, lib.loc = lib.loc)
      if (dir.exists(registryDir)){
        previousRegistry <- resetRegistry(registryDir, verbose = verbose)
        options("polmineR.templates" = list())
        setTemplate()
      } else {
        stop("pkg exists, but is not a standardized package - registry directory missing")
      }
    }
    
  } else {
    if (dir.exists(dir)){
      previousRegistry <- resetRegistry(dir, verbose = verbose)
      options("polmineR.templates" = list())
      setTemplate()
    } else {
      warning("directory provided does not exist")
    }
    
  }
  # the variable previousRegistry is not necessarily defined
  if (exists("previousRegistry")) invisible(previousRegistry) else invisible(NULL)
}  