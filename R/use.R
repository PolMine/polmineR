#' @include S4classes.R
NULL

#' Use a packaged corpus.
#' 
#' Use a CWB corpus shipped in a package, or reset registry directory.
#' 
#' If \code{pkg} is the name of a data package with a CWB indexed corpus,
#' the function will reset the CORPUS_REGISTRY environment variable and
#' re-direct the interfacing libraries (RcppCWB or rcqp) to the registry
#' directory in the package. The registry directory is assumed to be the
#' ./extdata/cwb/registry subdirectory of the installed package.
#' 
#' If \code{pkg} is NULL (default), calling \code{use} will 
#' reset the registry directory to the directory defined by \code{dir}
#' (defaults to the option \code{polmineR.defaultRegistry}, to return to the registry
#' that was used when loading \code{polmineR}).
#' 
#' When resetting the registry directory, templates for formatting fulltext
#' output are reloaded.
#' 
#' @param pkg package with a CWB indexed corpus to use (defaults to NULL)
#' @param lib.loc a character vector with path names of \code{R} libraries
#' @param dir a registry directory, defaults to \code{getOption("polmineR.defaultRegistry")}
#' @param verbose logical, whether to output status messages
#' @return the function returns invisibly the registry that was previously set
#' @export use
#' @rdname use
#' @name use
#' @examples
#' # to get the registry directory of the sample data in the polmineR package
#' system.file(package = "polmineR", "extdata", "cwb", "registry")
#' 
#' use("polmineR")
#' @seealso the worker to reset the registry is \code{registry_reset}
use <- function(pkg = NULL, lib.loc = .libPaths(), dir = getOption("polmineR.defaultRegistry"), verbose = TRUE){
  if (!is.null(pkg)){
    if (!pkg %in% unname(installed.packages(lib.loc = lib.loc)[,"Package"])) {
      stop("Could not reset registry directory - package not available. Please check for typos,",
           "and/or whether it is installed for the R version you are using.")
    }
    registry_dir <- system.file("extdata", "cwb", "registry", package = pkg, lib.loc = lib.loc)
    if (!dir.exists(registry_dir)) stop("pkg exists, but is not a standardized package - registry directory missing")
    
    for (corpus in list.files(registry_dir)){
      .message(sprintf("activating corpus: %s", corpus))
      registry_move(
        corpus = corpus,
        registry = registry_dir,
        registry_new = registry(),
        home_dir_new = system.file("extdata", "cwb", "indexed_corpora", tolower(corpus), package = pkg, lib.loc = lib.loc)
        )
      set_template(toupper(corpus))
    }
  } else {
    if (dir.exists(dir)){
      previousRegistry <- registry_reset(dir, verbose = verbose)
      options("polmineR.templates" = list())
      set_template()
    } else {
      warning("directory provided does not exist")
    }
    
  }
  # the variable previousRegistry is not necessarily defined
  if (exists("previousRegistry")) invisible(previousRegistry) else invisible(NULL)
}  