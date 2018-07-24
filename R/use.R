#' @include S4classes.R
NULL

#' Add corpora in R data packages to session registry.
#' 
#' Use CWB indexed corpora in R data packages by adding registry file to session
#' registry.
#'
#' {pkg} is expected to be an installed data package that includes CWB indexed
#' corpora. The \code{use}-function will add the registry files describing the
#' corpus (or the corpora) to the session registry directory and adjust the path
#' pointing to the data in the package.
#'
#' The registry files within the package are assumed to be in the subdirectory
#' \code{./extdata/cwb/registry} of the installed package. The data directories
#' for corpora are assumed to be in a subdirectory named after the corpus (lower
#' case) in the package subdirectory \code{./extdata/cwb/indexed_corpora/}. When
#' adding a corpus to the registry, templates for formatting fulltext output are
#' reloaded.
#' 
#' @param pkg A package including at least one CWB indexed corpus.
#' @param lib.loc A character vector with path names of \code{R} libraries.
#' @param verbose Logical, whether to output status messages.
#' @export use
#' @rdname use
#' @name use
#' @examples
#' use("polmineR")
#' corpus()
#' @seealso To get the session registry registry, see \code{\link{registry}};
#'   to reset the registry, see \code{\link{registry_reset}}.
#' @importFrom RcppCWB cqp_reset_registry 
use <- function(pkg, lib.loc = .libPaths(), verbose = TRUE){
  
  if (!pkg %in% unname(installed.packages(lib.loc = lib.loc)[,"Package"]))
    stop("Could not find package specified. Please check for typos,",
         "and/or whether it is installed for the R version you are using.")
  
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
  # If CQP has been initialized before, it will not yet now about the
  # corpora that have been added
  if (cqp_is_initialized()) cqp_reset_registry(registry())
  
  invisible(NULL)
}  