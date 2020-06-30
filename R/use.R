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
#' If the path to the data directory in a package includes a non-ASCII character,
#' binary data files of the corpora in package are copied to a subdirectory of the
#' per-session temporary data directory.
#' 
#' @param pkg A package including at least one CWB indexed corpus.
#' @param lib.loc A character vector with path names of \code{R} libraries.
#' @param tmp Whether to use a temporary data directory.
#' @param verbose Logical, whether to output status messages.
#' @export use
#' @rdname use
#' @name use
#' @examples
#' use("polmineR")
#' corpus()
#' @seealso To get the session registry directory, see \code{\link{registry}};
#'   to reset the registry, see \code{\link{registry_reset}}.
#' @importFrom RcppCWB cqp_reset_registry 
#' @importFrom stringi stri_enc_mark
use <- function(pkg, lib.loc = .libPaths(), tmp = FALSE, verbose = TRUE){
  
  if (nchar(system.file(package = pkg)) == 0L)
    stop("Could not find package specified. Please check for typos,",
         "and/or whether it is installed for the R version you are using.")
  
  registry_dir <- system.file("extdata", "cwb", "registry", package = pkg, lib.loc = lib.loc)
  if (!dir.exists(registry_dir)) stop("pkg exists, but is not a standardized package - registry directory missing")
  
  for (corpus in list.files(registry_dir)){
    
    properties <- registry_get_properties(corpus, registry = registry_dir)
    additional_info <- c(
      if ("version" %in% names(properties)) sprintf("version: %s", properties[["version"]]) else character(),
      if ("build_date" %in% names(properties)) sprintf("build date: %s", properties[["build_date"]]) else character()
    )
    additional_info <- paste(additional_info, collapse = " | ")
    if (nchar(additional_info) > 0L) additional_info <- sprintf(" (%s)", additional_info)
    .message(sprintf("activating corpus: %s%s", toupper(corpus), additional_info), verbose = verbose)
    
    corpus_data_srcdir <- system.file(
      "extdata", "cwb", "indexed_corpora", tolower(corpus),
      package = pkg, lib.loc = lib.loc
    )
    
    if ((stri_enc_mark(corpus_data_srcdir) != "ASCII") || (tmp == TRUE)){
      if (.Platform$OS.type == "windows"){
        corpus_data_targetdir <- gsub("\\\\", "/", utils::shortPathName(corpus_data_srcdir))
      } else {
        # Copying files to a temporary directory that does not include non-ASCII characters
        # may still be necessary on macOS machines
        
        if (!dir.exists(corpus_data_targetdir))
          dir.create(corpus_data_targetdir)
        else
          file.remove(list.files(corpus_data_targetdir, full.names = TRUE))
        
        .message(
          sprintf(
            "path includes non-ASCII characters, moving binary corpus data to temporary data directory (%s/%s)",
            data_dir(), tolower(corpus)
          ),
          verbose = verbose
        )
        for (x in list.files(corpus_data_srcdir, full.names = TRUE))
          file.copy(from = x, to = file.path(corpus_data_targetdir, basename(x)))
        
      }
    } else {
      corpus_data_targetdir <- corpus_data_srcdir
    }
    
    registry_move(
      corpus = corpus,
      registry = registry_dir,
      registry_new = registry(),
      home_dir_new = corpus_data_targetdir
    )
  }
  
  # If CQP has been initialized before, it will not yet now about the
  # corpora that have been added
  if (cqp_is_initialized()) cqp_reset_registry(registry())
  
  invisible(NULL)
}  