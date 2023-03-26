#' @include S4classes.R
NULL

#' Add corpora in R data packages to session registry.
#' 
#' Use CWB indexed corpora in R data packages by adding registry file to session
#' registry.
#'
#' `pkg` is expected to be an installed data package that includes CWB indexed
#' corpora. The `use()`-function will add the registry files describing the
#' corpus (or the corpora) to the session registry directory and adjust the path
#' pointing to the data in the package.
#'
#' The registry files within the package are assumed to be in the subdirectory
#' './extdata/cwb/registry' of the installed package. The data directories
#' for corpora are assumed to be in a subdirectory named after the corpus (lower
#' case) in the package subdirectory './extdata/cwb/indexed_corpora/'. When
#' adding a corpus to the registry, templates for formatting fulltext output are
#' reloaded.
#' 
#' If the path to the data directory in a package includes a non-ASCII character,
#' binary data files of the corpora in package are copied to a subdirectory of the
#' per-session temporary data directory.
#' 
#' @param pkg A package including at least one CWB indexed corpus.
#' @param corpus A corpus (or corpora) to be loaded selectively.
#' @param lib.loc A character vector with path names of `R` libraries.
#' @param tmp A `logical` value, whether to use a temporary data directory.
#' @param verbose Logical, whether to output status messages.
#' @export use
#' @rdname use
#' @name use
#' @return A `logical` value: `TRUE` if corpus has been loaded successfully, or
#'   `FALSE`, if any kind of error occurred.
#' @examples
#' use("polmineR")
#' corpus()
#' @seealso To get the temporary registry directory, see \code{\link{registry}}.
#' @importFrom RcppCWB cqp_reset_registry cl_load_corpus cqp_load_corpus
#' @importFrom stringi stri_enc_mark
#' @importFrom cli cli_alert_danger cli_alert_success
use <- function(pkg, corpus, lib.loc = .libPaths(), tmp = FALSE, verbose = TRUE){
  
  if (nchar(system.file(package = pkg)) == 0L){
    if (verbose) cli_alert_danger(
      paste0(
        "Could not find package {.pkg {pkg}}. Please check for typos, ",
        "and/or whether it is installed for the R version you are using."
      )
    )
    return(FALSE)
  }

  registry_dir <- system.file(
    "extdata", "cwb", "registry", package = pkg, lib.loc = lib.loc
  )
  if (!dir.exists(registry_dir)){
    if (verbose) cli_alert_danger(
      paste0(
        "package {.pkg {pkg}} exists, but is not a standardized package - ",
        "no registry directory found"
      )
    )
    return(FALSE)
  }
    
  corpora <- list.files(registry_dir)
  if (!missing(corpus)){
    if (all(tolower(corpus) %in% corpora)){
      corpora <- tolower(corpus)
    } else {
      if (verbose) cli_alert_danger("corpus/corpora not included in package")
      return(FALSE)
    }
  }

  for (corpus in corpora){
    
    corpus_data_srcdir <- system.file(
      "extdata", "cwb", "indexed_corpora", tolower(corpus),
      package = pkg, lib.loc = lib.loc
    )
    
    if ((stri_enc_mark(corpus_data_srcdir) != "ASCII") || (tmp == TRUE)){
      if (.Platform$OS.type == "windows"){
        corpus_data_targetdir <- gsub(
          "\\\\", "/", utils::shortPathName(corpus_data_srcdir)
        )
      } else {
        # Copying files to a temporary directory that does not include non-ASCII
        # characters may still be necessary on macOS machines
        
        if (!dir.exists(corpus_data_targetdir))
          dir.create(corpus_data_targetdir)
        else
          file.remove(list.files(corpus_data_targetdir, full.names = TRUE))
        
        if (verbose) cli_alert_info(
          paste0(
            "path includes non-ASCII characters, moving binary corpus data to ",
            "temporary data directory ",
            "{.path {path(data_dir(), tolower(corpus))}}"
          )
        )
        for (x in list.files(corpus_data_srcdir, full.names = TRUE))
          file.copy(from = x, to = path(corpus_data_targetdir, basename(x)))
        
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
    success <- cl_load_corpus(corpus = toupper(corpus), registry = registry())
    if (isFALSE(success)){
      if (verbose) cli_alert_danger("failed to load corpus")
      return(FALSE)
    }
    success <- cqp_load_corpus(corpus = toupper(corpus), registry = registry())
    if (isFALSE(success)){
      if (verbose) cli_alert_danger("failed to load corpus")
      return(FALSE)
    }

    properties <- sapply(
      corpus_properties(corpus, registry = registry()),
      function(p)
        corpus_property(corpus = corpus, registry = registry(), property = p)
    )
    additional_info <- c(
      if ("version" %in% names(properties))
        sprintf("version: %s", properties[["version"]]) else character(),
      if ("build_date" %in% names(properties))
        sprintf("build date: %s", properties[["build_date"]]) else character()
    )
    additional_info <- paste(additional_info, collapse = " | ")
    if (nchar(additional_info) > 0L)
      additional_info <- sprintf(" (%s)", additional_info)
    
    if (verbose) cli_alert_success(
      sprintf("corpus loaded: %s%s", toupper(corpus), additional_info)
    )
  }
  
  invisible(TRUE)
}  