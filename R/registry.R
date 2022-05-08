#' Reset registry directory.
#' 
#' A utility function to reset the environment variable CORPUS_REGISTRY. That may
#' be necessary if you want use a CWB corpus that is not stored in the usual
#' place. In particular, resetting the environment variable is required if you
#' want to use a corpus delivered in a R package,
#' 
#' Resetting the CORPUS_REGISTRY environment variable is also necessary for the
#' interface to CWB corpora. 
#' 
#' To get the path to a package that contains a CWB corpus, use
#' \code{system.file} (see examples).
#' @param registryDir path to the registry directory to be used
#' @param verbose logical, whether to be verbose
#' @return the registry directory used before resetting CORPUS_REGISTRY
#' @export registry_reset
#' @rdname registry_reset
#' @name registry_reset
#' @importFrom utils capture.output
#' @importFrom stringi stri_match_all_regex
#' @importFrom RcppCWB cqp_reset_registry cqp_get_registry cqp_initialize
#' @seealso To conveniently reset registry, see \code{\link{use}}.
#' @keywords internal
#' @examples
#' \dontrun{
#' x <- system.file(package = "polmineR", "extdata", "cwb", "registry")
#' registry_reset(registryDir = x)
#' }
registry_reset <- function(registryDir = registry(), verbose = TRUE) {
  lifecycle::deprecate_warn("0.8.7", "registry_reset()")
  
  if (!file.exists(registryDir)) stop("registry directory does not exist")
  oldRegistry <- Sys.getenv("CORPUS_REGISTRY")
  
  Sys.setenv(CORPUS_REGISTRY = registryDir)

  if (!cqp_is_initialized()){
    .message("initializing CQP", verbose = verbose)
    cqp_initialize(registry = registryDir)
  } else {
    cqp_reset_registry(registry = registryDir)
  }
  if (cqp_is_initialized() && (cqp_get_registry() == registryDir)){
    .message("status: OK", verbose = verbose)
  } else {
    .message("status: FAIL", verbose = verbose)
  }
  invisible(oldRegistry)
}

.registry_eval <- function(corpus, registry, regex){
  a <- readLines(path(registry, tolower(corpus)))
  b <- stringi::stri_match_all_regex(str = a, pattern  = regex, omit_no_match = TRUE)
  do.call(rbind, b)[,2]
}


#' Evaluate registry file.
#' 
#' Functions to extract information from a registry file describing a corpus.
#' Several operations could be accomplished with the 'cwb-regedit' tool, the
#' functions defined here ensure that manipulating the registry is possible
#' without a full installation of the CWB.
#' 
#' An appendix to the 'Corpus Encoding Tutorial'
#' (https://cwb.sourceforge.io/files/CWB_Encoding_Tutorial.pdf) includes an
#' explanation of the registry file format.
#' 
#' @param corpus name of the CWB corpus
#' @param registry directory of the registry (defaults to CORPUS_Registry environment variable)
#' @importFrom stringi stri_match_all_regex
#' @export registry_get_name
#' @rdname registry_eval
#' @keywords internal
registry_get_name = function(corpus, registry = Sys.getenv("CORPUS_REGISTRY")){
  lifecycle::deprecate_warn(
    "0.8.7",
    "registry_get_name()",
    "RcppCWB::corpus_full_name()"
  )
  .registry_eval(corpus = corpus, registry = registry, regex = "^NAME\\s+(.*?)\\s*$")
}

#' @export registry_get_id
#' @rdname registry_eval
#' @keywords internal
registry_get_id = function(corpus, registry = Sys.getenv("CORPUS_REGISTRY")){
  lifecycle::deprecate_warn("0.8.7", "registry_get_id()")
  .registry_eval(corpus = corpus, registry = registry, regex = "^ID\\s+(.*?)\\s*$")
}


#' @export registry_get_home
#' @rdname registry_eval
#' @keywords internal
registry_get_home = function(corpus, registry = Sys.getenv("CORPUS_REGISTRY")) {
  lifecycle::deprecate_warn(
    "0.8.6",
    "registry_get_home()",
    "RcppCWB::corpus_data_dir()"
  )
  y <- .registry_eval(corpus = corpus, registry = registry, regex = '^HOME\\s+"?(.*?)"?\\s*$')
  normalizePath(path = y, winslash = "/", mustWork = FALSE)
}

#' @export registry_get_info
#' @rdname registry_eval
#' @keywords internal
registry_get_info = function(corpus, registry = Sys.getenv("CORPUS_REGISTRY")) {
  lifecycle::deprecate_warn(
    "0.8.7",
    "registry_get_info()",
    "RcppCWB::corpus_info_file()"
  )
  y <- .registry_eval(corpus = corpus, registry = registry, regex = '^INFO\\s+"?(.*?)"?\\s*$')
  normalizePath(path = y, winslash = "/", mustWork = FALSE)
}


#' @details `registry_get_encoding` will parse the registry file for a
#'   corpus and return the encoding that is defined (corpus property "charset").
#'   If parsing the registry does not yield a result (corpus property "charset"
#'   not defined), the CWB standard encoding ("latin1") is assigned to prevent
#'   errors. Note that \code{RcppCWB::cl_charset_name} is equivalent but is
#'   faster as it uses the internal C representation of a corpus rather than
#'   parsing the registry file.
#' @export registry_get_encoding
#' @rdname registry_eval
#' @importFrom lifecycle deprecate_warn
#' @keywords internal
registry_get_encoding = function(corpus, registry = Sys.getenv("CORPUS_REGISTRY")) {
  lifecycle::deprecate_warn(
    "0.8.6",
    "registry_get_encoding()",
    "RcppCWB::cl_charset_name()"
  )
  
  y <- .registry_eval(corpus = corpus, registry = registry, regex = '^.*charset\\s*=\\s*"(.+?)".*$')
  
  # use CWB standard corpus encoding if parsing registry yields no result
  if (length(y) == 0L) y <- "latin1" 
  if (y == "utf8") y <- "UTF-8"
  if (!toupper(y) %in% iconvlist()) warning('potentially unknown encoding')
  y
}

#' @export registry_get_p_attributes
#' @rdname registry_eval
#' @keywords internal
registry_get_p_attributes = function(corpus, registry = Sys.getenv("CORPUS_REGISTRY")) {
  lifecycle::deprecate_warn(
    "0.8.7",
    "registry_get_p_attributes()",
    "RcppCWB::corpus_p_attributes()"
  )
  
  .registry_eval(corpus = corpus, registry = registry, regex = "^ATTRIBUTE\\s+(.*?)$")
}


#' @export registry_get_s_attributes
#' @rdname registry_eval
#' @keywords internal
registry_get_s_attributes = function(corpus, registry = Sys.getenv("CORPUS_REGISTRY")) {
  lifecycle::deprecate_warn(
    "0.8.7",
    "registry_get_s_attributes()",
    "RcppCWB::corpus_s_attributes()"
  )
  .registry_eval(corpus = corpus, registry = registry, regex = "^STRUCTURE\\s+(.*?)(|\\s+.*?)$")
}


#' @export registry_get_properties
#' @rdname registry_eval
#' @keywords internal
registry_get_properties = function(corpus, registry = Sys.getenv("CORPUS_REGISTRY")) {
  lifecycle::deprecate_warn(
    "0.8.7",
    "registry_get_properties()",
    "RcppCWB::corpus_properties()"
  )
  
  registry_file <- path(registry, tolower(corpus))
  if (isFALSE(file.exists(registry_file))){
    stop("There is no registry file for corpus ", corpus, ".")
  }
  x <- stri_match_all_regex(
    readLines(registry_file),
    pattern = '^##::\\s*(.*?)\\s*=\\s*"(.*?)".*?$',
    omit_no_match = TRUE
  )
  m <- do.call(rbind, x)
  setNames(m[,3], m[,2])
}


#' @details The \code{registry_move} is an auxiliary function to create a copy
#'   of a registry file in the directory specified by the argument
#'   \code{registry_new}.
#' @rdname registry
#' @param registry The old registry directory.
#' @param corpus The ID of the corpus for which the registry file shall be moved.
#' @param registry_new The new registry directory.
#' @param home_dir_new The new home directory.
#' @export registry_move
registry_move <- function(corpus, registry, registry_new, home_dir_new){
  registry <- readLines(path(registry, tolower(corpus)))
  
  home_line_no <- grep("^HOME", registry)
  registry[home_line_no] <- sprintf("HOME \"%s\"", path(home_dir_new))
  
  info_line_no <- grep("^INFO", registry)
  registry_info_file <- gsub('^INFO\\s+"?(.*?)"?\\s*$', "\\1", registry[info_line_no])
  info_file_new <- path(home_dir_new, basename(registry_info_file))
  registry[info_line_no] <- sprintf("INFO \"%s\"", info_file_new)
  
  # RcppCWB v0.2.4 does not digest declarations of s-attributes when they are followed up
  # by # [attributes], the usual output CWB indexing generates - so remove it
  sattr_lines <- grep("^STRUCTURE", registry)
  sattrs_declared <- gsub("^STRUCTURE\\s+(.*?)(\\s.*$|$)", "\\1", registry[sattr_lines])
  registry[sattr_lines] <- sprintf("STRUCTURE %s", sattrs_declared)

  writeLines(text = registry, con = path(registry_new, corpus), sep = "\n")
  invisible(NULL)
}

#' Get registry and data directories.
#' 
#' The Corpus Workbench (CWB) uses a registry directory with plain text files
#' describing corpora in a standardized format. The binary files of a corpus are
#' stored in a data directory defined in the registry directory. The
#' \code{registry} and \code{data_dir} functions return the respective
#' direcories within a package, if the argument \code{pkg} is used, or the
#' temporary registry and data directory in the per-session temporary directory,
#' if \code{pkg} is \code{NULL} (default value).
#'
#' @details Upon loading the polmineR package, there is a check whether the
#'   environment variable \code{CORPUS_REGISTRY} is defined. In case it is, the
#'   registry files in the directory defined by the \code{CORPUS_REGISTRY}
#'   environment variable are copied to the temporary registry directory, which
#'   serves as the central place to store all registry files for all corpora, be
#'   it system corpora, corpora included in R packages, or temporary corpora.
#' @details The Corpus Workbench may have problems to cope with a registry path
#'   that includes registry non-ASCII characters. On Windows, a call to
#'   \code{utils::shortPathName} will generate the short MS-DOS path name that
#'   circumvents resulting problems.
#' @details Usage of the temporary registry directory can be suppress by setting
#'   the environment variable POLMINER_USE_TMP_REGISTRY as 'false'. In this
#'   case, the \code{registry} function will return the environment variable
#'   CORPUS_REGISTRY unchanged. The \code{data_dir} function will return the
#'   "indexed_corpus" directory that is assumed to live in the same parent
#'   directory as the registry directory.
#' 
#' @param pkg A character string with the name of a single package; if \code{NULL} (default),
#' the temporary registry and data directory is returned.
#' @return A path to a (registry or data) directory, or NULL, if package does not exist
#' or is not a package including a corpus.
#' @export registry
#' @rdname registry
#' @name registry
#' @examples
#' registry() # returns temporary registry directory
#' registry(pkg = "polmineR") # returns registry directory in polmineR-package
#' 
#' data_dir()
#' data_dir(pkg = "polmineR")
#' @importFrom stringi stri_enc_mark
registry <- function(pkg = NULL){
  if (is.null(pkg)){
    y <- path(tempdir(), "polmineR_registry")
    
    # The user name may include special characters. On windows, a possible solutions to avoid
    # error messages, is to use the DOS short path name.
    if (stri_enc_mark(y) != "ASCII"){
      if (.Platform$OS.type == "windows") y <- utils::shortPathName(y)
    }
    return(y)
  } else {
    stopifnot(
      is.character(pkg),
      length(pkg) == 1L
    )
    
    pkg_home <- system.file(package = pkg)
    if (pkg_home == ""){
      warning(sprintf("Package '%s' is not available for R version %s.", pkg, getRversion()))
      return( NULL )
    }
    regdir <- path(pkg_home, "extdata", "cwb", "registry")
    if (isFALSE(file.exists(regdir))){
      warning(sprintf("Cannot find registry directory (%s) in package %s.", regdir, pkg))
    }
    if (stri_enc_mark(regdir) != "ASCII"){
      if (.Platform$OS.type == "windows") regdir <- utils::shortPathName(regdir)
    }
    return(regdir)
  }
}


#' @export data_dir
#' @rdname registry
data_dir <- function(pkg = NULL){
  if (is.null(pkg)){
    if (tolower(Sys.getenv("POLMINER_USE_TMP_REGISTRY")) != "false"){
      y <- path(tempdir(), "polmineR_data_dir")
    } else {
      y <- path(dirname(Sys.getenv("CORPUS_REGISTRY")), "indexed_corpora")
    }
    return(y)
  } else {
    stopifnot(
      is.character(pkg),
      length(pkg) == 1L
    )
    
    pkg_home <- system.file(package = pkg)
    if (pkg_home == ""){
      warning(sprintf("Package '%s' is not available for R version %s.", pkg, getRversion()))
      return( NULL )
    }
    data_dir <- fs::path(pkg_home, "extdata", "cwb", "indexed_corpora")
    if (isFALSE(file.exists(data_dir))){
      warning(sprintf("Cannot find data directory (%s) in package %s.", data_dir, pkg))
    }
    if (stri_enc_mark(data_dir) != "ASCII"){
      if (.Platform$OS.type == "windows") data_dir <- utils::shortPathName(data_dir)
    }
    return(data_dir)
    
    
  }
}