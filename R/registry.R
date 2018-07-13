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
#' @examples
#' x <- system.file(package = "polmineR", "extdata", "cwb", "registry")
#' registry_reset(registryDir = x)
registry_reset <- function(registryDir = registry(), verbose = TRUE) {
  
  if (!file.exists(registryDir)) stop("registry directory does not exist")
  oldRegistry <- Sys.getenv("CORPUS_REGISTRY")
  
  Sys.setenv(CORPUS_REGISTRY = registryDir)

  if (class(CQI)[1] == "CQI.RcppCWB"){
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
  } 

  set_template()
  invisible(oldRegistry)
}

.registry_eval <- function(corpus, registry, regex){
  a <- readLines(file.path(registry, tolower(corpus)))
  b <- stringi::stri_match_all_regex(str = a, pattern  = regex, omit_no_match = TRUE)
  do.call(rbind, b)[,2]
}


#' Evaluate registry file.
#' 
#' Functions to extract information from a registry file describing a corpus.
#' Several operations could be accomplished with the 'cwb-regedit' tool,
#' the functions defined here ensure that manipulating the registry is 
#' possible without a full installation of the CWB.
#' 
#' An appendix to the 'Corpus Encoding Tutorial' (http://cwb.sourceforge.net/files/CWB_Encoding_Tutorial.pdf)
#' includes an explanation of the registry file format. 
#' 
#' @param corpus name of the CWB corpus
#' @param registry directory of the registry (defaults to CORPUS_Registry environment variable)
#' @importFrom utils installed.packages
#' @importFrom stringi stri_match_all_regex
#' @export registry_get_name
#' @rdname registry_eval
registry_get_name = function(corpus, registry = Sys.getenv("CORPUS_REGISTRY")){
  .registry_eval(corpus = corpus, registry = registry, regex = "^NAME\\s+(.*?)\\s*$")
}

#' @export registry_get_id
#' @rdname registry_eval
registry_get_id = function(corpus, registry = Sys.getenv("CORPUS_REGISTRY")){
  .registry_eval(corpus = corpus, registry = registry, regex = "^ID\\s+(.*?)\\s*$")
}


#' @export registry_get_home
#' @rdname registry_eval
registry_get_home = function(corpus, registry = Sys.getenv("CORPUS_REGISTRY")) {
  y <- .registry_eval(corpus = corpus, registry = registry, regex = '^HOME\\s+"?(.*?)"?\\s*$')
  normalizePath(path = y, winslash = "/", mustWork = FALSE)
}

#' @export registry_get_info
#' @rdname registry_eval
registry_get_info = function(corpus, registry = Sys.getenv("CORPUS_REGISTRY")) {
  y <- .registry_eval(corpus = corpus, registry = registry, regex = '^INFO\\s+"?(.*?)"?\\s*$')
  normalizePath(path = y, winslash = "/", mustWork = FALSE)
}


#' @export registry_get_encoding
#' @rdname registry_eval
registry_get_encoding = function(corpus, registry = Sys.getenv("CORPUS_REGISTRY")) {
  y <- .registry_eval(corpus = corpus, registry = registry, regex = '^.*charset\\s*=\\s*"(.+?)".*$')
  if (y == "utf8") y <- "UTF-8"
  if (!toupper(y) %in% iconvlist()) warning('potentially unknown encoding')
  y
}

#' @export registry_get_p_attributes
#' @rdname registry_eval
registry_get_p_attributes = function(corpus, registry = Sys.getenv("CORPUS_REGISTRY")) {
  .registry_eval(corpus = corpus, registry = registry, regex = "^ATTRIBUTE\\s+(.*?)$")
}


#' @export registry_get_s_attributes
#' @rdname registry_eval
registry_get_s_attributes = function(corpus, registry = Sys.getenv("CORPUS_REGISTRY")) {
  .registry_eval(corpus = corpus, registry = registry, regex = "^STRUCTURE\\s+(.*?)(|\\s+.*?)$")
}


#' @export registry_get_properties
#' @rdname registry_eval
registry_get_properties = function(corpus, registry = Sys.getenv("CORPUS_REGISTRY")) {
  x <- stri_match_all_regex(
    readLines(file.path(registry, tolower(corpus))),
    pattern = '^##::\\s*(.*?)\\s*=\\s*"(.*?)".*?$',
    omit_no_match = TRUE
  )
  m <- do.call(rbind, x)
  setNames(m[,3], m[,2])
}


#' @noRd
registry_move <- function(corpus, registry, registry_new, home_dir_new){
  registry <- readLines(file.path(registry, tolower(corpus)))
  
  home_line_no <- grep("^HOME", registry)
  registry[home_line_no] <- sprintf("HOME \"%s\"", file.path(home_dir_new))
  
  info_line_no <- grep("^INFO", registry)
  registry_info_file <- gsub('^INFO\\s+"?(.*?)"?\\s*$', "\\1", registry[info_line_no])
  info_file_new <- file.path(home_dir_new, basename(registry_info_file), fsep = "/")
  registry[info_line_no] <- sprintf("INFO \"%s\"", info_file_new)
  
  # RcppCWB v0.2.4 does not digest declarations of s-attributes when they are followed up
  # by # [attributes], the usual output CWB indexing generates - so remove it
  sattr_lines <- grep("^STRUCTURE", registry)
  sattrs_declared <- gsub("^STRUCTURE\\s+(.*?)(\\s.*$|$)", "\\1", registry[sattr_lines])
  registry[sattr_lines] <- sprintf("STRUCTURE %s", sattrs_declared)

  writeLines(text = registry, con = file.path(registry_new, corpus), sep = "\n")
  invisible(NULL)
}

#' Get session registry directory.
#' 
#' The \code{polmineR} package uses a subdirectory of the per-session temporary directory
#' as a (temporary) registry. The \code{registry} function will return the path 
#' to this directory.
#' 
#' @export registry
#' @rdname registry
#' @name registry
#' @examples
#' registry()
registry <- function() file.path(normalizePath(tempdir(), winslash = "/"), "polmineR_registry", fsep = "/")
