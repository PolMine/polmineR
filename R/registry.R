#' Reload using new CORPUS_REGISTRY.
#' 
#' A utility function to reset the environment variable CORPUS_REGISTRY. That may
#' be necessary if you want use a CWB corpus that is not stored in the usual
#' place. In particular, resetting the environment variable is required if you
#' want to use a corpus delivered in a R package,
#' 
#' Resetting the CORPUS_REGISTRY environment variable is achieved by unloading 
#' and reloading the C library 'rcqp' that is the backend for the rcqp package. 
#' It may not be the most artful way to do things, but it works.
#' 
#' To get the path to a package that contains a CWB corpus, use
#' \code{system.file}.
#' @param registryDir path to the registry directory to be used
#' @param verbose logical, whether to be verbose
#' @return the registry directory used before resetting CORPUS_REGISTRY
#' @export resetRegistry
#' @rdname registry
#' @name resetRegistry
#' @importFrom utils capture.output
#' @importFrom stringi stri_match_all_regex
resetRegistry <- function(registryDir = getOption("polmineR.defaultRegistry"), verbose = TRUE) {
  .message("resetting CORPUS_REGISTRY environment variable:", verbose = verbose)
  if (.Platform$OS.type == "windows"){
    if (grepl("^[A-Z]:.*$", registryDir)) registryDir <- gsub("^[A-Z]:(.*)$", "\\1", registryDir)
    stopifnot(file.exists(file.path(getOption("polmineR.volume"), registryDir)))
  } else {
    stopifnot(file.exists(registryDir))
  }
  oldRegistry <- Sys.getenv("CORPUS_REGISTRY")
  Sys.setenv(CORPUS_REGISTRY = registryDir)
  .message("setting registry:", registryDir, verbose = verbose)
  
  if ("rcqp" %in% sapply(library.dynam(), function(x) x[["name"]])){
    .message("unloading rcqp library", verbose = verbose)
    library.dynam.unload("rcqp", libpath = system.file(package = "rcqp"))
    
    .message("reloading rcqp library", verbose = verbose)
    dummy <- capture.output(
      library.dynam(
        "rcqp", package = "rcqp",
        lib.loc = gsub("^(.*?)/rcqp$", "\\1", system.file(package = "rcqp"))
      ),
      type = "output"
    )
    if ("rcqp" %in% sapply(library.dynam(), function(x) x[["name"]])){
      .message("status: OK", verbose = verbose) 
    } else {
      .message("status: WARNING - rcqp dynamic library not loaded", verbose = verbose)
    }
    
  } else {
    if (requireNamespace("rcqp", quietly = TRUE)){
      .message("loading rcqp library", verbose = verbose)
      dummy <- capture.output(
        library.dynam(
          "rcqp", package = "rcqp",
          lib.loc = gsub("^(.*?)/rcqp$", "\\1", system.file(package = "rcqp"))
        ),
        type = "output"
      )
    }
  }
  setTemplate()
  invisible(oldRegistry)
}

.registry_eval <- function(corpus, registry, regex){
  a <- readLines(file.path(registry, corpus))
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
#' @export corpus_get_name
#' @rdname registry_eval
corpus_get_name = function(corpus, registry = Sys.getenv("CORPUS_REGISTRY")){
  .registry_eval(corpus = corpus, registry = registry, regex = "^NAME\\s+(.*?)\\s*$")
}

#' @export corpus_get_id
#' @rdname registry_eval
corpus_get_id = function(corpus, registry = Sys.getenv("CORPUS_REGISTRY")){
  .registry_eval(corpus = corpus, registry = registry, regex = "^ID\\s+(.*?)\\s*$")
}


#' @export corpus_get_home
#' @rdname registry_eval
corpus_get_home = function(corpus, registry = Sys.getenv("CORPUS_REGISTRY")) {
  .registry_eval(corpus = corpus, registry = registry, regex = "^HOME\\s+(.*?)\\s*$")
}

#' @export corpus_get_info
#' @rdname registry_eval
corpus_get_info = function(corpus, registry = Sys.getenv("CORPUS_REGISTRY")) {
  .registry_eval(corpus = corpus, registry = registry, regex = "^INFO\\s+(.*?)\\s*$")
}


#' @export corpus_get_encoding
#' @rdname registry_eval
corpus_get_encoding = function(corpus, registry = Sys.getenv("CORPUS_REGISTRY")) {
  y <- .registry_eval(corpus = corpus, registry = registry, regex = '^.*charset\\s*=\\s*"(.+?)".*$')
  if (y == "utf8") y <- "UTF-8"
  if (!toupper(y) %in% iconvlist()) warning('potentially unknown encoding')
  y
}

#' @export corpus_get_p_attributes
#' @rdname registry_eval
corpus_get_p_attributes = function(corpus, registry = Sys.getenv("CORPUS_REGISTRY")) {
  .registry_eval(corpus = corpus, registry = registry, regex = "^ATTRIBUTE\\s+(.*?)$")
}


#' @export corpus_get_s_attributes
#' @rdname registry_eval
corpus_get_s_attributes = function(corpus, registry = Sys.getenv("CORPUS_REGISTRY")) {
  .registry_eval(corpus = corpus, registry = registry, regex = "^STRUCTURE\\s+(.*?)(|\\s+.*?)$")
}


#' @export corpus_get_properties
#' @rdname registry_eval
corpus_get_properties = function(corpus, registry = Sys.getenv("CORPUS_REGISTRY")) {
  x <- stri_match_all_regex(
    readLines(file.path(registry, corpus)),
    pattern = '^##::\\s*(.*?)\\s*=\\s*"(.*?)".*?$',
    omit_no_match = TRUE
  )
  m <- do.call(rbind, x)
  setNames(m[,3], m[,2])
}

