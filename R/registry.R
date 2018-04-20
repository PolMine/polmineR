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

