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
#' @examples 
#' \donttest{
#' sampleCorpusPkgDir <- system.file(package="polmineR.sampleCorpus")
#' sampleCorpusRegistryDir <- file.path(sampleCorpusPkgDir, "extdata", "cwb", "registry")
#' oldRegistryDir <- resetRegistry(sampleCorpusRegistryDir)
#' sampleCorpusPartition <- partition("PLPRBTTXT", text_speaker = "Volker Kauder")
#' resetRegistry(oldRegistryDir)
#' }
#' @param registryDir path to the registry directory to be used
#' @param verbose logical, whether to be verbose
#' @return the registry directory used before resetting CORPUS_REGISTRY
#' @export resetRegistry
#' @rdname registry
#' @name resetRegistry
resetRegistry <- function(registryDir = getOption("polmineR.defaultRegistry"), verbose = TRUE) {
  if (verbose) message("... reseting CORPUS_REGISTRY environment variable:")
  if (dir.exists(registryDir)){
    oldRegistry <- Sys.getenv("CORPUS_REGISTRY")
    Sys.setenv(CORPUS_REGISTRY = registryDir)
    message("    ", registryDir)
  } else {
    stop("registryDir does not exist")
  }
  
  if ("rcqp" %in% sapply(library.dynam(), function(x) x[["name"]])){
    
    if (verbose) message("... unloading rcqp library")
    library.dynam.unload("rcqp", libpath = system.file(package = "rcqp"))
    
    if (verbose) message("... reloading rcqp library")
    library.dynam(
      "rcqp", package = "rcqp",
      lib.loc = gsub("^(.*?)/rcqp$", "\\1", system.file(package = "rcqp"))
    )
    if (("rcqp" %in% sapply(library.dynam(), function(x) x[["name"]])) && verbose == TRUE){
      message("... status: OK") 
    } else {
      message("... status: WARNING - rcqp dynamic library not loaded")
    }
    
  } else {
    
  }
  invisible(oldRegistry)
}

