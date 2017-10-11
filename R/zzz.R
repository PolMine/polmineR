# not sure whether it is necessary to have this outside .onLoad or .onAttach
# included to pass CRAN tests - check on occasion, whether this is really necessary
# if (Sys.getenv("CORPUS_REGISTRY") == ""){
#   Sys.setenv("CORPUS_REGISTRY" = file.path(libname, pkgname, "extdata", "cwb", "registry"))
# }

.onLoad <- function (libname, pkgname) {
  
  # if environment variable CORPUS_REGISTRY is not set, use data in the polmineR package
  if (Sys.getenv("CORPUS_REGISTRY") == ""){
    Sys.setenv("CORPUS_REGISTRY" = file.path(libname, pkgname, "extdata", "cwb", "registry"))
  }
  
  # polmineR:::CQI
  CQI <- switch(
    Sys.getenv("POLMINER_INTERFACE"),
    "rcqp" = CQI.rcqp$new(),
    "perl" = CQI.cqpserver$new(),
    "cqpserver" = CQI.cqpserver$new(),
    if (requireNamespace("rcqp", lib.loc = .libPaths(), quietly = TRUE)) CQI.rcqp$new() else CQI.perl$new()
  )
  assign("CQI", CQI, envir = parent.env(environment()))
  
  options(
    "polmineR.project" = "",
    "polmineR.projectDir" = "",
    "polmineR.corpus" = "PLPRBTTXT",
    "polmineR.pAttribute" = "word",
    "polmineR.left" = 5,
    "polmineR.right" = 5,
    "polmineR.minSignificance" = 3.84,
    "polmineR.minFrequency" = 5,
    "polmineR.filterType" = "include",
    "polmineR.lineview" = FALSE,
    "polmineR.meta " =  character(),
    "polmineR.mc" = FALSE,
    "polmineR.cores" = 2,
    "polmineR.smtpServer" = "",
    "polmineR.smtpPort" = "",
    "polmineR.email" = "",
    "polmineR.partitionDir" = "",
    "polmineR.browse" = FALSE,
    "polmineR.backend" = "doSNOW",
    "polmineR.specialChars" = "^[a-zA-Z\u00e9\u00e4\u00f6\u00fc\u00c4\u00d6\u00dc-\u00df|-]+$",
    "polmineR.interface" = "rcqp",
    "polmineR.template" = "default",
    "polmineR.templates" = list(),
    "polmineR.cutoff" = 5000,
    "polmineR.Rcpp" = TRUE,
    "polmineR.cwb-s-decode" = FALSE,
    "polmineR.cwb-encode" = FALSE,
    "polmineR.cwb-lexdecode" = FALSE,
    "polmineR.cwb-regedit" = FALSE,
    "polmineR.defaultRegistry" = Sys.getenv("CORPUS_REGISTRY")
  )
  
  
  # Some operations run faster when using the utils of the CWB (cwb-s-decode etc.)
  # These may only be available on *nix systems, so this is checked first
  if (.Platform$OS.type == "unix"){
    if (system("cwb-s-decode -h", intern = FALSE, ignore.stderr =  TRUE) == 1)
      options("polmineR.cwb-s-decode" = TRUE)
    if (system("cwb-encode -h", intern = FALSE, ignore.stderr =  TRUE) == 2)
      options("polmineR.cwb-encode" = TRUE)
    if (system("cwb-s-encode -h", intern = FALSE, ignore.stderr =  TRUE) == 2)
      options("polmineR.cwb-s-encode" = TRUE)
    if (system("cwb-lexdecode -h", intern = FALSE, ignore.stderr =  TRUE) == 2)
      options("polmineR.cwb-lexdecode" = TRUE)
    if (system("cwb-regedit -h", intern = FALSE, ignore.stderr = TRUE) == 255)
      options("polmineR.cwb-regedit" = TRUE)
    
  }
}


#' @importFrom utils packageVersion
.onAttach <- function(libname, pkgname){
  
  # same as in .onLoad, potentially duplicated - included to be sure
  if (Sys.getenv("CORPUS_REGISTRY") == ""){
    Sys.setenv("CORPUS_REGISTRY" = file.path(libname, pkgname, "extdata", "cwb", "registry"))
  }
  
  # adjust dataDir, if it has not yet been set
  REUTERS <- RegistryFile$new(
    "REUTERS",
    filename = file.path(libname, pkgname, "extdata", "cwb", "registry", "reuters")
  )
  correctDataDir <- file.path(libname, pkgname, "extdata", "cwb", "indexed_corpora", "reuters")
  print(correctDataDir)
  print(REUTERS$getHome())
  if (REUTERS$getHome() != correctDataDir){
    REUTERS$setHome(new = correctDataDir) 
    REUTERS$write(verbose = FALSE)
    print("e")
  }

  setTemplate()

  packageStartupMessage(sprintf("polmineR %s", packageVersion("polmineR")))
  
  if (Sys.getenv("CORPUS_REGISTRY") %in% c("", "/")){
    packageStartupMessage(
      "The CORPUS_REGISTRY environment variable is not defined. ",
      "See the package vignette to learn how to set it!"
    )
  } else {
    packageStartupMessage("registry:  ", getOption("polmineR.defaultRegistry"))
  }
  
  packageStartupMessage("interface: ", class(CQI)[1])
  print("f")
}
