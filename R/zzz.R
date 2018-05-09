.onLoad <- function (libname, pkgname) {
  
  # polmineR:::CQI - assign it to package namespace
  CQI <- switch(
    Sys.getenv("POLMINER_INTERFACE"),
    "rcqp" = CQI.rcqp$new(),
    "cqpserver" = CQI.cqpserver$new(),
    "RcppCWB" = CQI.RcppCWB$new(),
    if (requireNamespace("RcppCWB", lib.loc = .libPaths(), quietly = TRUE)){
      CQI.RcppCWB$new()
    } else if (requireNamespace("rcqp", lib.loc = .libPaths(), quietly = TRUE)){
      CQI.rcqp$new()
    }
  )
  assign("CQI", CQI, envir = parent.env(environment()))

  # if environment variable CORPUS_REGISTRY is not set, use data in the polmineR package
  # this needs to be done after assigning CQI, as registry_reset will call setTemplate
  if (Sys.getenv("CORPUS_REGISTRY") == ""){
    polmineRPackageRegistry <- file.path(libname, pkgname, "extdata", "cwb", "registry")
    # if (.Platform$OS.type == "windows"){
    #   options("polmineR.volume" = gsub("^([A-Z]?:?).*$", "\\1", polmineRPackageRegistry))
    #   polmineRPackageRegistry <- gsub("^[A-Z]?:?(.*)$", "\\1", polmineRPackageRegistry)
    # }
    Sys.setenv("CORPUS_REGISTRY" = polmineRPackageRegistry)
    registry_reset(registryDir = polmineRPackageRegistry, verbose = FALSE)
  }
  
  options(
    "polmineR.project" = "",
    "polmineR.projectDir" = "",
    "polmineR.corpus" = "GERMAPARLMINI",
    "polmineR.pAttribute" = "word",
    "polmineR.left" = 5,
    "polmineR.right" = 5,
    "polmineR.minSignificance" = 3.84,
    "polmineR.minFrequency" = 5,
    "polmineR.filterType" = "include",
    "polmineR.lineview" = FALSE,
    "polmineR.meta " =  character(),
    "polmineR.mc" = FALSE,
    "polmineR.cores" = if (.Platform$OS.type == "windows") 1 else 2,
    "polmineR.smtpServer" = "",
    "polmineR.smtpPort" = "",
    "polmineR.email" = "",
    "polmineR.partitionDir" = "",
    "polmineR.browse" = FALSE,
    "polmineR.specialChars" = "^[a-zA-Z\u00e9\u00e4\u00f6\u00fc\u00c4\u00d6\u00dc-\u00df|-]+$",
    "polmineR.interface" = "RcppCWB",
    "polmineR.template" = "default",
    "polmineR.templates" = list(),
    "polmineR.cutoff" = 5000,
    "polmineR.cwb-encode" = FALSE,
    "polmineR.cwb-lexdecode" = FALSE,
    "polmineR.cwb-regedit" = FALSE,
    "polmineR.defaultRegistry" = Sys.getenv("CORPUS_REGISTRY")
  )
  
  
  # Some operations run faster when using the utils of the CWB
  # These may only be available on *nix systems, so this is checked first
  if (.Platform$OS.type == "unix"){
    if (system("cwb-encode -h", intern = FALSE, ignore.stderr =  TRUE) == 2)
      options("polmineR.cwb-encode" = TRUE)
    if (system("cwb-lexdecode -h", intern = FALSE, ignore.stderr =  TRUE) == 2)
      options("polmineR.cwb-lexdecode" = TRUE)
    if (system("cwb-regedit -h", intern = FALSE, ignore.stderr = TRUE) == 255)
      options("polmineR.cwb-regedit" = TRUE)
    if (system("cqp -h", intern = FALSE, ignore.stderr = TRUE) == 1)
      options("polmineR.cqp" = TRUE)
  }
  
  # rcqp is not always accessible here - setTemplates would not work with perl interface
  if (class(CQI)[1] %in% c("CQI.rcqp", "CQI.RcppCWB")) setTemplate()
  NULL
}


#' @importFrom utils packageVersion
.onAttach <- function(libname, pkgname){
  # initializing CQP by calling RcppCWB::cqp_initialize would logically done here,
  # but for some (unknown) reason, a package crash occurrs, when CQP is initialized
  # on attach - thus, the 'on demand'-solution (call cqp_initialize before calling cqp_query)
  packageStartupMessage(sprintf("polmineR %s", packageVersion("polmineR")))
  packageStartupMessage("registry:  ", getOption("polmineR.defaultRegistry"))
  packageStartupMessage("interface: ", if (exists("CQI")) class(CQI)[1] else "not set")
}
