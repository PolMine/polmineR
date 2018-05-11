.onLoad <- function (libname, pkgname) {
  
  # polmineR:::CQI - assign it to package namespace
  CQI <- switch(
    Sys.getenv("POLMINER_INTERFACE"),
    "cqpserver" = CQI.cqpserver$new(),
    "RcppCWB" = CQI.RcppCWB$new(),
    CQI.RcppCWB$new()
  )
  assign("CQI", CQI, envir = parent.env(environment()))

  # if environment variable CORPUS_REGISTRY is not set, use data in the polmineR package
  # this needs to be done after assigning CQI, as registry_reset will call setTemplate
  if (Sys.getenv("CORPUS_REGISTRY") %in% c("", "/")){
    polmineRPackageRegistry <- file.path(libname, pkgname, "extdata", "cwb", "registry")
    Sys.setenv("CORPUS_REGISTRY" = polmineRPackageRegistry)
    registry_reset(registryDir = polmineRPackageRegistry, verbose = FALSE)
  }
  
  options(
    "polmineR.corpus" = "GERMAPARLMINI",
    "polmineR.pAttribute" = "word",
    "polmineR.left" = 5,
    "polmineR.right" = 5,
    "polmineR.lineview" = FALSE,
    "polmineR.meta " =  character(),
    "polmineR.mc" = FALSE,
    "polmineR.cores" = if (.Platform$OS.type == "windows") 1L else 2L,
    "polmineR.smtpServer" = "",
    "polmineR.smtpPort" = "",
    "polmineR.email" = "",
    "polmineR.browse" = FALSE,
    "polmineR.specialChars" = "^[a-zA-Z\u00e9\u00e4\u00f6\u00fc\u00c4\u00d6\u00dc-\u00df|-]+$",
    "polmineR.templates" = list(),
    "polmineR.cutoff" = 5000,
    "polmineR.defaultRegistry" = Sys.getenv("CORPUS_REGISTRY")
  )
  
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
