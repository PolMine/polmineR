.onLoad <- function (libname, pkgname) {
  
  # adjust dataDir, if it has not yet been set
  cwbTmpDir <- file.path(libname, pkgname, "extdata", "cwb")
  if (.Platform$OS.type == "windows") cwbTmpDir <- gsub("^[A-Z]?:?(.*)$", "\\1", cwbTmpDir)
  
  reutersTmpRegistry <- file.path(cwbTmpDir, "registry", "reuters")
  reutersTmpDataDir <- file.path(cwbTmpDir, "indexed_corpora", "reuters")
  # checking whether the registry file exists is necessary to circumvent a 
  # devtools::document-problem
  if (file.exists(reutersTmpRegistry)){
    REUTERS <- RegistryFile$new("REUTERS", filename = reutersTmpRegistry)
    if (REUTERS$getHome() != reutersTmpDataDir){
      REUTERS$setHome(new = reutersTmpDataDir) 
      REUTERS$setInfo(new = sprintf("%s/info.md", reutersTmpDataDir))
      REUTERS$write(verbose = FALSE)
    }
  }

  # polmineR:::CQI - assign it to package namespace
  CQI <- switch(
    Sys.getenv("POLMINER_INTERFACE"),
    "rcqp" = CQI.rcqp$new(),
    "perl" = CQI.perl$new(),
    "cqpserver" = CQI.cqpserver$new(),
    "Rcpp" = CQI.Rcpp$new(),
    if (requireNamespace("rcqp", lib.loc = .libPaths(), quietly = TRUE)){
      CQI.rcqp$new()
    } else if (requireNamespace("polmineR.Rcpp", lib.loc = .libPaths(), quietly = TRUE)){
      CQI.Rcpp$new()
    } else {
      CQI.perl$new()
    }
  )
  assign("CQI", CQI, envir = parent.env(environment()))

  # if environment variable CORPUS_REGISTRY is not set, use data in the polmineR package
  # this needs to be done after assigning CQI, as resetRegistry will call setTemplate
  if (Sys.getenv("CORPUS_REGISTRY") == ""){
    polmineRPackageRegistry <- file.path(libname, pkgname, "extdata", "cwb", "registry")
    if (.Platform$OS.type == "windows"){
      polmineRPackageRegistry <- gsub("^[A-Z]?:?(.*)$", "\\1", polmineRPackageRegistry)
      options("polmineR.volume" = gsub("^([A-Z]?:?).*$", "\\1", polmineRPackageRegistry))
    }
    Sys.setenv("CORPUS_REGISTRY" = polmineRPackageRegistry)
    resetRegistry(registryDir = polmineRPackageRegistry, verbose = FALSE)
  }
  
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
    "polmineR.cores" = if (.Platform$OS.type == "windows") 1 else 2,
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
    if (system("cqp -h", intern = FALSE, ignore.stderr = TRUE) == 1)
      options("polmineR.cqp" = TRUE)
  }
  
  if (.Platform$OS.type == "windows"){
    cwb <- system.file(package = "polmineR", "extdata", "cwb", "CWB", "bin", "cqp.exe")
    if (cwb == "") cwb <- 'C:/"Program Files"/CWB/bin/cqp.exe'
    tryCatch(
      expr = {cqpVersion <- shell(sprintf("%s -v", cwb), intern = TRUE)},
      warning = function(x) options("polmineR.cqp" = FALSE)
    )
    if (exists("cqpVersion")){
      if (grepl("The IMS Open Corpus Workbench", cqpVersion[2])) options("polmineR.cqp" = TRUE)
    } else {
      options("polmineR.cqp" = FALSE)
    }
  }
  
  # rcqp is not always accessible here - setTemplates would not work with perl interface
  if (class(CQI)[1] %in% c("CQI.rcqp", "CQI.Rcpp")) setTemplate()
}


#' @importFrom utils packageVersion
.onAttach <- function(libname, pkgname){
  packageStartupMessage(sprintf("polmineR %s", packageVersion("polmineR")))
  packageStartupMessage("registry:  ", getOption("polmineR.defaultRegistry"))
  packageStartupMessage("interface: ", if (exists("CQI")) class(CQI)[1] else "not set")
}
