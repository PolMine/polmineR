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
  # this needs to be done after assigning CQI, as registry_reset will call set_template
  if (Sys.getenv("CORPUS_REGISTRY") %in% c("", "/")){
    pkg_registry_dir <- file.path(libname, pkgname, "extdata", "cwb", "registry")
    Sys.setenv("CORPUS_REGISTRY" = pkg_registry_dir)
    registry_reset(registryDir = pkg_registry_dir, verbose = FALSE)
  }
  
  options(
    "polmineR.corpus" = "REUTERS",
    "polmineR.p_attribute" = "word",
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
  
  # rcqp is not always accessible here - set_templates would not work with perl interface
  if (class(CQI)[1] == "CQI.RcppCWB") set_template()
  NULL
}




#' @importFrom utils packageVersion
.onAttach <- function(libname, pkgname){
  
  # The package includes the files configure / configure.win in the top-level
  # directory that will set paths in the registry files for corpora correctly
  # upon installation. However, configure / configure.win may not be executed
  # when a pre-compiled package is distributed. Therefore, the following lines
  # create a temporary registry, if necessary.

  pkg_registry_dir <- file.path(normalizePath(libname, winslash = "/"), pkgname, "extdata", "cwb", "registry", fsep = "/")
  pkg_indexed_corpora_dir <- file.path(normalizePath(libname, winslash = "/"), pkgname, "extdata", "cwb", "indexed_corpora", fsep = "/")
  polmineR_registry_dir <- registry()
  if (!file.exists(polmineR_registry_dir)) dir.create(polmineR_registry_dir)

  if (Sys.getenv("CORPUS_REGISTRY") != ""){
    for (corpus in list.files(Sys.getenv("CORPUS_REGISTRY"), full.names = FALSE)){
      file.copy(
        from = file.path(Sys.getenv("CORPUS_REGISTRY"), corpus),
        to = file.path(polmineR_registry_dir, corpus)
        )
    }
  }

  for (corpus in list.files(pkg_registry_dir)){
    registry_move(
      corpus = corpus,
      registry = pkg_registry_dir,
      registry_new = polmineR_registry_dir,
      home_dir_new = file.path(pkg_indexed_corpora_dir, tolower(corpus))
    )
  }
  
  Sys.setenv("CORPUS_REGISTRY" = polmineR_registry_dir)
  
  # if (cqp_is_initialized()) cqp_reset_registry(tmp_registry_dir)

  # initializing CQP by calling RcppCWB::cqp_initialize would logically done here,
  # but for some (unknown) reason, a package crash occurrs, when CQP is initialized
  # on attach - thus, the 'on demand'-solution (call cqp_initialize before calling cqp_query)
  
  packageStartupMessage(sprintf("polmineR v%s", packageVersion("polmineR")))
  packageStartupMessage("session registry:  ", Sys.getenv("CORPUS_REGISTRY"))
  # packageStartupMessage("interface: ", if (exists("CQI")) class(CQI)[1] else "not set")
}
