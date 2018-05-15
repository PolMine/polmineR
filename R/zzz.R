.write_registry <- function(text, con) writeLines(text = text, con = con, sep = "\n")

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
    pkg_registry_dir <- file.path(libname, pkgname, "extdata", "cwb", "registry")
    Sys.setenv("CORPUS_REGISTRY" = pkg_registry_dir)
    registry_reset(registryDir = pkg_registry_dir, verbose = FALSE)
  }
  
  options(
    "polmineR.corpus" = "REUTERS",
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
  
  # The package includes the files configure / configure.win in the top-level
  # directory that will set paths in the registry files for corpora correctly
  # upon installation. However, configure / configure.win may not be executed
  # when a pre-compiled package is distributed. Therefore, the following lines
  # check upon whenever loading polmineR whether the paths are set correctly,
  # and adjust them if necessary.
  pkg_cwb_dir <- file.path(libname, pkgname, "extdata", "cwb")
  pkg_registry_dir <- file.path(pkg_cwb_dir, "registry")
  pkg_indexed_corpora_dir <- file.path(pkg_cwb_dir, "indexed_corpora")
  
  for (corpus in list.files(pkg_registry_dir)){
    registry_file <- file.path(pkg_registry_dir, corpus)
    registry <- readLines(registry_file)
    
    home_line_no <- grep("^HOME", registry)
    info_line_no <- grep("^INFO", registry)
    registry_home_dir <- gsub('^HOME\\s+"*(.*?)"*\\s*$', "\\1", registry[home_line_no])
    registry_info_file <- gsub('^INFO\\s+"*(.*?)"*\\s*$', "\\1", registry[info_line_no])
    
    pkg_home_dir <- file.path(pkg_indexed_corpora_dir, corpus)
    if (!identical(x = registry_home_dir, y = pkg_home_dir)){
      packageStartupMessage(sprintf("data directory set for: %s", corpus))
      info_file_new <- file.path(pkg_home_dir, basename(registry_info_file), fsep = "/")
      # quote paths on Windows, and unix-likes, if path includes whitespace
      if (.Platform$OS.type == "windows"){
        registry[home_line_no] <- sprintf('HOME "%s"', pkg_home_dir)
        registry[info_line_no] <- sprintf('INFO "%s"', info_file_new)
      } else {
        if (grepl("\\s+", pkg_home_dir)){
          registry[grep("^HOME", registry)] <- sprintf('HOME "%s"', pkg_home_dir)
          registry[info_line_no] <- sprintf('INFO "%s"', info_file_new)
        } else {
          registry[grep("^HOME", registry)] <- sprintf("HOME %s", pkg_home_dir)
          registry[info_line_no] <- sprintf("INFO %s", info_file_new)
        }
      }
      .write_registry(text = registry, con = registry_file)
    }
  }
  
  # initializing CQP by calling RcppCWB::cqp_initialize would logically done here,
  # but for some (unknown) reason, a package crash occurrs, when CQP is initialized
  # on attach - thus, the 'on demand'-solution (call cqp_initialize before calling cqp_query)
  
  packageStartupMessage(sprintf("polmineR v%s", packageVersion("polmineR")))
  packageStartupMessage("registry:  ", getOption("polmineR.defaultRegistry"))
  packageStartupMessage("interface: ", if (exists("CQI")) class(CQI)[1] else "not set")
}
