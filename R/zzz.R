.onLoad <- function (libname, pkgname) {
  
  pkg_registry_dir <- file.path(normalizePath(libname, winslash = "/"), pkgname, "extdata", "cwb", "registry", fsep = "/")
  pkg_indexed_corpora_dir <- file.path(normalizePath(libname, winslash = "/"), pkgname, "extdata", "cwb", "indexed_corpora", fsep = "/")
  
  polmineR_registry_dir <- registry()
  if (!dir.exists(polmineR_registry_dir)) dir.create(polmineR_registry_dir)
  
  if (!dir.exists(data_dir())) dir.create(data_dir())
  
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
  registry_reset(registryDir = registry(), verbose = FALSE)
  
  options(
    "polmineR.p_attribute" = "word",
    "polmineR.left" = 5L,
    "polmineR.right" = 5L,
    "polmineR.lineview" = FALSE,
    "polmineR.pagelength" = 10L,
    "polmineR.meta " =  character(),
    "polmineR.mc" = FALSE,
    "polmineR.cores" = if (.Platform$OS.type == "windows") 1L else 2L,
    "polmineR.smtp_server" = if (length(getOption("polmineR.smtp_server")) > 0) getOption("polmineR.smtp_server") else "",
    "polmineR.smtp_port" = if (length(getOption("polmineR.smtp_port")) > 0) getOption("polmineR.smtp_port") else "",
    "polmineR.email" = if (length(getOption("polmineR.email")) > 0) getOption("polmineR.email") else "",
    "polmineR.browse" = FALSE,
    "polmineR.specialChars" = "^[a-zA-Z\u00e9\u00e4\u00f6\u00fc\u00c4\u00d6\u00dc-\u00df|-]+$",
    "polmineR.templates" = list(),
    "polmineR.cutoff" = 5000,
    "polmineR.corpus_registry" = Sys.getenv("CORPUS_REGISTRY"),
    "polmineR.shiny" = FALSE
  )
  
  # rcqp is not always accessible here - set_templates would not work with perl interface
  set_template()
  NULL
}




#' @importFrom utils packageVersion
.onAttach <- function(libname, pkgname){
  
  # The package includes the files configure / configure.win in the top-level
  # directory that will set paths in the registry files for corpora correctly
  # upon installation. However, configure / configure.win may not be executed
  # when a pre-compiled package is distributed. Therefore, the following lines
  # create a temporary registry, if necessary.

  
  if (Sys.getlocale() == "C"){
    if (Sys.info()["sysname"] == "Darwin"){
      packageStartupMessage(
        "WARNING: The locale of the R session is 'C': ",
        "The character set is not specified. You may encounter problems ",
        "when working with corpora with a latin-1 or UTF-8 encoding that include special characters.\n",
        "For macOS, a potential solution is to enter 'defaults write org.R-project.R force.LANG en_US.UTF-8' ",
        "in a terminal once, replacing 'en_US' with the language/country that is relevant for you ",
        "(see https://community.rstudio.com/t/strange-locale-problems-in-r-after-update-to-mojave/15533)."
      )
    } else {
      packageStartupMessage(
        "WARNING: The locale of the R session is 'C': ",
        "The character set is not specified. You may encounter problems ",
        "when working with corpora with a latin-1 or UTF-8 encoding that include special characters."
      )
      
    }
  }

  # if (cqp_is_initialized()) cqp_reset_registry(tmp_registry_dir)

  # initializing CQP by calling RcppCWB::cqp_initialize would logically done here,
  # but for some (unknown) reason, a package crash occurrs, when CQP is initialized
  # on attach - thus, the 'on demand'-solution (call cqp_initialize before calling cqp_query)
}

.onDetach <- function(libpath){
  unlink(registry(), recursive = TRUE, force = TRUE)
  unlink(data_dir(), recursive = TRUE, force = TRUE)
}
