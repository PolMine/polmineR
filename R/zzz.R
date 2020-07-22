.onLoad <- function (libname, pkgname) {
  
  # Options are set before anything else mainly to catch the CORPUS_REGISTRY
  # environment variable before it is reset.
  options(
    "polmineR.p_attribute" = "word",
    "polmineR.left" = 5L,
    "polmineR.right" = 5L,
    "polmineR.lineview" = FALSE,
    "polmineR.pagelength" = 10L,
    "polmineR.meta" =  character(),
    "polmineR.mc" = FALSE,
    "polmineR.cores" = if (.Platform$OS.type == "windows") 1L else 2L,
    "polmineR.browse" = FALSE,
    "polmineR.buttons" = interactive(),
    "polmineR.specialChars" = "^[a-zA-Z\u00e9\u00e4\u00f6\u00fc\u00c4\u00d6\u00dc-\u00df|-]+$",
    "polmineR.villainChars" = c("\u0084", "\u0093"),
    "polmineR.cutoff" = 5000,
    "polmineR.corpus_registry" = Sys.getenv("CORPUS_REGISTRY"),
    "polmineR.shiny" = FALSE,
    "polmineR.warn.size" = FALSE
  )
  
  # Upon loading the package, registry files available in the polmineR package, or in a directory
  # defined by the environment variable CORPUS_REGISTRY are moved to a temporary registry.
  # The operation is deliberately in .onLoad, and not in .onAttach, so that attaching the 
  # package is not a prerequisite for executing the code.
  
  # If the package is loaded from the the files of the raw code, the extdata dir will still be 
  # a subdirectory of the inst directory. Therefore both options are considered whether extdata
  # is in the inst directory, or immediately in main directory of the package.
  pkg_registry_dir_alternatives <- c(
    file.path(normalizePath(libname, winslash = "/"), pkgname, "extdata", "cwb", "registry", fsep = "/"),
    file.path(normalizePath(libname, winslash = "/"), pkgname, "inst", "extdata", "cwb", "registry", fsep = "/")
  )
  pkg_registry_dir <- pkg_registry_dir_alternatives[dir.exists(pkg_registry_dir_alternatives)]
  
  pkg_indexed_corpora_dir <- file.path(normalizePath(libname, winslash = "/"), pkgname, "extdata", "cwb", "indexed_corpora", fsep = "/")
  
  polmineR_registry_dir <- registry()
  
  if (!dir.exists(polmineR_registry_dir)) dir.create(polmineR_registry_dir)
  if (!dir.exists(data_dir())) dir.create(data_dir())
  
  if (tolower(Sys.getenv("POLMINER_USE_TMP_REGISTRY")) != "false"){
    
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
  }
  
  registry_reset(registryDir = registry(), verbose = FALSE)
    NULL
}




#' @importFrom utils packageVersion
.onAttach <- function(libname, pkgname){

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

}

.onDetach <- function(libpath){
  
  # Assign value of system variable CORPUS_REGISTRY it had before loading polmineR
  Sys.setenv("CORPUS_REGISTRY" = getOption("polmineR.corpus_registry"))
  
  # Remove all options defined when loading the package
  do.call(options, args = sapply(grep("polmineR\\.", names(options()), value = TRUE), function(x) NULL))

  # Remove temporary directories
  unlink(registry(), recursive = TRUE, force = TRUE)
  unlink(data_dir(), recursive = TRUE, force = TRUE)
}
