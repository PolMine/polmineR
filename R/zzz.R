#' @importFrom data.table setDTthreads
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
    "polmineR.specialChars" = "[^a-zA-Z\u00e9\u00e4\u00f6\u00fc\u00c4\u00d6\u00dc-\u00df-]+",
    "polmineR.villainChars" = c("\u0084", "\u0093"),
    "polmineR.cutoff" = 5000,
    "polmineR.corpus_registry" = Sys.getenv("CORPUS_REGISTRY"),
    "polmineR.shiny" = FALSE,
    "polmineR.warn.size" = FALSE,
    "polmineR.segments" = c("s", "p"),
    "polmineR.mdsub" = list(
      c('\u201c', '"'),
      c('\u201D', '"'),
      c('``', '"'), # the `` would wrongly be interpreted as comments
      c('_', ':')
    )
  )

  # Upon loading the package, registry files available in the polmineR package,
  # or in a directory defined by the environment variable CORPUS_REGISTRY are
  # moved to a temporary registry. The operation is deliberately in .onLoad, and
  # not in .onAttach, so that attaching the package is not a prerequisite for
  # executing the code.
  
  # If the package is loaded from the the files of the raw code, the extdata dir
  # will still be a subdirectory of the inst directory. Therefore both options
  # are considered whether extdata is in the inst directory, or immediately in
  # main directory of the package.
  alt <- c(
    path(libname, pkgname, "extdata", "cwb", "registry"),
    path(libname, pkgname, "inst", "extdata", "cwb", "registry")
  )
  pkg_registry_dir <- alt[dir.exists(alt)]
  
  pkg_indexed_corpora_dir <- path(
    libname, pkgname,
    "extdata", "cwb", "indexed_corpora"
  )

  if (!dir.exists(registry())) dir.create(registry())
  if (!dir.exists(data_dir())) dir.create(data_dir())
  
  for (corpus in list.files(pkg_registry_dir)){
    registry_move(
      corpus = corpus,
      registry = pkg_registry_dir,
      registry_new = registry(),
      home_dir_new = path(pkg_indexed_corpora_dir, tolower(corpus))
    )
  }
  
  if (nchar(Sys.getenv("CORPUS_REGISTRY")) > 0L){
    if (file.exists(Sys.getenv("CORPUS_REGISTRY"))){
      if (!cqp_is_initialized()){
        cqp_initialize(registry = Sys.getenv("CORPUS_REGISTRY"))
      } else {
        cqp_reset_registry(registry = Sys.getenv("CORPUS_REGISTRY"))
      }
    }
  } else {
    if (!cqp_is_initialized()){
      cqp_initialize(registry = registry())
    } else {
      cqp_reset_registry(registry = registry())
    }
  }

  NULL
}




#' @importFrom utils packageVersion
.onAttach <- function(libname, pkgname){

  if (isFALSE(l10n_info()[["UTF-8"]]) && is.na(localeToCharset()[1])){
    packageStartupMessage(
      "Cannot guess session character set by using 'localeToCharset()' (yields NA). ",
      "To avoid errors, it will be assumed that 'UTF-8' is applicable. ",
      "Please adjust locale to prevent unwanted behavior and note that LC_CTYPE needs to ",
      "be set such that it can be processed by 'localeToCharset()'."
    )
  }
  
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
  
  # Limit data.table's usage to 2 threads on CRAN, cp.CRAN Repository Policy:
  # "If running a package uses multiple threads/cores it must never use more
  # than two simultaneously: the check farm is a shared resource and will
  # typically be running many checks simultaneously."
  setDTthreads(if (.Platform$OS.type == "windows") 1L else 2L)
  
  packageStartupMessage(
    "polmineR is throttled to use 2 cores as required by CRAN Repository Policy. To get full performance:\n",
    "* Use `n_cores <- parallel::detectCores()` to detect the number of cores available on your machine\n",
    "* Set number of cores using `options('polmineR.cores' = n_cores - 1)` and `data.table::setDTthreads(n_cores - 1)`"
  )
}

.onUnload <- function(libpath){
  
  # Assign value of system variable CORPUS_REGISTRY it had before loading polmineR
  Sys.setenv("CORPUS_REGISTRY" = getOption("polmineR.corpus_registry"))
  
  # Remove all options defined when loading the package
  do.call(options, args = sapply(grep("polmineR\\.", names(options()), value = TRUE), function(x) NULL))

  # Remove temporary directories
  unlink(registry(), recursive = TRUE, force = TRUE)
  unlink(data_dir(), recursive = TRUE, force = TRUE)
}
