#' Install and manage corpora. 
#' 
#' Convenience function for making the installation of indexed CWB corpora
#' wrapped into R data packages as easy as possible. Packaged corpora can
#' then be used by calling \code{\link{use}}.
#' 
#' The function combines two steps necessary to install a CWB corpus wrapped into
#' a R data package. First, it calls \code{install.packages}, then it 
#' resets the path pointing to the directory with the indexed corpus
#' files in the registry file.
#' 
#' The corpus will be installed to the standard library directory for installing R packages
#' (\code{.libPaths{}[1]}). Another location can be used by stating the param 'lib'
#' explicitly (see documentation for \code{\link{install.packages}}).
#' 
#' The function can also be used to install a corpus from a password protected repository. Further
#' parameters are handed over to install.packages, so you might add 
#' \code{method = "wget" extra = "--user donald --password duck"}.
#' 
#' See examples how to check whether the directory has been set correctly. 
#' 
#' An installed data package with a CWB corpus is assumed to include a directory
#' \code{/extdata/cwb/registry} for registry files and a directory
#' \code{/extdata/cwb/indexed_corpora} for the inexed corpus files.
#' 
#' @param old name of the (old) corpus
#' @param new name of the (new) corpus
#' @param pkgs names of data packages with corpora
#' @param repo URL of the repository 
#' @param lib directory for R packages, defaults to \code{.libPaths()[1]}; the path may not 
#' include a whitespace sign
#' @param verbose logical, whether to be verbose
#' @param ... further parameters that will be passed into \code{install.packages}
#' @export install.corpus
#' @name install.corpus
#' @seealso For managing registry files, see class \code{\link{RegistryFile}}, see \code{\link{use}}
#' for switching to a packaged corpus. 
#' @examples
#' \donttest{
#' install.corpus("GermaParl")
#' # is equivalent to:
#' install.corpus("GermaParl", repo = "http://polmine.sowi.uni-due.de/packages")
#' RegistryFile(package = "GermaParl")$adjustHome()
#' # check the directory that has been set
#' RegistryFile$new(package = "GermaParl")$getHome()
#' }
#' @importFrom utils available.packages contrib.url install.packages
#' @rdname install.corpus
#' @aliases remove.corpus rename.corpus copy.corpus
install.corpus <- function(pkgs, repo = "http://polmine.sowi.uni-due.de/packages", lib = .libPaths()[1], ...){
  for (package in pkgs){
    if (package %in% utils::available.packages(utils::contrib.url(repos = repo))){
      if (grepl("\\s", lib)){
        stop(
          "There is a whitespace sign in the directory specified by 'lib'. ",
          "The corpus library will not swallow a directory with a whitespace sign. ",
          "Please provide another directory."
          )
      }
      if (file.access(lib, "6") == -1){
        stop("You do not have write permissions for directory ", lib,
             ". Please run R with the required privileges, or provide another directory (param 'lib').")
      } else {
        install.packages(pkgs = package, repos = repo, lib = lib, ...)
        RegistryFile$new(package = package)$adjustHome()
      }
    } else {
      stop("package ", package, " is not available")
    }
  }
}

#' @rdname install.corpus
#' @export packaged.corpora
packaged.corpora <- function(){
  matrices <- lapply(
    .libPaths(),
    function(lib){
      vectors <- lapply(
        installed.packages(lib.loc = lib)[,"Package"],
        function(package){
          c(
            package = package,
            lib = lib,
            registry = system.file(package = package, "extdata", "cwb", "registry")
          )
        }
      )
      do.call(rbind, vectors)
    }
  )
  M <- data.table(do.call(rbind, matrices))
  M <- M[which(nchar(M[["registry"]]) > 0)]
  M
}

#' @rdname install.corpus
#' @export copy.corpus
copy.corpus <- function(old, new, verbose = TRUE){
  stopifnot(old %in% CQI$list_corpora())
  
  # copy data directory
  .message("copying data directory", verbose = verbose)
  R <- RegistryFile$new(old)
  newDataDir <- file.path(dirname(R$getHome()), tolower(new))
  if (file.exists(newDataDir)){
    if (!readline(prompt = "Data directory already exists. Proceed anyway (Y for yes)? ") == "Y")
      stop("Aborting the operation.")
  } else {
    dir.create(newDataDir)
  }
  filesToCopy <- list.files(R$getHome(), full.names = TRUE)
  success <- pbapply::pblapply(
    filesToCopy,
    function(x){
      file.copy(from = x, to = newDataDir, recursive = TRUE)
    })
  if (!all(unlist(success))){
    stop("copying the data directory failed")
  } else {
    message("... copying data directory succeeded")
  }
  
  # generate copy of registry file
  .message("make copy of registry file", verbose = verbose)
  newRegistryFile <- file.path(Sys.getenv("CORPUS_REGISTRY"), tolower(new))
  if (file.exists(newRegistryFile)){
    if (readline(prompt = "New registry file already exists. Proceed anyway (Y for yes)? ") == "Y"){
      file.remove(newRegistryFile)
    } else {
      stop("Aborting the operation.")
    }
      
  }
  success <- file.copy(
    from = file.path(Sys.getenv("CORPUS_REGISTRY"), tolower(old)),
    to = file.path(Sys.getenv("CORPUS_REGISTRY"), tolower(new))
  )
  if (!success){
    stop("copying the registry file failed")
  } else {
    message("... copying registry file succeeded")
  }
  
  # modify the new registry file 
  .message("updating new registry file", verbose = verbose)
  newRegistry <- RegistryFile$new(new)
  newRegistry$setId(tolower(new))
  newRegistry$setHome(new = newDataDir)
  newRegistry$write()
}

#' @rdname install.corpus
#' @export rename.corpus
rename.corpus <- function(old, new, verbose = TRUE){
  
  # check that old corpus exists
  stopifnot(old %in% CQI$list_corpora())
  # check that new corpus does not yet exist
  if (toupper(new) %in% CQI$list_corpora()){
    stop("Corpus provided by 'new' already exists - do not overwrite an existing corpus")
  }
  
  # rename registry file
  .message("renaming registry file", verbose = verbose)
  registry_old <- file.path(Sys.getenv("CORPUS_REGISTRY"), tolower(old))
  registry_new <- file.path(dirname(registry_old), tolower(new))
  success <- file.rename(from = registry_old, to = registry_new)
  if (!success) stop("renaming the registry file failed")
  
  # rename data directory
  .message("renaming data directory", verbose = verbose)
  R <- RegistryFile$new(filename = registry_new)
  data_directory_old <- R$getHome()
  data_directory_new <- file.path(dirname(data_directory_old), tolower(new))
  success <- file.rename(from = data_directory_old, to = data_directory_new)
  if (!success) stop("renaming the data directory failed")
  
  # modify and save registry file
  .message("modifying and saving registry file", verbose = verbose)
  R$setHome(new = data_directory_new)
  R$setId(new = tolower(new))
  R$write()
}

#' @rdname install.corpus
#' @export remove.corpus
remove.corpus <- function(old){
  
  stopifnot(old %in% CQI$list_corpora()) # check that corpus exists
  
  data_directory <- RegistryFile$new(old)$getHome()
  if (readline(prompt = "Are you sure you want to delete the data directory (Y for yes): ") == "Y"){
    for (x in list.files(data_directory, full.names = TRUE)) file.remove(x)
    file.remove(data_directory)
  }
  if (readline(prompt = "Are you sure you want to delete the registry file (Y for yes): ") == "Y"){
    file.remove(file.path(Sys.getenv("CORPUS_REGISTRY"), tolower(old)))
  }
}

