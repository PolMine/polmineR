#' Install packaged corpus from repository. 
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
#' See examples how to check whether the directory has been set correctly. 
#' 
#' An installed data package with a CWB corpus is assumed to include a directory
#' \code{/extdata/cwb/registry} for registry files and a directory
#' \code{/extdata/cwb/indexed_corpora} for the inexed corpus files.
#' 
#' @param pkgs names of data packages with corpora
#' @param repo URL of the repository 
#' @param ... further parameters that will be passed into \code{install.packages}
#' @export install.corpus
#' @name install.corpus
#' @seealso For managing registry files, see class \code{\link{RegistryFile}}, see \code{\link{use}}
#' for switching to a packaged corpus. 
#' @examples
#' \donttest{
#' install.corpus("plprbt.pvs2017")
#' # is equivalent to:
#' install.corpus("plprbt.pvs2017", repo = "http://polmine.sowi.uni-due.de/packages")
#' RegistryFile(package = "plprbt.pvs2017")$adjustHome()
#' # check the directory that has been set
#' RegistryFile$new(package = "plprbt.pvs2017")$getHome()
#' }
#' @importFrom utils available.packages contrib.url install.packages
install.corpus <- function(pkgs, repo = "http://polmine.sowi.uni-due.de/packages", ...){
  for (package in pkgs){
    if (package %in% utils::available.packages(utils::contrib.url(repos = repo))){
      if ("lib" %in% names(list(...))){
        destdir <- list(...)[["lib"]]
      } else {
        destdir <- .libPaths()[1]
      }
      if (file.access(destdir, "6") == -1){
        stop("You do not have write permissions for directory ", destdir,
             ". Please run R with the required privileges, or provide another directory (param 'lib').")
      } else {
        install.packages(pkgs = package, repos = repo, ...)
        RegistryFile$new(package = package)$adjustHome()
      }
    } else {
      stop("package ", package, " is not available")
    }
  }
}
