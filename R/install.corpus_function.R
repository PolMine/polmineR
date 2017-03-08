#' Install packaged corpus from repository. 
#' 
#' Convenience function for making the installation of indexed CWB corpora
#' wrapped into R data packages as easy as possible.
#' 
#' @param pkgs names of data packages with corpora
#' @param repo URL of the repository 
#' @param ... further parameters that will be passed into \code{install.packages}
#' @export install.corpus
#' @examples
#' \dontrun{
#' }
install.corpus <- function(pkgs, repo = "http://polmine.sowi.uni-due.de/packages", ...){
  for (package in pkgs){
    if (package %in% available.packages(contrib.url(repos = repo))){
      install.packages(pkgs = package, repos = repo, ...)
      RegistryFile$new(package = package)$adjustHome()
    } else {
      stop("package ", package, " is not available")
    }
  }
}
