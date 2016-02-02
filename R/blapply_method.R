#' apply a function over a list or bundle with and without verbose parallelization
#' 
#' @param f a function that can be applied to each object contained in the bundle
#' @param ... further parameters
#' @rdname blapply
#' @exportMethod blapply
#' @importFrom foreach %do%
#' @importFrom foreach %dopar%
#' @importFrom foreach foreach
#' @rdname blapply
setGeneric("blapply", function(x, ...) standardGeneric("blapply"))

#' @rdname blapply
setMethod("blapply", "list", function(x, f, mc=TRUE, progress=TRUE, verbose=FALSE, ...){
  if (mc == FALSE){
    # retval <- lapply(X=x@objects, FUN=f, ...)
    total <- length(x)
    pb <- txtProgressBar(min = 0, max = total, style = 3)
    # progressBar <- function(n) setTxtProgressBar(pb, n)
    retval <- foreach(i=c(1:length(x))) %do% {
      setTxtProgressBar(pb, i)
      f(x[[i]], mc=FALSE, progress=FALSE, verbose=FALSE, ...)
    }
  } else {
    backend <- slot(get("session", ".GlobalEnv"), "backend")
    stopifnot(backend %in% c("doMC", "doSNOW", "doMPI", "doRedis"))
    if (requireNamespace(backend, quietly=TRUE)){
      if (backend == "doSNOW"){
        pb <- txtProgressBar(min = 0, max = length(x), style = 3)
        progressBar <- function(n) setTxtProgressBar(pb, n)
        cl <- snow::makeSOCKcluster(slot(get("session", ".GlobalEnv"), "cores"))
        doSNOW::registerDoSNOW(cl)
        snow::clusterCall(cl, function() library(polmineR))
        snow::clusterExport(cl, names(list(...)))
        retval <- foreach(
          i=c(1:length(x)),
          .options.snow=list(progress=progressBar)
        ) %dopar% {
          f(x[[i]], mc=FALSE, progress=FALSE, verbose=FALSE, ...)
        }
        snow::stopCluster(cl)
        close(pb)
      } else if (backend == "doMC"){
        doMC::registerDoMC(cores=slot(get("session", ".GlobalEnv"), "cores"))  
        retval <- foreach(i=c(1:length(x))) %dopar% {
          f(x[[i]], mc=FALSE, progress=FALSE, verbose=FALSE, ...)
        }
      }
    }
  }
  retval
})

#' @rdname blapply
setMethod("blapply", "bundle", function(x, f, mc=FALSE, progress=TRUE, verbose=FALSE, ...){
  x@objects <- blapply(x@objects, f=f, mc=mc, progress=progress, verbose=verbose, ...)
})

