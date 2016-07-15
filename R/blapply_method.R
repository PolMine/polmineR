#' apply a function over a list or bundle
#' 
#' Very similar to lapply, but applicable to bundle-objects, in particular.
#' The purpose of the method is to supply a uniform und convenient parallel
#' backend for the polmineR package. In particular, progress bars are supported
#' (the naming of the method is derived from bla bla).
#' 
#' Parallel backend supported so far are the parallel package (mclapply), and 
#' doMC, doParallel and doSNOW in combination with foreach. The parallel backend
#' to be used is taken from the option 'polmineR.backend' (getOption("polmineR.backend")),
#' the number of cores from the option 'polmineR.cores' (getOption("polmineR.cores")).
#' @param x a list or a bundle object
#' @param f a function that can be applied to each object contained in the
#'   bundle, note that it should swallow the parameters mc, verbose and progress
#'   (use ... to catch these params )
#' @param mc logical, whether to use multicore - if TRUE, the number of cores
#'   will be taken from the polmineR-options
#' @param progress logical, whether to display progress bar
#' @param verbose logical, whether to print intermediate messages
#' @param ... further parameters
#' @rdname blapply
#' @exportMethod blapply
#' @importFrom foreach foreach %dopar%
#' @rdname blapply
#' @examples
#' if (require(polmineR.sampleCorpus) && require(rcqp)){
#'   use(polmineR.sampleCorpus)
#'   bt <- partition("PLPRBTTXT", list(text_id=".*"), regex=TRUE)
#'   speeches <- as.speeches(bt, sAttributeDates="text_date", sAttributeNames="text_name")
#'   foo <- blapply(speeches, function(x, ...) slot(x, "cpos"))
#' }
setGeneric("blapply", function(x, ...) standardGeneric("blapply"))

#' @rdname blapply
setMethod("blapply", "list", function(x, f, mc=TRUE, progress=TRUE, verbose=FALSE, ...){
  if (mc == FALSE){
    total <- length(x)
    if (progress) pb <- txtProgressBar(min = 0, max = total, style = 3)
    i <- 0 # just to pass R CMD check
    retval <- lapply(
      c(1:length(x)),
      function(i){
        if (progress) setTxtProgressBar(pb, i)
        f(x[[i]], mc=FALSE, progress=FALSE, verbose=FALSE, ...)
      }
    )
    if (progress) close(pb)
  } else {
    backend <- getOption("polmineR.backend")
    stopifnot(backend %in% c("parallel", "doParallel", "doSNOW", "doMC"))
    
    if (requireNamespace(backend, quietly=TRUE)){
      if (backend == "parallel"){
        if (progress == FALSE){
          retval <- parallel::mclapply(x, function(y) f(y, mc=FALSE, progress=FALSE, verbose=FALSE, ...))
        } else {
          stop("progress for parallel backend not yet implemented")
        }
      } else if (backend == "doParallel"){
        cl <- makeCluster(getOption("polmineR.cores"))
        doParallel::registerDoParallel(cl)
        retval <- foreach(i=c(1:length(x))) %dopar% {
          f(x[[i]], mc=FALSE, progress=FALSE, verbose=FALSE, ...)
        }
        stopCluster(cl)
      } else if (backend == "doSNOW"){
        cl <- snow::makeSOCKcluster(getOption("polmineR.cores"))
        doSNOW::registerDoSNOW(cl)
        snow::clusterEvalQ(cl, library(polmineR))
        for (i in names(list(...))) assign(i, value=list(...)[[i]], envir=environment())
        snow::clusterExport(cl, names(list(...)), envir=environment())
        if (progress == TRUE){
          pb <- txtProgressBar(min = 0, max = length(x), style = 3)
          progressBar <- function(n) setTxtProgressBar(pb, n)
          retval <- foreach(
            i=c(1:length(x)),
            .options.snow=list(progress=progressBar)
          ) %dopar% f(x[[i]], mc=FALSE, progress=FALSE, verbose=FALSE, ...)
          close(pb)
        } else {
          retval <- foreach(i=c(1:length(x))) %dopar% {
            f(x[[i]], mc=FALSE, progress=FALSE, verbose=FALSE, ...)
          }
        }
        snow::stopCluster(cl)
      } else if (backend == "doMC"){
        doMC::registerDoMC(cores=getOption("polmineR.cores"))  
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
  x@objects <- setNames(
    blapply(x@objects, f=f, mc=mc, progress=progress, verbose=verbose, ...),
    names(x)
  )
  x
})

