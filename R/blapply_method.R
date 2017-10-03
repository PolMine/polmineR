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
#' @rdname blapply
#' @examples
#' \dontrun{
#'   use("polmineR.sampleCorpus")
#'   bt <- partition("PLPRBTTXT", list(text_id=".*"), regex=TRUE)
#'   speeches <- as.speeches(bt, sAttributeDates="text_date", sAttributeNames="text_name")
#'   foo <- blapply(speeches, function(x, ...) slot(x, "cpos"))
#' }
#' @importFrom pbapply pblapply
setGeneric("blapply", function(x, ...) standardGeneric("blapply"))

#' @rdname blapply
setMethod("blapply", "list", function(x, f, mc = TRUE, progress = TRUE, verbose = FALSE, ...){
  if (mc == FALSE){
    if (requireNamespace("pbapply", quietly = TRUE)){
      if (progress){
        arg_list <- c(list(X = x, FUN = f, cl = 1), list(...))
        if ("verbose" %in% names(formals(f)) || "..." %in% names(formals(f))) 
          arg_list <- c(list(verbose = FALSE), arg_list)
        retval <- do.call(what = pbapply::pblapply, args = arg_list)
        # retval <- pbapply::pblapply(X = x, FUN = f, ..., cl = 1)
      } else {
        retval <- lapply(X = x, FUN = f, ...)
      }
      return(retval)
    } else {
      if (progress){
        total <- length(x)
        pb <- txtProgressBar(min = 0, max = total, style = 3, width = getOption("width") - 10)
        i <- 0 # just to pass R CMD check
        retval <- lapply(
          1:length(x),
          function(i){
            setTxtProgressBar(pb, i)
            arg_list <- c(list(x[[i]], mc = FALSE, progress = FALSE), list(...))
            if ("verbose" %in% names(formals(f)) || "..." %in% names(formals(f))) 
              arg_list <- c(list(verbose = FALSE), arg_list)
            do.call(what = f, args = arg_list)
            # f(x[[i]], mc = FALSE, progress = FALSE, verbose = FALSE, ...)
          }
        )
        close(pb)
      } else {
        i <- 0 # just to pass R CMD check
        retval <- lapply(
          1:length(x),
          function(i){
            f(x[[i]], mc = FALSE, progress = FALSE, verbose = FALSE, ...)
          }
        )
      }
      return(retval)
    }
  } else {
    if (progress){
      if (requireNamespace("pbapply", quietly = TRUE)){
        
        arg_list <- c(list(X = x, FUN = f, cl = getOption("polmineR.cores")), list(...))
        if ("verbose" %in% names(formals(f)) || "..." %in% names(formals(f)))
          arg_list <- c(list(verbose = FALSE), arg_list)
        retval <- do.call(what = pbapply::pblapply, args = arg_list)
        return( retval )
        
      } else {
        stop("Package 'pbapply' needed but not installed to have progress bars and use multicore")
      }
    } else {
      if (requireNamespace("parallel", quietly = TRUE)){
        retval <- parallel::mclapply(X = x, FUN = f, ..., mc.cores = getOption("polmineR.cores"))
        return (retval)
      }
    }
  }
})


#' @rdname blapply
setMethod("blapply", "vector", function(x, f, mc=FALSE, progress=TRUE, verbose=FALSE, ...){
  blapply(as.list(x), f = f, mc = mc, progress = progress, verbose = verbose, ...)
})

#' @rdname blapply
setMethod("blapply", "bundle", function(x, f, mc = FALSE, progress = TRUE, verbose = FALSE, ...){
  L <- setNames(
    blapply(x@objects, f = f, mc = mc, progress = progress, verbose = verbose, ...),
    names(x)
  )
  if (all(sapply(L, function(x) "partition" %in% is(x)))){
    x@objects <- L
    return(x)
  } else {
    return(L)
  }
})
