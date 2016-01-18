#' mclapply with progress
#' 
#' @param index index
#' @param f function
#' @param verbose logical
#' @param mc logical
#' @param param list
#' @rdname papply
#' @name papply
#' @export papply
papply <- function(index, f, verbose=FALSE, mc=TRUE, param=list()){
  if (is.numeric(mc) == TRUE){
    noCores <- mc
    if (verbose == TRUE) message("... using ", noCores, " cores")
  } else {
    noCores <- 3  
    if (verbose == TRUE) message("... number of cores not provided explicitly, using ", noCores, " cores")
  }
  breaksRaw <- unlist(lapply(c(1:noCores), function(x) rep(x, times=trunc(length(index) / noCores))))
  breaks <- c(breaksRaw, rep(noCores, times=length(index)-length(breaksRaw)))
  indexChunks <- split(index, breaks)
  dummyDir <- tempdir()
  filesRemoved <- file.remove(list.files(dummyDir, full.names=T, pattern="\\.multicore", include.dirs=FALSE))
  if (verbose == TRUE) message ("... cleaning tempdir (", length(filesRemoved), " files removed)")
  fWrapped <- function(i, verbose, param){
    cat("", file=file.path(dummyDir, paste(as.character(i), "multicore", sep=".")))
    f(i, verbose=verbose, param=param)
  }
  if (length(parallel:::children()) > 0){
    warning("there have been zombie processes collected with mccollect()")
    graveyard <- mccollect()
  }
  threadNames <- paste("thread", c(1:noCores), sep="")
  startTime <- Sys.time()
  for (i in c(1:noCores)){
    assign(
      threadNames[i],
      mcparallel(
        lapply(
          indexChunks[[i]],
          function(i) fWrapped(i, verbose=FALSE, param=param)
        )),
      envir=environment())
  }
  while (length(parallel:::selectChildren()) < noCores){
    filesTargetDir <- list.files(path=dummyDir, pattern="\\.multicore")
    .progressBar(length(filesTargetDir), length(index))
    Sys.sleep(1)
  }
  retval <- parallel::mccollect(
    jobs=lapply(threadNames, function(x) get(x, envir=environment())),
    wait=TRUE
  )
  filesTargetDir <- list.files(path=dummyDir, pattern="\\.multicore")
  .progressBar(length(filesTargetDir), length(index))
  dummy <- file.remove(list.files(dummyDir, full.names=T, pattern="\\.multicore", include.dirs=FALSE))
  retval <- unlist(retval, recursive = FALSE)
  # names(retval)
  retval
}
