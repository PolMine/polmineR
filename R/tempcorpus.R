#' @include S4classes.R
NULL

#' create a tempcorpus
#' 
#' Based on the corpus positions defining a partition, a temporary CWB corpus is 
#' generated that is stored in a temporary directory.
#' @param .Object a partition object
#' @param ... further parameters
#' @rdname tempcorpus
#' @name tempcorpus
setGeneric("tempcorpus", function(.Object, ...) standardGeneric("tempcorpus"))

setMethod("tempcorpus", "partition", function(.Object){
  tmpCorpusDir <- tempdir()
  tempcorpus <- new(
    "tempcorpus",
    dir = tmpCorpusDir,
    cpos = .Object@cpos,
    registry = file.path(tmpCorpusDir, "registry"),
    indexed = file.path(tmpCorpusDir, "indexed_corpora")
  )
  dir.create(tempcorpus@registry, showWarnings=FALSE)
  dir.create(tempcorpus@indexed, showWarnings=FALSE)
  cmdRaw <- c(
    "cwb-decode",
    "-s", as.character(.Object@cpos[1,1]),
    "-e", as.character(.Object@cpos[nrow(.Object@cpos), 2]),
    "-r", Sys.getenv("CORPUS_REGISTRY"),
    "-Cx", .Object@corpus,
    paste(unlist(lapply(p_attributes(.Object), function(x) c("-P", x))), collapse=" "),
    "|",
    "cwb-encode", "-d", tempcorpus@indexed, "-x", "-S", "corpus+name+start+end",
    "-R", file.path(tempcorpus@registry, "tmpcorpus")
  )
  cmd <- paste(cmdRaw, collapse=" ")
  system(cmd)
  cmdMake <- paste(c("cwb-make", "-r", tempcorpus@registry, "TMPCORPUS"), collapse=" ")
  system(cmdMake)
  tempcorpus
})

