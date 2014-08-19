#' Split partition into speeches
#' 
#' A partition is split into speeches. Method particularly for corpora from the
#' PolMine corpora of plenary protocols.
#' 
#' @param object a partition object
#' @param sAttribute the s-attribute that provides the speaker names
#' @param gap number of tokens between strucs to identify speeches
#' @param exclude names of speakers to exclude
#' @param addMeta whether to add metainformation to partition label, if wanted, provide s-attribute with date as character vector
#' @param mc whether to use multicore, defaults to FALSE
#' @param verbose logical, defaults to TRUE
#' @return a partitionCluster object
#' @include methods.R partition.R
#' @importFrom plyr llply
#' @name speeches
#' @aliases speeches speeches-method speeches,partition-method
#' @rdname speeches
#' @docType methods
#' @exportMethod speeches
setMethod("speeches", "partition", function(object, sAttribute, gap=500, exclude=NULL, addMeta=NULL, mc=FALSE, verbose=TRUE){
  names <- meta(object, sAttribute)
  message("... creating partitions for individual speakers")
  if (mc == FALSE){
    speakers <- lapply(names, function(name){
      who <- list()
      who[[sAttribute]] <- name
      zoom(object, def=who, tf=NULL, label=name)
    })
  } else if (mc == TRUE){
    speakers <- mclapply(names, function(name){
      who <- list()
      who[[sAttribute]] <- name
      zoom(object, def=who, tf=NULL, label=name)
      })
  }
  speakers <- as.partitionCluster(speakers)
  if (!is.null(exclude)) speakers <- trim(speakers, drop=exclude)
  if (verbose == TRUE) message("... splitting speaker partitions into speeches")
  if (mc == FALSE){
    bag <- lapply(speakers@partitions, function(p) {
      if (verbose == TRUE) message("... splitting partition ", p@label)
      split(p, gap=gap)
      })  
  } else {
    bag <- mclapply(speakers@partitions, function(p) split(p, gap=gap))      
  }
  message("... flattening data structure")
  bagFlat <- lapply(bag, function(x) x@partitions)  
  cluster <- do.call(c, unlist(bagFlat, recursive=FALSE))
  names(cluster) <- unname(unlist(lapply(cluster, function(x)x@label)))
  cluster <- as.partitionCluster(cluster)
  if (!is.null(addMeta)){
    if (verbose == TRUE) message("... enhancing partition labels")
    cluster@partitions <- llply(.data=cluster@partitions, .fun=function(x){
      labelOld <- x@label
      if (grepl("^.*(\\d+)$", labelOld)){
        no <- sub("^.*?(\\d+)$", "\\1", labelOld, perl=TRUE)
        txt <- sub("(.*?)\\d+$", "\\1", labelOld, perl=TRUE)
      }  else {
        no <- "1"
        txt <- labelOld
      }
      metadata <- vapply(addMeta, function(sAttr) meta(x, sAttr)[1], FUN.VALUE="character") 
      labelNew <- paste(txt, paste("|", paste(metadata, collapse="|"), "|", sep=""), no, sep="")
      x@label <- labelNew
      x
    })
    names(cluster@partitions) <- unname(unlist(lapply(cluster@partitions, function(x)x@label)))
  }
  cluster
})