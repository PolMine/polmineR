#' Split partition into speeches
#' 
#' This method is designed for corpora from the PolMine corpora of plenary protocols.
#' 
#' @param object a partition object
#' @param sAttribute the s-attribute that provides the speaker names
#' @param gap number of tokens between strucs to identify speeches
#' @param exclude names of speakers to exclude
#' @param addMeta whether to add metainformation to partition label, if wanted, provide s-attribute with date as character vector
#' @param mc whether to use multicore, defaults to FALSE
#' @param verbose logical, defaults to TRUE
#' @return a partitionCluster object
#' @include generics.R partition.R
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
  message("... splitting speaker partitions into speeches")
  bag <- lapply(speakers@partitions, function(p) split(p, gap=gap))  
  cluster <- new("partitionCluster")
  for (i in 1:length(bag)) cluster <- cluster + bag[[i]]
  if (!is.null(addMeta)){
    for (i in 1:length(cluster@partitions)){
      labelOld <- cluster@partitions[[i]]@label
      if (grepl("^.*(\\d+)$", labelOld)){
        no <- sub("^.*?(\\d+)$", "\\1", labelOld, perl=TRUE)
        txt <- sub("(.*?)\\d+$", "\\1", labelOld, perl=TRUE)
      }  else {
        no <- "1"
        txt <- labelOld
      }
      metadata <- vapply(addMeta, function(m) meta(cluster@partitions[[i]], m)[1], FUN.VALUE="character") 
      labelNew <- paste(txt, paste("|", paste(metadata, collapse="|"), "|", sep=""), no, sep="")
      cluster@partitions[[i]]@label <- labelNew
      names(cluster@partitions)[i] <- labelNew
    }
  }
  cluster
})