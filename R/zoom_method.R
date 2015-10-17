#' @include partition_class.R
NULL

setGeneric("zoom", function(object, ...){standardGeneric("zoom")})

#' zoom into a partition
#' 
#' add a further specification of a s-attribute to an existing partition
#' 
#' @param object a partition object
#' @param def a list supplying further sAttributes
#' @param name a name for the new partition
#' @param method either "in" or "grep"
#' @param tf character vector, pAttributes for which term frequencies shall be retrieved
#' @param id2str whether to transfer ids to strings
#' @param type the type of the resulting partition
#' @param verbose logical, show progress report or not (defaults to TRUE)
#' @aliases zoom zoom,partition-method
#' @exportMethod zoom
#' @docType methods
#' @name zoom
setMethod("zoom", "partition", function(object, def, name=c(""), method="in", tf=c("word", "lemma"), id2str=TRUE, type=NULL, verbose=TRUE){
  # these lines are identical with partition method
  if (is.null(type)){
    newPartition <- new('partition')  
  } else {
    pkgName <- paste("polmineR.", type, sep="")
    cName <- paste(type, "Partition", sep="")
    if (requireNamespace(pkgName, quietly=TRUE)){
      newPartition <- new(cName)
    } else {
      warning("to set a specific partition type, the respective package needs to be available")
    }
  }
  newPartition@corpus <- object@corpus
  message('Zooming into partition ', name)
  newPartition@name <- name
  def <- lapply(def, function(x).adjustEncoding(x, object@encoding))  
  newPartition@sAttributes <- c(object@sAttributes, def)
  newPartition@sAttributeStrucs <- names(newPartition@sAttributes)[length(newPartition@sAttributes)]
  newPartition@xml <- object@xml
  newPartition@encoding <- object@encoding
  message('... specifying strucs and corpus positions')
  newPartition <- .zoomingSattributes2cpos(object, newPartition, def, method)
  message('... computing partition size')
  newPartition@size <- size(newPartition)
  if (length(tf)>0) {
    for (p in tf){
      if (verbose==TRUE) message('... computing term frequencies (for p-attribute ', p, ')')  
      newPartition@tf[[p]] <- .cpos2tf(newPartition, p, id2str=id2str)
    }
  }
  newPartition
})

#' Augment the partition object by strucs and cpos
#' 
#' @param Partition the partition object to be specified
#' @param newPartition the new partition
#' @param sAttribute info for specification (a list)
#' @param method either "in" or "grep"
#' @return an augmented partition object
#' @noRd
.zoomingSattributes2cpos <- function(Partition, newPartition, sAttribute, method){
  sAttr <- paste(Partition@corpus, '.', names(sAttribute), sep='')
  if (Partition@xml == "flat") {
    str <- cqi_struc2str(sAttr, Partition@strucs)    
  } else if (Partition@xml == "nested") {
    str <- cqi_struc2str(sAttr, cqi_cpos2struc(sAttr, Partition@cpos[,1]))    
  }
  Encoding(str) <- newPartition@encoding
  if (method == "in") {
    hits <- which(str %in% sAttribute[[1]])
  } else if (method == "grep") {
    hits <- grep(sAttributes[[1]], str)
  }
  newCpos <- Partition@cpos[hits,]
  if (class(newCpos) == "matrix"){
    newPartition@cpos <- newCpos
  } else if (class(newCpos) == "integer") {
    newPartition@cpos <- matrix(newCpos, ncol=2, byrow=TRUE)     
  }
  newPartition@strucs <- Partition@strucs[hits]
  if (length(Partition@metadata) == 2) {
    message('... adjusting metadata')
    newPartition@metadata$table <- Partition@metadata$table[hits,]
    newPartition <- .partitionMetadataValues(newPartition)
  }
  newPartition
}

