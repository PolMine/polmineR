#'@include partition.R partitionCluster-class.R
NULL

#' Show method for partitionCluster Objects 
#'
#' Prints the number of partitions in the cluster and returns the respective sizes
#' 
#' @param object the partitionCluster object
#' @exportMethod show
#' @noRd
setMethod("show", "partitionCluster", function (object) {
  stat <- summary(object)
  cat('** PartitionCluster object: **\n')
  cat(sprintf('%-25s', 'Number of partitions:'), length(object@partitions), '\n')
  # same code as in show-method for partition
  sFix <- unlist(lapply(
    names(object@sAttributesFixed),
    function(x) {paste(x, "=", paste(object@sAttributesFixed[[x]], collapse="/"))}
  ))
  cat(sprintf("%-25s", "sAttributes Fixed:"), sFix[1], '\n')
  if (length(sFix)>1) {for (i in length(sFix)){cat(sprintf("%-25s", " "), sFix[i], '\n')}}
  cat("\n")
  print(stat, quote=FALSE)
})

#' Summary method for partitionCluster Objects 
#'
#' simple statistic 
#' 
#' @param object the partitionCluster object
#' @exportMethod summary
#' @noRd
setMethod("summary", "partitionCluster", function (object) {
  summary <- data.frame(
    partition=names(object@partitions),
    token=unlist(lapply(object@partitions, function(x) x@size)),
    stringsAsFactors=FALSE
   )
  pAttr <- unique(unlist(lapply(object@partitions, function(x) names(x@tf))))
  if (!is.null(pAttr)){
    raw <- lapply(pAttr, function(x) unlist(lapply(object@partitions, function(y) nrow(y@tf[[x]]))))
    raw <- do.call(data.frame, raw)
    colnames(raw) <- paste("unique_", pAttr, sep="")
    summary <- data.frame(summary, raw, stringsAsFactors=FALSE)
  }
  rownames(summary) <- c(1:nrow(summary))
  summary
})




#' Merge the partitions in a cluster into one partition
#' 
#' The partitions in a cluster object will be merged into one new partition
#' 
#' The function aggregates several partitions into one partition. The
#' prerequisite for this function to work properly is that there are no
#' overlaps of the different partitions that are to be summarized.
#' Encodings and the root node need to be identical, too.
#' 
#' @param object a cluster object
#' @param label the label for the new partition
#' @return An object of the class 'partition. See partition for the
#' details on the class.
#' @author Andreas Blaette
#' @exportMethod merge
#' @noRd
setMethod("merge", "partitionCluster", function(x, label=c("")){
  y <- new("partition")
  cat('There are', length(x@partitions), 'partitions to be merged\n')
  y@corpus <- unique(vapply(x@partitions, FUN.VALUE="characer", function(p) p@corpus))
  if (length(y@corpus) >  1) warning("WARNING: This function will not work correctly, as the cluster comprises different corpora")
  y@xml <- unique(vapply(x@partitions, function(p)p@xml, FUN.VALUE="character"))
  y@encoding <- unique(vapply(x@partitions, function(p)p@encoding, FUN.VALUE="character"))
  y@sAttributeStrucs <- unique(vapply(x@partitions, function(p) p@sAttributeStrucs, FUN.VALUE="character"))
  message('... merging the struc vectors')
  for (name in names(x@partitions)) {y@strucs <- union(y@strucs, x@partitions[[name]]@strucs)}
  message('... generating corpus positions')
  cpos <- data.matrix(t(data.frame(lapply(
    y@strucs,
    function(s){cqi_struc2cpos(paste(y@corpus,'.', y@sAttributeStrucs, sep=''),s)})
    )))
  rownames(cpos) <- NULL
  y@cpos <- cpos
  y@explanation=c(paste("this partition is a merger of the partitions", paste(names(x@partitions), collapse=', ')))
  y@label <- label
  y
})

#' @exportMethod [[
setMethod('[[', 'partitionCluster', function(x,i){
  return(x@partitions[[i]])
}
)

#' @exportMethod [
setMethod('[', 'partitionCluster', function(x,i){
  a <- unname(unlist(lapply(x@partitions, function(y) y@tf$word[i,2])))
  sizes <- unlist(lapply(x@partitions, function(y) y@size))
  dist <- data.frame(
    partition=names(x@partitions),
    tfAbs=a,
    tfRel=round(a/sizes*100000,2),
    row.names=c(1:length(x@partitions))
    )
  dist
}
)


#' Turn a partition cluster into a matrix
#' 
#' Method based on the tm package.
#' 
#' The partitions need to be derived from the same corpus (because the lexicon of the corpus is used).
#' 
#' @param x a partitionCluster object (S3 class)
#' @param pAttribute the counts for the patttribute to show up in the matrix
#' @param weight whether to introduce a weight ("tfidf", for example)
#' @param ... necessary for S3 method?!
#' @method as.matrix partitionCluster
#' @return a matrix
#' @author Andreas Blaette
#' @exportMethod as.matrix
#' @noRd
setMethod("as.matrix", "partitionCluster", function(x, pAttribute, weight=NULL, rmBlank=TRUE, ...) {
  as.matrix(as.TermDocumentMatrix(x, pAttribute, weight, rmBlank))
})


#' @exportMethod names
setMethod("names", "partitionCluster", function(x){
  names(x@partitions)
})

# '@include partition.R
#' @exportMethod +
setMethod("+", signature(e1="partitionCluster", e2="partitionCluster"), function(e1, e2){
  newPartition <- new("partitionCluster")
  newPartition@partitions <- c(e1@partitions, e2@partitions)
  corpus <- unique(e1@corpus, e2@corpus)
  encoding <- unique(e1@encoding, e2@encoding)
  explanation <- paste(e1@explanation, e2@explanation)
  xml <- "not available"
  newPartition
})

#' @exportMethod +
#' @include partition.R
setMethod("+", signature(e1="partitionCluster", e2="partition"), function(e1, e2){
  if (e1@corpus != e2@corpus) warning("Please be careful - partition is from a different CWB corpus")
  e1@partitions[[length(e1@partitions)+1]] <- e2
  names(e1@partitions)[length(e1@partitions)] <- e2@label
  e1
})


