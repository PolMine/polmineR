#'@include partition-class.R partitionCluster-class.R
NULL

#' Show method for partitionCluster Objects 
#'
#' Prints the number of partitions in the cluster and returns the respective sizes
#' 
#' @param object the partitionCluster object
#' @exportMethod show
#' @docType methods
#' @noRd
setMethod("show", "partitionCluster", function (object) {
  cat('** PartitionCluster object: **\n')
  cat(sprintf('%-25s', 'Number of partitions:'), length(object@objects), '\n')
  # same code as in show-method for partition
  sFix <- unlist(lapply(
    names(object@sAttributesFixed),
    function(x) {paste(x, "=", paste(object@sAttributesFixed[[x]], collapse="/"))}
  ))
  cat(sprintf("%-25s", "sAttributes Fixed:"), sFix[1], '\n')
  if (length(sFix)>1) {for (i in length(sFix)){cat(sprintf("%-25s", " "), sFix[i], '\n')}}
  cat("\n")
})

#' Summary method for partitionCluster Objects 
#'
#' simple statistic 
#' 
#' @param object the partitionCluster object
#' @exportMethod summary
#' @docType methods
#' @noRd
setMethod("summary", "partitionCluster", function (object) {
  summary <- data.frame(
    partition=names(object@objects),
    token=unlist(lapply(object@objects, function(x) x@size)),
    stringsAsFactors=FALSE
   )
  pAttr <- unique(unlist(lapply(object@objects, function(x) names(x@tf))))
  if (!is.null(pAttr)){
    raw <- lapply(pAttr, function(x) unlist(lapply(object@objects, function(y) nrow(y@tf[[x]]))))
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
#' @docType methods
#' @noRd
setMethod("merge", "partitionCluster", function(x, label=c("")){
  y <- new("partition")
  cat('There are', length(x@objects), 'partitions to be merged\n')
  y@corpus <- unique(vapply(x@objects, FUN.VALUE="characer", function(p) p@corpus))
  if (length(y@corpus) >  1) warning("WARNING: This function will not work correctly, as the cluster comprises different corpora")
  y@xml <- unique(vapply(x@objects, function(p)p@xml, FUN.VALUE="character"))
  y@encoding <- unique(vapply(x@objects, function(p)p@encoding, FUN.VALUE="character"))
  y@sAttributeStrucs <- unique(vapply(x@objects, function(p) p@sAttributeStrucs, FUN.VALUE="character"))
  message('... merging the struc vectors')
  for (name in names(x@objects)) {y@strucs <- union(y@strucs, x@objects[[name]]@strucs)}
  message('... generating corpus positions')
  cpos <- data.matrix(t(data.frame(lapply(
    y@strucs,
    function(s){cqi_struc2cpos(paste(y@corpus,'.', y@sAttributeStrucs, sep=''),s)})
    )))
  rownames(cpos) <- NULL
  y@cpos <- cpos
  y@explanation=c(paste("this partition is a merger of the partitions", paste(names(x@objects), collapse=', ')))
  y@label <- label
  y
})


#' @exportMethod [[
#' @docType methods
#' @rdname partitionCluster-class
setMethod('[[', 'partitionCluster', function(x,i){
  return(x@objects[[i]])
}
)

#' @exportMethod [
#' @docType methods
setMethod('[', 'partitionCluster', function(x,i){
  a <- unname(unlist(lapply(x@objects, function(y) y@tf$word[i,2])))
  sizes <- unlist(lapply(x@objects, function(y) y@size))
  dist <- data.frame(
    partition=names(x@objects),
    tfAbs=a,
    tfRel=round(a/sizes*100000,2),
    row.names=c(1:length(x@objects))
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
#' @docType methods
#' @noRd
setMethod("as.matrix", "partitionCluster", function(x, pAttribute, weight=NULL, rmBlank=TRUE, ...) {
  as.matrix(as.TermDocumentMatrix(x, pAttribute, weight, rmBlank))
})


#' @exportMethod +
#' @docType methods
setMethod("+", signature(e1="partitionCluster", e2="partitionCluster"), function(e1, e2){
  newPartition <- new("partitionCluster")
  newPartition@objects <- c(e1@objects, e2@objects)
  corpus <- unique(e1@corpus, e2@corpus)
  encoding <- unique(e1@encoding, e2@encoding)
  explanation <- paste(e1@explanation, e2@explanation)
  xml <- "not available"
  newPartition
})

#' @exportMethod +
#' @docType methods
setMethod("+", signature(e1="partitionCluster", e2="partition"), function(e1, e2){
  if (e1@corpus != e2@corpus) warning("Please be careful - partition is from a different CWB corpus")
  e1@objects[[length(e1@objects)+1]] <- e2
  names(e1@objects)[length(e1@objects)] <- e2@label
  e1
})


# setMethod("plot", signature(x="partitionCluster"),
#           function(x, y){
#             val <- as.matrix(x, y)
#             val <- val[rowSums(val)!=0,]
#             data <- data.frame(rank(val[,1]), rank(val[,2]))
#             plot(data[,1], data[,2])
#           })

#' barplot of a partitionCluster
#' 
#' @param pCluster a partitionCluster object
#' @exportMethod barplot
#' @noRd
setMethod("barplot", "partitionCluster", function(height, ...){
  tab <- summary(height)
  tab <- tab[order(tab[, "token"], decreasing=TRUE),]
  barplot(tab$token, names.arg=tab$partition, ...)
})


#' @rdname partitionCluster
setMethod("label", "partitionCluster", function(object){
  unname(unlist(lapply(object@objects, function(x) label(x))))
})

#' @rdname partitionCluster
#' @exportMethod label<-
setReplaceMethod(
  "label",
  signature=c(object="partitionCluster", value="character"),
  function(object, value) {
    if ( length(value) != length(object@objects) ) {
      warning("length of value provided does not match number of partitions")
      stop()
    }
    if ( !is.character(label(object)) ){
      warning("value needs to be a character vector")
      stop()
    }
    for (i in c(1:length(object@objects))){
      object@objects[[i]]@label <- value[i]
    }
    names(object@objects) <- value
    object
  }
)

#' @exportMethod unique
#' @rdname partitionCluster
setMethod("unique", "partitionCluster", function(x){
  labels <- lapply(x@objects, function(p) p@label)
  uniqueLabels <- unique(unlist(labels))
  uniquePartitionsPos <- sapply(uniqueLabels, function(x) grep(x, labels)[1])
  partitionsToDrop <- which(c(1:length(labels)) %in% uniquePartitionsPos == FALSE)
  partitionsToDrop <- partitionsToDrop[order(partitionsToDrop, decreasing=TRUE)]
  for (pos in partitionsToDrop) x@objects[pos] <- NULL
  x
})

#' @exportMethod length
#' @rdname partitionCluster-class
setMethod("length", "partitionCluster", function(x) length(x@objects))
