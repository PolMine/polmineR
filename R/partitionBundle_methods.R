#'@include partition_class.R partitionBundle_class.R
NULL

#' @docType methods
#' @rdname partitionBundle-class
setMethod("show", "partitionBundle", function (object) {
  cat('** PartitionBundle object: **\n')
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

#' Summary method for partitionBundle Objects 
#'
#' simple statistic 
#' 
#' @param object the partitionBundle object
#' @exportMethod summary
#' @docType methods
#' @noRd
setMethod("summary", "partitionBundle", function (object) {
  summary <- data.frame(
    partition=names(object@objects),
    token=unlist(lapply(object@objects, function(x) x@size)),
    stringsAsFactors=FALSE
   )
  pAttr <- unique(unlist(lapply(object@objects, function(x) x@pAttribute)))
  if (length(pAttr == 1)){
    raw <- lapply(pAttr, function(x) unlist(lapply(object@objects, function(y) nrow(y@tf))))
    raw <- do.call(data.frame, raw)
    colnames(raw) <- paste("unique_", pAttr, sep="")
    summary <- data.frame(summary, raw, stringsAsFactors=FALSE)
  }
  rownames(summary) <- c(1:nrow(summary))
  summary
})




#' Merge the partitions in a bundle into one partition
#' 
#' The partitions in a bundle object will be merged into one new partition
#' 
#' The function aggregates several partitions into one partition. The
#' prerequisite for this function to work properly is that there are no
#' overlaps of the different partitions that are to be summarized.
#' Encodings and the root node need to be identical, too.
#' 
#' @param object a bundle object
#' @param name the name for the new partition
#' @return An object of the class 'partition. See partition for the
#' details on the class.
#' @author Andreas Blaette
#' @exportMethod merge
#' @docType methods
#' @noRd
setMethod("merge", "partitionBundle", function(x, name=c("")){
  y <- new("partition")
  cat('There are', length(x@objects), 'partitions to be merged\n')
  y@corpus <- unique(vapply(x@objects, FUN.VALUE="characer", function(p) p@corpus))
  if (length(y@corpus) >  1) warning("WARNING: This function will not work correctly, as the bundle comprises different corpora")
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
  y@name <- name
  y
})


#' @exportMethod [[
#' @docType methods
#' @rdname partitionBundle-class
setMethod('[[', 'partitionBundle', function(x,i){
  return(x@objects[[i]])
}
)

#' @exportMethod [
#' @rdname partitionBundle-class
#' @docType methods
setMethod('[', 'partitionBundle', function(x,i){
  a <- unname(unlist(lapply(x@objects, function(y) y@tf[i,2])))
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

#' @exportMethod +
#' @docType methods
#' @rdname bundle-class
setMethod("+", signature(e1="partitionBundle", e2="partition"), function(e1, e2){
  if (e1@corpus != e2@corpus) warning("Please be careful - partition is from a different CWB corpus")
  e1@objects[[length(e1@objects)+1]] <- e2
  names(e1@objects)[length(e1@objects)] <- e2@name
  e1
})



#' Turn a partition bundle into a matrix
#' 
#' Method based on the tm package.
#' 
#' The partitions need to be derived from the same corpus (because the lexicon of the corpus is used).
#' 
#' @param x a partitionBundle object (S3 class)
#' @param pAttribute the counts for the patttribute to show up in the matrix
#' @param weight whether to introduce a weight ("tfidf", for example)
#' @param ... necessary for S3 method?!
#' @method as.matrix partitionBundle
#' @return a matrix
#' @author Andreas Blaette
#' @exportMethod as.matrix
#' @docType methods
#' @noRd
setMethod("as.matrix", "partitionBundle", function(x, pAttribute, weight=NULL, rmBlank=TRUE, ...) {
  as.matrix(as.TermDocumentMatrix(x, pAttribute, weight, rmBlank))
})



# setMethod("plot", signature(x="partitionBundle"),
#           function(x, y){
#             val <- as.matrix(x, y)
#             val <- val[rowSums(val)!=0,]
#             data <- data.frame(rank(val[,1]), rank(val[,2]))
#             plot(data[,1], data[,2])
#           })

#' barplot of a partitionBundle
#' 
#' @param pBundle a partitionBundle object
#' @exportMethod barplot
#' @noRd
setMethod("barplot", "partitionBundle", function(height, ...){
  tab <- summary(height)
  tab <- tab[order(tab[, "token"], decreasing=TRUE),]
  barplot(tab$token, names.arg=tab$partition, ...)
})


#' @rdname partitionBundle-class
setMethod("names", "partitionBundle", function(x){
  unname(unlist(lapply(x@objects, function(y) name(y))))
})

#' @rdname partitionBundle-class
#' @docType methods
#' @exportMethod names<-
setReplaceMethod(
  "names",
  signature=c(x="partitionBundle", value="character"),
  function(x, value) {
    if ( length(value) != length(x@objects) ) {
      warning("length of value provided does not match number of partitions")
      stop()
    }
    if ( !is.character(name(x)) ){
      warning("value needs to be a character vector")
      stop()
    }
    for (i in c(1:length(x@objects))){
      x@objects[[i]]@name <- value[i]
    }
    names(x@objects) <- value
    x
  }
)

#' @exportMethod unique
#' @docType methods
#' @rdname partitionBundle-class
setMethod("unique", "partitionBundle", function(x){
  partitionNames <- lapply(x@objects, function(p) p@name)
  uniquePartitionNames <- unique(unlist(partitionNames))
  uniquePartitionsPos <- sapply(uniquePartitionNames, function(x) grep(x, partitionNames)[1])
  partitionsToDrop <- which(c(1:length(partitionNames)) %in% uniquePartitionsPos == FALSE)
  partitionsToDrop <- partitionsToDrop[order(partitionsToDrop, decreasing=TRUE)]
  for (pos in partitionsToDrop) x@objects[pos] <- NULL
  x
})

#' @exportMethod length
#' @rdname partitionBundle-class
setMethod("length", "partitionBundle", function(x) length(x@objects))


