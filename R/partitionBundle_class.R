#' @include partition_class.R bundle_class.R
NULL

#' Bundle of partitions (partitionBundle class).
#' 
#' Class and methods to manage bundles of partitions. 
#' 
#' @slot objects Object of class \code{"list"} the partitions making up the bundle
#' @slot corpus Object of class \code{"character"} the CWB corpus the partition is based on
#' @slot sAttributesFixed Object of class \code{"list"} fixed sAttributes
#' @slot encoding Object of class \code{"character"} encoding of the corpus
#' @slot explanation Object of class \code{"character"} an explanation of the partition
#' @slot xml Object of class \code{"character"} whether the xml is flat or nested
#' @slot call Object of class \code{"character"} the call that generated the partitionBundle
#' @aliases partitionBundle-class
#'   [,partitionBundle-method [[,partitionBundle-method
#'   as.matrix,partitionBundle-method 
#'   merge,partitionBundle-method
#'   +,partitionBundle-method names,partitionBundle-method 
#'   summary,partitionBundle-method +,partitionBundle,ANY-method
#'   [,partitionBundle,ANY,ANY,ANY-method +,partitionBundle,partition-method 
#'   +,partitionBundle,partitionBundle-method as.partitionBundle,list-method 
#'   barplot,partitionBundle-method
#' @param x a partitionBundle object
#' @param .Object a partitionBundle object
#' @param object a partitionBundle object
#' @param i integer index
#' @param sAttribute the s-attribute to use
#' @param height height
#' @param ... further parameters
#' @rdname partitionBundle-class
#' @name partitionBundle-class
#' @exportClass partitionBundle
#' @author Andreas Blaette
setClass("partitionBundle",
         representation(objects = "list", 
                        corpus = "character",
                        sAttributesFixed = "list",
                        encoding = "character",
                        explanation = "character",
                        xml = "character",
                        call = "character"
         ),
         contains = c("bundle")
)

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

#' @rdname partitionBundle-class
setMethod("summary", "partitionBundle", function (object) {
  summary <- data.frame(
    partition=names(object@objects),
    token=unlist(lapply(object@objects, function(x) x@size)),
    stringsAsFactors=FALSE
  )
  pAttr <- unique(unlist(lapply(object@objects, function(x) x@pAttribute)))
  if (length(pAttr) == 1){
    raw <- lapply(pAttr, function(x) unlist(lapply(object@objects, function(y) nrow(y@stat))))
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
#' @param x a bundle object
#' @param name the name for the new partition
#' @return An object of the class 'partition. See partition for the
#' details on the class.
#' @author Andreas Blaette
#' @exportMethod merge
#' @noRd
setMethod("merge", "partitionBundle", function(x, name = "", verbose = TRUE){
  y <- new("partition")
  .message('number of partitions to be merged: ', length(x@objects), verbose = verbose)
  y@corpus <- unique(vapply(x@objects, FUN.VALUE = "characer", function(p) p@corpus))
  if (length(y@corpus) >  1) warning("WARNING: This function will not work correctly, as the bundle comprises different corpora")
  y@xml <- unique(vapply(x@objects, function(p) p@xml, FUN.VALUE = "character"))
  y@encoding <- unique(vapply(x@objects, function(p) p@encoding, FUN.VALUE = "character"))
  y@sAttributeStrucs <- unique(vapply(x@objects, function(p) p@sAttributeStrucs, FUN.VALUE="character"))
  .message('merging the struc vectors', verbose = verbose)
  for (name in names(x@objects)) y@strucs <- union(y@strucs, x@objects[[name]]@strucs)
  .message('generating corpus positions', verbose = verbose)
  cpos <- data.matrix(t(data.frame(lapply(
    y@strucs,
    function(s) CQI$struc2cpos(y@corpus, y@sAttributeStrucs, s) )
  )))
  rownames(cpos) <- NULL
  y@cpos <- cpos
  y@size <- size(y)
  y@explanation = c(paste("this partition is a merger of the partitions", paste(names(x@objects), collapse=', ')))
  y@name <- name
  y
})



#' @exportMethod [
#' @rdname partitionBundle-class
setMethod('[', 'partitionBundle', function(x,i){
  a <- unname(unlist(lapply(x@objects, function(y) y@stat[i,2])))
  sizes <- unlist(lapply(x@objects, function(y) y@size))
  dist <- data.frame(
    partition=names(x@objects),
    count=a,
    freq=round(a/sizes*100000,2),
    row.names=c(1:length(x@objects))
  )
  dist
}
)


#' @exportMethod barplot
#' @rdname partitionBundle-class
setMethod("barplot", "partitionBundle", function(height, ...){
  tab <- summary(height)
  tab <- tab[order(tab[, "token"], decreasing=TRUE),]
  barplot(tab$token, names.arg=tab$partition, ...)
})


