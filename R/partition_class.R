#' @include polmineR_package.R textstat_class.R generics.R
NULL



# this file includes the partition class, the constructor function 'partition'
# for generating the partition class, and the helper functions used 
# by the constructur


#' Partition class and methods.
#' 
#' S4 partition class and methods for instances of class partition.
#' 
#' @slot name Object of class \code{"character"} a name that may be useful 
#' @slot corpus Object of class \code{"character"} the CWB corpus the partition is based on 
#' @slot encoding Object of class \code{"character"} encoding of the corpus 
#' @slot sAttributes Object of class \code{"list"} s-attributes specifying the partition 
#' @slot explanation Object of class \code{"character"} an explanation of the partition 
#' @slot cpos Object of class \code{"matrix"} corpus positions
#' @slot annotations Object of class \code{"list"}
#' @slot pos Object of class \code{"list"} with tables "abs", "rel" and "max"
#' @slot size Object of class \code{"numeric"} total size of the partition 
#' @slot metadata Object of class \code{"data.frame"} metadata information 
#' @slot strucs Object of class \code{"numeric"} the strucs defining the partition 
#' @slot pAttribute Object of class \code{"character"} indicating the pAttribute of the
#' count in slot tf
#' @slot xml Object of class \code{"character"} whether the xml is flat or nested 
#' @slot sAttributeStrucs Object of class \code{"character"} the base node 
#' @slot call Object of class \code{"character"} the call that generated the partition 
#' @param .Object a partition object
#' @param pAttribute a p-attribute (for enriching)
#' @param x a partition object
#' @param verbose logical
#' @param cpos ...
#' @param html ...
#' @param highlight ... 
#' @param cqp ...
#' @param tooltips ...
#' @param meta ...
#' @param cutoff maximum number of tokens to decode
#' @param ... further parameters
#' @param value value
#' @param template template to use
#' @aliases partition-class show,partition-method [,partition,ANY,ANY,ANY-method 
#'   [,partition-method as.partitionBundle 
#'   as.partitionBundle,partition-method export export,partition-method split
#' @rdname partition_class
#' @name partition_class
#' @exportClass partition
#' @docType class
#' @author Andreas Blaette
#' @seealso The \code{partition}-class inherits from the \code{\link{textstat-class}}, see
#' respective documentation to learn more.
setClass(
  "partition",
  representation(
    sAttributes = "list",
    explanation = "character",
    cpos = "matrix",
    pos = "list",
    annotations = "list",
    size = "numeric",
    metadata = "data.frame",
    strucs = "numeric",
    xml = "character",
    sAttributeStrucs = "character",
    call = "character"
  ),
  contains = "textstat"
)


#' @rdname partition_class
setClass("plprPartition", contains = "partition")

#' @rdname partition_class
setClass("pressPartition", contains = "partition")

setAs(from = "partition", to = "plprPartition", function(from){
  y <- new("plprPartition")
  for (z in slotNames(from)) slot(y, z) <- slot(from, z)
  y
})




#' @exportMethod show
#' @docType methods
#' @noRd
setMethod("show", "partition",
          function(object){
            cat("** partition object **\n")
            cat(sprintf("%-20s", "corpus:"), object@corpus, "\n")
            cat(sprintf("%-20s", "name:"), object@name, "\n")
            if (length(object@sAttributes)==0) {
              cat(sprintf("%-20s", "sAttributes:"), "no specification\n")
            } else {
              s <- unlist(lapply(
                names(object@sAttributes),
                function(x) {paste(x, "=", paste(object@sAttributes[[x]], collapse="/"))}
              ))
              cat(sprintf("%-20s", "sAttributes:"), s[1], '\n')
              if (length(s)>1) {for (i in length(s)){cat(sprintf("%-20s", " "), s[i], '\n')}}
            } 
            cat(sprintf("%-21s", "cpos:"))
            if (nrow(object@cpos)==0) {cat("not available\n")}
            else {cat(nrow(object@cpos), "pairs of corpus positions\n")}
            cat(sprintf("%-21s", "size:"))
            if (is.null(object@size)) {cat("not available\n")}
            else {cat(object@size, "tokens\n")}
            cat(sprintf("%-21s", "count:"))
            if (length(object@pAttribute) == 0) {cat("not available\n")}
            else {cat("available for ", object@pAttribute, "\n")}
          })



#' split partition into partitionBundle
#' 
#' Split a partition object into a partition Bundle if gap between strucs
#' exceeds a minimum number of tokens specified by 'gap'. Relevant to 
#' split up a plenary protocol into speeches. Note: To speed things up, the
#' returned partitions will not include frequency lists. The lists can be
#' prepared by applying \code{enrich} on the partitionBundle object that
#' is returned.
#' 
#' @param x a partition object
#' @param gap an integer specifying the minimum gap for performing the split
#' @param drop not yet implemented
#' @param ... further arguments
#' @return a partitionBundle
#' @aliases split,partition
#' @rdname split-partition-method 
#' @exportMethod split
#' @docType methods
setMethod("split", "partition", function(x, gap, drop=FALSE, ...){
  # if (length(x@metadata) == 0) warning("no metadata, method potentially fails -> please check what happens")
  cpos <- x@cpos
  if (nrow(cpos) > 1){
    distance <- cpos[,1][2:nrow(cpos)] - cpos[,2][1:(nrow(cpos)-1)]
    beginning <- c(1, ifelse(distance>gap, 1, 0))
    no <- vapply(1:length(beginning), FUN.VALUE=1, function(x) ifelse (beginning[x]==1, sum(beginning[1:x]), 0))
    for (i in (1:length(no))) no[i] <- ifelse (no[i]==0, no[i-1], no[i])
    strucsClassified <- cbind(x@strucs, no)
    strucList <- split(strucsClassified[,1], strucsClassified[,2])
    cposClassified <- cbind(cpos, no)
    cposList1 <- split(cposClassified[,1], cposClassified[,3])
    cposList2 <- split(cposClassified[,2], cposClassified[,3])
    bundleRaw <- lapply(c(1:length(strucList)), function(i) {
      p <- new(
        class(x)[1],
        strucs=strucList[[i]],
        cpos=cbind(cposList1[[i]], cposList2[[i]]),
        corpus=x@corpus, encoding=x@encoding,
        sAttributes=x@sAttributes,
        xml=x@xml, sAttributeStrucs=x@sAttributeStrucs,
        explanation=c("partition results from split, sAttributes do not necessarily define partition"),
        name=paste(x@name, i, collapse="_", sep=""),
        stat=data.table()
      )
      if (is.null(names(x@metadata))){
        meta <- NULL
      } else {
        meta <- colnames(x@metadata)
      }
      p <- enrich(
        p, size=TRUE,
        pAttribute=NULL,
        meta=meta,
        verbose=FALSE
      )
      p
    })
  } else {
    bundleRaw <- list(x)
  }
  names(bundleRaw) <- unlist(lapply(bundleRaw, function(y) y@name))
  bundle <- as.bundle(bundleRaw)
  bundle
})


#' @rdname partition_class
setMethod("name", "partition", function(x) x@name)

#' @rdname partition_class
#' @exportMethod name<-
setReplaceMethod("name", signature=c(x="partition", value="character"), function(x, value) {
  x@name <- value
  x
})


#' @exportMethod length
#' @rdname partition_class
setMethod("length", "partition", function(x) x@size)


setAs("partition", "data.table", function(from) data.table(count(from)) )


#' @exportMethod hist
#' @rdname partition_class
setMethod("hist", "partition", function(x, ...){hist(x@stat[,"count"], ...)})

#' @rdname partition_class
setMethod("length", "partition", function(x) 1)
