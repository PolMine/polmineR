setGeneric("sAttributes", function(object,...){standardGeneric("sAttributes")})

setGeneric("as.sparseMatrix", function(x,...){standardGeneric("as.sparseMatrix")})

setGeneric("as.partitionCluster", function(object,...){standardGeneric("as.partitionCluster")})

setGeneric("as.TermContextMatrix", function(x, col, ...) {standardGeneric("as.TermContextMatrix")})

setGeneric("score", function(object, ...){standardGeneric("score")})

setGeneric("speeches", function(object, ...){standardGeneric("speeches")})



#' Transform a context cluster into a Term Context Matrix
#' 
#' Method based on the tm package, adds to as.TermDocumentMatrix
#' 
#' The partitions need to be derived from the same corpus (because the lexicon of the corpus is used).
#' 
#' @param x a contextCluster object (S3 class)
#' @param col the col of the stat table to take
#' @param ... to make the check happy
#' @method as.TermContextMatrix contextCluster
#' @return a TermContextMatrix
#' @author Andreas Blaette
#' @docType method
#' @importFrom slam simple_triplet_matrix
#' @exportMethod as.TermContextMatrix
#' @noRd
setMethod("as.TermContextMatrix", "contextCluster", function (x, col, ...) {
  encoding <- unique(unlist(lapply(x@contexts, function(c) c@encoding)))
  corpus <- unique(unlist(lapply(x@contexts, function(c) c@corpus)))
  pAttribute <- unique(unlist(lapply(x@contexts, function(c) c@pattribute)))
  pAttr <- paste(corpus, '.', pAttribute, sep='')
  i <- unlist(lapply(x@contexts, function(c) (cqi_str2id(pAttr, rownames(c@stat))+1)))
  j <- unlist(lapply(c(1:length(x@contexts)), function(m) {rep(m,times=nrow(x[[m]]@stat))}))
  v <- unlist(lapply(x@contexts, function(c) c@stat[,col]))
  lexiconSize <- cqi_lexicon_size(pAttr)
  mat <- simple_triplet_matrix(i=i, j=j, v=v,
                               ncol=length(x@contexts),
                               nrow=lexiconSize+1,
                               dimnames=list(
                                 Terms=cqi_id2str(pAttr, c(0:lexiconSize)),
                                 Docs=names(x@contexts))
  )
  mat$dimnames$Terms <- iconv(mat$dimnames$Terms, from=encoding, to="UTF-8")
  class(mat) <- c("TermContextMatrix", "TermDocumentMatrix", "simple_triplet_matrix")
  mat
})


#' Get statistics table from an object
#' 
#' @param x object with a statistics table
#' @param ... any further arguments
#' @rdname asDataFrame-method
#' @name as.data.frame
#' @aliases as.data.frame,keyness-method as.data.frame,context-method
#' @exportMethod as.data.frame
setMethod("as.data.frame", "keyness", function(x, ...) x@stat )
setMethod("as.data.frame", "context", function(x, ...) x@stat )

#' Basic information on an object
#' 
#' @param x the object to learn about
#' @exportMethod info
#' @docType methods
#' @rdname info-method
#' @aliases info info,keyness-method
setGeneric("info", function(x){standardGeneric("info")})

#' Information on a keyness object
#' 
#' @exportMethod info
#' @noRd
setMethod(
  "info", "keyness",
  function(x){
    cat("the statistics table has", nrow(x@stat), "rows\n")
    cat("pos attributest have been added: ")
    if ("pos" %in% colnames(x@stat)){
      cat("YES\n")
    } else {
      cat("NO\n")
    }
  })

#'@include partition.R
#' @exportMethod as.partitionCluster
setMethod("as.partitionCluster", "partition", function(object){
  newCluster <- new("partitionCluster")
  newCluster@partitions[[1]] <- object
  names(newCluster@partitions)[1] <- object@label
  newCluster@corpus <- object@corpus
  newCluster@encoding <- object@encoding
  newCluster@explanation <- c("derived from a partition object")
  newCluster
})

setMethod("as.partitionCluster", "list", function(object, ...){
  if (!all(unlist(lapply(object, class))=="partition")) warning("all objects in list need to be partition objects")
  newCluster <- new("partitionCluster")
  newCluster@partitions <- object
  newCluster@corpus <- unique(unlist(lapply(newCluster@partitions, function(x) x@corpus)))
  newCluster@encoding <- unique(unlist(lapply(newCluster@partitions, function(x) x@encoding)))
  names(newCluster@partitions) <- vapply(newCluster@partitions, function(x) x@label, FUN.VALUE="character")
  newCluster
})

