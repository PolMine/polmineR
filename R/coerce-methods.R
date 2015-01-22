#' @include polmineR-package.R partition-class.R partitionCluster-class.R context-class.R collocations-class.R contextCluster-class.R
NULL

setGeneric("as.TermDocumentMatrix", function(x, ...){UseMethod("as.TermDocumentMatrix")})
setGeneric("as.DocumentTermMatrix", function(x, ...){UseMethod("as.DocumentTermMatrix")})
#' @exportMethod as.sparseMatrix
setGeneric("as.sparseMatrix", function(x,...){standardGeneric("as.sparseMatrix")})
setGeneric("as.partitionCluster", function(object,...){standardGeneric("as.partitionCluster")})
setGeneric("as.TermContextMatrix", function(x, col, ...) {standardGeneric("as.TermContextMatrix")})

#' @param mat a TermDocumentMatrix
#' @param verbose logicla, whether to be talkative
#' @return a TermDocumentMatrix
#' @importFrom slam as.simple_triplet_matrix
#' 
#' @noRd
.rmBlank <- function(mat, verbose=TRUE){
  if (verbose==TRUE) message("... removing empty rows")
  matTmp <- as.sparseMatrix(mat)
  matTmp <- matTmp[which(rowSums(matTmp) > 0),]
  mat <- as.simple_triplet_matrix(matTmp)
  class(mat) <- c("TermDocumentMatrix", "simple_triplet_matrix")
  mat
}


#' @docType methods
#' @importFrom Matrix sparseMatrix
setMethod("as.sparseMatrix", "TermDocumentMatrix", function(x){
  retval <-  sparseMatrix(i=x$i,
                          j=x$j,
                          x=x$v,
                          dims=c(x$nrow, x$ncol),
                          dimnames = dimnames(x),
                          giveCsparse = TRUE)
  return(retval)  
})

#' Transform a partition cluster into a Term Document Matrix
#' 
#' Method based on the tm package, adds to as.TermDocumentMatrix
#' 
#' The partitions need to be derived from the same corpus (because the lexicon of the corpus is used).
#' 
#' @param x a partitionCluster object (S4 class)
#' @param pAttribute the counts for the patttribute to show up in the matrix
#' @param weight whether to introduce a weight ("tfidf" and "rel" are implemented)
#' @param rmBlank whether to remove blank lines
#' @param verbose logical, whether to be talkative
#' @param ... to make the check happy
#' @method as.TermDocumentMatrix partitionCluster
#' @return a TermDocumentMatrix
#' @author Andreas Blaette
#' @importFrom slam simple_triplet_matrix
#' @importFrom tm as.TermDocumentMatrix
#' @exportMethod as.TermDocumentMatrix
#' @docType methods
#' @noRd
setMethod("as.TermDocumentMatrix", "partitionCluster", function (x, pAttribute, weight=NULL, rmBlank=TRUE, verbose=TRUE, ...) {
  encoding <- unique(unlist(lapply(x@partitions, function(c) c@encoding)))
  corpus <- unique(unlist(lapply(x@partitions, function(c) c@corpus)))
  message("... putting together the matrix")
  i <- as.integer(unname(unlist(lapply(x@partitions,
                                       function(c) {a <- c@tf[[pAttribute]][,1]
                                                    a <- a+1
                                                    a})
  )))
  j <- unlist(lapply(c(1:length(x@partitions)),
                     function(m) {rep(
                       m,times=nrow(x@partitions[[m]]@tf[[pAttribute]])
                     )
                     }
  ))
  v <- as.integer(unlist(lapply(x@partitions, function(c) c@tf[[pAttribute]][,2])))
  attr <- paste(corpus, '.', pAttribute, sep='')
  lexicon.size <- cqi_lexicon_size(attr)
  mat <- simple_triplet_matrix(i=i, j=j, v=v,
                               ncol=length(x@partitions),
                               nrow=lexicon.size+1,
                               dimnames=list(
                                 Terms=cqi_id2str(attr, c(0:lexicon.size)),
                                 Docs=names(x@partitions))
  )
  mat$dimnames$Terms <- iconv(mat$dimnames$Terms, from=encoding, to="UTF-8")  
  class(mat) <- c("TermDocumentMatrix", "simple_triplet_matrix")
  if (rmBlank == TRUE) mt <- .rmBlank(mat, verbose=verbose)
  if (!is.null(weight)){
    if (weight == "tfidf"){
      message("... applying tf/idf as a weight")
      mat <- weigh(mat, method="tfidf", corpusSizes=summary(x)$token)
    } else if (weight == "rel"){
      message("... computing relative frequencies")
      mat <- weigh(mat, method="rel", corpusSizes=summary(x)$token)
    }
  }
  mat
})


#' @docType methods
#' @noRd
setMethod("as.sparseMatrix", "partitionCluster", function(x, pAttribute, ...){
  message("... converting partitionCluster to TermDocumentMatrix")
  tdm_stm <- as.TermDocumentMatrix(x, pAttribute=pAttribute)
  message("... converting TermDocumentMatrix to Matrix")
  retval <-  as.sparseMatrix(tdm_stm)
  return(retval)
})


#' Turn a partition cluster into a document-term matrix
#' 
#' Method based on the tm package.
#' 
#' The partitions need to be derived from the same corpus (because the lexicon of the corpus is used).
#' 
#' @param x a partitionCluster object (S3 class)
#' @param pAttribute the counts for the patttribute to show up in the matrix
#' @param weight whether to weigh the matrix
#' @param rmBlank whether to remove blank lines
#' @param ... make R happy
#' @method as.DocumentTermMatrix partitionCluster
#' @importFrom tm as.DocumentTermMatrix
#' @return a DocumentTermMatrix
#' @author Andreas Blaette
#' @exportMethod as.DocumentTermMatrix
#' @docType methods
#' @noRd
setMethod("as.DocumentTermMatrix", "partitionCluster", function(x, pAttribute, weight=NULL, rmBlank=TRUE, ...) {
  retval <- as.DocumentTermMatrix(as.TermDocumentMatrix(x, pAttribute, weight=weight, rmBlank=rmBlank))
  retval
})






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
  pAttribute <- unique(unlist(lapply(x@contexts, function(c) c@pAttribute)))
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
#' @param row.names defaults to NULL
#' @param optional see documentation of as.data.frame
#' @param ... any further arguments
#' @rdname asDataFrame-method
#' @name as.data.frame
#' @aliases as.data.frame,keyness-method as.data.frame,context-method
#' @exportMethod as.data.frame
#' @docType methods
setMethod("as.data.frame", "keyness", function(x, ...) x@stat )
setMethod("as.data.frame", "context", function(x, ...) x@stat )

#' @docType methods
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


