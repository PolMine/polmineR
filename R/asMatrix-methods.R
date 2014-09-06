#' @include partitionCluster.R
NULL

setGeneric("as.TermDocumentMatrix", function(x, ...){UseMethod("as.TermDocumentMatrix")})
setGeneric("as.DocumentTermMatrix", function(x, ...){UseMethod("as.DocumentTermMatrix")})

#' @exportMethod as.sparseMatrix
setGeneric("as.sparseMatrix", function(x, ...){standardGeneric("as.sparseMatrix")})


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
#' @param weight whether to introduce a weight ("tfidf" is implemented)
#' @param rmBlank whether to remove blank lines
#' @param ... to make the check happy
#' @method as.TermDocumentMatrix partitionCluster
#' @return a TermDocumentMatrix
#' @author Andreas Blaette
#' @importFrom slam simple_triplet_matrix
#' @importFrom tm as.TermDocumentMatrix
#' @exportMethod as.TermDocumentMatrix
#' @noRd
setMethod("as.TermDocumentMatrix", "partitionCluster", function (x, pAttribute, weight=NULL, rmBlank=TRUE, ...) {
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
  if (rmBlank == TRUE) {
    message("... removing empty rows")
    matTmp <- as.sparseMatrix(mat)
    matTmp <- matTmp[which(rowSums(matTmp) > 0),]
    mat <- as.simple_triplet_matrix(matTmp)
    class(mat) <- c("TermDocumentMatrix", "simple_triplet_matrix")
  }
  if (!is.null(weight)){
    if (weight == "tfidf"){
      message("... applying tf/idf as a weight")
      mat <- weigh(mat, method="tfidf", corpusSizes=summary(x)$token)
    }
  }
  mat
})



#' @import Matrix
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
#' @noRd
setMethod("as.DocumentTermMatrix", "partitionCluster", function(x, pAttribute, weight=NULL, rmBlank=TRUE, ...) {
  retval <- as.DocumentTermMatrix(as.TermDocumentMatrix(x, pAttribute, weight=weight, rmBlank=rmBlank))
  retval
})

