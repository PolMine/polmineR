#' Transform a partition cluster into a Term Document Matrix
#' 
#' Method based on the tm package, adds to as.TermDocumentMatrix
#' 
#' The partitions need to be derived from the same corpus (because the lexicon of the corpus is used).
#' 
#' @param x a partitionCluster object (S3 class)
#' @param pAttribute the counts for the patttribute to show up in the matrix
#' @param ... to make the check happy
#' @method as.TermDocumentMatrix partitionCluster
#' @return a TermDocumentMatrix
#' @author Andreas Blaette
#' @importFrom slam simple_triplet_matrix
#' @importFrom tm as.TermDocumentMatrix
#' @S3method as.TermDocumentMatrix partitionCluster
#' @noRd
as.TermDocumentMatrix.partitionCluster <- function (x, pAttribute, ...) {
  encoding <- unique(unlist(lapply(x, function(c) c@encoding)))
  corpus <- unique(unlist(lapply(x, function(c) c@corpus)))
  i <- unlist(lapply(x,
                     function(c) {a <- c@tf[[pAttribute]][,1]
                                  a <- a+1})
  )
  j <- unlist(lapply(c(1:length(x)),
                     function(m) {rep(
                       m,times=nrow(x[[m]]@tf[[pAttribute]])
                     )
                     }
  ))
  v <- as.integer(unlist(lapply(x, function(c) c@tf[[pAttribute]][,2])))
  attr <- paste(corpus, '.', pAttribute, sep='')
  lexicon.size <- cqi_lexicon_size(attr)
  mat <- simple_triplet_matrix(i=i, j=j, v=v,
                               ncol=length(x),
                               nrow=lexicon.size+1,
                               dimnames=list(
                                 Terms=cqi_id2str(attr, c(0:lexicon.size)),
                                 Docs=names(x))
  )
  mat$dimnames$Terms <- iconv(mat$dimnames$Terms, from=encoding, to="UTF-8")
  class(mat) <- c("TermDocumentMatrix", "simple_triplet_matrix")
  mat
}  


#' Turn a partition cluster into a matrix
#' 
#' Method based on the tm package.
#' 
#' The partitions need to be derived from the same corpus (because the lexicon of the corpus is used).
#' 
#' @param x a partitionCluster object (S3 class)
#' @param pAttribute the counts for the patttribute to show up in the matrix
#' @param ... necessary for S3 method?!
#' @method as.matrix partitionCluster
#' @return a matrix
#' @author Andreas Blaette
#' @S3method as.matrix partitionCluster
#' @noRd
as.matrix.partitionCluster <- function(x, pAttribute, ...) {
  as.matrix(as.TermDocumentMatrix(x, pAttribute))
}

#' Turn a partition cluster into a document-term matrix
#' 
#' Method based on the tm package.
#' 
#' The partitions need to be derived from the same corpus (because the lexicon of the corpus is used).
#' 
#' @param x a partitionCluster object (S3 class)
#' @param pAttribute the counts for the patttribute to show up in the matrix
#' @param ... make R happy
#' @method as.DocumentTermMatrix partitionCluster
#' @return a DocumentTermMatrix
#' @author Andreas Blaette
#' @importFrom tm as.DocumentTermMatrix
#' @S3method as.DocumentTermMatrix partitionCluster
#' @noRd
as.DocumentTermMatrix.partitionCluster <- function(x, pAttribute, ...) {
  as.DocumentTermMatrix(as.TermDocumentMatrix(x, pAttribute))
}

as.TermContextMatrix <- function(x, col, ...) UseMethod("as.TermContextMatrix", x)

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
#' @S3method as.TermContextMatrix contextCluster
#' @noRd
as.TermContextMatrix.contextCluster <- function (x, col, ...) {
  encoding <- unique(unlist(lapply(x, function(c) c@encoding)))
  corpus <- unique(unlist(lapply(x, function(c) c@corpus)))
  pAttribute <- unique(unlist(lapply(x, function(c) c@pattribute)))
  pAttr <- paste(corpus, '.', pAttribute, sep='')
  i <- unlist(lapply(x, function(c) (cqi_str2id(pAttr, rownames(c@stat))+1)))
  j <- unlist(lapply(c(1:length(x)), function(m) {rep(m,times=nrow(x[[m]]@stat))}))
  v <- unlist(lapply(x, function(c) c@stat[,col]))
  lexiconSize <- cqi_lexicon_size(pAttr)
  mat <- simple_triplet_matrix(i=i, j=j, v=v,
                               ncol=length(x),
                               nrow=lexiconSize+1,
                               dimnames=list(
                                 Terms=cqi_id2str(pAttr, c(0:lexiconSize)),
                                 Docs=names(x))
  )
  mat$dimnames$Terms <- iconv(mat$dimnames$Terms, from=encoding, to="UTF-8")
  class(mat) <- c("TermContextMatrix", "TermDocumentMatrix", "simple_triplet_matrix")
  mat
}  

#' Turn a context cluster into a matrix
#' 
#' Method based on the tm package.
#' @param x a contextCluster object (S3 class)
#' @param col the to be used
#' @param ... furhter arguments
#' @method as.matrix contextCluster
#' @return a matrix
#' @author Andreas Blaette
#' @S3method as.matrix contextCluster
#' @noRd
as.matrix.contextCluster <- function(x, col, ...) {
  slamStyle <- as.TermContextMatrix(x, col)
  mat <- as.matrix(slamStyle)
  mat <- mat[which(rowSums(mat)>0),]
  mat
}
