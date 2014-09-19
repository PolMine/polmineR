setGeneric("weigh", function(tdm, ...){standardGeneric("weigh")})

#' weigh a matrix 
#' 
#' @param tdm a matrix (which is a term-document-matrix, terms in the rows, docs in the columns)
#' @param method which weighting to apply (currently only tfidf is implemented)
#' @param corpusSizes the total number of tokens in the documents
#' @aliases weigh weigh,matrix-method weigh,TermDocumentMatrix-method
#' @rdname weigh
#' @exportMethod weigh
#' @docType methods
setMethod("weigh", "matrix", function(tdm, method="tfidf", corpusSizes){
  if (method=="tfidf"){
    tdmWeighed <- .tfidf(tdm)  
  } else if (method=="rel"){
    tdmWeighed <- tdm / corpusSizes
  } else if (method=="rank"){
    tdmWeighed <- apply(tdm, 2, order)
    rownames(tdmWeighed) <- rownames(tdm)
  }
  return(tdmWeighed)
  }
)

#' @exportMethod weigh
#' @docType methods
#' @noRd
setMethod("weigh", "TermDocumentMatrix", function(tdm, method="tfidf", corpusSizes){
  tdmMatrix <- as.sparseMatrix(tdm)
  if (method=="tfidf"){
    tdmWeighed <- .tfidf(tdmMatrix, corpusSizes)  
  } else if (method=="rel"){
    tdmWeighed <- tdmMatrix / corpusSizes
  } else if (method=="rank"){
    tdmWeighedRaw <- apply(tdm, 2, order)
    rownames(tdmWeighedRaw) <- rownames(tdm)
    tdmWeighed <- Matrix(tdmWeighedRaw, sparse=TRUE)
  }
  tdmWeighed <- as.simple_triplet_matrix(tdmWeighed)
  class(tdmWeighed) <- c("TermDocumentMatrix", "simple_triplet_matrix")
  return(tdmWeighed)
})


.tfidf <- function(tdm, corpusSizes){
  idf <- log(ncol(tdm)/colSums(tdm/tdm, na.rm=TRUE))
  tf <- tdm / corpusSizes
  tdmWeighed <- tf * idf
  return(tdmWeighed)
}