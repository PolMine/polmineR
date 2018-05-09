#' @include partition_class.R partitionBundle_class.R context_class.R contextBundle_class.R
#' @include features_class.R
NULL

#' trim an object
#' 
#' Method to trim and adjust objects by 
#' applying thresholds, minimum frequencies etc. It can be applied to 'context',
#' 'features', 'context', 'partition' and 'partitionBundle' objects.
#' 
#' @param object the object to be trimmed
#' @param termsToKeep ...
#' @param termsToDrop ...
#' @param docsToKeep ...
#' @param docsToDrop ...
#' @param verbose logical
#' @param ... further arguments
#' @author Andreas Blaette
#' @docType methods
#' @aliases trim trim-method trim,TermDocumentMatrix-method
#' @rdname trim-method
setGeneric("trim", function(object, ...){standardGeneric("trim")})



#' @importFrom Matrix rowSums
#' @importFrom tm stopwords
#' @importFrom slam as.simple_triplet_matrix
#' @rdname trim-method
setMethod("trim", "TermDocumentMatrix", function(object, termsToKeep=NULL, termsToDrop=NULL, docsToKeep=NULL, docsToDrop=NULL, verbose=TRUE){
  .rmBlank <- function(mat, verbose=TRUE){
    .message("removing empty rows", verbose = verbose)
    matTmp <- as.sparseMatrix(mat)
    matTmp <- matTmp[which(rowSums(matTmp) > 0),]
    mat <- as.simple_triplet_matrix(matTmp)
    class(mat) <- c("TermDocumentMatrix", "simple_triplet_matrix")
    mat
  }
  if (!is.null(docsToKeep)){
    object <- object[,which(colnames(object) %in% docsToKeep)]
  }
  if (!is.null(docsToDrop)){
    object <- object[,which(!colnames(object) %in% docsToDrop)]
  }
  if (!is.null(termsToKeep)){
    object <- object[which(rownames(object) %in% termsToKeep),]
  }
  if (!is.null(termsToDrop)){
    object <- object[which(!rownames(object) %in% termsToDrop), ]
  }
  object
})

#' @rdname trim-method
setMethod("trim", "DocumentTermMatrix", function(object, ...){
  t(trim(t(object), ...))
})




#' @rdname context-class
setMethod("trim", "context", function(object, sAttribute = NULL, positivelist = NULL, pAttribute = pAttributes(object), regex = FALSE, stoplist = NULL, verbose = TRUE, progress = TRUE){
  
  if(!is.null(sAttribute)){
    stopifnot(length(sAttribute) == 1)
    sAttrCol <- paste(sAttribute, "int", sep = "_")
    if (!sAttrCol %in% colnames(object@cpos)){
      object <- enrich(object, sAttribute = sAttribute)
    }
    setnames(object@cpos, old = sAttrCol, new = "struc")
    
    position <- 0 # work around to make data.table syntax pass R CMD check
    struc <- 0 # work around to make data.table syntax pass R CMD check
    
    .message("checking boundaries of regions", verbose = verbose)
    if (progress) pb <- txtProgressBar(min = 1, max = object@count, style = 3)
    .checkBoundary <- function(.SD, .GRP){
      if (progress) setTxtProgressBar(pb, value = .GRP)
      struc_hit <- .SD[position == 0][["struc"]][1]
      .SD[struc == struc_hit]
    }
    object@cpos <- object@cpos[, .checkBoundary(.SD, .GRP), by = "hit_no"]
    close(pb)
    setnames(object@cpos, old = "struc", new = sAttrCol)
  }
  
  if (!is.null(positivelist)){
    .message("filtering by positivelist", verbose = verbose)
    before <- length(unique(object@cpos[["hit_no"]]))
    positivelistIds <- .token2id(corpus = object@corpus, pAttribute = pAttribute, token = positivelist, regex = regex)
    .keepPositives <- function(.SD){
      pAttr <- paste(pAttribute[1], "id", sep = "_")
      positives <- which(.SD[[pAttr]] %in% positivelistIds)
      positives <- positives[ -which(.SD[["position"]] == 0) ] # exclude node
      if (any(positives)) return( .SD ) else return( NULL )
    }
    object@cpos <- object@cpos[, .keepPositives(.SD), by = "hit_no", with = TRUE]
    after <- length(unique(object@cpos[["hit_no"]]))
    .message("number of hits droped due to positivelist:", before - after, verbose = verbose)
    if (nrow(object@cpos) == 0) {
      warning("no remaining hits after applying positivelist, returning NULL object")
      return( invisible(NULL) )
    }
  }
  
  if (!is.null(stoplist)){
    .message("applying stoplist", verbose = verbose)
    before <- length(unique(object@cpos[["hit_no"]]))
    stoplistIds <- .token2id(corpus = object@corpus, pAttribute = pAttribute, token = stoplist, regex = regex)
    .dropNegatives <- function(.SD){
      pAttr <- paste(pAttribute[1], "id", sep = "_")
      negatives <- which(.SD[[pAttr]] %in% stoplistIds)
      negatives <- negatives[ -which(.SD[["position"]] == 0) ] # exclude node
      if (any(negatives)) return( NULL ) else return( .SD ) # this is the only difference
    }
    object@cpos <- object@cpos[, .dropNegatives(.SD), by = "hit_no", with = TRUE]
    after <- length(unique(object@cpos[["hit_no"]]))
    .message("number of hits droped due to stoplist:", before - after, verbose = verbose)
    if (nrow(object@cpos) == 0) {
      warning("no remaining hits after applying stoplist, returning NULL object")
      return( NULL )
    }
  }
  
  object
})
