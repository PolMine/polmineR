#' @include bundle_class.R partitionBundle_class.R context_class.R
NULL



#' Generate TermDocumentMatrix / DocumentTermMatrix.
#' 
#' Method for type conversion, to generate the classes
#' \code{"TermDocumentMatrix"} or \code{"DocumentTermMatrix"} contained in the
#' \code{"tm"} package. The classes inherit from the
#' \code{"simple_triplet_matrix"}-class defined in the \code{"slam"}-package. A
#' \code{"DocumentTermMatrix"} is required as input by the \code{"topicmodels"}
#' package, for instance.
#'
#' The type conversion-method can be applied on object of the class
#' \code{"bundle"}, or classes inheriting from the \code{"bundle"} class. If
#' counts or some other measure is present in the \code{"stat"} slots of the
#' objects in the bundle, then the values in the column indicated by
#' \code{"col"} will be turned into the values of the sparse matrix that is
#' generated. A special case is the generation of the sparse matrix based on a
#' \code{"partitionBundle"} that does not yet include counts. In this case, a 
#' \code{"pAttribute"} needs to be provided, then counting will be performed,
#' too.
#' 
#' @param x some object
#' @param pAttribute the p-attribute
#' @param sAttribute the s-attribute
#' @param col the column to use of assembling the matrix
#' @param verbose bla
#' @param ... to make the check happy
#' @return a TermDocumentMatrix
#' @author Andreas Blaette
#' @exportMethod as.TermDocumentMatrix
#' @exportMethod as.DocumentTermMatrix
#' @docType methods
#' @rdname as.DocumentTermMatrix
#' @name as.TermDocumentMatrix
#' @aliases as.DocumentTermMatrix
#' @author me
setGeneric("as.TermDocumentMatrix", function(x, ...){UseMethod("as.TermDocumentMatrix")})
setGeneric("as.DocumentTermMatrix", function(x, ...){UseMethod("as.DocumentTermMatrix")})

#' @examples
#' \dontrun{
#' use("polmineR.sampleCorpus")
#'  
#' # do-it-yourself 
#' p <- partition("PLPRBTTXT", text_date=".*", regex=TRUE)
#' pB <- partitionBundle(p, sAttribute="text_date")
#' pB <- enrich(pB, pAttribute="word")
#' tdm <- as.TermDocumentMatrix(pB, col = "count")
#'    
#'  # leave the counting to the as.TermDocumentMatrix-method
#' pB2 <- partitionBundle(p, sAttribute = "text_date")
#' tdm <- as.TermDocumentMatrix(pB2, pAttribute = "word", verbose = TRUE)
#'    
#' # diretissima
#' tdm <- as.TermDocumentMatrix("PLPRBTTXT", pAttribute = "word", sAttribute = "text_date")
#' }
#' @rdname as.DocumentTermMatrix
setMethod("as.TermDocumentMatrix", "character",function (x, pAttribute, sAttribute, verbose = TRUE) {
  y <- as.DocumentTermMatrix(x = x, pAttribute = pAttribute, sAttribute = sAttribute, verbose = verbose)
  as.TermDocumentMatrix(y)
})



#' @rdname as.DocumentTermMatrix
setMethod("as.DocumentTermMatrix", "character", function(x, pAttribute, sAttribute, verbose = TRUE){
  cpos_vector <- seq.int(from = 0, to = CQI$attribute_size(x, pAttribute, type = "p") - 1, by = 1)
  
  if (verbose) message("... generate data.table with token and struc ids")
  token_id <- CQI$cpos2id(x, pAttribute, cpos_vector)
  struc_id <- CQI$cpos2struc(x, sAttribute, cpos_vector)
  tokenStreamDT <- data.table(token_id = token_id, struc_id = struc_id)
  rm(token_id, struc_id)
  
  if (verbose) message("... counting token per doc")
  countDT <- tokenStreamDT[, .N, by = c("token_id", "struc_id"), with = TRUE]
  
  if(verbose) message("... generate simple_triplet_matrix")
  y <- simple_triplet_matrix(
    i = countDT[["struc_id"]] + 1,
    j = countDT[["token_id"]] + 1,
    v = countDT[["N"]],
  )
  docs <- CQI$struc2str(x, sAttribute, seq.int(0, CQI$attribute_size(x, sAttribute, type = "s") - 1, by = 1))
  terms <- CQI$id2str(x, pAttribute, seq.int(0, max(countDT[["token_id"]]), by = 1))
  terms <- as.nativeEnc(terms, from = getEncoding(x))
  dimnames(y) <- list(docs, terms)
  class(y) <- c("TermDocumentMatrix", "simple_triplet_matrix")
  attr(y, "weighting") <- c("term frequency", "tf")
  y
})



#' @rdname as.DocumentTermMatrix
#' @importFrom slam simple_triplet_matrix
setMethod("as.TermDocumentMatrix", "bundle", function(x, col, pAttribute = NULL, verbose = TRUE){
  if (is.null(pAttribute)){
    pAttribute <- x@objects[[1]]@pAttribute
    if (verbose) message("... using the pAttribute-slot of the first object in the bundle as pAttribute: ", pAttribute)
  }
  if (verbose) message("... generating (temporary) key column")
  if (length(pAttribute) > 1){
    dummy <- lapply(
      c(1:length(x@objects)),
      function(i){
        keysRaw <- x@objects[[i]]@stat[, c(pAttribute), with=FALSE]
        keys <- apply(keys, 1, function(row) paste(row, collapse="//"))
        x@objects[[i]]@stat[, key := keys]
      })
    rm(dummy)
  } else {
    dummy <- lapply(
      1:length(x@objects),
      function(i) setnames(x@objects[[i]]@stat, old = pAttribute, new = "key")
      )
    rm(dummy)
  }
  if (verbose) message("... generating cumulated data.table")
  DT <- data.table::rbindlist(lapply(x@objects, function(y) y@stat))
  j <- unlist(lapply(c(1:length(x@objects)), function(i) rep(i, times = nrow(x@objects[[i]]@stat))))
  DT[, "j" := j]
  DT <- DT[which(DT[["key"]] != "")] # to avoid errors
  if (verbose) message("... getting unique keys")
  uniqueKeys <- unique(DT[["key"]])
  keys <- setNames(c(1:length(uniqueKeys)), uniqueKeys)
  if (verbose) message("... generating integer keys")
  i <- keys[ DT[["key"]] ]
  retval <- simple_triplet_matrix(
    i = unname(i), j = DT[["j"]], v = DT[[col]],
    nrow = length(names(keys)), ncol = length(names(x@objects)),
    dimnames = list(Terms=names(keys), Docs = names(x@objects))
  )
  class(retval) <- c("TermDocumentMatrix", "simple_triplet_matrix")
  
  if (verbose == TRUE) message("... cleaning up temporary key columns")
  if (length(pAttribute) > 1){
    dummy <- lapply(c(1:length(x@objects)), function(i) x@objects[[i]]@stat[, key := NULL])
  } else {
    dummy <- lapply(1:length(x@objects), function(i) setnames(x@objects[[i]]@stat, old="key", new=pAttribute))
  }
  attr(retval, "weighting") <- c("term frequency", "tf")
  retval
})

#' @rdname as.DocumentTermMatrix
setMethod("as.DocumentTermMatrix", "bundle", function(x, col) {
  as.DocumentTermMatrix(as.TermDocumentMatrix(x=x, col=col))
})

#' @rdname as.DocumentTermMatrix
setMethod("as.TermDocumentMatrix", "partitionBundle", function(x, pAttribute = NULL, col = NULL, verbose = TRUE){
  if (!is.null(col)){
    callNextMethod()
  } else if (!is.null(pAttribute)){
    encoding <- unique(sapply(x@objects, function(y) y@encoding))
    if (verbose) message("... generating corpus positions")
    
    cposList <- lapply(
      c(1:length(x@objects)),
      function(i) cbind(i, cpos(x@objects[[i]]@cpos))
    )
    cposMatrix <- do.call(rbind, cposList)
    if (verbose) message("... getting ids")
    id_vector <- CQI$cpos2id(x[[1]]@corpus, pAttribute, cposMatrix[,2])
    DT <- data.table(i = cposMatrix[,1], id = id_vector, key = c("i", "id"))
    if (verbose) message("... performing count")
    TF <- DT[,.N, by = c("i", "id"), with=TRUE]
    setnames(TF, old = "N", new = "count")
    TF[, pAttribute := as.nativeEnc(CQI$id2str(x[[1]]@corpus, pAttribute, TF[["id"]]), from = encoding), with = FALSE]
    if (verbose == TRUE) message("... generating keys")
    uniqueTerms <- unique(TF[[pAttribute]])
    keys <- setNames(c(1:length(uniqueTerms)), uniqueTerms)
    if (verbose == TRUE) message("... generating simple triplet matrix")
    retval <- simple_triplet_matrix(
      i = keys[ TF[[pAttribute]] ], j = TF[["i"]], v = TF[["count"]],
      dimnames = list(Terms=names(keys), Docs=names(x@objects))
    )
    class(retval) <- c("TermDocumentMatrix", "simple_triplet_matrix")
    return(retval)
  } else {
    message("... doing nothing, as pAttribute and col is NULL")
  }
})

#' @rdname as.DocumentTermMatrix
setMethod("as.DocumentTermMatrix", "partitionBundle", function(x, pAttribute=NULL, col=NULL, verbose=TRUE){
  as.DocumentTermMatrix(as.TermDocumentMatrix(x=x, pAttribute=pAttribute, col=col, verbose=verbose))
})

#' @rdname as.DocumentTermMatrix
setMethod("as.DocumentTermMatrix", "context", function(x, pAttribute, verbose = TRUE){
  if (!paste(pAttribute, "id", "_") %in% colnames(x@cpos)){
    if (verbose) message("... adding token ids for p-attribute: ", pAttribute)
    x <- enrich(x, pAttribute = pAttribute)
  }
  
  if (verbose) message("... dropping nodes")
  CPOS <- x@cpos[which(x@cpos[["position"]] != 0)]
  
  if (verbose) message("... counting tokens in context")
  CPOS2 <- CPOS[, .N, by = c("hit_no", paste(pAttribute, "id", sep = "_"))]
  
  # create new index for hits
  # may be necessary if negativelist/positivelist has been applied
  if (verbose) message("... creating new index for hits")
  hits <- unique(CPOS[["hit_no"]])
  hits <- hits[order(hits, decreasing = FALSE)]
  hit_index_new <- 1:length(hits)
  names(hit_index_new) <- as.character(hits)
  CPOS2[, "i" := hit_index_new[as.character(CPOS2[["hit_no"]])], with = TRUE]
  
  # create new index for word_ids
  if (verbose) message("... creating new index for tokens")
  uniqueIDs <- unique(CPOS2[[paste(pAttribute, "id", sep = "_")]])
  uniqueIDs <- uniqueIDs[order(uniqueIDs, decreasing = FALSE)]
  idIndexNew <- setNames(1:length(uniqueIDs), as.character(uniqueIDs))
  decodedTokens <- as.nativeEnc(
    CQI$id2str(x@corpus, pAttribute, uniqueIDs),
    from = x@encoding
    )
  CPOS2[, "j" := idIndexNew[as.character(CPOS2[[paste(pAttribute, "id", sep = "_")]])], with = TRUE]
  
  if (verbose) message("... putting together matrix")
  dtm <- simple_triplet_matrix(
    i = CPOS2[["i"]], j = CPOS2[["j"]], v = CPOS2[["N"]],
    dimnames = list(Docs = as.character(1:max(CPOS2[["i"]])), Terms = decodedTokens)
  )
  class(dtm) <- c("DocumentTermMatrix", "simple_triplet_matrix")
  attr(dtm, "weighting") <- c("term frequency", "tf")
  dtm
})

#' @rdname as.DocumentTermMatrix
setMethod("as.TermDocumentMatrix", "context", function(x, pAttribute, verbose = TRUE){
  as.DocumentTermMatrix(x = x, pAttribute = pAttribute, verbose = verbose)
})
