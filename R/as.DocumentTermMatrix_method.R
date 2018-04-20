#' @include bundle_class.R partitionBundle_class.R context_class.R
NULL


#' Generate TermDocumentMatrix / DocumentTermMatrix.
#' 
#' Method to generate the classes \code{TermDocumentMatrix} or 
#' \code{DocumentTermMatrix} as defined in the \code{tm} package. These classes
#' inherit from the \code{simple_triplet_matrix}-class defined in the 
#' \code{slam}-package. There are many text mining applications for 
#' document-term matrices. A \code{DocumentTermMatrix} is required as input by
#' the \code{topicmodels} package, for instance.
#' 
#' The method can be applied on objects of the class 
#' \code{character}, \code{bundle}, or classes inheriting from the
#' \code{bundle} class.
#' 
#' If \code{x} refers to a corpus (i.e. is a length 1 character vector), a
#' \code{TermDocumentMatrix}, or \code{DocumentTermMatrix} will be generated for
#' subsets of the corpus based on the \code{sAttribute} provided. Counts are
#' performed for the \code{pAttribute}. Further parameters provided (passed in
#' as \code{...} are interpreted as s-attributes that define a subset of the
#' corpus for splitting it according to \code{sAttribute}. If struc values for
#' \code{sAttribute} are not unique, the necessary aggregation is performed, slowing
#' things somewhat down.
#' 
#' If \code{x} is a \code{bundle} or a class inheriting from it, the counts or
#' whatever measure is present in the \code{stat} slots (in the column
#' indicated by \code{col}) will be turned into the values of the sparse
#' matrix that is generated. A special case is the generation of the sparse
#' matrix based on a \code{partitionBundle} that does not yet include counts.
#' In this case, a \code{pAttribute} needs to be provided. Then counting will
#' be performed, too.
#' 
#' @param x a \code{character} vector indicating a corpus, or an object of class
#'   \code{bundle}, or inheriting from class \code{bundle} (e.g. \code{partitionBundle})
#' @param pAttribute p-attribute counting is be based on
#' @param sAttribute s-attribute that defines content of columns, or rows
#' @param col the column of \code{data.table} in slot \code{stat} (if \code{x}
#'   is a \code{bundle}) to use of assembling the matrix
#' @param verbose logial, whether to output progress messages
#' @param ... s-attribute definitions used for subsetting the corpus, compare partition-method
#' @return a TermDocumentMatrix
#' @author Andreas Blaette
#' @exportMethod as.TermDocumentMatrix
#' @exportMethod as.DocumentTermMatrix
#' @docType methods
#' @rdname as.DocumentTermMatrix
#' @name as.TermDocumentMatrix
#' @aliases as.DocumentTermMatrix
setGeneric("as.TermDocumentMatrix", function(x, ...) UseMethod("as.TermDocumentMatrix") )
setGeneric("as.DocumentTermMatrix", function(x, ...) UseMethod("as.DocumentTermMatrix") )

#' @examples
#' use("polmineR")
#'  
#' # do-it-yourself 
#' p <- partition("GERMAPARLMINI", date=".*", regex=TRUE)
#' pB <- partitionBundle(p, sAttribute = "date")
#' pB <- enrich(pB, pAttribute="word")
#' tdm <- as.TermDocumentMatrix(pB, col = "count")
#'    
#'  # leave the counting to the as.TermDocumentMatrix-method
#' pB2 <- partitionBundle(p, sAttribute = "date")
#' tdm <- as.TermDocumentMatrix(pB2, pAttribute = "word", verbose = TRUE)
#'    
#' # diretissima
#' tdm <- as.TermDocumentMatrix("GERMAPARLMINI", pAttribute = "word", sAttribute = "date")
#' @rdname as.DocumentTermMatrix
setMethod("as.TermDocumentMatrix", "character",function (x, pAttribute, sAttribute, verbose = TRUE, ...) {
  y <- as.DocumentTermMatrix(x = x, pAttribute = pAttribute, sAttribute = sAttribute, verbose = verbose, ...)
  as.TermDocumentMatrix(y)
})



#' @rdname as.DocumentTermMatrix
setMethod("as.DocumentTermMatrix", "character", function(x, pAttribute, sAttribute, verbose = TRUE, ...){
  
  stopifnot(
    length(x) == 1,
    x %in% CQI$list_corpora(),
    is.character(pAttribute),
    length(pAttribute) == 1,
    pAttribute %in% CQI$attributes(x, "p"),
    is.character(sAttribute),
    length(sAttribute) == 1,
    sAttribute %in% CQI$attributes(x, "s"),
    is.logical(verbose),
    all(names(list(...)) %in% CQI$attributes(x, "s"))
    )
  
  .message("generate data.table with token and struc ids", verbose = verbose)
  cpos_vector <- 0:(CQI$attribute_size(x, pAttribute, type = "p") - 1)
  token_id <- CQI$cpos2id(x, pAttribute, cpos_vector)
  struc_vector <- 0:(CQI$attribute_size(x, sAttribute, type = "s") - 1)
  struc_id <- CQI$cpos2struc(x, sAttribute, cpos_vector)
  tokenStreamDT <- data.table(cpos = cpos_vector, token_id = token_id, struc_id = struc_id)
  tokenStreamDT <- tokenStreamDT[which(tokenStreamDT[["struc_id"]] != -1)]
  rm(token_id, struc_id)
  
  sAttrSelect <- list(...)
  
  if (
    length(sAttrSelect) == 0
    && length(unique(CQI$struc2str(x, sAttribute, struc_vector))) == CQI$attribute_size(x, sAttribute, type = "s")
    ){
    
    token_id <- CQI$cpos2id(x, pAttribute, cpos_vector)
    struc_id <- CQI$cpos2struc(x, sAttribute, cpos_vector)
    tokenStreamDT <- data.table(token_id = token_id, struc_id = struc_id)
    rm(token_id, struc_id)
    tokenStreamDT <- tokenStreamDT[which(tokenStreamDT[["struc_id"]] != -1)]
    
    if (verbose) message("... counting token per doc")
    countDT <- tokenStreamDT[, .N, by = c("token_id", "struc_id"), with = TRUE]
    
    if(verbose) message("... generate simple_triplet_matrix")
    dtm <- simple_triplet_matrix(
      i = countDT[["struc_id"]] + 1,
      j = countDT[["token_id"]] + 1,
      v = countDT[["N"]],
    )
    docs <- CQI$struc2str(x, sAttribute, 0:(CQI$attribute_size(x, sAttribute, type = "s") - 1))
    terms <- CQI$id2str(x, pAttribute, 0:max(countDT[["token_id"]]))
    terms <- as.nativeEnc(terms, from = getEncoding(x))
    dimnames(dtm) <- list(docs, terms)
    
  } else {
    if (length(sAttrSelect) >= 1){
      for (i in 1:length(sAttrSelect)){
        sAttrSub <- names(sAttrSelect)[i]
        .message("subsetting data.table by s-attribute", sAttrSub, verbose = verbose)
        struc_id <- CQI$cpos2struc(x, sAttrSub, tokenStreamDT[["cpos"]])
        struc_values <- CQI$struc2str(x, sAttrSub, struc_id)
        tokenStreamDT <- tokenStreamDT[ which(struc_values %in% as.character(sAttrSelect[[i]])) ]
      }
    }
    .message("generate unique document ids", verbose = verbose)
    struc_values <- CQI$struc2str(x, sAttribute, tokenStreamDT[["struc_id"]])
    unique_struc_values <- unique(struc_values)
    doc_index <- setNames(object = 1:length(unique_struc_values), nm = unique_struc_values)
    tokenStreamDT[["doc_id"]] <- doc_index[ struc_values ]
    tokenStreamDT[, "struc_id" := NULL][, "cpos" := NULL]
    
    .message("counting token per doc", verbose = verbose)
    countDT <- tokenStreamDT[, .N, by = c("token_id", "doc_id"), with = TRUE]
    unique_token_ids <- unique(tokenStreamDT[["token_id"]])
    new_token_index <- setNames(1:length(unique_token_ids), as.character(unique_token_ids))
    countDT[["new_token_id"]] <- new_token_index[ as.character(countDT[["token_id"]]) ]
    names(new_token_index) <- CQI$id2str(x, pAttribute, as.integer(names(new_token_index)))
    
    .message("generate simple_triplet_matrix", verbose = verbose)
    dtm <- simple_triplet_matrix(
      i = countDT[["doc_id"]],
      j = countDT[["new_token_id"]],
      v = countDT[["N"]],
    )
    
    .message("add row and column labels", verbose = verbose)
    terms <- as.nativeEnc(names(new_token_index), from = getEncoding(x))
    documents <- as.nativeEnc(names(doc_index), from = getEncoding(x))
    
    dimnames(dtm) <- list(documents, terms)
  }
  class(dtm) <- c("TermDocumentMatrix", "simple_triplet_matrix")
  attr(dtm, "weighting") <- c("term frequency", "tf")
  dtm
})



#' @rdname as.DocumentTermMatrix
#' @importFrom slam simple_triplet_matrix
setMethod("as.TermDocumentMatrix", "bundle", function(x, col, pAttribute = NULL, verbose = TRUE){
  if (is.null(pAttribute)){
    pAttribute <- x@objects[[1]]@pAttribute
    .message("using the pAttribute-slot of the first object in the bundle as pAttribute:", pAttribute, verbose = verbose)
  }
  .message("generating (temporary) key column", verbose = verbose)
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
  .message("generating cumulated data.table", verbose = verbose)
  DT <- data.table::rbindlist(lapply(x@objects, function(y) y@stat))
  j <- unlist(lapply(c(1:length(x@objects)), function(i) rep(i, times = nrow(x@objects[[i]]@stat))))
  DT[, "j" := j]
  DT <- DT[which(DT[["key"]] != "")] # to avoid errors
  .message("getting unique keys", verbose = verbose)
  uniqueKeys <- unique(DT[["key"]])
  keys <- setNames(c(1:length(uniqueKeys)), uniqueKeys)
  .message("generating integer keys", verbose = verbose)
  i <- keys[ DT[["key"]] ]
  retval <- simple_triplet_matrix(
    i = unname(i), j = DT[["j"]], v = DT[[col]],
    nrow = length(names(keys)), ncol = length(names(x@objects)),
    dimnames = list(Terms=names(keys), Docs = names(x@objects))
  )
  class(retval) <- c("TermDocumentMatrix", "simple_triplet_matrix")
  
  .message("cleaning up temporary key columns", verbose = verbose)
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
    .message("generating corpus positions", verbose = verbose)
    
    cposList <- lapply(
      c(1:length(x@objects)),
      function(i) cbind(i, cpos(x@objects[[i]]@cpos))
    )
    cposMatrix <- do.call(rbind, cposList)
    .message("getting ids", verbose = verbose)
    id_vector <- CQI$cpos2id(x[[1]]@corpus, pAttribute, cposMatrix[,2])
    DT <- data.table(i = cposMatrix[,1], id = id_vector, key = c("i", "id"))
    .message("performing count", verbose = verbose)
    TF <- DT[,.N, by = c("i", "id"), with=TRUE]
    setnames(TF, old = "N", new = "count")
    TF[, pAttribute := as.nativeEnc(CQI$id2str(x[[1]]@corpus, pAttribute, TF[["id"]]), from = encoding), with = FALSE]
    .message("generating keys", verbose = verbose)
    uniqueTerms <- unique(TF[[pAttribute]])
    keys <- setNames(c(1:length(uniqueTerms)), uniqueTerms)
    .message("generating simple triplet matrix", verbose = verbose)
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
    .message("adding token ids for p-attribute:", pAttribute, verbose = verbose)
    x <- enrich(x, pAttribute = pAttribute)
  }
  
  .message("dropping nodes", verbose = verbose)
  CPOS <- x@cpos[which(x@cpos[["position"]] != 0)]
  
  .message("counting tokens in context", verbose = verbose)
  CPOS2 <- CPOS[, .N, by = c("hit_no", paste(pAttribute, "id", sep = "_"))]
  
  # create new index for hits
  # may be necessary if negativelist/positivelist has been applied
  .message("creating new index for hits", verbose = verbose)
  hits <- unique(CPOS[["hit_no"]])
  hits <- hits[order(hits, decreasing = FALSE)]
  hit_index_new <- 1:length(hits)
  names(hit_index_new) <- as.character(hits)
  CPOS2[, "i" := hit_index_new[as.character(CPOS2[["hit_no"]])], with = TRUE]
  
  # create new index for word_ids
  .message("creating new index for tokens", verbose = verbose)
  uniqueIDs <- unique(CPOS2[[paste(pAttribute, "id", sep = "_")]])
  uniqueIDs <- uniqueIDs[order(uniqueIDs, decreasing = FALSE)]
  idIndexNew <- setNames(1:length(uniqueIDs), as.character(uniqueIDs))
  decodedTokens <- as.nativeEnc(
    CQI$id2str(x@corpus, pAttribute, uniqueIDs),
    from = x@encoding
    )
  CPOS2[, "j" := idIndexNew[as.character(CPOS2[[paste(pAttribute, "id", sep = "_")]])], with = TRUE]
  
  .message("putting together matrix", verbose = verbose)
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
