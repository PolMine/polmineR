#' @include S4classes.R bundle.R partition_bundle.R context.R
NULL

setOldClass("TermDocumentMatrix")
setOldClass("DocumentTermMatrix")


#' @importFrom slam as.simple_triplet_matrix
setMethod("cbind2", signature = c(x = "TermDocumentMatrix", y = "TermDocumentMatrix"), function(x,y){
  combinedMatrix <- do.call(cbind2, lapply(list(x,y), as.sparseMatrix))
  returnedMatrix <- as.simple_triplet_matrix(combinedMatrix)
  class(returnedMatrix) <- c("TermDocumentMatrix", "simple_triplet_matrix")
  returnedMatrix
})



#' Generate TermDocumentMatrix / DocumentTermMatrix.
#' 
#' Methods to generate the classes \code{TermDocumentMatrix} or 
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
#' subsets of the corpus based on the \code{s_attribute} provided. Counts are
#' performed for the \code{p_attribute}. Further parameters provided (passed in
#' as \code{...} are interpreted as s-attributes that define a subset of the
#' corpus for splitting it according to \code{s_attribute}. If struc values for
#' \code{s_attribute} are not unique, the necessary aggregation is performed, slowing
#' things somewhat down.
#' 
#' If \code{x} is a \code{bundle} or a class inheriting from it, the counts or
#' whatever measure is present in the \code{stat} slots (in the column
#' indicated by \code{col}) will be turned into the values of the sparse
#' matrix that is generated. A special case is the generation of the sparse
#' matrix based on a \code{partition_bundle} that does not yet include counts.
#' In this case, a \code{p_attribute} needs to be provided. Then counting will
#' be performed, too.
#' 
#' @param x a \code{character} vector indicating a corpus, or an object of class
#'   \code{bundle}, or inheriting from class \code{bundle} (e.g. \code{partition_bundle})
#' @param p_attribute p-attribute counting is be based on
#' @param s_attribute s-attribute that defines content of columns, or rows
#' @param col the column of \code{data.table} in slot \code{stat} (if \code{x}
#'   is a \code{bundle}) to use of assembling the matrix
#' @param verbose logial, whether to output progress messages
#' @param ... s-attribute definitions used for subsetting the corpus, compare partition-method
#' @return a TermDocumentMatrix
#' @author Andreas Blaette
#' @exportMethod as.TermDocumentMatrix
#' @docType methods
#' @rdname as.DocumentTermMatrix
#' @name as.TermDocumentMatrix
#' @aliases as.DocumentTermMatrix
setGeneric("as.TermDocumentMatrix", function(x, ...) UseMethod("as.TermDocumentMatrix") )


#' @exportMethod as.DocumentTermMatrix
#' @rdname as.DocumentTermMatrix
setGeneric("as.DocumentTermMatrix", function(x, ...) UseMethod("as.DocumentTermMatrix") )

#' @examples
#' use("polmineR")
#'  
#' # do-it-yourself 
#' p <- partition("GERMAPARLMINI", date = ".*", regex = TRUE)
#' pB <- partition_bundle(p, s_attribute = "date")
#' pB <- enrich(pB, p_attribute="word")
#' tdm <- as.TermDocumentMatrix(pB, col = "count")
#'    
#'  # leave the counting to the as.TermDocumentMatrix-method
#' pB2 <- partition_bundle(p, s_attribute = "date")
#' tdm <- as.TermDocumentMatrix(pB2, p_attribute = "word", verbose = TRUE)
#'    
#' # diretissima
#' tdm <- as.TermDocumentMatrix("GERMAPARLMINI", p_attribute = "word", s_attribute = "date")
#' @rdname as.DocumentTermMatrix
setMethod("as.TermDocumentMatrix", "character",function (x, p_attribute, s_attribute, verbose = TRUE, ...) {
  if ("pAttribute" %in% names(list(...))) p_attribute <- list(...)[["pAttribute"]]
  if ("sAttribute" %in% names(list(...))) s_attribute <- list(...)[["sAttribute"]]
  
  y <- as.DocumentTermMatrix(x = x, p_attribute = p_attribute, s_attribute = s_attribute, verbose = verbose, ...)
  as.TermDocumentMatrix(y)
})



#' @rdname as.DocumentTermMatrix
setMethod("as.DocumentTermMatrix", "character", function(x, p_attribute, s_attribute, verbose = TRUE, ...){
  
  if ("pAttribute" %in% names(list(...))) p_attribute <- list(...)[["pAttribute"]]
  dot_list <- list(...)
  if ("sAttribute" %in% names(dot_list)){
    s_attribute <- list(...)[["sAttribute"]]
    dot_list[["sAttribute"]] <- NULL
  }
  
  stopifnot(
    length(x) == 1,
    x %in% CQI$list_corpora(),
    is.character(p_attribute),
    length(p_attribute) == 1,
    p_attribute %in% CQI$attributes(x, "p"),
    is.character(s_attribute),
    length(s_attribute) == 1,
    s_attribute %in% CQI$attributes(x, "s"),
    is.logical(verbose),
    all(names(dot_list) %in% CQI$attributes(x, "s"))
  )
  
  .message("generate data.table with token and struc ids", verbose = verbose)
  cpos_vector <- 0:(CQI$attribute_size(x, p_attribute, type = "p") - 1)
  token_id <- CQI$cpos2id(x, p_attribute, cpos_vector)
  struc_vector <- 0:(CQI$attribute_size(x, s_attribute, type = "s") - 1)
  struc_id <- CQI$cpos2struc(x, s_attribute, cpos_vector)
  tokenStreamDT <- data.table(cpos = cpos_vector, token_id = token_id, struc_id = struc_id)
  tokenStreamDT <- tokenStreamDT[which(tokenStreamDT[["struc_id"]] != -1)]
  rm(token_id, struc_id)
  
  sAttrSelect <- dot_list
  
  if (
    length(sAttrSelect) == 0
    && length(unique(CQI$struc2str(x, s_attribute, struc_vector))) == CQI$attribute_size(x, s_attribute, type = "s")
  ){
    
    token_id <- CQI$cpos2id(x, p_attribute, cpos_vector)
    struc_id <- CQI$cpos2struc(x, s_attribute, cpos_vector)
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
    docs <- CQI$struc2str(x, s_attribute, 0:(CQI$attribute_size(x, s_attribute, type = "s") - 1))
    terms <- CQI$id2str(x, p_attribute, 0:max(countDT[["token_id"]]))
    terms <- as.nativeEnc(terms, from = registry_get_encoding(x))
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
    struc_values <- CQI$struc2str(x, s_attribute, tokenStreamDT[["struc_id"]])
    unique_struc_values <- unique(struc_values)
    doc_index <- setNames(object = 1:length(unique_struc_values), nm = unique_struc_values)
    tokenStreamDT[["doc_id"]] <- doc_index[ struc_values ]
    tokenStreamDT[, "struc_id" := NULL][, "cpos" := NULL]
    
    .message("counting token per doc", verbose = verbose)
    countDT <- tokenStreamDT[, .N, by = c("token_id", "doc_id"), with = TRUE]
    unique_token_ids <- unique(tokenStreamDT[["token_id"]])
    new_token_index <- setNames(1:length(unique_token_ids), as.character(unique_token_ids))
    countDT[["new_token_id"]] <- new_token_index[ as.character(countDT[["token_id"]]) ]
    names(new_token_index) <- CQI$id2str(x, p_attribute, as.integer(names(new_token_index)))
    
    .message("generate simple_triplet_matrix", verbose = verbose)
    dtm <- simple_triplet_matrix(
      i = countDT[["doc_id"]],
      j = countDT[["new_token_id"]],
      v = countDT[["N"]],
    )
    
    .message("add row and column labels", verbose = verbose)
    terms <- as.nativeEnc(names(new_token_index), from = registry_get_encoding(x))
    documents <- as.nativeEnc(names(doc_index), from = registry_get_encoding(x))
    
    dimnames(dtm) <- list(documents, terms)
  }
  class(dtm) <- c("TermDocumentMatrix", "simple_triplet_matrix")
  attr(dtm, "weighting") <- c("term frequency", "tf")
  dtm
})



#' @rdname as.DocumentTermMatrix
#' @importFrom slam simple_triplet_matrix
setMethod("as.TermDocumentMatrix", "bundle", function(x, col, p_attribute = NULL, verbose = TRUE, ...){
  
  if ("pAttribute" %in% names(list(...))) p_attribute <- list(...)[["pAttribute"]]
  
  if (is.null(p_attribute)){
    p_attribute <- x@objects[[1]]@p_attribute
    .message("using the p_attribute-slot of the first object in the bundle as p_attribute:", p_attribute, verbose = verbose)
  }
  .message("generating (temporary) key column", verbose = verbose)
  if (length(p_attribute) > 1){
    dummy <- lapply(
      1L:length(x@objects),
      function(i){
        keysRaw <- x@objects[[i]]@stat[, c(p_attribute), with = FALSE]
        keys <- apply(keys, 1, function(row) paste(row, collapse="//"))
        x@objects[[i]]@stat[, key := keys]
      })
    rm(dummy)
  } else {
    lapply(
      1L:length(x@objects),
      function(i) setnames(x@objects[[i]]@stat, old = p_attribute, new = "key")
    )
  }
  .message("generating cumulated data.table", verbose = verbose)
  DT <- data.table::rbindlist(lapply(x@objects, function(y) y@stat))
  j <- unlist(lapply(1L:length(x@objects), function(i) rep(i, times = nrow(x@objects[[i]]@stat))))
  DT[, "j" := j]
  DT <- DT[which(DT[["key"]] != "")] # to avoid errors
  .message("getting unique keys", verbose = verbose)
  uniqueKeys <- unique(DT[["key"]])
  keys <- setNames(1L:length(uniqueKeys), uniqueKeys)
  .message("generating integer keys", verbose = verbose)
  i <- keys[ DT[["key"]] ]
  retval <- simple_triplet_matrix(
    i = unname(i), j = DT[["j"]], v = DT[[col]],
    nrow = length(names(keys)), ncol = length(names(x@objects)),
    dimnames = list(Terms = names(keys), Docs = names(x@objects))
  )
  class(retval) <- c("TermDocumentMatrix", "simple_triplet_matrix")
  
  .message("cleaning up temporary key columns", verbose = verbose)
  if (length(p_attribute) > 1){
    dummy <- lapply(1L:length(x@objects), function(i) x@objects[[i]]@stat[, key := NULL])
  } else {
    dummy <- lapply(1L:length(x@objects), function(i) setnames(x@objects[[i]]@stat, old = "key", new = p_attribute))
  }
  attr(retval, "weighting") <- c("term frequency", "tf")
  retval
})

#' @rdname as.DocumentTermMatrix
setMethod("as.DocumentTermMatrix", "bundle", function(x, col, p_attribute = NULL, verbose = TRUE, ...) {
  if ("pAttribute" %in% names(list(...))) p_attribute <- list(...)[["pAttribute"]]
  as.DocumentTermMatrix(as.TermDocumentMatrix(x = x, col = col, p_attribute = p_attribute, verbose = verbose))
})

#' @rdname as.DocumentTermMatrix
setMethod("as.TermDocumentMatrix", "partition_bundle", function(x, p_attribute = NULL, col = NULL, verbose = TRUE, ...){
  if ("pAttribute" %in% names(list(...))) p_attribute <- list(...)[["pAttribute"]]
  if (!is.null(col)){
    callNextMethod()
  } else if (!is.null(p_attribute)){
    encoding <- unique(sapply(x@objects, function(y) y@encoding))
    .message("generating corpus positions", verbose = verbose)
    
    cposList <- lapply(
      1L:length(x@objects),
      function(i) cbind(i, cpos(x@objects[[i]]@cpos))
    )
    cposMatrix <- do.call(rbind, cposList)
    .message("getting ids", verbose = verbose)
    id_vector <- CQI$cpos2id(x[[1]]@corpus, p_attribute, cposMatrix[,2])
    DT <- data.table(i = cposMatrix[,1], id = id_vector, key = c("i", "id"))
    .message("performing count", verbose = verbose)
    TF <- DT[,.N, by = c("i", "id"), with = TRUE]
    setnames(TF, old = "N", new = "count")
    TF[, (p_attribute) := as.nativeEnc(CQI$id2str(x[[1]]@corpus, p_attribute, TF[["id"]]), from = encoding)]
    .message("generating keys", verbose = verbose)
    uniqueTerms <- unique(TF[[p_attribute]])
    keys <- setNames(1L:length(uniqueTerms), uniqueTerms)
    .message("generating simple triplet matrix", verbose = verbose)
    retval <- simple_triplet_matrix(
      i = keys[ TF[[p_attribute]] ], j = TF[["i"]], v = TF[["count"]],
      dimnames = list(Terms = names(keys), Docs = names(x@objects))
    )
    class(retval) <- c("TermDocumentMatrix", "simple_triplet_matrix")
    return( retval )
  } else {
    message("... doing nothing, as p_attribute and col is NULL")
  }
})

#' @rdname as.DocumentTermMatrix
setMethod("as.DocumentTermMatrix", "partition_bundle", function(x, p_attribute = NULL, col = NULL, verbose = TRUE, ...){
  if ("pAttribute" %in% names(list(...))) p_attribute <- list(...)[["pAttribute"]]
  as.DocumentTermMatrix(as.TermDocumentMatrix(x = x, p_attribute = p_attribute, col = col, verbose = verbose))
})

#' @rdname as.DocumentTermMatrix
setMethod("as.DocumentTermMatrix", "context", function(x, p_attribute, verbose = TRUE, ...){
  if ("pAttribute" %in% names(list(...))) p_attribute <- list(...)[["pAttribute"]]
  if (!paste(p_attribute, "id", "_") %in% colnames(x@cpos)){
    .message("adding token ids for p-attribute:", p_attribute, verbose = verbose)
    x <- enrich(x, p_attribute = p_attribute)
  }
  
  .message("dropping nodes", verbose = verbose)
  CPOS <- x@cpos[which(x@cpos[["position"]] != 0)]
  
  .message("counting tokens in context", verbose = verbose)
  CPOS2 <- CPOS[, .N, by = c("hit_no", paste(p_attribute, "id", sep = "_"))]
  
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
  uniqueIDs <- unique(CPOS2[[paste(p_attribute, "id", sep = "_")]])
  uniqueIDs <- uniqueIDs[order(uniqueIDs, decreasing = FALSE)]
  idIndexNew <- setNames(1:length(uniqueIDs), as.character(uniqueIDs))
  decodedTokens <- as.nativeEnc(
    CQI$id2str(x@corpus, p_attribute, uniqueIDs),
    from = x@encoding
  )
  CPOS2[, "j" := idIndexNew[as.character(CPOS2[[paste(p_attribute, "id", sep = "_")]])], with = TRUE]
  
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
setMethod("as.TermDocumentMatrix", "context", function(x, p_attribute, verbose = TRUE, ...){
  if ("pAttribute" %in% names(list(...))) p_attribute <- list(...)[["pAttribute"]]
  as.DocumentTermMatrix(x = x, p_attribute = p_attribute, verbose = verbose)
})
