#' @include partition.R partition_bundle.R context.R S4classes.R
#' @include features.R
NULL

#' trim an object
#' 
#' Method to trim and adjust objects by 
#' applying thresholds, minimum frequencies etc. It can be applied to \code{context},
#' \code{features}, \code{context}, \code{partition} and \code{partition_bundle} objects.
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
setMethod("trim", "TermDocumentMatrix", function(object, termsToKeep = NULL, termsToDrop = NULL, docsToKeep = NULL, docsToDrop = NULL, verbose = TRUE){
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
setMethod("trim", "context", function(object, s_attribute = NULL, positivelist = NULL, p_attribute = p_attributes(object), regex = FALSE, stoplist = NULL, verbose = TRUE, progress = TRUE, ...){
  
  if ("sAttribute" %in% names(list(...))) s_attribute <- list(...)[["sAttribute"]]
  
  if (!is.null(s_attribute)){
    stopifnot(length(s_attribute) == 1L)
    s_attr_col <- paste(s_attribute, "int", sep = "_")
    if (!s_attr_col %in% colnames(object@cpos)){
      enrich(object, s_attribute = s_attribute) # in-place operation
    }
    setnames(object@cpos, old = s_attr_col, new = "struc")

    .message("checking boundaries of regions", verbose = verbose)
    if (progress) pb <- txtProgressBar(min = 1, max = object@count, style = 3)
    .checkBoundary <- function(.SD, .GRP){
      if (progress) setTxtProgressBar(pb, value = .GRP)
      struc_hit <- .SD[.SD[["position"]] == 0][["struc"]][1]
      .SD[.SD[["struc"]] == struc_hit]
    }
    object@cpos <- object@cpos[, .checkBoundary(.SD, .GRP), by = "match_id"]
    if (progress) close(pb)
    setnames(object@cpos, old = "struc", new = s_attr_col)
  }
  
  if (!is.null(positivelist)){
    .message("filtering by positivelist", verbose = verbose)
    before <- length(unique(object@cpos[["match_id"]]))
    if (is.matrix(positivelist)){
      dt <- data.table(cpos = cpos(positivelist), positivelist = TRUE)
      cpos_min <- dt[object@cpos[object@cpos[["position"]] != 0], on = "cpos"]
      matches_to_keep <- cpos_min[,
        if (any(!is.na(.SD$positivelist))) .SD else NULL,
        by = "match_id"
      ][["match_id"]]
      object@cpos <- object@cpos[object@cpos[["match_id"]] %in% matches_to_keep]
    } else {
      positivelist_ids <- .token2id(
        corpus = object@corpus, p_attribute = p_attribute,
        token = positivelist, regex = regex
      )
      .fn <- function(.SD){
        neighbors <- .SD[[paste(p_attribute[1], "id", sep = "_")]][.SD[["position"]] != 0]
        if (any(neighbors %in% positivelist_ids)) return( .SD ) else return( NULL )
      }
      object@cpos <- object@cpos[, .fn(.SD), by = "match_id", with = TRUE]
    }
    after <- length(unique(object@cpos[["match_id"]]))
    .message("number of hits dropped due to positivelist:", before - after, verbose = verbose)
    if (nrow(object@cpos) == 0) {
      warning("no remaining hits after applying positivelist, returning NULL object")
      return( invisible(NULL) )
    }
  }
  
  if (!is.null(stoplist)){
    .message("applying stoplist", verbose = verbose)
    before <- length(unique(object@cpos[["match_id"]]))
    stoplist_ids <- .token2id(corpus = object@corpus, p_attribute = p_attribute, token = stoplist, regex = regex)
    .fn <- function(.SD){
      p_attr <- paste(p_attribute[1], "id", sep = "_")
      negatives <- which(.SD[[p_attr]] %in% stoplist_ids)
      negatives <- negatives[ -which(.SD[["position"]] == 0) ] # exclude node
      if (any(negatives)) return( NULL ) else return( .SD ) # this is the only difference
    }
    object@cpos <- object@cpos[, .fn(.SD), by = "match_id", with = TRUE]
    after <- length(unique(object@cpos[["match_id"]]))
    .message("number of hits dropped due to stoplist:", before - after, verbose = verbose)
    if (nrow(object@cpos) == 0) {
      warning("no remaining hits after applying stoplist, returning NULL object")
      return( NULL )
    }
  }
  
  object
})
