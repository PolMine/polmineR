#' @include partition.R partition_bundle.R context.R S4classes.R
#' @include features.R
NULL

#' Trim an object.
#' 
#' Method to trim and adjust objects by applying thresholds, minimum frequencies
#' etc. It can be applied to `context`, `features`, `context`, `partition` and
#' `partition_bundle` objects.
#' 
#' @param .Object The object to be trimmed
#' @param termsToKeep ...
#' @param termsToDrop ...
#' @param docsToKeep ...
#' @param docsToDrop ...
#' @param verbose logical
#' @param ... further arguments
#' @author Andreas Blaette
#' @docType methods
#' @aliases trim trim-method trim,TermDocumentMatrix-method
#' @export
#' @rdname trim-method
setGeneric("trim", function(.Object, ...) standardGeneric("trim") )



#' @importFrom Matrix rowSums
#' @importFrom tm stopwords
#' @importFrom slam as.simple_triplet_matrix
#' @rdname trim-method
setMethod("trim", "TermDocumentMatrix", function(.Object, termsToKeep = NULL, termsToDrop = NULL, docsToKeep = NULL, docsToDrop = NULL, verbose = TRUE){
  .rmBlank <- function(mat, verbose=TRUE){
    .message("removing empty rows", verbose = verbose)
    matTmp <- as.sparseMatrix(mat)
    matTmp <- matTmp[which(rowSums(matTmp) > 0),]
    mat <- as.simple_triplet_matrix(matTmp)
    class(mat) <- c("TermDocumentMatrix", "simple_triplet_matrix")
    mat
  }
  if (!is.null(docsToKeep)){
    .Object <- .Object[,which(colnames(.Object) %in% docsToKeep)]
  }
  if (!is.null(docsToDrop)){
    .Object <- .Object[,which(!colnames(.Object) %in% docsToDrop)]
  }
  if (!is.null(termsToKeep)){
    .Object <- .Object[which(rownames(.Object) %in% termsToKeep),]
  }
  if (!is.null(termsToDrop)){
    .Object <- .Object[which(!rownames(.Object) %in% termsToDrop), ]
  }
  .Object
})


#' @rdname trim-method
setMethod("trim", "DocumentTermMatrix", function(.Object, ...){
  t(trim(t(.Object), ...))
})


#' @rdname context-class
#' @examples 
#' # Keep matches for 'oil' only if first position to the left is 'crude'
#' .fn <- function(x) if (x[position == -1L][["word"]] == "crude") x else NULL
#' crude_oil <- context("REUTERS", "oil") %>%
#'   enrich(p_attribute = "word", decode = TRUE) %>%
#'   trim(fn = .fn)
setMethod("trim", "context", function(.Object, s_attribute = NULL, positivelist = NULL, p_attribute = p_attributes(.Object), regex = FALSE, stoplist = NULL, fn = NULL, verbose = TRUE, progress = TRUE, ...){
  
  if ("sAttribute" %in% names(list(...))){
    lifecycle::deprecate_warn(
      when = "0.8.7", 
      what = "trim(sAttribute)",
      with = "trim(s_attribute)"
    )
    s_attribute <- list(...)[["sAttribute"]]
  }
  
  if (!is.null(s_attribute)){
    stopifnot(length(s_attribute) == 1L)
    s_attr_col <- paste(s_attribute, "int", sep = "_")
    if (!s_attr_col %in% colnames(.Object@cpos)){
      enrich(.Object, s_attribute = s_attribute) # in-place operation
    }
    setnames(.Object@cpos, old = s_attr_col, new = "struc")

    .message("checking boundaries of regions", verbose = verbose)
    if (progress) pb <- txtProgressBar(min = 1, max = .Object@count, style = 3)
    .check <- function(.SD, .GRP){
      if (progress) setTxtProgressBar(pb, value = .GRP)
      struc_hit <- .SD[.SD[["position"]] == 0][["struc"]][1]
      .SD[.SD[["struc"]] == struc_hit]
    }
    .Object@cpos <- .Object@cpos[, .check(.SD, .GRP), by = "match_id"]
    if (progress) close(pb)
    setnames(.Object@cpos, old = "struc", new = s_attr_col)
  }
  
  if (!is.null(positivelist)){
    .message("filtering by positivelist", verbose = verbose)
    before <- length(unique(.Object@cpos[["match_id"]]))
    if (is.matrix(positivelist)){
      dt <- data.table(cpos = cpos(positivelist), positivelist = TRUE)
      cpos_min <- dt[.Object@cpos[.Object@cpos[["position"]] != 0], on = "cpos"]
      matches_to_keep <- cpos_min[,
        if (any(!is.na(.SD$positivelist))) .SD else NULL,
        by = "match_id"
      ][["match_id"]]
      .Object@cpos <- .Object@cpos[.Object@cpos[["match_id"]] %in% matches_to_keep]
    } else {
      positivelist_ids <- .token2id(
        corpus = .Object@corpus, p_attribute = p_attribute,
        token = positivelist, regex = regex
      )
      .fn <- function(.SD){
        neighbors <- .SD[[paste(p_attribute[1], "id", sep = "_")]][.SD[["position"]] != 0]
        if (any(neighbors %in% positivelist_ids)) return( .SD ) else return( NULL )
      }
      .Object@cpos <- .Object@cpos[, .fn(.SD), by = "match_id", with = TRUE]
    }
    
    if (nrow(.Object@cpos) == 0) {
      warning("no remaining hits after applying positivelist, returning NULL")
      return( invisible(NULL) )
    }

    .Object@count <- length(unique(.Object@cpos[["match_id"]]))
    .message(
      "number of hits dropped due to positivelist:",
      before - .Object@count, verbose = verbose
    )
    
    .Object <- enrich(.Object, stat = TRUE)
    
  }
  
  if (!is.null(stoplist)){
    .message("applying stoplist", verbose = verbose)
    before <- length(unique(.Object@cpos[["match_id"]]))
    stoplist_ids <- .token2id(corpus = .Object@corpus, p_attribute = p_attribute, token = stoplist, regex = regex)
    .fn <- function(.SD){
      p_attr <- paste(p_attribute[1], "id", sep = "_")
      negatives <- which(.SD[[p_attr]] %in% stoplist_ids)
      negatives <- negatives[ -which(.SD[["position"]] == 0) ] # exclude node
      if (any(negatives)) return( NULL ) else return( .SD ) # this is the only difference
    }
    .Object@cpos <- .Object@cpos[, .fn(.SD), by = "match_id", with = TRUE]
    
    if (nrow(.Object@cpos) == 0L) {
      warning("no remaining hits after applying stoplist, returning NULL")
      return( NULL )
    }

    .Object@count <- length(unique(.Object@cpos[["match_id"]]))
    .message(
      "number of hits dropped due to stoplist:",
      before - .Object@count,
      verbose = verbose
    )
    
    .Object <- enrich(.Object, stat = TRUE)
  }
  
  if (!is.null(fn)){
    before <- length(unique(.Object@cpos[["match_id"]]))
    .Object@cpos <- rbindlist(lapply(split(.Object@cpos, by = "match_id"), fn))
    
    if (nrow(.Object@cpos) == 0L) {
      warning("no remaining hits after applying trimming fn, returning NULL")
      return( NULL )
    }
    
    .Object@count <- length(unique(.Object@cpos[["match_id"]]))
    .message("new number of hits:", .Object@count, verbose = verbose)
    .Object <- enrich(.Object, stat = TRUE)
  }
  
  .Object
})
