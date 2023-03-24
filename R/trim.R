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
#' @param terms_to_drop A `character` vector with terms to exclude from matrix
#'   (terms used as stopwords).
#' @param docs_to_keep A `character` vector with documents to keep.
#' @param min_count A `numeric` value with a minimum value of total term
#'   frequency across documents to exclude rare terms from matrix.
#' @param min_doc_length A `numeric` value with minimum total of the summed-up
#'  occurrence of tokens in a document. Exclude documents below this value and
#'  filter out short documents. Note that the `min_doc_length` filter is applied
#'  before filtering for `min_count` and `terms_to_keep`, and that these filters
#'  will reduce document lengths.
#' @param verbose A `logical` value, whether to output progress messages.
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
#' @importFrom lifecycle deprecate_warn
#' @examples
#' use("RcppCWB", corpus = "REUTERS")
#' dtm <- corpus("REUTERS") %>%
#'   split(s_attribute = "id") %>%
#'   as.DocumentTermMatrix(p_attribute = "word", verbose = FALSE)
#' trim(dtm, min_doc_length = 100)
setMethod("trim", "TermDocumentMatrix", function(.Object, terms_to_drop, docs_to_keep, min_count, min_doc_length, verbose = TRUE, ...){
  t(trim(
    t(.Object),
    terms_to_drop = terms_to_drop,
    docs_to_keep = docs_to_keep,
    min_count = min_count,
    min_doc_length = min_doc_length
  ))
})


#' @rdname trim-method
#' @importFrom slam row_sums
#' @importFrom cli cli_alert_info
#' @importFrom slam col_sums
setMethod("trim", "DocumentTermMatrix", function(.Object, terms_to_drop, docs_to_keep, min_count, min_doc_length, verbose = TRUE, ...){
  if ("termsToKeep" %in% names(list(...)))
    deprecate_warn("0.8.8", "DocumentTermMatrix(termsToKeep)")
  
  if ("termsToDrop" %in% names(list(...))){
    deprecate_warn(
      "0.8.8",
      what = "DocumentTermMatrix(termsToDrop)",
      with = "DocumentTermMatrix(terms_to_drop)"
    )
  }

  if ("docsToKeep" %in% names(list(...))){
    deprecate_warn(
      "0.8.8",
      what = "DocumentTermMatrix(docsToKeep)",
      with = "DocumentTermMatrix(docs_to_keep)"
    )
  }
    
  if ("docsToDrop" %in% names(list(...))){
    deprecate_warn("0.8.8", "DocumentTermMatrix(docsToDrop)")
  }
  
  purge <- FALSE # set to TRUE on demand
  
  if (!missing(docs_to_keep)){
    nrow_old <- nrow(.Object)
    rows_to_keep <- which(rownames(.Object) %in% docs_to_keep)
    .Object <- .Object[rows_to_keep,]
    if (verbose) cli_alert_info(
      "apply docs_to_keep - dropped {.val {nrow_old - nrow(.Object)}} docs"
    )
  }
  
  if (!missing(min_doc_length)){
    nrow_old <- nrow(.Object)
    stopifnot(is.numeric(min_doc_length))
    docs_to_drop <- which(row_sums(.Object) < min_doc_length)
    if (length(docs_to_drop) > 0L) .Object <- .Object[-docs_to_drop,]
    if (verbose) cli_alert_info(
      "minimum document length {.val {min_doc_length}} - dropped {.val {nrow_old - nrow(.Object)}} docs (keeping {.val {nrow(.Object)}})"
    )
  }
   
  if (!missing(terms_to_drop)){
    ncol_old <- ncol(.Object)
    stopifnot(is.character(terms_to_drop))
    cols_to_drop <- which(colnames(.Object) %in% unique(terms_to_drop))
    if (length(cols_to_drop) > 0L) .Object <- .Object[,-cols_to_drop]
    if (verbose) cli_alert_info(
      "exclude terms_to_drop - dropped {.val {ncol_old - ncol(.Object)}} terms (keeping {.val {ncol(.Object)}})"
    )
    purge <- TRUE # cleanup empty docs later
  }

  if (!missing(min_count)){
    stopifnot(is.numeric(min_count))
    ncol_old <- ncol(.Object)
    rare_terms <- which(col_sums(.Object) < min_count)
    if (length(rare_terms) > 0L){
      .Object <- .Object[,-rare_terms]
      purge <- TRUE
    }
    if (verbose) cli_alert_info(
      "exclude infrequent terms - dropped {.val {ncol_old - ncol(.Object)}} terms (keeping {.val {ncol(.Object)}})"
    )
  }
  
  if (purge){
    empty_docs <- which(row_sums(.Object) == 0L)
    if (length(empty_docs) > 0L){
      .Object <- .Object[-empty_docs,]
      if (verbose) cli_alert_info(
        "dropped {.val {length(empty_docs)}} docs with no tokens left"
      )
    }
  }

  .Object
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
      dt <- data.table(cpos = ranges_to_cpos(positivelist), positivelist = TRUE)
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
