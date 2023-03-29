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
#' \code{DocumentTermMatrix} as defined in the \code{tm} package.  There are
#' many text mining applications for document-term matrices. A
#' \code{DocumentTermMatrix} is required as input by the \code{topicmodels}
#' package, for instance.
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
#' @param x A `character` vector indicating a corpus, or an object of class
#'   `bundle`, or inheriting from class `bundle` (e.g. `partition_bundle`).
#' @param p_attribute A p-attribute counting is be based on.
#' @param s_attribute An s-attribute that defines content of columns, or rows.
#' @param col The column of \code{data.table} in slot \code{stat} (if \code{x}
#'   is a \code{bundle}) to use of assembling the matrix.
#' @param verbose A \code{logial} value, whether to output progress messages.
#' @param ... Definitions of s-attribute used for subsetting the corpus, compare
#'   partition-method.
#' @return A \code{TermDocumentMatrix}, or a \code{DocumentTermMatrix} object.
#'   These classes are defined in the \code{tm} package, and inherit from the
#'   \code{simple_triplet_matrix}-class defined in the \code{slam}-package.
#' @author Andreas Blaette
#' @exportMethod as.TermDocumentMatrix
#' @docType methods
#' @rdname as.DocumentTermMatrix
#' @name as.TermDocumentMatrix
#' @aliases as.DocumentTermMatrix
setGeneric(
  "as.TermDocumentMatrix",
  function(x, ...) UseMethod("as.TermDocumentMatrix")
)


#' @exportMethod as.DocumentTermMatrix
#' @rdname as.DocumentTermMatrix
setGeneric(
  "as.DocumentTermMatrix",
  function(x, ...) UseMethod("as.DocumentTermMatrix")
)

#' @examples
#' # examples not run by default to save time on CRAN test machines
#' \donttest{
#' #' use(pkg = "RcppCWB", corpus = "REUTERS")
#'  
#' # enriching partition_bundle explicitly 
#' tdm <- corpus("REUTERS") %>% 
#'   partition_bundle(s_attribute = "id") %>% 
#'   enrich(p_attribute = "word") %>%
#'   as.TermDocumentMatrix(col = "count")
#'    
#' # leave the counting to the as.TermDocumentMatrix-method
#' tdm <- partition_bundle("REUTERS", s_attribute = "id") %>% 
#'   as.TermDocumentMatrix(p_attribute = "word", verbose = FALSE)
#'   
#' # obtain TermDocumentMatrix directly (fastest option)
#' tdm <- as.TermDocumentMatrix(
#'   "REUTERS",
#'   p_attribute = "word",
#'   s_attribute = "id",
#'   verbose = FALSE
#' )
#' 
#' # workflow using split()
#' dtm <- corpus("REUTERS") %>%
#'   split(s_attribute = "id") %>%
#'   as.TermDocumentMatrix(p_attribute = "word")
#' }
#' @rdname as.DocumentTermMatrix
setMethod("as.TermDocumentMatrix", "character",function (x, p_attribute, s_attribute, verbose = TRUE, ...) {
  if ("pAttribute" %in% names(list(...))){
    lifecycle::deprecate_warn(
      when = "0.8.7", 
      what = "as.TermDocumentMatrix(pAttribute)",
      with = "as.TermDocumentMatrix(p_attribute)"
    )
    p_attribute <- list(...)[["pAttribute"]]
  }
  if ("sAttribute" %in% names(list(...))){
    lifecycle::deprecate_warn(
      when = "0.8.7", 
      what = "as.TermDocumentMatrix(sAttribute)",
      with = "as.TermDocumentMatrix(s_attribute)"
    )
    s_attribute <- list(...)[["sAttribute"]]
  }
  
  y <- as.DocumentTermMatrix(x = x, p_attribute = p_attribute, s_attribute = s_attribute, verbose = verbose, ...)
  as.TermDocumentMatrix(y)
})


#' @importFrom cli cli_progress_step
#' @param binarize A `logical` value. If `TRUE`, report occurence of term, not
#'   absoulte count.
#' @param stoplist A `character` vector of tokens to exclude from the matrix, as
#'   memory efficient way to exclude irrelevant terms early on.
#' @rdname as.DocumentTermMatrix
setMethod("as.DocumentTermMatrix", "corpus", function(x, p_attribute, s_attribute, stoplist = NULL, binarize = FALSE, verbose = TRUE, ...){
  
  s_attr_select <- list(...)

  stopifnot(
    is.character(p_attribute),
    length(p_attribute) == 1L,
    p_attribute %in% p_attributes(x),
    
    is.character(s_attribute),
    length(s_attribute) == 1L,
    s_attribute %in% s_attributes(x),
    is.logical(verbose),
    
    is.list(s_attr_select),
    all(names(s_attr_select) %in% s_attributes(x)),
    
    is.logical(binarize)
  )
  
  p_attr_size <- cl_attribute_size(
    corpus = x@corpus, registry = x@registry_dir,
    attribute = p_attribute, attribute_type = "p"
  )
  token_stream_dt <- data.table(cpos = 0L:(p_attr_size - 1L))
  
  if (verbose) cli_progress_step("get token ids")
  token_stream_dt[, "token_id" := cpos2id(x = x, p_attribute = p_attribute, cpos = token_stream_dt[["cpos"]])]
  
  if (verbose) cli_progress_step("get struc ids")
  token_stream_dt[, "struc_id" := cl_cpos2struc(
    corpus = x@corpus, registry = x@registry_dir,
    s_attribute = s_attribute, cpos = token_stream_dt[["cpos"]]
  )]
  
  if (verbose) cli_progress_step("drop tokens outside strucs")
  token_stream_dt <- token_stream_dt[token_stream_dt[["struc_id"]] >= 0L]
  
  if (!is.null(stoplist)){
    if (!is.numeric(stoplist)){
      if (verbose) cli_progress_step("get ids for tokens on stoplist")
      stoplist <- cl_str2id(
        corpus = x@corpus, registry = x@registry_dir,
        p_attribute = p_attribute, str = stoplist
      )
    }
    token_stream_dt <- token_stream_dt[!token_stream_dt[["token_id"]] %in% stoplist]
  }

  if (verbose) cli_progress_step("get unique s-attribute values")
  s_attr_size <- cl_attribute_size(
    corpus = x@corpus, registry = x@registry_dir,
    attribute = s_attribute,
    attribute_type = "s"
  )

  avs_file <- sprintf("%s/%s.avs", x@data_dir, s_attribute)
  avs_size <- file.info(avs_file)$size
  s_attr_values <- readBin(con = avs_file, what = character(), n = avs_size)
  
  if (length(s_attr_select) == 0L && length(unique(s_attr_values)) == s_attr_size){

    token_stream_dt[, "cpos" := NULL] # to save memory
    gc()
    if (!binarize){
      if (verbose) cli_progress_step("counting token per doc")
      count_dt <- token_stream_dt[, .N, by = c("token_id", "struc_id"), with = TRUE]
      rm(token_stream_dt)
      gc()
    } else {
      if (verbose) cli_progress_step("counting token per doc (binarized)")
      count_dt <- token_stream_dt[, unique(.SD), by = "struc_id"]
      rm(token_stream_dt)
      gc()
      count_dt[, "N" := 1L]
    }

    if (verbose) cli_progress_step("decode strucs")
    docs <- cl_struc2str(
      corpus = x@corpus, registry = x@registry_dir,
      s_attribute = s_attribute, struc = 0L:(s_attr_size - 1L)
    )
    
    if (verbose) cli_progress_step("decode and recode token ids")
    terms <- cl_id2str(
      corpus = x@corpus, registry = x@registry_dir,
      p_attribute = p_attribute, id = 0L:max(count_dt[["token_id"]])
    )
    terms <- as.nativeEnc(terms, from = x@encoding)
    
    if (verbose) cli_progress_step("adjust ids")
    count_dt[, "struc_id" :=  count_dt[["struc_id"]] + 1L]
    count_dt[, "token_id" :=  count_dt[["token_id"]] + 1L]

    # The following code is extracted from slam::simple_triplet_matrix()
    # It skips anyDuplicated(cbind(stm$i, stm$j)) which is not necessary
    # and time consuming for big data
    if (verbose) cli_progress_step("generate simple_triplet_matrix")
    dtm <- list(
      i = count_dt[["struc_id"]],
      j = count_dt[["token_id"]],
      v = count_dt[["N"]],
      nrow = max(count_dt[["struc_id"]]),
      ncol = max(count_dt[["token_id"]]),
      dimnames = list(docs, terms)
    )

  } else {
    if (length(s_attr_select) >= 1L){
      for (i in 1L:length(s_attr_select)){
        s_attr_sub <- names(s_attr_select)[i]
        if (verbose) cli_progress_step("subsetting data.table by s-attribute", s_attr_sub)
        struc_id <- cl_cpos2struc(
          corpus = x@corpus, registry = x@registry_dir,
          s_attribute = s_attr_sub, cpos = token_stream_dt[["cpos"]]
        )
        token_stream_dt[, "cpos" := NULL]
        struc_values <- cl_struc2str(
          corpus = x@corpus, registry = x@registry_dir,
          s_attribute = s_attr_sub, struc = struc_id
        )
        token_stream_dt <- token_stream_dt[ which(struc_values %in% as.character(s_attr_select[[i]])) ]
      }
    }
    if (verbose) cli_progress_step("generate unique document ids")
    struc_values <- cl_struc2str(
      corpus = x@corpus, registry = x@registry_dir,
      s_attribute = s_attribute, struc = token_stream_dt[["struc_id"]]
    )
    s_attr_factor <- factor(struc_values)
    token_stream_dt[, "doc_id" := as.integer(s_attr_factor)]
    token_stream_dt[, "struc_id" := NULL]
    
    if (!binarize){
      if (verbose) cli_progress_step("counting token per doc")
      count_dt <- token_stream_dt[, .N, by = c("token_id", "doc_id"), with = TRUE]
    } else {
      if (verbose) cli_progress_step("counting token per doc (binarized)")
      count_dt <- token_stream_dt[, unique(.SD), by = "doc_id"]
      rm(token_stream_dt)
      gc()
      count_dt[, "N" := 1L]
    }
    
    if (verbose) cli_progress_step("decode tokens and generate factor")
    token_decoded <- cl_id2str(
      corpus = x@corpus, registry = x@registry_dir,
      p_attribute = p_attribute, id = count_dt[["token_id"]]
    )
    token_factor <- factor(token_decoded)
    
    if (verbose) cli_progress_step("prepare row and column labels")
    terms <- as.nativeEnc(levels(token_factor), from = x@encoding)
    docs <- as.nativeEnc(levels(s_attr_factor), from = x@encoding)

    if (verbose) cli_progress_step("generate simple_triplet_matrix")
    token_ids <- as.integer(token_factor)
    
    dtm <- list(
      i = count_dt[["doc_id"]],
      j = token_ids,
      v = count_dt[["N"]],
      nrow = max(count_dt[["doc_id"]]),
      ncol = max(token_ids),
      dimnames = list(Docs = docs, Terms = terms)
    )
  }
  class(dtm) <- c("DocumentTermMatrix", "simple_triplet_matrix")
  attr(dtm, "weighting") <- c("term frequency", "tf")
  dtm
})



#' @rdname as.DocumentTermMatrix
setMethod("as.DocumentTermMatrix", "character", function(x, p_attribute, s_attribute, verbose = TRUE, ...){
  as.DocumentTermMatrix(
    x = corpus(x),
    p_attribute = p_attribute, s_attribute = s_attribute,
    verbose = verbose,
    ...
  )
})



#' @rdname as.DocumentTermMatrix
#' @importFrom slam simple_triplet_matrix
setMethod("as.TermDocumentMatrix", "bundle", function(x, col, p_attribute = NULL, verbose = TRUE, ...){
  
  if ("pAttribute" %in% names(list(...))){
    lifecycle::deprecate_warn(
      when = "0.8.8", 
      what = "as.TermDocumentMatrix(pAttribute)",
      with = "as.TermDocumentMatrix(p_attribute)"
    )
    p_attribute <- list(...)[["pAttribute"]]
  }
  
  if (is.null(names(x))) names(x) <- as.character(seq_along(x))

  if (is.null(p_attribute)){
    p_attribute <- x@objects[[1]]@p_attribute
    if (verbose) cli::cli_alert_info(
      sprintf("using p-attribute %s", col_blue(p_attribute))
    )
  }
  
  if (verbose) cli_process_start("create temporary data.table")
  DT <- data.table::rbindlist(lapply(x@objects, slot, "stat"))
  DT[, "j" := unlist(mapply(
      rep,
      seq_along(x@objects),
      lapply(x@objects, function(obj) nrow(obj@stat))
    ))
  ]
  if (verbose) cli_process_done()

  if (verbose) cli_process_start("assign keys to data.table")
  if (length(p_attribute) == 1L){
    setnames(DT, old = p_attribute, new = "key")
  } else {
    DT[, "key" := do.call(paste, c(DT[, p_attribute, with = FALSE], sep = "//"))]
    for (p_attr in p_attribute) DT[, (p_attr) := NULL]
  }
  DT <- DT[which(DT[["key"]] != ""), c("key", col, "j"), with = FALSE] # to avoid errors
  if (verbose) cli_process_done()
  
  if (verbose) cli_process_start("make unique keys")
  unique_keys <- unique(DT[["key"]])
  keys <- setNames(seq_along(unique_keys), unique_keys)
  rm(unique_keys)
  i <- keys[ DT[["key"]] ]
  DT[, "key" := NULL]
  if (verbose) cli_process_done()
  
  if (verbose) cli_process_start("run garbage collection")
  gc()
  if (verbose) cli_process_done()

  if (verbose) cli_process_start("create TermDocumentMatrix")
  retval <- simple_triplet_matrix(
    i = unname(i),
    j = DT[["j"]],
    v = DT[[col]],
    nrow = length(names(keys)),
    ncol = length(names(x@objects)),
    dimnames = list(
      Terms = names(keys),
      Docs = names(x@objects)
    )
  )
  class(retval) <- c("TermDocumentMatrix", "simple_triplet_matrix")
  attr(retval, "weighting") <- c("term frequency", "tf")
  if (verbose) cli_process_done()
  
  retval
})

#' @rdname as.DocumentTermMatrix
setMethod("as.DocumentTermMatrix", "bundle", function(x, col = NULL, p_attribute = NULL, verbose = TRUE, ...) {
  if ("pAttribute" %in% names(list(...))) p_attribute <- list(...)[["pAttribute"]]
  as.DocumentTermMatrix(
    as.TermDocumentMatrix(
      x = x,
      col = col, p_attribute = p_attribute,
      verbose = verbose
    )
  )
})

#' @rdname as.DocumentTermMatrix
setMethod("as.DocumentTermMatrix", "partition_bundle", function(x, col = NULL, p_attribute = NULL, verbose = TRUE, ...) {
  if ("pAttribute" %in% names(list(...))) p_attribute <- list(...)[["pAttribute"]]
  as.DocumentTermMatrix(
    as.TermDocumentMatrix(x = x, col = col, p_attribute = p_attribute, verbose = verbose)
  )
})


#' @details If \code{x} is a \code{partition_bundle}, and argument \code{col} is
#'   not \code{NULL}, as \code{TermDocumentMatrix} is generated based on the
#'   column indicated by \code{col} of the \code{data.table} with counts in the
#'   \code{stat} slots of the objects in the bundle. If \code{col} is
#'   \code{NULL}, the p-attribute indicated by \code{p_attribute} is decoded,
#'   and a count is performed to obtain the values of the resulting
#'   \code{TermDocumentMatrix}. The same procedure applies to get a
#'   \code{DocumentTermMatrix}.
#' @rdname as.DocumentTermMatrix
#' @importFrom cli cli_alert_danger
setMethod("as.TermDocumentMatrix", "partition_bundle", function(x, p_attribute = NULL, col = NULL, verbose = TRUE, ...){
  if ("pAttribute" %in% names(list(...))) p_attribute <- list(...)[["pAttribute"]]
  
  if (!is.null(col)){
    return(callNextMethod())
  } else if (is.null(p_attribute)){
    cli::cli_alert_danger("return `NULL`, as p_attribute and col is NULL")
    return(NULL)
  }
  
  encoding <- unique(sapply(x@objects, slot, name = "encoding"))
  
  if (verbose) cli::cli_process_start("generating document ids")
  DT <- data.table(
    i = do.call(
      c,
      mapply(rep, seq_along(x@objects), lapply(x@objects, slot, "size"))
    )
  )
  if (verbose) cli::cli_process_done()
  
  if (verbose) cli::cli_process_start("getting ids")
  DT[, "id" := region_matrix_to_ids(
    corpus = x@corpus, registry = x@registry_dir,
    p_attribute = p_attribute,
    matrix = do.call(rbind, lapply(x@objects, slot, name = "cpos")))
  ]
  if (verbose) cli::cli_process_done()
  
  if (verbose) cli::cli_process_start("performing count")
  TF <- DT[, .N, by = c("i", "id"), with = TRUE]
  rm(DT)
  setnames(TF, old = "N", new = "count")
  if (verbose) cli::cli_process_done()
  
  if (verbose) cli::cli_process_start("decoding token ids")
  str <- cl_id2str(
    corpus = x[[1]]@corpus, registry = x[[1]]@registry_dir,
    p_attribute = p_attribute, id = TF[["id"]]
  )
  TF[, (p_attribute) := as.nativeEnc(str, from = encoding)]
  rm(str)
  if (verbose) cli::cli_process_done()
  
  if (verbose) cli::cli_process_start("generating keys")
  unique_terms <- unique(TF[[p_attribute]])
  keys <- setNames(1L:length(unique_terms), unique_terms)
  if (verbose) cli::cli_process_done()
  
  if (verbose) cli::cli_process_start("generating simple triplet matrix")
  retval <- simple_triplet_matrix(
    i = keys[ TF[[p_attribute]] ],
    j = TF[["i"]],
    v = TF[["count"]],
    dimnames = list(Terms = names(keys), Docs = names(x@objects))
  )
  class(retval) <- c("TermDocumentMatrix", "simple_triplet_matrix")
  attr(retval, "weighting") <- c("term frequency", "tf")
  if (verbose) cli::cli_process_done()
  
  retval
})


#' @details If \code{x} is a \code{subcorpus_bundle}, the p-attribute provided
#'   by argument \code{p_attribute} is decoded, and a count is performed to
#'   obtain the resulting \code{TermDocumentMatrix} or
#'   \code{DocumentTermMatrix}.
#' @rdname as.DocumentTermMatrix
setMethod("as.TermDocumentMatrix", "subcorpus_bundle", function(x, p_attribute = NULL, verbose = TRUE, ...){
  callNextMethod()
})


#' @rdname as.DocumentTermMatrix
setMethod("as.DocumentTermMatrix", "subcorpus_bundle", function(x, p_attribute = NULL, verbose = TRUE, ...){
  callNextMethod()
})



#' @rdname as.DocumentTermMatrix
setMethod("as.DocumentTermMatrix", "partition_bundle", function(x, p_attribute = NULL, col = NULL, verbose = TRUE, ...){
  if ("pAttribute" %in% names(list(...))) p_attribute <- list(...)[["pAttribute"]]
  as.DocumentTermMatrix(as.TermDocumentMatrix(x = x, p_attribute = p_attribute, col = col, verbose = verbose))
})

#' @rdname as.DocumentTermMatrix
setMethod("as.DocumentTermMatrix", "context", function(x, p_attribute, verbose = TRUE, ...){
  if ("pAttribute" %in% names(list(...))) p_attribute <- list(...)[["pAttribute"]]
  p_attr_id <- paste(p_attribute, "id", sep = "_")
  if (!p_attr_id %in% colnames(x@cpos)){
    x <- enrich(x, p_attribute = p_attribute, verbose = verbose)
  }
  
  .message("dropping nodes", verbose = verbose)
  cpos_min <- x@cpos[which(x@cpos[["position"]] != 0)]
  
  .message("counting tokens in context", verbose = verbose)
  cnt <- cpos_min[, .N, by = c("match_id", p_attr_id)]
  
  # create new index for hits
  # may be necessary if negativelist/positivelist has been applied
  .message("creating new index for hits", verbose = verbose)
  hits <- unique(cpos_min[["match_id"]])
  hits <- hits[order(hits, decreasing = FALSE)]
  hit_index_new <- 1L:length(hits)
  names(hit_index_new) <- as.character(hits)
  cnt[, "i" := hit_index_new[as.character(cnt[["match_id"]])], with = TRUE]
  
  # create new index for word_ids
  .message("creating new index for tokens", verbose = verbose)
  unique_ids <- unique(cnt[[p_attr_id]])
  unique_ids <- unique_ids[order(unique_ids, decreasing = FALSE)]
  id_index_new <- setNames(1L:length(unique_ids), as.character(unique_ids))
  decoded_tokens <- as.nativeEnc(
    cl_id2str(
      corpus = x@corpus, registry = x@registry_dir,
      p_attribute = p_attribute, id = unique_ids
    ),
    from = x@encoding
  )
  cnt[, "j" := id_index_new[as.character(cnt[[p_attr_id]])], with = TRUE]
  
  .message("putting together matrix", verbose = verbose)
  dtm <- simple_triplet_matrix(
    i = cnt[["i"]],
    j = cnt[["j"]],
    v = cnt[["N"]],
    dimnames = list(
      Docs = as.character(1L:max(cnt[["i"]])),
      Terms = decoded_tokens
    )
  )
  class(dtm) <- c("DocumentTermMatrix", "simple_triplet_matrix")
  attr(dtm, "weighting") <- c("term frequency", "tf")
  dtm
})


#' @rdname kwic-class
setMethod("as.DocumentTermMatrix", "kwic", function(x, p_attribute, verbose = TRUE, ...){
  as.DocumentTermMatrix(as(x, "context"), p_attribute = p_attribute, verbose = verbose, ...)
})

#' @rdname kwic-class
setMethod("as.TermDocumentMatrix", "kwic", function(x, p_attribute, verbose = TRUE, ...){
  as.TermDocumentMatrix(as(x, "context"), p_attribute = p_attribute, verbose = verbose, ...)
})


#' @rdname as.DocumentTermMatrix
setMethod("as.TermDocumentMatrix", "context", function(x, p_attribute, verbose = TRUE, ...){
  if ("pAttribute" %in% names(list(...))) p_attribute <- list(...)[["pAttribute"]]
  dtm <- as.DocumentTermMatrix(x = x, p_attribute = p_attribute, verbose = verbose)
  as.TermDocumentMatrix(dtm)
})



