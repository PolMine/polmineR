#' @include context.R textstat.R partition.R polmineR.R cooccurrences.R bundle.R S4classes.R ll.R decode.R
NULL


#' @docType methods
#' @rdname cooccurrences-class
setMethod("show", "cooccurrences", function(object) {
  y <- format(object, digits = 2L)
  if (Sys.getenv("RSTUDIO") == "1" && interactive() && is.na(Sys.getenv("NOT_CRAN", unset = NA))){
    view(y)
  } else {
    if (getOption("polmineR.browse")) browse(object@stat) else return(y) 
  }
})



#' @importFrom data.table copy
#' @rdname cooccurrences-class
setMethod("as.data.frame", "cooccurrences_bundle", function(x){
  dts <- lapply(
    x@objects,
    function(object) copy(object@stat)[, "a" := object@query, with = TRUE]
  )
  dt <- rbindlist(dts)
  pAttr <- unique(unlist(lapply(x@objects, function(C) C@p_attribute)))
  if (length(pAttr) > 1){
    b <- dt[[ pAttr[1] ]]
    for (i in 2:length(pAttr)) b <- paste(b, dt[[pAttr[i]]], sep = "//")
    dt[, "b":= b, with = TRUE]
    for (i in 1:length(pAttr)) dt[, eval(pAttr[i]) := NULL, with = TRUE]
  } else if (length(pAttr) == 1){
    setnames(dt, old = pAttr, new = "b")
  }
  setcolorder(dt, c("a", "b", colnames(dt)[-which(colnames(dt) %in% c("a", "b"))]))
  as.data.frame(dt)
})

#' Get cooccurrence statistics.
#' 
#' @param .Object a partition object, or a character vector with a CWB corpus
#' @param query query, may by a character vector to match a token, or a CQP query
#' @param cqp defaults to \code{is.cqp}-function, or provide TRUE/FALSE, relevant only if query is not NULL
#' @param cpos integer vector with corpus positions, defaults to NULL - then the corpus positions for the whole corpus will be used
#' @param p_attribute the p-attribute of the tokens/the query
#' @param s_attribute if provided, it will be checked that cpos do not extend beyond
#' the region defined by the s-attribute 
#' @param left Number of tokens to the left of the query match.
#' @param right Number of tokens to the right of the query match.
#' @param stoplist Exclude a query hit from analysis if stopword(s) is/are in
#'   context (relevant only if query is not NULL).
#' @param positivelist character vector or numeric vector: include a query hit
#'   only if token in positivelist is present. If positivelist is a character
#'   vector, it is assumed to provide regex expressions (incredibly long if the
#'   list is long) (relevant only if query is nut NULL)
#' @param regex logical, whether stoplist/positivelist are dealt with as regular expressions
#' @param method statistical test to use (defaults to "ll")
#' @param verbose logical, whether to be verbose
#' @param progress logical, whether to be verbose
#' @param keep list with tokens to keep
#' @param mc whether to use multicore
#' @param ... further parameters that will be passed into bigmatrix (applies only of big=TRUE)
#' @return a cooccurrences-class object
#' @exportMethod cooccurrences
#' @docType methods
#' @author Andreas Blaette
#' @export cooccurrences
#' @name cooccurrences
#' @rdname cooccurrences
#' @references 
#' Baker, Paul (2006): \emph{Using Corpora in Discourse Analysis}. London: continuum, p. 95-120 (ch. 5).
#' 
#' Manning, Christopher D.; Schuetze, Hinrich (1999): \emph{Foundations of Statistical Natural Language
#' Processing}. MIT Press: Cambridge, Mass., pp. 151-189 (ch. 5).
#' @examples
#' use("polmineR")
#' merkel <- partition("GERMAPARLMINI", interjection = "speech", speaker = ".*Merkel", regex = TRUE)
#' merkel <- enrich(merkel, p_attribute = "word")
#' cooc <- cooccurrences(merkel, query = "Deutschland")
setGeneric("cooccurrences", function(.Object, ...) standardGeneric("cooccurrences") )

#' @rdname cooccurrences
setMethod("cooccurrences", "character", function(
  .Object, query, cqp = is.cqp,
  p_attribute = getOption("polmineR.p_attribute"), s_attribute = NULL,
  left = getOption("polmineR.left"), right = getOption("polmineR.right"),
  stoplist = NULL, positivelist = NULL, regex = FALSE,
  keep = NULL, cpos = NULL, method = "ll",
  mc = getOption("polmineR.mc"), verbose = FALSE, progress = FALSE,
  ...
){
  if ("pAttribute" %in% names(list(...))) p_attribute <- list(...)[["pAttribute"]]
  if ("sAttribute" %in% names(list(...))) s_attribute <- list(...)[["sAttribute"]]
  
  if (missing(query)) stop("query missing - it is not possible to calculate cooccurrences")
  C <- context(
    .Object = .Object, query = query, cqp = is.cqp,
    p_attribute = p_attribute, s_attribute = s_attribute,
    left = left, right = right,
    stoplist = stoplist, positivelist = positivelist, regex = regex,
    count = TRUE, 
    mc = mc, verbose = verbose, progress = progress
  )
  if (is.null(C)) invisible(NULL) else cooccurrences(C, method = method, verbose = verbose)
})

#' @rdname cooccurrences
setMethod(
  "cooccurrences", "partition",
  function(
    .Object, query, cqp = is.cqp,
    left = getOption("polmineR.left"), right = getOption("polmineR.right"),
    p_attribute = getOption("polmineR.p_attribute"), s_attribute = NULL,
    stoplist = NULL, positivelist = NULL, keep = NULL,
    method = "ll",
    mc = FALSE, progress = TRUE, verbose = FALSE,
    ...
  ){
    if ("pAttribute" %in% names(list(...))) p_attribute <- list(...)[["pAttribute"]]
    if ("sAttribute" %in% names(list(...))) s_attribute <- list(...)[["sAttribute"]]
    C <- context(
      .Object = .Object, query = query, cqp = is.cqp,
      p_attribute = p_attribute, s_attribute = s_attribute,
      left = left, right = right,
      stoplist = stoplist, positivelist = positivelist,
      count = TRUE, 
      mc = mc, verbose = verbose, progress = progress
    )
    retval <- if (is.null(C)) invisible(NULL) else cooccurrences(C, method = method, verbose = verbose)
    retval
  }
)

#' @rdname cooccurrences
setMethod("cooccurrences", "context", function(.Object, method = "ll", verbose = FALSE){
  if (!is.null(method)){
    
    # enrich partition if necessary
    if (!all(paste(.Object@p_attribute, "id", sep = "_") %in% colnames(.Object@partition@stat))){
      # It may not seem logical that counts are performed for all p-attribute-combinations if
      # we deal with more than p-attribute. But doing it selectively is much, much slower
      # than the the comprehensive approach.
      .message("enrichtung partition by missing count for p-attribute: ", .Object@p_attribute, verbose = verbose)
      .Object@partition <- enrich(.Object@partition, p_attribute = .Object@p_attribute, decode = FALSE, verbose = FALSE)
    }
    
    setkeyv(.Object@stat, cols = paste(.Object@p_attribute, "id", sep = "_"))
    setkeyv(.Object@partition@stat, cols = paste(.Object@p_attribute, "id", sep = "_"))
    .Object@stat <- .Object@partition@stat[.Object@stat]
    for (p_attr in .Object@p_attribute){
      if (paste("i", p_attr, sep = ".") %in% colnames(.Object@stat)){
        .Object@stat[, eval(paste("i", p_attr, sep = ".")) := NULL, with = TRUE]
      }
    }
    setnames(.Object@stat, old = "count", new = "count_partition")
    for (test in method){
      .message("statistical test:", test, verbose = verbose)
      .Object <- do.call(test, args = list(.Object = .Object))  
    }
  }
  
  # finishing
  if (nrow(.Object@stat) > 0L){
    setkeyv(.Object@stat, .Object@p_attribute)
    for (x in grep("_id$", colnames(.Object@stat), value = TRUE)) .Object@stat[[x]] <- NULL
    setcolorder(
      .Object@stat,
      c(.Object@p_attribute, colnames(.Object@stat)[-which(colnames(.Object@stat) %in% .Object@p_attribute)])
    )
    setorderv(.Object@stat, cols = method[1], order = -1L)
  }
  
  retval <- new(
    "cooccurrences",
    stat = data.table(), cpos = data.table(),
    partition = new("partition", stat = data.table(), size = 0L),
    count = 0L
    )
  slots_to_get <- slotNames(retval)[-grep("partition", slotNames(retval))]
  for (x in slots_to_get) slot(retval, x) <- slot(.Object, x)
  retval
})


#' @rdname cooccurrences
setMethod("cooccurrences", "Corpus", function(.Object, query, p_attribute = getOption("polmineR.p_attribute"), ...){
  if ("pAttribute" %in% names(list(...))) p_attribute <- list(...)[["pAttribute"]]
  if (nrow(.Object$stat) == 0) .Object$count(p_attribute, decode = FALSE)
  P <- .Object$as.partition()
  cooccurrences(P, query = query, p_attribute = p_attribute, ...)
})



#' @rdname cooccurrences
setMethod("cooccurrences", "partition_bundle", function(.Object, query, mc = getOption("polmineR.mc"), ...){
  bundle <- new("cooccurrences_bundle")
  bundle@objects <- pbapply::pblapply(
    .Object@objects,
    function(x) cooccurrences(x, query = query, mc = mc, ...) 
  )
  names(bundle@objects) <- names(.Object@objects)
  for (i in 1L:length(bundle@objects)){
    if (!is.null(bundle@objects[[i]])) bundle@objects[[i]]@name <- .Object@objects[[i]]@name
  }
  for (i in rev(which(sapply(bundle@objects, is.null)))) bundle@objects[[i]] <- NULL
  bundle
})



#' Cooccurrences class for corpus/partition.
#' 
#' The class inherits from the \code{textstat}-class.
#' 
#' @param .Object A \code{Cooccurrences}-class object.
#' @param verbose Logical.
#' @param col A column to extract.
#' @slot left  Single \code{integer} value, number of tokens to the left.
#' @slot right  Single \code{integer} value, number of tokens to the right.
#' @slot p_attribute  A \code{character} vector, the p-attribute the evaluation of the corpus is based on.
#' @slot corpus  Length-one \code{character} vector, the CWB corpus used.
#' @slot stat  A \code{data.table} with the statistical analysis of cooccurrences.
#' @slot encoding  Length-one \code{character} vector, the encoding of the corpus.
#' @slot keep A \code{list} of named character vectors, names are p-attributes.
#' @slot drop A \code{list} of named character vectors, names are p-attributes.
#' @slot partition A \code{partition}.
#' @slot window_sizes A \code{data.table} linking the number of tokens in the
#'   context of a token identified by id.
#' @slot minimized Logical, whether the object has been minimized.
#' @docType class
#' @exportClass Cooccurrences
#' @rdname all-cooccurrences-class
setClass(
  "Cooccurrences",
  contains = "textstat", # slots inherited: corpus, p_attribute, encoding, stat, name
  slots = c(
    left = "integer",
    right = "integer",
    keep = "list",
    drop = "list",
    partition = "partition",
    window_sizes = "data.table",
    minimized = "logical"
  )
)

#' @rdname cooccurrences
setMethod("cooccurrences", "Cooccurrences", function(.Object, query){
  tests <- "ll"["ll" %in% colnames(.Object)]
  y <- new(
    "cooccurrences",
    corpus = .Object@corpus,
    p_attribute = .Object@p_attribute,
    encoding = .Object@partition@encoding,
    query = query,
    partition = .Object@partition,
    size_partition = size(.Object@partition),
    left = .Object@left,
    right = .Object@right,
    size = sum(.Object@window_sizes),
    boundary = character(),
    cpos = data.table(),
    call = character(),
    stat = subset(.Object@stat, .Object@stat[[paste("a", .Object@p_attribute, sep = "_")]] == query),
    method = tests,
    included = FALSE,
    size_ref = size(.Object@partition) - sum(.Object@window_sizes),
    size_coi = sum(.Object@window_sizes)
  )
  for (colname in c("a_word_id", "b_word_id", "size_window", "a_word", "a_count"))
    if (colname %in% colnames(y)) y@stat[, eval(colname) := NULL, with = TRUE]
  
  setnames(
    y@stat,
    old = c("ab_count", "b_count", "exp_coi", "exp_ref", "b_word"),
    new = c("count_window", "count_partition", "exp_window", "exp_partition", "word")
  )
  setorderv(y@stat, cols = tests[1], order = -1L)
  y@stat[[paste("rank", tests[1], sep = "_")]] <- 1L:nrow(y@stat)
  y
})



#' @exportMethod Cooccurrences
#' @rdname all_cooccurrences
setGeneric("Cooccurrences", function(.Object, ...) standardGeneric("Cooccurrences"))


#' @exportMethod Cooccurrences
#' @rdname all_cooccurrences
setMethod("Cooccurrences", "character", function(.Object, ...){
  Cooccurrences(Corpus$new(.Object)$as.partition(), ...)
})


#' Get all cooccurrences.
#' 
#' @param .Object A length-one character vector indicating a corpus, or a
#'   \code{partition} object.
#' @param p_attribute The positional attribute.
#' @param left Size of left context.
#' @param right Size of right context.
#' @param stoplist Tokens to drop.
#' @param mc Logical.
#' @param progress Logical.
#' @param verbose Logical.
#' @param ... Further arguments.
#' @importFrom parallel mcparallel mccollect
#' @importFrom data.table data.table melt.data.table as.data.table
#' @importFrom RcppCWB cl_id2str cl_str2id cl_cpos2id
#' @import methods
#' @importFrom RcppCWB get_cbow_matrix
#' @exportMethod Cooccurrences
#' @rdname all_cooccurrences
#' @examples 
#' stopwords <- unname(unlist(noise(terms("REUTERS", p_attribute = "word"), stopwordsLanguage = "en")))
#' r <- Cooccurrences(.Object = "REUTERS", p_attribute = "word", left = 5L, right = 5L, stoplist = stopwords)
#' ll(r)
#' r <- subset(r, ll > 11.83 & ab_count >= 5)
#' decode(r)
#' data.table::setorderv(r@stat, cols = "ll", order = -1L)
#' head(r, 25)
#' 
#' if (requireNamespace("igraph", quietly = TRUE)){
#'   r@partition <- enrich(r@partition, p_attribute = "word")
#'   g <- as_igraph(r, as.undirected = TRUE)
#'   plot(g)
#' }
#' 
#' a <- cooccurrences(r, query = "oil")
#' a <- data.table::as.data.table(a)
#' 
#' b <- cooccurrences("REUTERS", query = "oil", left = 5, right = 5, p_attribute = "word")
#' b <- data.table::as.data.table(b)
#' b <- b[!word %in% stopwords]
#' 
#' # now let's check whether results are identical
#' all(b[["word"]][1:5] == a[["word"]][1:5])
#' 
#' 
#' stopwords <- unlist(noise(terms("GERMAPARLMINI", p_attribute = "word"), stopwordsLanguage = "german"))
#' 
#' plpr_partition <- partition("GERMAPARLMINI", date = "2009-11-10", interjection = "speech", p_attribute = "word")
#' plpr_cooc <- Cooccurrences(
#'   plpr_partition, p_attribute = "word",
#'   left = 3L, right = 3L,
#'   stoplist = stopwords,
#'   verbose = TRUE
#' )
#' decode(plpr_cooc)
#' ll(plpr_cooc)
#' 
#' merkel <- partition("GERMAPARLMINI", speaker = "Merkel", date = "2009-11-10", interjection = "speech", regex = TRUE, p_attribute = "word")
#' merkel_cooc <- Cooccurrences(
#'   merkel, p_attribute = "word",
#'   left = 3L, right = 3L,
#'   stoplist = stopwords, 
#'   verbose = TRUE
#' )
#' decode(merkel_cooc)
#' ll(merkel_cooc)
#' 
#' merkel_min <- subset(
#'   merkel_cooc,
#'   by = subset(features(merkel_cooc, plpr_cooc), rank_ll <= 50)
#'   )
setMethod("Cooccurrences", "partition", function(
  .Object, p_attribute, left, right,
  stoplist = NULL,
  mc = getOption("polmineR.mc"), verbose = FALSE, progress = FALSE,
  ...
){
  y <- new(
    "Cooccurrences",
    corpus = .Object@corpus,
    encoding = .Object@encoding,
    left = as.integer(left),
    right = as.integer(right),
    p_attribute = p_attribute,
    stat = data.table(),
    window_sizes = data.table(),
    name = character(),
    minimized = FALSE,
    partition = .Object
  )
  
  
  if (length(p_attribute) == 1L){
    
    id_list <- lapply(
      1L:nrow(.Object@cpos),
      function(j)
        RcppCWB::cl_cpos2id(
          corpus = .Object@corpus,
          p_attribute = p_attribute,
          cpos = .Object@cpos[j,1]:.Object@cpos[j,2]
        )
    )

    if (!is.null(stoplist)){
      stoplist_ids <- cl_str2id(corpus = .Object@corpus, p_attribute = p_attribute, str = stoplist)
      stoplist_ids <- unique(stoplist_ids[which(stoplist_ids >= 0)])
    }

    for (i in c(-left:-1L, 1L:right)){
      
      if (verbose) message("Processing tokens at position: ", i)

      node_vector <- unlist(lapply(
        id_list,
        function(ids){
          if (i < 0){
            from <- -i + 1L
            to <- length(ids)
            if (to > from) return(ids[from:to]) else return(NULL)
          } else {
            to <- length(ids) - i
            if (to > 1L) return(ids[1L:to]) else return(NULL)
          }
        }
      ))
      
      collocate_vector <- unlist(lapply(
        id_list,
        function(ids){
          if (i < 0){
            to <- length(ids) + i
            if (to > 1L) return(ids[1L:to]) else return(NULL)
          } else {
            from <- i + 1L
            to <- length(ids)
            if (to > from) return(ids[from:to]) else return(NULL)
          }
        }
      ))
      
      dt <- data.table(a_id = node_vector, b_id = collocate_vector) [, .N, by = c("a_id", "b_id")]
      setkeyv(dt, cols = c("a_id", "b_id"))
      
      a_id <- 0L; b_id <- 0L # to pass R CMD check
      if (!is.null(stoplist)) dt <- dt[!a_id %in% stoplist_ids]
      
      if (i == -left){
        y@window_sizes <- dt[, {sum(.SD[["N"]])}, by = "a_id"]
        setnames(y@window_sizes, old = "V1", new = "size_window")
        setkeyv(y@window_sizes, cols = "a_id")
        if (!is.null(stoplist)) y@stat <- dt[!b_id %in% stoplist_ids] else y@stat <- dt
      } else {
        sizes <- dt[, {sum(.SD[["N"]])}, by = "a_id"]
        setkeyv(sizes, cols = "a_id")
        y@window_sizes <- merge(y@window_sizes, sizes, all = TRUE)
        y@window_sizes[, "size_window" := ifelse(is.na(y@window_sizes[["size_window"]]), 0L, y@window_sizes[["size_window"]]) + ifelse(is.na(y@window_sizes[["V1"]]), 0L, y@window_sizes[["V1"]])]
        y@window_sizes[, "V1" := NULL]
        
        if (!is.null(stoplist)) dt <- dt[!a_id %in% stoplist_ids][!b_id %in% stoplist_ids]
        
        y@stat <- merge(y@stat, dt, all = TRUE)
        y@stat[, "N" := ifelse(is.na(y@stat[["N.x"]]), 0L, y@stat[["N.x"]]) + ifelse(is.na(y@stat[["N.y"]]), 0L, y@stat[["N.y"]])]
        y@stat[, "N.x" := NULL][, "N.y" := NULL]
        rm(dt); gc()
      }
    }
    
    setnames(y@stat, old = "N", new = "ab_count")
    
  } else {
    
    if (length(.Object@p_attribute) == 0) stop("The partition is required to included counts. Enrich the object first!")
    
    pAttr <- sapply(p_attribute, function(x) paste(.Object@corpus, x, sep = "."))
    aColsId <- setNames(paste("a", p_attribute, "id", sep="_"), p_attribute)
    bColsId <- setNames(paste("b", p_attribute, "id", sep="_"), p_attribute)
    aColsStr <- setNames(paste("a", p_attribute, sep="_"), p_attribute)
    bColsStr <- setNames(paste("b", p_attribute, sep="_"), p_attribute)
    
    .makeWindows <- function(i, cpos, ...){
      cposMin <- cpos[i,1]
      cposMax <- cpos[i,2]
      if (cposMin != cposMax){
        cposRange <- cposMin:cposMax
        lapply(
          setNames(cposRange, cposRange),
          function(x) {
            cpos <- c((x - left):(x - 1L), (x + 1L):(x + right))
            cpos <- cpos[which(cpos >= cposMin)]
            cpos[which(cpos <= cposMax)]
          })
      }
    }
    bag <- blapply(as.list(1L:nrow(.Object@cpos)), f = .makeWindows, cpos = .Object@cpos, mc = mc)
    bCpos <- lapply(
      bag,
      function(x) lapply(names(x), function(y) rep(as.numeric(y), times = length(x[[y]])))
    )
    if (verbose) message("... putting together data.table")
    DT <- data.table(a_cpos = unlist(bag), b_cpos = unlist(bCpos))
    
    if (verbose) message("... getting token ids")
    lapply(
      p_attribute, function(x){
        DT[, eval(aColsId[x]) := cl_cpos2id(corpus = .Object@corpus, p_attribute = x, cpos = DT[["a_cpos"]]), with = TRUE]
        DT[, eval(bColsId[x]) := cl_cpos2id(corpus = .Object@corpus, p_attribute = x, cpos = DT[["b_cpos"]]), with = TRUE]
      }
    )
    if (verbose) message("... counting window size")
    
    contextDT <- DT[, .N, by = c(eval(aColsId)), with = TRUE]
    setnames(contextDT, "N", "size_window")
    y@window_sizes <- contextDT
    
    if (verbose) message("... counting co-occurrences")
    TF <- DT[, .N, by = c(eval(c(aColsId, bColsId))), with = TRUE]
    setnames(TF, "N", "ab_count")
    
    if (verbose) message("... adding window size")
    setkeyv(contextDT, cols = aColsId)
    setkeyv(TF, cols = aColsId)
    TF <- contextDT[TF]
  }
  y
})



#' @details The \code{as.simple_triplet_matrix}-method will transform a
#'   \code{Cooccurrences} object into a sparse matrix. For reasons of memory
#'   efficiency, decoding token ids is performed within the method at the
#'   latests possible stage. It is NOT necessary that decoded tokens are present
#'   in the table in the \code{Cooccurrences} object.
#' @exportMethod as.simple_triplet_matrix
#' @rdname all-cooccurrences-class
#' @examples
#' X <- Cooccurrences("REUTERS", p_attribute = "word", left = 5L, right = 5L)
#' m <- as.simple_triplet_matrix(X)
setMethod("as.simple_triplet_matrix", "Cooccurrences", function(x){
  
  if (length(x@p_attribute) > 1L) stop("Method only works if one and only one p-attribute is used.")
  unique_tokens <- unique(x@stat[["a_word"]], x@stat[["b_word"]])
  
  
  message("... creating data.table for reindexing")
  dt <- data.table(id = unique(x@stat[["a_id"]]))
  setkeyv(dt, cols = "id")
  setorderv(dt, cols = "id")
  dt[, "id_new" := 1L:nrow(dt), with = TRUE]
  setkeyv(x@stat, "a_id")
  
  message("... id2str for a")
  x@stat[, "a_new_key" := x@stat[dt][["id_new"]]]
  setkeyv(x@stat, "b_id")
  message("... id2str for b")
  x@stat[, "b_new_key" := x@stat[dt][["id_new"]]]
  message("... preparing simple_triplet_matrix")
  decoded_tokens <- as.nativeEnc(
    cl_id2str(corpus = x@corpus, p_attribute = x@p_attribute, id = dt[["id"]]),
    from = getEncoding(x@corpus)
  )
  rm(dt); gc()
  retval <- slam::simple_triplet_matrix(
    i = x@stat[["a_new_key"]], j = x@stat[["b_new_key"]], v = x@stat[["ab_count"]],
    dimnames = list(decoded_tokens, decoded_tokens)
  )
  x@stat[, "a_new_key" := NULL][, "b_new_key" := NULL]
  retval
})


#' @exportMethod ll
#' @rdname all-cooccurrences-class
setMethod("ll", "Cooccurrences", function(.Object, verbose = TRUE){
  
  if (verbose) message("... adding window size")
  
  setkeyv(.Object@window_sizes, "a_id")
  setkeyv(.Object@stat, "a_id")
  .Object@stat[, "size_window" := .Object@window_sizes[.Object@stat][["size_window"]]]
  
  if (length(.Object@p_attribute) == 1L){
    
    # if ("a_id" %in% colnames(.Object)) decode(.Object)
    
    cnt <- if (nrow(.Object@partition@stat) > 0L)
      .Object@partition@stat
    else 
      count(.Object@partition, p_attribute = .Object@p_attribute, decode = FALSE)@stat
    
    setkeyv(cnt, paste(.Object@p_attribute, "id", sep = "_"))
    
    setkeyv(.Object@stat, cols = "a_id")
    .Object@stat[, "a_count" := cnt[.Object@stat][["count"]] ]

    setkeyv(.Object@stat, cols = "b_id")
    .Object@stat[, "b_count" := cnt[.Object@stat][["count"]] ]

    # setnames(.Object@stat, old = c("a", "b"), new = c(paste("a", .Object@p_attribute, sep = "_"), paste("b", .Object@p_attribute, sep = "_")))
    
  } else {
    # if (verbose == TRUE) message("... converting ids to strings")
    # lapply(
    #   c(1:length(p_attribute)),
    #   function(i){
    #     TF[, eval(aColsStr[i]) := as.utf8(CQI$id2str(.Object@corpus, p_attribute[i], TF[[aColsId[i]]])), with = TRUE]
    #     TF[, eval(bColsStr[i]) := as.utf8(CQI$id2str(.Object@corpus, p_attribute[i], TF[[bColsId[i]]])), with=TRUE]
    #     TF[, eval(aColsId[i]) := NULL]
    #     TF[, eval(bColsId[i]) := NULL]
    #   }
    # )
    # setkeyv(TF, cols = aColsStr)
    # setkeyv(.Object@stat, cols = p_attribute)
    # TF[, "count_a" := .Object@stat[TF][["count"]]]
    # setkeyv(TF, cols=bColsStr)
    # TF[, "count_b" := .Object@stat[TF][["count"]]]
    # setcolorder(TF, c(aColsStr, bColsStr, "ab_count", "count_a", "count_b", "size_window"))
    # if (tcm == FALSE){
    #   coll@stat <- TF
    #   if ("ll" %in% method) {
    #     message('... g2-Test')
    #     coll <- ll(coll)
    #     coll@stat <- setorderv(coll@stat, cols="ll", order=-1)
    #   }
    #   return(coll)
    # } else if (tcm == TRUE){
    #   concatenate <- function(x) paste(x, collapse = "//")
    #   if (length(p_attribute) > 1){
    #     TF[, "strKeyA" := apply(TF[, eval(paste("a", p_attribute, sep = "_")), with = FALSE], 1, concatenate)]
    #     TF[, "strKeyB" := apply(TF[, eval(paste("b", p_attribute, sep = "_")), with = FALSE], 1, concatenate)]
    #   } else {
    #     setnames(TF, old = paste("a", p_attribute, sep = "_"), new = "strKeyA")
    #     setnames(TF, old = paste("b", p_attribute, sep = "_"), new = "strKeyB")
    #   }
    #   uniqueKey <- unique(c(TF[["strKeyA"]], TF[["strKeyB"]]))
    #   keys <- setNames(c(1:length(uniqueKey)), uniqueKey)
    #   i <- unname(keys[TF[["strKeyA"]]])
    #   j <- unname(keys[TF[["strKeyB"]]])
    #   retval <- simple_triplet_matrix(
    #     i = i, j = j, v = TF[["ab_count"]],
    #     dimnames = list(a = names(keys)[1:max(i)], b = names(keys)[1:max(j)])
    #   )
    #   return(retval)
    # }
    
  }
  
  if (verbose) message('... g2-Test')
  
  exp_total <- .Object@stat[["b_count"]] / .Object@partition@size
  count_ref <- .Object@stat[["b_count"]] - .Object@stat[["ab_count"]]
  count_ref <- ifelse(count_ref < 0L, 0L, count_ref)
  .Object@stat[, "exp_coi" := .Object@stat[["size_window"]] * exp_total]
  .Object@stat[, "exp_ref" := (.Object@partition@size - .Object@stat[["size_window"]]) * exp_total]
  
  A <- .Object@stat[["ab_count"]] / .Object@stat[["exp_coi"]]
  B <- .Object@stat[["b_count"]] - .Object@stat[["exp_coi"]]
  C <- B / .Object@stat[["exp_ref"]]
  D <- ifelse(C > 0, log(C), log(0.0000001))
  E <- B * D
  ll_value <- 2 * (.Object@stat[["ab_count"]] * log(A) + E)
  
  direction <- ifelse(.Object@stat[["ab_count"]] < .Object@stat[["exp_coi"]], -1L, 1L)
  .Object@stat[, "ll" := ll_value * direction]
  
  setorderv(.Object@stat, cols = "ll", order = -1)
  .Object@stat[, "rank_ll" := 1L:nrow(.Object@stat)]
  
  invisible(.Object)
  
})


#' @rdname features
#' @exportMethod features
setMethod("features", "Cooccurrences", function(x, y, included = FALSE, method = "ll", verbose = TRUE){
  
  if (!identical(x@p_attribute, y@p_attribute)) {
    warning("BEWARE: cooccurrences objects are not based on the same p_attribute!")
  }
  
  if (verbose) message("... preparing tabs for matching")
  keys <- unlist(lapply(c("a", "b"), function(ab) paste(ab, x@p_attribute, sep = "_"))) 
  setkeyv(x@stat, keys)
  setkeyv(y@stat, keys)
  MATCH <- y@stat[x@stat]
  
  # remove columns not needed
  setnames(MATCH, old = c("ab_count", "i.ab_count"), new = c("count_ref", "count_coi"))
  cols_to_keep <- c(keys, "count_ref", "count_coi")
  cols_to_drop <- colnames(MATCH)[!colnames(MATCH) %in% cols_to_keep]
  for (drop in cols_to_drop) MATCH[, eval(drop) := NULL, with = TRUE]
  if (included) MATCH[, "count_ref" := MATCH[["count_ref"]] - MATCH[["count_coi"]] ]
  
  retval <- new(
    "features",
    included = FALSE,
    corpus = x@corpus,
    size_coi = x@partition@size,
    size_ref = if (included) y@partition@size - x@partition@size else y@partition@size,
    p_attribute = x@p_attribute,
    stat = MATCH
  )
  
  for (how in method){
    if (verbose) message("... statistical test: ", how)
    retval <- do.call(how, args = list(.Object = retval))
  }
  retval
})

    
# If more than one p_attribute has been used, concatenate decoded p_attributes.
# minimize = function(x){
#   DT <- copy(self$stat)
#   aColsStr <- paste("a", x@p_attribute, sep = "_")
#   bColsStr <- paste("b", x@p_attribute, sep = "_")
#   KEY <- data.table(
#     i = 1L:nrow(DT),
#     aKey = apply(DT, 1L, function(x) paste(x[aColsStr], collapse = "//")),
#     bKey = apply(DT, 1L, function(x) paste(x[bColsStr], collapse = "//"))
#   )
#   DT[, "order" := KEY[, order(c(.SD[["aKey"]][1], .SD[["bKey"]][1]))[1], by = "i"][["V1"]]]
#   setkey(DT, "order")
#   aToB <- DT[list(1)]
#   setkeyv(aToB, cols = c(aColsStr, bColsStr))
#   bToA <- DT[list(2)]
#   setnames(bToA, old = c(aColsStr, bColsStr), new = c(bColsStr, aColsStr))
#   setkeyv(bToA, cols = c(aColsStr, bColsStr))
#   merger <- merge(aToB, bToA, all.x = FALSE, all.y = TRUE)
#   FIN <- merger[, c(aColsStr, bColsStr, "ab_count.x", "ll.x", "ll.y", "a_count.x", "b_count.x"), with = FALSE]
#   setnames(
#     FIN,
#     c("ab_count.x", "ll.x", "ll.y", "a_count.x", "b_count.x"),
#     c("ab_count", "ab_ll", "ba_ll", "a_count", "b_count")
#   )
#   setcolorder(FIN, c(aColsStr, bColsStr, "ab_count", "a_count", "b_count", "ab_ll", "ba_ll"))
#   setkeyv(FIN, cols = c(aColsStr, bColsStr))
#   x@minimized <- TRUE
#   x@stat <- FIN
#   x
# }



#' @details The \code{as_igraph()}-function can be used to turn an object of the \code{Cooccurrences}-class 
#' into an \code{igraph}-object.
#' @param x A \code{Cooccurrences} class object.
#' @param edge_attributes Attributes from stat \code{data.table} in x to add to edges.
#' @param vertex_attributes Vertex attributes to add to nodes.
#' @param as.undirected Logical, whether to return directed or undirected graph.
#' @param drop A character vector indicating names of nodes to drop from
#'   \code{igraph} object that is prepared.
#' @rdname all-cooccurrences-class
#' @export as_igraph
as_igraph = function(x, edge_attributes = c("ll", "ab_count", "rank_ll"), vertex_attributes = "count", as.undirected = TRUE, drop = c("\u0084", "\u0093")){
  
  if (!requireNamespace("igraph", quietly = TRUE))
    stop("Package 'igraph' is required for as.igraph()-method, but not yet installed.")
  
  if (!all(edge_attributes %in% colnames(x@stat)))
    warning("edge_attribute supplied is not available")
  
  a_cols <- paste("a", x@p_attribute, sep = "_")
  b_cols <- paste("b", x@p_attribute, sep = "_")
  
  if (length(x@p_attribute) > 1L){
    x@stat[, "node" := do.call(paste, c(x@stat[, b_cols], sep = "//"))]
    x@stat[, "collocate" := do.call(paste, c(x@stat[, a_cols], sep = "//"))]
    g <- igraph::graph_from_data_frame(x@stat[, c("node", "collocate", edge_attributes), with = FALSE])
  } else {
    g <- igraph::graph_from_data_frame(x@stat[, c(a_cols, b_cols, edge_attributes), with = FALSE])
  }
  
  if ("count" %in% vertex_attributes){
    if (length(x@p_attribute) == 1){
      setkeyv(x@partition@stat, x@p_attribute)
      igraph::V(g)$count <- x@partition@stat[names(igraph::V(g))][["count"]]
    } else{
      x@partition@stat[, "key" := do.call(paste, c(x@partition@stat[, x@p_attribute], sep = "//"))]
      # x@partition@stat[, "key" := apply(x@partition@stat, 1, function(row) paste(row[x@p_attribute], collapse = "//"))]
      setkeyv(x@partition@stat, cols = "key")
      igraph::V(g)$count <- x@partition@stat[names(igraph::V(g))][["count"]]
    }
    igraph::V(g)$freq <- round((igraph::V(g)$count / x@partition@size) * 100000, 3)
  }
  if (as.undirected) g <- igraph::as.undirected(g, edge.attr.comb = "concat")
  if (length(drop) > 0) for (x in drop) g <- igraph::delete_vertices(g, igraph::V(g)[name == x])
  g
}


#' @details The \code{subset} method, as a particular feature, allows a
#'   \code{Coocccurrences}-object to be subsetted by a \code{featurs}-Object
#'   resulting from a features extraction that compares two Cooccurrences
#'   objects.
#' @param by A \code{features}-class object.
#' @param ... Further arguments passed into a further call of \code{subset}.
#' @rdname all-cooccurrences-class
#' @exportMethod subset
setMethod("subset", "Cooccurrences", function(x, ..., by){
  if (!missing(by)){
    if (is(by)[1] != "features") stop("If 'by' is provided, a features object is expected")
    keys <- unlist(lapply(c("a", "b"), function(what) paste(what, x@p_attribute, sep = "_")))
    setkeyv(x@stat, keys)
    setkeyv(by@stat, keys)
    x@stat <- x@stat[by@stat]
  }
  x@stat <- subset(copy(x@stat), ...)
  x
})


#' @exportMethod decode
#' @rdname all-cooccurrences-class
#' @details For reasons of memory efficiency, the initial \code{data.table} in
#'   the slot \code{stat} of a \code{Cooccurrences}-object will identify tokens by an
#'   integer id, not by the string of the token. The \code{decode()}-method will
#'   replace these integer columns with human-readable character vectors. Due to
#'   the reference logic of the \code{data.table} object, this is an in-place
#'   operation, peformed without copying the table. The modified object is
#'   returned invisibly; usually it will not be necessary to catch the return
#'   value.
setMethod("decode", "Cooccurrences", function(.Object){
  .Object@stat[, paste("a", .Object@p_attribute, sep = "_") := as.nativeEnc(
    cl_id2str(corpus = .Object@corpus, p_attribute = .Object@p_attribute, id = .Object@stat[["a_id"]]),
    from = registry_get_encoding(.Object@corpus))
    ]
  .Object@stat[, paste("b", .Object@p_attribute, sep = "_") := as.nativeEnc(
    cl_id2str(corpus = .Object@corpus, p_attribute = .Object@p_attribute, id = .Object@stat[["b_id"]]),
    from = registry_get_encoding(.Object@corpus))
    ]
  # .Object@stat[, "a_id" := NULL][, "b_id" := NULL]
  invisible(.Object)
})