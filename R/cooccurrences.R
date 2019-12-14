#' @include context.R textstat.R partition.R polmineR.R cooccurrences.R bundle.R S4classes.R decode.R as.sparseMatrix.R  kwic.R
NULL


#' @docType methods
#' @rdname cooccurrences-class
setMethod("show", "cooccurrences", function(object) {
  object@stat <- format(object, digits = 2L)
  if (Sys.getenv("RSTUDIO") == "1" && interactive() && is.na(Sys.getenv("NOT_CRAN", unset = NA))){
    view(object)
  } else {
    if (getOption("polmineR.browse")) browse(object@stat) else return(object@stat) 
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
#' @param .Object A \code{partition} object, or a \code{character} vector with a CWB corpus.
#' @param query A query, either a character vector to match a token, or a CQP query.
#' @param cqp Defaults to \code{is.cqp}-function, or provide
#'   \code{TRUE}/\code{FALSE}; relevant only if query is not \code{NULL}.
#' @param cpos integer vector with corpus positions, defaults to NULL - then the
#'   corpus positions for the whole corpus will be used
#' @param p_attribute The p-attribute of the tokens/the query.
#' @param s_attribute If provided, it will be checked that corpus positions of
#'   windows do not extend beyond the region defined by the s-attribute.
#' @param left Number of tokens to the left of the query match.
#' @param right Number of tokens to the right of the query match.
#' @param stoplist Exclude a query hit from analysis if stopword(s) is/are in
#'   context (relevant only if query is not \code{NULL}).
#' @param positivelist Character vector or numeric vector: include a query hit
#'   only if token in \code{positivelist} is present. If \code{positivelist} is
#'   a character vector, it is assumed to provide regex expressions (incredibly
#'   long if the list is long) (relevant only if query is nut NULL)
#' @param regex A \code{logical} value, whether stoplist/positivelist are
#'   interpreted as regular expressions.
#' @param method The statistical test(s) to use (defaults to "ll").
#' @param verbose A \code{logical} value, whether to be verbose.
#' @param progress A \code{logical} value, whether to output progress bar.
#' @param keep list with tokens to keep
#' @param mc whether to use multicore
#' @param ... Further parameters that will be passed into bigmatrix (applies only of big = TRUE).
#' @return a cooccurrences-class object
#' @seealso See the documentation for the \code{\link{ll}}-method for an
#'   explanation of the computation of the log-likelihood statistic.
#' @exportMethod cooccurrences
#' @docType methods
#' @author Andreas Blaette
#' @export cooccurrences
#' @name cooccurrences
#' @rdname cooccurrences
#' @references Baker, Paul (2006): \emph{Using Corpora in Discourse Analysis}. London: continuum, p. 95-120 (ch. 5).
#' @references Manning, Christopher D.; Schuetze, Hinrich (1999): \emph{Foundations of Statistical Natural Language
#' Processing}. MIT Press: Cambridge, Mass., pp. 151-189 (ch. 5).
#' @examples
#' use("polmineR")
#' merkel <- partition("GERMAPARLMINI", interjection = "speech", speaker = ".*Merkel", regex = TRUE)
#' merkel <- enrich(merkel, p_attribute = "word")
#' cooc <- cooccurrences(merkel, query = "Deutschland")
#' 
#' # use subset-method to filter results
#' a <- cooccurrences("REUTERS", query = "oil")
#' b <- subset(a, !is.na(ll))
#' c <- subset(b, !word %in% tm::stopwords("en"))
#' d <- subset(c, count_coi >= 5)
#' e <- subset(c, ll >= 10.83)
#' format(e)
#' 
#' # using pipe operator may be convenient
#' if (require(magrittr)){
#' cooccurrences("REUTERS", query = "oil") %>%
#'   subset(!is.na(ll)) %>%
#'   subset(!word %in% tm::stopwords("en")) %>%
#'   subset(count_coi >= 5) %>%
#'   subset(ll >= 10.83) %>%
#'   format()
#' }
setGeneric("cooccurrences", function(.Object, ...) standardGeneric("cooccurrences") )

#' @rdname cooccurrences
setMethod("cooccurrences", "corpus", function(
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
  ctxt <- context(
    .Object = .Object, query = query, cqp = is.cqp,
    p_attribute = p_attribute, s_attribute = s_attribute,
    left = left, right = right,
    stoplist = stoplist, positivelist = positivelist, regex = regex,
    count = TRUE, 
    mc = mc, verbose = verbose, progress = progress
  )
  if (is.null(ctxt)) invisible(NULL) else cooccurrences(ctxt, method = method, verbose = verbose)
})



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
  cooccurrences(
    .Object = corpus(.Object),
    query = query,
    cqp = cqp,
    p_attribute = p_attribute,
    s_attribute = s_attribute,
    left = left,
    right = right,
    stoplist = stoplist,
    positivelist = positivelist,
    regex = regex,
    keep = keep,
    cpos = cpos,
    method = method,
    mc = mc,
    verbose = verbose,
    progress = progress,
    ...
  )
})



#' @rdname cooccurrences
setMethod(
  "cooccurrences", "slice",
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
    y <- context(
      .Object = .Object, query = query, cqp = is.cqp,
      p_attribute = p_attribute, s_attribute = s_attribute,
      left = left, right = right,
      stoplist = stoplist, positivelist = positivelist,
      count = TRUE, 
      mc = mc, verbose = verbose, progress = progress
    )
    if (is.null(y)) invisible(NULL) else cooccurrences(y, method = method, verbose = verbose)
  }
)

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
  ) callNextMethod()
)


#' @rdname cooccurrences
setMethod(
  "cooccurrences", "subcorpus",
  function(
    .Object, query, cqp = is.cqp,
    left = getOption("polmineR.left"), right = getOption("polmineR.right"),
    p_attribute = getOption("polmineR.p_attribute"), s_attribute = NULL,
    stoplist = NULL, positivelist = NULL, keep = NULL,
    method = "ll",
    mc = FALSE, progress = TRUE, verbose = FALSE,
    ...
  ) callNextMethod()
)



#' @rdname cooccurrences
setMethod("cooccurrences", "context", function(.Object, method = "ll", verbose = FALSE){
  
  
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
  
  count_ref <- .Object@stat[["count_partition"]] - .Object@stat[["count_coi"]]
  
  # If may appear very odd, but count_ref may assume values < 0
  # consider the times "Intermediate" and "West" are counted as cooccurrences
  # of "Texas" in the following example:
  # "Texas Intermediate and West Texas Sour"
  # For hits for a query that include the query in the window, tokens may be 
  # counted several times, which may result in a count for the token that is
  # higher than the overall occurrence of the token in the corpus
  # The solution is to count again tokens in cpos, but this time controlling 
  # for corpus positions
  if (TRUE){
    .Object@stat[, "count_ref" := ifelse(count_ref >= 0L, count_ref,  0L)]
  } else {
    multi_min <- .Object@cpos[which(.Object@cpos[["position"]] != 0)][, .N, by = c("cpos", paste(.Object@p_attribute, "id", sep = "_")), with = TRUE]
    multicount_min <- multi_min[multi_min[["N"]] > 1L][, "cpos" := NULL]
    multicount_min[, "N" := (multicount_min[["N"]] - 1)]
    multicount_min2 <- multicount_min[, sum(.SD[["N"]]), by = c(paste(.Object@p_attribute, "id", sep = "_"))]
    setkeyv(multicount_min2, cols = paste(.Object@p_attribute, "id", sep = "_"))
    dt <- multicount_min2[.Object@stat]
    .Object@stat[, "count_ref" := ifelse(!is.na(dt[["V1"]]), count_ref + dt[["V1"]], count_ref)]
  }
  
  setkeyv(.Object@stat, .Object@p_attribute)
  
  if (!is.null(method)){
    for (test in method){
      .message("statistical test:", test, verbose = verbose)
      .Object <- do.call(test, args = list(.Object = .Object))  
    }
  }
  
  
  
  # finishing
  if (nrow(.Object@stat) > 0L){
    setcolorder(
      .Object@stat,
      c(.Object@p_attribute, colnames(.Object@stat)[-which(colnames(.Object@stat) %in% .Object@p_attribute)])
    )
  }
  
  retval <- new(
    "cooccurrences",
    stat = data.table(), cpos = data.table(),
    partition = new("partition", stat = data.table(), size = 0L),
    count = 0L
  )
  slots_to_get <- slotNames(retval)[-grep("^partition$", slotNames(retval))]
  for (x in slots_to_get) slot(retval, x) <- slot(.Object, x)
  retval
})


#' @rdname cooccurrences
#' @examples
#' pb <- partition_bundle("GERMAPARLMINI", s_attribute = "speaker")
#' pb_min <- pb[[ count(pb, query = "Deutschland")[Deutschland >= 25][["partition"]] ]]
#' y <- cooccurrences(pb_min, query = "Deutschland")
#' if (interactive()) y[[1]]
#' if (interactive()) y[[2]]
#' 
#' y2 <- corpus("GERMAPARLMINI") %>%
#'   subset(speaker %in% c("Hubertus Heil", "Angela Dorothea Merkel")) %>%
#'   split(s_attribute = "speaker") %>%
#'   cooccurrences(query = "Deutschland")
setMethod("cooccurrences", "partition_bundle", function(.Object, query, mc = getOption("polmineR.mc"), ...){
  bundle <- new("cooccurrences_bundle")
  bundle@objects <- pbapply::pblapply(
    .Object@objects,
    function(x) cooccurrences(x, query = query, mc = mc, ...) 
  )
  names(bundle@objects) <- names(.Object@objects)
  for (i in seq_along(bundle@objects)){
    if (!is.null(bundle@objects[[i]])) bundle@objects[[i]]@name <- .Object@objects[[i]]@name
  }
  for (i in rev(which(sapply(bundle@objects, is.null)))) bundle@objects[[i]] <- NULL
  bundle
})



#' Cooccurrences class for corpus/partition.
#' 
#' The \code{Cooccurrences}-class stores the information for all cooccurrences
#' in a corpus. As this data can be bulky, in-place modifications of the
#' \code{data.table} in the stat-slot of a \code{Cooccurrences}-object are used
#' wherever possible, to avoid copying potentially large objects whenever
#' possible. The class inherits from the \code{textstat}-class, so that methods
#' for \code{textstat}-objects are inherited (see examples).
#' 
#' @param .Object A \code{Cooccurrences}-class object.
#' @param verbose Logical.
#' @param col A column to extract.
#' @slot left  Single \code{integer} value, number of tokens to the left of the node.
#' @slot right  Single \code{integer} value, number of tokens to the right of the node.
#' @slot p_attribute  A \code{character} vector, the p-attribute(s) the evaluation of the corpus is based on.
#' @slot corpus  Length-one \code{character} vector, the CWB corpus used.
#' @slot stat  A \code{data.table} with the statistical analysis of cooccurrences.
#' @slot encoding  Length-one \code{character} vector, the encoding of the corpus.
#' @slot partition The \code{partition} that is the basis for computations.
#' @seealso See the documentation of the \code{\link{Cooccurrences}}-method
#'   (including examples) for procedures to get and filter cooccurrence
#'   information. See the documentation for the \code{\link{textstat-class}}
#'   explaining which methods for this superclass of the
#'   \code{Cooccurrences}-class which are available.
#' @slot window_sizes A \code{data.table} linking the number of tokens in the
#'   context of a token identified by id.
#' @slot minimized Logical, whether the object has been minimized.
#' @docType class
#' @exportClass Cooccurrences
#' @rdname all-cooccurrences-class
#' @aliases as_igraph
setClass(
  "Cooccurrences",
  contains = "features", # slots inherited: corpus, p_attribute, encoding, stat, size_coi, size_ref, method, included, call, name
  slots = c(
    left = "integer",
    right = "integer",
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
  for (colname in c("a_word_id", "b_word_id", "size_coi", "a_word", "a_count"))
    if (colname %in% colnames(y)) y@stat[, eval(colname) := NULL, with = TRUE]
  
  setnames(
    y@stat,
    old = c("ab_count", "b_count", "b_word"),
    new = c("count_coi", "count_ref", "word")
  )
  setorderv(y@stat, cols = tests[1], order = -1L)
  y@stat[[paste("rank", tests[1], sep = "_")]] <- 1L:nrow(y@stat)
  y
})



#' @noRd
setGeneric("Cooccurrences", function(.Object, ...) standardGeneric("Cooccurrences"))


#' @exportMethod Cooccurrences
#' @rdname all_cooccurrences
#' @aliases Cooccurrences
setMethod("Cooccurrences", "character", function(
  .Object, p_attribute, left, right,
  stoplist = NULL,
  mc = getOption("polmineR.mc"), verbose = FALSE, progress = FALSE
){
  Cooccurrences(
    Corpus$new(.Object)$as.partition(),
    p_attribute = p_attribute,
    left = left, right = right, stoplist = stoplist,
    mc = mc, verbose = verbose, progress = progress)
})


#' Get all cooccurrences in corpus/partition.
#' 
#' Obtain all cooccurrences in a corpus, or a \code{partition}. The result is a
#' \code{Cooccurrences}-class object which includes a \code{data.table} with
#' counts of cooccurrences. See the documentation entry for the
#' \code{Cooccurrences}-class for methods to process \code{Cooccurrences}-class
#' objects.
#' 
#' The implementation uses a \code{data.table} to store information and makes
#' heavy use of the reference logic of the \code{data.table} package, to avoid
#' copying potentially large objects, and to be parsimonious with limited
#' memory. The behaviour resulting from in-place changes may be uncommon, see
#' examples.
#' 
#' @param .Object A length-one character vector indicating a corpus, or a
#'   \code{partition} object.
#' @param p_attribute Positional attributes to evaluate.
#' @param left A scalar \code{integer} value, size of left context.
#' @param right A scalar \code{integer} value, size of right context.
#' @param stoplist Tokens to exclude from the analysis.
#' @param mc Logical value, whether to use multiple cores.
#' @param progress Logical value, whether to display a progress bar.
#' @param verbose Logical value, whether to output messages.
#' @importFrom data.table data.table melt.data.table as.data.table
#' @importFrom RcppCWB cl_id2str cl_str2id cl_cpos2id
#' @import methods
#' @importFrom RcppCWB get_cbow_matrix
#' @importFrom parallel mclapply 
#' @exportMethod Cooccurrences
#' @rdname all_cooccurrences
#' @seealso To learn about methods available for the object that is returned,
#'   see the documentation of the \code{\link{Cooccurrences-class}}. See the
#'   \code{\link{cooccurrences}}-method (starting with a lower case c) to get
#'   the cooccurrences for the match for a query, which may also be a CQP query.
#' @examples 
#' \dontrun{
#' # In a first scenario, we get all cooccurrences for the REUTERS corpus,
#' # excluding stopwords
#' 
#' stopwords <- unname(unlist(
#'   noise(
#'     terms("REUTERS", p_attribute = "word"),
#'     stopwordsLanguage = "en"
#'     )
#'   ))
#' r <- Cooccurrences(
#'   .Object = "REUTERS", p_attribute = "word",
#'   left = 5L, right = 5L, stoplist = stopwords
#' )
#' ll(r) # note that the table in the stat slot is augmented in-place
#' decode(r) # in-place modification, again
#' r <- subset(r, ll > 11.83 & ab_count >= 5)
#' data.table::setorderv(r@stat, cols = "ll", order = -1L)
#' head(r, 25)
#' 
#' if (requireNamespace("igraph", quietly = TRUE)){
#'   r@partition <- enrich(r@partition, p_attribute = "word")
#'   g <- as_igraph(r, as.undirected = TRUE)
#'   plot(g)
#' }
#' 
#' # The next scenario is a cross-check that extracting cooccurrences from
#' # from a Cooccurrences-class object with all cooccurrences and the result
#' # for getting cooccurrences for a single object are identical
#' 
#' a <- cooccurrences(r, query = "oil")
#' a <- data.table::as.data.table(a)
#' 
#' b <- cooccurrences("REUTERS", query = "oil", left = 5, right = 5, p_attribute = "word")
#' b <- data.table::as.data.table(b)
#' b <- b[!word %in% stopwords]
#' 
#' all(b[["word"]][1:5] == a[["word"]][1:5]) # needs to be identical!
#' 
#' 
#' stopwords <- unlist(noise(
#'   terms("GERMAPARLMINI", p_attribute = "word"),
#'   stopwordsLanguage = "german"
#'   )
#' )
#' 
#' # We now filter cooccurrences by keeping only the statistically 
#' # significant cooccurrens, identified by comparison with cooccurrences
#' # derived from a reference corpus
#' 
#' plpr_partition <- partition(
#'   "GERMAPARLMINI", date = "2009-11-10", interjection = "speech",
#'   p_attribute = "word"
#' )
#' plpr_cooc <- Cooccurrences(
#'   plpr_partition, p_attribute = "word",
#'   left = 3L, right = 3L,
#'   stoplist = stopwords,
#'   verbose = TRUE
#' )
#' decode(plpr_cooc)
#' ll(plpr_cooc)
#' 
#' merkel <- partition(
#'   "GERMAPARLMINI", speaker = "Merkel", date = "2009-11-10", interjection = "speech",
#'   regex = TRUE,
#'   p_attribute = "word"
#' )
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
#'   
#' # Esentially the same procedure as in the previous example, but with 
#' # two positional attributes, so that part-of-speech annotation is 
#' # used for additional filtering.
#'    
#'          
#' protocol <- partition(
#'   "GERMAPARLMINI",
#'   date = "2009-11-10",
#'   p_attribute = c("word", "pos"),
#'   interjection = "speech"
#' )
#' protocol_cooc <- Cooccurrences(
#'   protocol,
#'   p_attribute = c("word", "pos"),
#'   left = 3L, right = 3L
#'   )
#' ll(protocol_cooc)
#' decode(protocol_cooc)
#' 
#' merkel <- partition(
#'   "GERMAPARLMINI",
#'   speaker = "Merkel",
#'   date = "2009-11-10",
#'   interjection = "speech",
#'   regex = TRUE,
#'   p_attribute = c("word", "pos")
#' )
#' merkel_cooc <- Cooccurrences(
#'   merkel,
#'   p_attribute = c("word", "pos"),
#'   left = 3L, right = 3L,
#'   verbose = TRUE
#' )
#' ll(merkel_cooc)
#' decode(merkel_cooc)
#' 
#' f <- features(merkel_cooc, protocol_cooc)
#' f <- subset(f, a_pos %in% c("NN", "ADJA"))
#' f <- subset(f, b_pos %in% c("NN", "ADJA"))
#' f <- subset(f, c(rep(TRUE, times = 50), rep(FALSE, times = nrow(f) - 50)))
#' 
#' merkel_min <- subset(merkel_cooc, by = f)
#' 
#' if (requireNamespace("igraph", quietly = TRUE)){
#'   g <- as_igraph(merkel_min, as.undirected = TRUE)
#'   plot(g)
#' }
#' 
#' }
setMethod("Cooccurrences", "slice", function(
  .Object, p_attribute, left, right,
  stoplist = NULL,
  mc = getOption("polmineR.mc"), verbose = FALSE, progress = FALSE
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
    partition = if ("partition" %in% is(.Object)) .Object else as(.Object, "partition")
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
        setnames(y@window_sizes, old = "V1", new = "size_coi")
        setkeyv(y@window_sizes, cols = "a_id")
        if (!is.null(stoplist)) y@stat <- dt[!b_id %in% stoplist_ids] else y@stat <- dt
      } else {
        sizes <- dt[, {sum(.SD[["N"]])}, by = "a_id"]
        setkeyv(sizes, cols = "a_id")
        y@window_sizes <- merge(y@window_sizes, sizes, all = TRUE)
        y@window_sizes[, "size_coi" := ifelse(is.na(y@window_sizes[["size_coi"]]), 0L, y@window_sizes[["size_coi"]]) + ifelse(is.na(y@window_sizes[["V1"]]), 0L, y@window_sizes[["V1"]])]
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
    
    
    # if (length(.Object@p_attribute) == 0)
    #  stop("The partition is required to included counts. Enrich the object first!")
    
    a_cols_id <- setNames(paste("a", p_attribute, "id", sep = "_"), p_attribute)
    b_cols_id <- setNames(paste("b", p_attribute, "id", sep = "_"), p_attribute)
    a_cols_str <- setNames(paste("a", p_attribute, sep = "_"), p_attribute)
    b_cols_str <- setNames(paste("b", p_attribute, sep = "_"), p_attribute)
    
    .make_window <- function(i){
      cpos_min <- .Object@cpos[i,1]
      cpos_max <- .Object@cpos[i,2]
      if (cpos_min < cpos_max){
        range <- cpos_min:cpos_max
        lapply(
          setNames(range, range),
          function(x) {
            cpos <- c((x - left):(x - 1L), (x + 1L):(x + right))
            cpos <- cpos[which(cpos >= cpos_min)]
            cpos[which(cpos <= cpos_max)]
          })
      }
    }
    
    if (progress){
      bag <- pblapply(1L:nrow(.Object@cpos), .make_window, cl = mc)
    } else{
      bag <- if (mc) lapply(1L:nrow(.Object@cpos), .make_window) else mclapply(1L:nrow(.Object@cpos), .make_window)
    }
      
    
    b_cpos <- lapply(
      bag,
      function(x) lapply(names(x), function(y) rep(as.integer(y), times = length(x[[y]])))
    )
    if (verbose) message("... putting together data.table")
    dt <- data.table(a_cpos = unlist(bag), b_cpos = unlist(b_cpos))
    
    if (verbose) message("... getting token ids")
    lapply(
      p_attribute,
      function(x){
        dt[, eval(a_cols_id[x]) := cl_cpos2id(corpus = .Object@corpus, p_attribute = x, cpos = dt[["a_cpos"]]), with = TRUE]
        dt[, eval(b_cols_id[x]) := cl_cpos2id(corpus = .Object@corpus, p_attribute = x, cpos = dt[["b_cpos"]]), with = TRUE]
      }
    )
    if (verbose) message("... counting window size")
    
    y@window_sizes <- dt[, .N, by = c(eval(a_cols_id)), with = TRUE]
    setnames(y@window_sizes, "N", "size_coi")

    if (verbose) message("... counting co-occurrences")
    y@stat <- dt[, .N, by = c(eval(c(a_cols_id, b_cols_id))), with = TRUE]
    setnames(y@stat, "N", "ab_count")
  }
  y
})


#' @rdname all_cooccurrences
setMethod("Cooccurrences", "partition", function(
  .Object, p_attribute, left, right,
  stoplist = NULL,
  mc = getOption("polmineR.mc"), verbose = FALSE, progress = FALSE
){ callNextMethod() })


#' @rdname all_cooccurrences
setMethod("Cooccurrences", "subcorpus", function(
  .Object, p_attribute, left, right,
  stoplist = NULL,
  mc = getOption("polmineR.mc"), verbose = FALSE, progress = FALSE
){ callNextMethod() })


#' @details The \code{as.simple_triplet_matrix}-method will transform a
#'   \code{Cooccurrences} object into a sparse matrix. For reasons of memory
#'   efficiency, decoding token ids is performed within the method at the
#'   as late as possible. It is NOT necessary that decoded tokens are present
#'   in the table in the \code{Cooccurrences} object.
#' @exportMethod as.simple_triplet_matrix
#' @rdname all-cooccurrences-class
#' @examples
#' X <- Cooccurrences("REUTERS", p_attribute = "word", left = 2L, right = 2L)
#' m <- as.simple_triplet_matrix(X)
setMethod("as.simple_triplet_matrix", "Cooccurrences", function(x){
  
  verbose <- interactive()
  
  decoded_tokens <- reindex(x)
  if (length(x@p_attribute) > 1L) stop("Method only works if one and only one p-attribute is used.")

  if (verbose) message("... creating simple triplet matrix")
  retval <- slam::simple_triplet_matrix(
    i = x@stat[["a_new_index"]], j = x@stat[["b_new_index"]], v = x@stat[["ab_count"]],
    dimnames = list(decoded_tokens, decoded_tokens)
  )
  
  # restore original data.table and remove columns generated during reindexing
  x@stat[, "a_new_index" := NULL][, "b_new_index" := NULL]
  retval
})




#' @rdname features
#' @exportMethod features
setMethod("features", "Cooccurrences", function(x, y, included = FALSE, method = "ll", verbose = TRUE){
  
  if (!identical(x@p_attribute, y@p_attribute))
    warning("BEWARE: cooccurrences objects are not based on the same p_attribute!")

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





#' @noRd
setGeneric("as_igraph", function(x, ...) standardGeneric("as_igraph"))

#' @details The \code{as_igraph}-method can be used to turn an object of the \code{Cooccurrences}-class 
#' into an \code{igraph}-object.
#' @param x A \code{Cooccurrences} class object.
#' @param left Number of tokens to the left of the node.
#' @param right Number of tokens to the right of the node.
#' @param progress Logical, whether to show progress bar.
#' @param edge_attributes Attributes from stat \code{data.table} in x to add to edges.
#' @param vertex_attributes Vertex attributes to add to nodes.
#' @param as.undirected Logical, whether to return directed or undirected graph.
#' @param drop A character vector indicating names of nodes to drop from
#'   \code{igraph} object that is prepared.
#' @rdname all-cooccurrences-class
#' @exportMethod as_igraph
setMethod("as_igraph", "Cooccurrences", function(x, edge_attributes = c("ll", "ab_count", "rank_ll"), vertex_attributes = "count", as.undirected = TRUE, drop = c("\u0084", "\u0093")){
  
  if (!requireNamespace("igraph", quietly = TRUE))
    stop("Package 'igraph' is required for as.igraph()-method, but not yet installed.")
  
  if (!all(edge_attributes %in% colnames(x@stat)))
    warning("edge_attribute supplied is not available")
  
  if ("kwic" %in% colnames(x)) edge_attributes <- unique(c(edge_attributes, "kwic"))

  a_cols <- paste("a", x@p_attribute, sep = "_")
  b_cols <- paste("b", x@p_attribute, sep = "_")
  
  if (length(x@p_attribute) > 1L){
    x@stat[, "node" := do.call(paste, c(x@stat[, b_cols, with = FALSE], sep = "//"))]
    x@stat[, "collocate" := do.call(paste, c(x@stat[, a_cols, with = FALSE], sep = "//"))]
    g <- igraph::graph_from_data_frame(x@stat[, c("node", "collocate", edge_attributes), with = FALSE])
  } else {
    g <- igraph::graph_from_data_frame(x@stat[, c(a_cols, b_cols, edge_attributes), with = FALSE])
  }
  if ("kwic" %in% igraph::edge_attr_names(g)){
    igraph::E(g)$info <- unlist(lapply(igraph::E(g)$kwic, function(x) x[1]))
    g <- igraph::delete_edge_attr(g, "kwic")
  }
  
  if ("count" %in% vertex_attributes){
    if (length(x@p_attribute) == 1L){
      if (!x@p_attribute %in% colnames(x@partition@stat))
        x@partition <- enrich(x@partition, p_attribute = x@p_attribute)
      setkeyv(x@partition@stat, x@p_attribute)
      igraph::V(g)$count <- x@partition@stat[names(igraph::V(g))][["count"]]
    } else{
      x@partition@stat[, "key" := do.call(paste, c(x@partition@stat[, x@p_attribute, with = FALSE], sep = "//"))]
      # x@partition@stat[, "key" := apply(x@partition@stat, 1, function(row) paste(row[x@p_attribute], collapse = "//"))]
      setkeyv(x@partition@stat, cols = "key")
      igraph::V(g)$count <- x@partition@stat[names(igraph::V(g))][["count"]]
    }
    igraph::V(g)$freq <- round((igraph::V(g)$count / x@partition@size) * 100000, 3)
  }
  
  if ("kwic" %in% colnames(x@partition)){
    setkeyv(x@partition@stat, cols = x@p_attribute[1])
    igraph::V(g)$info <- unlist(lapply(igraph::V(g)$name, function(n) x@partition@stat[n][["kwic"]]))
  }
  
  if (as.undirected) g <- igraph::as.undirected(g, edge.attr.comb = "concat")
  if (length(drop) > 0) for (x in drop) g <- igraph::delete_vertices(g, igraph::V(g)[name == x])
  g
})


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
  for (p_attr in .Object@p_attribute){
    a_col <- if (length(.Object@p_attribute) == 1L) "a_id" else paste("a", p_attr, "id", sep = "_")
    .Object@stat[, paste("a", p_attr, sep = "_") := as.nativeEnc(
      cl_id2str(corpus = .Object@corpus, p_attribute = p_attr, id = .Object@stat[[a_col]]),
      from = registry_get_encoding(.Object@corpus))
      ]
    b_col <- if (length(.Object@p_attribute) == 1L) "b_id" else paste("b", p_attr, "id", sep = "_")
    .Object@stat[, paste("b", p_attr, sep = "_") := as.nativeEnc(
      cl_id2str(corpus = .Object@corpus, p_attribute = p_attr, id = .Object@stat[[b_col]]),
      from = registry_get_encoding(.Object@corpus))
      ]
  }
  # .Object@stat[, "a_id" := NULL][, "b_id" := NULL]
  invisible(.Object)
})


#' @details The \code{kwic}-method will add a column to the \code{data.table} in
#'   the \code{stat}-slot with the concordances that are behind a statistical
#'   finding, and to the \code{data.table} in the \code{stat}-slot of the
#'   \code{partition} in the slot \code{partition}. It is an in-place operation.
#' @rdname all-cooccurrences-class
setMethod("kwic", "Cooccurrences", function(
  .Object,
  left = getOption("polmineR.left"), right = getOption("polmineR.right"),
  verbose = TRUE, progress = TRUE
  ){
  message("... getting context of nodes")
  
  stopifnot(length(.Object@p_attribute) == 1)
  
  token <- unique(c(
    .Object@stat[[paste("a", .Object@p_attribute, sep = "_")]],
    .Object@stat[[paste("b", .Object@p_attribute, sep = "_")]]
    ))
  names(token) <- token
  
  .fn_ctxt <- function(x) context(.Object@partition, query = x, left = left * 2L, right = right * 2L, p_attribute = .Object@p_attribute, cqp = FALSE, verbose = FALSE)
  context_list <- if (progress) pblapply(token, .fn_ctxt) else lapply(token, .fn_ctxt)
  
  if (verbose) message("... getting kwic for nodes")
  .get_kwic_for_nodes <- function(x){
    if (x %in% names(context_list)){
      a <- context_list[[x]]
      a@cpos <- a@cpos[between(a@cpos[["position"]], lower = -left, upper = right)]
      # k <- kwic(a, left = left, right = right, p_attribute = .Object@p_attribute, verbose = FALSE)
      k <- kwic(a)
      vec <- as.character(k, fmt = '<span style="background-color:yellow">%s</span>')
      el <- paste(vec, collapse = "<br/>")
      return( unlist(el) )
    } else {
      return( character() )
    }
  }
  if (nrow(.Object@partition@stat) == 0){
    .Object@partition@stat <- data.table(terms(.Object@partition, p_attribute = .Object@p_attribute))
    colnames(.Object@partition@stat) <- .Object@p_attribute
  }
  setkeyv(.Object@partition@stat, cols = .Object@p_attribute)
  .Object@partition@stat <- .Object@partition@stat[unname(token)]
  nodes <- .Object@partition@stat[[.Object@p_attribute]]
  node_kwic <- if (progress) pblapply(nodes, .get_kwic_for_nodes) else lapply(nodes, .get_kwic_for_nodes)
  .Object@partition@stat[, "kwic" := node_kwic]

  if (verbose) message("... creating edge data")
  .Object@stat[, "i" := 1L:nrow(.Object@stat)]
  .fn_edges <- function(.SD){
    context_min <- trim(
      context_list[[ .SD[[paste("a", .Object@p_attribute, sep = "_")]][1] ]],
      positivelist = as.corpusEnc(.SD[[paste("b", .Object@p_attribute, sep = "_")]][1], corpusEnc = .Object@partition@encoding),
      verbose = FALSE
    )
    if (is.null(context_min)){
      return("")
    } else {
      K <- kwic(context_min, verbose = FALSE)
      K <- highlight(K, yellow = .SD[[2]][1])
      y <- as.character(K, fmt = '<b style="background-color:yellow">%s</b>')
      paste(y, collapse = "</br>")
    }
  }
  .Object@stat[, "kwic" := .Object@stat[, .fn_edges(.SD), by = "i"][["V1"]] ]
  .Object@stat[, "i" := NULL]

  invisible(.Object)
})
