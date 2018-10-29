#' @include context.R textstat.R partition.R polmineR.R cooccurrences.R bundle.R S4classes.R
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



#' Get all cooccurrences in partition.
#' 
#' R6 class to generate and manage cooccurrence statistics. The R6 class system
#' is used as a matter of memory efficiency, to avoid the copying of objects the
#' S4 class system usually requires.
#' 
#' To reduce the size of the \code{data.table} with the cooccurrence statistics,
#' the parameters \code{keep} and \code{drop} provide vectors with tokens that
#' will be kept and dropped, respectively. The parameters are used by the
#' \code{trim} method.
#' 
#' @section Slots:
#' \describe{
#'   \item{\code{corpus}}{The corpus the evaluation is based on.}
#'   \item{\code{partition}}{The \code{partition} for which all cooccurrences shall be computed.}
#'   \item{\code{p_attribute}}{character}
#'   \item{\code{keep}}{A list of named character vectors, names are p-attributes.}
#'   \item{\code{drop}}{A list of named character vectors, names are p-attributes.}
#'   \item{\code{method}}{The statistical test to use (such as "ll").}
#'   \item{\code{window}}{An integer value, the number of tokens to the left and
#'   to the right of nodes.}
#'   \item{\code{verbose}}{Logical.}
#'   \item{\code{window_sizes}}{A \code{data.table} linking the number of tokens
#'   in the context of a token identified by id.}
#'   \item{\code{stat}}{A \code{data.table} with the statistical information on cooccurrences.}
#' }
#' @section Arguments:
#' \describe{
#'   \item{\code{x}}{A corpus specified by a length-one character vector, or a \code{partition}-object.}
#'   \item{\code{keep}}{}
#'   \item{\code{verbose}}{}
#'   \item{\code{...}}{}
#' }
#' @section Methods:
#' \describe{
#'   \item{\code{$initialize(x, p_attribute = "word", window = 5L, verbose =
#'   TRUE, drop = c(polmineR::punctuation, tm::stopwords("de")), keep =
#'   NULL)}}{Initialize.}
#'   \item{\code{$count()}}{Count the cooccurrence of terms. The field 'dt' is
#'   populated with a \code{data.table} with the columns 'a_id', 'b_id' and
#'   'ab_count'.}
#'   \item{\code{$trim(function(action, by.id)}}{Trim the overall list of
#'   cooccurrences by dropping terms that are not frequent. Recommended to speed
#'   up computation of statistical test values.}
#'   \item{\code{$ll()}}{Based on counts of term cooccurrences, the
#'   \code{data.table} in the field \code{dt} is enriched by log likelihood
#'   test.}
#'   \item{\code{$get_features(reference, included = FALSE,
#'   method = "ll", verbose = TRUE)}}{}
#'   \item{\code{$select_features(reference, included = FALSE, method = "ll",
#'   verbose = TRUE, n = 250)}}{}
#'   \item{\code{$minimize()}}{If more than one p_attribute has been used,
#'   concatenate decoded p_attributes.}
#'   \item{\code{$as.simple_triplet_matrix()}}{Returns a simple triplet
#'   matrix based on the counts of term cooccurrences. If counts are not yet
#'   present, that is done first.}
#'   \item{\code{$as.sparseMatrix(x, col)}}{Return a sparse matrix with the term
#'   cooccurrence matrix.}
#'   \item{\code{$as.igraph(edge_attributes = "ll", vertex_attributes = NULL,
#'   as.undirected = TRUE)}}{Transform graph as igraph object.}
#' }
#' @importFrom parallel mcparallel mccollect
#' @importFrom data.table data.table melt.data.table as.data.table
#' @importFrom RcppCWB cl_id2str cl_str2id cl_cpos2id
#' @import methods
#' @importFrom RcppCWB get_cbow_matrix
#' @examples 
#' stopwords <- unname(unlist(noise(terms("REUTERS", p_attribute = "word"), stopwordsLanguage = "en")))
#' r <- Cooccurrences$new(
#'   x = "REUTERS",
#'   p_attribute = "word",
#'   window = 5L,
#'   drop = stopwords
#' )
#' r$count(parsimonious = TRUE)
#' r$trim(action = "drop", by.id = TRUE)
#' r$ll()
#' r$subset(ll > 11.83 & ab_count >= 5)
#' data.table::setorderv(r$stat, cols = "ll", order = -1L)
#' head(r$stat, 25)
#' 
#' if (requireNamespace("igraph", quietly = TRUE)){
#'   g <- r$as.igraph(as.undirected = TRUE)
#'   plot(g)
#' }
#' 
#' a <- cooccurrences(r, query = "oil")
#' a <- data.table::as.data.table(a)
#' 
#' b <- cooccurrences("REUTERS", query = "oil")
#' b <- data.table::as.data.table(b)
#' b <- b[!word %in% stopwords]
#' 
#' # now let's check whether results are identical
#' all(b[["word"]][1:5] == a[["word"]][1:5])
#' @importFrom slam simple_triplet_matrix
#' @export Cooccurrences
#' @rdname CooccurrencesR6
Cooccurrences <- R6::R6Class(
  
  classname = "Cooccurrences",
  
  public = list(
    
    corpus = NULL, # character 
    partition = NULL, # partition
    p_attribute = NULL, # character
    minimized = NULL, # logical
    keep = NULL, # list
    drop = NULL, # list
    method = NULL, # character
    window = NULL, # integer
    verbose = NULL, # logical
    window_sizes = NULL, # data.table
    stat = NULL, # data.table

    
    initialize = function(x = NULL, p_attribute = "word", window = 5L, verbose = TRUE, drop = c(polmineR::punctuation, tm::stopwords("de")), keep = NULL){
      
      if (is.character(x)){
        stopifnot(length(x) == 1L, x %in% corpus()[["corpus"]])
        C <- Corpus$new(x)
        C$p_attribute <- p_attribute
        C$count(p_attribute = p_attribute)
        self$partition <- C$as.partition()
        self$corpus <- x
      } else {
        stopifnot("partition" %in% is(x))
        self$partition <- x
        self$corpus <- x@corpus
      }
      
      self$p_attribute <- p_attribute
      self$window <- as.integer(window)
      self$verbose <- verbose
      self$method <- character()
      
      # for convencience, keep may be a character vector - turn that into list structure
      if (is.vector(keep)){
        if (length(p_attribute) == 1){
          self$keep <- list()
          self$keep[[p_attribute]] <- keep
        } else {
          stop("Param 'keep' is a vector, but length(p_attribute) > 1: Please supply a list of named character vectors.")
        }
      } else if (is.list(keep)){
        self$keep <- list
      }
      
      # the same for drop
      if (is.vector(drop)){
        if (length(p_attribute) == 1){
          self$drop <- list()
          self$drop[[p_attribute]] <- drop
        } else {
          stop("Param 'drop' is a vector, but length(p_attribute) > 1: Please supply a list of named character vectors.")
        }
      } else if (is.list(drop)){
        self$drop <- list
      }
      
      invisible(self)
    },
    
    show = function(){
      cat("Object of 'cooccurrences'-class\n")
    },
    
    summary = function(){
      message("not yet implemented")
    },
    
    count = function(parsimonious = TRUE){
      
      if (length(self$p_attribute) == 1L){
        
        if (parsimonious){
          
          for (i in c(-self$window:-1L, 1L:self$window)){
            
            if (self$verbose) message("Processing tokens at position: ", i)
            
            id_list <- lapply(
              1L:nrow(self$partition@cpos),
              function(i){
                corpus_positions <- self$partition@cpos[i,1]:self$partition@cpos[i,2]
                RcppCWB::cl_cpos2id(corpus = self$corpus, p_attribute = self$p_attribute, cpos = corpus_positions)
              }
            )
            
            node_vector <- unlist(lapply(
              id_list,
              function(ids) if (i < 0) ids[(abs(i) + 1L):length(ids)] else ids[1L:(length(ids) - abs(i))]
            ))
            
            collocate_vector <- unlist(lapply(
              id_list,
              function(ids) if (i < 0) ids[1L:(length(ids) - abs(i))] else ids[(abs(i) + 1L):length(ids)]
            ))
            
            dt <- data.table(a_id = node_vector, b_id = collocate_vector) [, .N, by = c("a_id", "b_id")]
            setkeyv(dt, cols = c("a_id", "b_id"))
            
            if (i == -self$window){
              self$stat <- dt
            } else {
              self$stat <- merge(self$stat, dt, all = TRUE)
              rm(dt); gc()
              self$stat[, "N" := ifelse(is.na(N.x), 0L, N.x) + ifelse(is.na(N.y), 0L, N.y)]
              self$stat[, "N.x" := NULL][, "N.y" := NULL]
            }
          }
          setnames(self$stat, old = "N", new = "ab_count")
          
          if (self$verbose) message("... data.table with sizes")
          
          self$window_sizes <- self$stat[, {sum(.SD[["ab_count"]])}, by = "a_id"]
          setnames(self$window_sizes, old = "V1", new = "size_window")

        } else {
          if (self$verbose) message("... getting window matrix (using RcppCWB)")
          window_matrix <- RcppCWB::get_cbow_matrix(
            corpus = self$corpus, p_attribute = self$p_attribute,
            registry = Sys.getenv("CORPUS_REGISTRY"),
            matrix = self$partition@cpos, window = self$window
          )
          window_dt <- as.data.table(window_matrix)
          
          rm(window_matrix); gc()
          
          setnames(window_dt, old = paste("V", self$window + 1, sep = ""), new = "a_id")
          if (self$verbose) message("... melting")
          cooc_dt <- data.table::melt.data.table(window_dt, id.vars = "a_id", value.name = "b_id")
          
          rm(window_dt); gc()
          
          cooc_dt[, "variable" := NULL, with = TRUE]
          
          if (self$verbose) message("... kicking out -1")
          dt_min <- cooc_dt[b_id != -1]
          self$window_sizes <- dt_min[, .N, by = "a_id"]
          setnames(self$window_sizes, old = "N", new = "size_window")
          if (self$verbose) message("... counting cooccurrences")
          self$stat <- dt_min[, .N, by = c("a_id", "b_id"), with = TRUE]
          setnames(self$stat, "N", "ab_count")
          setkeyv(self$stat, "a_id")
        }

      } else {
        
        if (length(self$p_attribute) == 0) stop("The partition is required to included counts. Enrich the object first!")
        
        pAttr <- sapply(p_attribute, function(x) paste(self$corpus, x, sep = "."))
        aColsId <- setNames(paste("a", self$p_attribute, "id", sep="_"), self$p_attribute)
        bColsId <- setNames(paste("b", self$p_attribute, "id", sep="_"), self$p_attribute)
        aColsStr <- setNames(paste("a", self$p_attribute, sep="_"), self$p_attribute)
        bColsStr <- setNames(paste("b", self$p_attribute, sep="_"), self$p_attribute)
        
        .makeWindows <- function(i, cpos, ...){
          cposMin <- cpos[i,1]
          cposMax <- cpos[i,2]
          if (cposMin != cposMax){
            cposRange <- cposMin:cposMax
            lapply(
              setNames(cposRange, cposRange),
              function(x) {
                cpos <- c((x - window):(x - 1L), (x + 1L):(x + window))
                cpos <- cpos[which(cpos >= cposMin)]
                cpos[which(cpos <= cposMax)]
              })
          }
        }
        bag <- blapply(as.list(1L:nrow(self$partition@cpos)), f = .makeWindows, cpos = self$partition@cpos, mc = mc)
        bCpos <- lapply(
          bag,
          function(x) lapply(names(x), function(y) rep(as.numeric(y), times = length(x[[y]])))
        )
        if (self$verbose) message("... putting together data.table")
        DT <- data.table(a_cpos = unlist(bag), b_cpos = unlist(bCpos))
        
        if (self$verbose) message("... getting token ids")
        lapply(
          p_attribute, function(x){
            DT[, eval(aColsId[x]) := cl_cpos2id(corpus = self$partition@corpus, p_attribute = x, cpos = DT[["a_cpos"]]), with = TRUE]
            DT[, eval(bColsId[x]) := cl_cpos2id(corpus = self$partition@corpus, p_attribute = x, cpos = DT[["b_cpos"]]), with = TRUE]
          }
        )
        if (self$verbose) message("... counting window size")
        
        contextDT <- DT[, .N, by = c(eval(aColsId)), with = TRUE]
        setnames(contextDT, "N", "size_window")
        self$window_sizes <- contextDT
        
        if (self$verbose) message("... counting co-occurrences")
        TF <- DT[, .N, by = c(eval(c(aColsId, bColsId))), with = TRUE]
        setnames(TF, "N", "ab_count")
        
        if (self$verbose) message("... adding window size")
        setkeyv(contextDT, cols = aColsId)
        setkeyv(TF, cols = aColsId)
        TF <- contextDT[TF]
      }
      invisible(self)
    },
    
    trim = function(action, by.id){
      # turn tokens to keep to id
      
      toMatch <- self[[action]]
      if (by.id == TRUE){
        toMatch <- lapply(
          setNames(names(toMatch), names(toMatch)),
          function(x) cl_str2id(corpus = self$corpus, p_attribute = x, str = toMatch[[x]])
        )
        colRegex <- if (length(self$p_attribute) > 1) paste(names(toMatch), "id$", sep = "_") else "_id$"
      } else {
        colRegex <- paste(names(toMatch), "$", sep = "")
      }
      
      indexList <- lapply(
        names(toMatch),
        function(pAttr){
          lapply(
            grep(colRegex, colnames(self$stat)),
            function(i) which(self$stat[[i]] %in% toMatch[[pAttr]])
          )}
      )
      
      rowindex <- unique(unlist(indexList))
      if (action == "keep"){
        self$stat <- self$stat[rowindex]
      } else if (action == "drop"){
        if (length(rowindex) > 0){
          self$stat <- self$stat[-rowindex]
        }
      }
      invisible(self)
    },
    
    as.simple_triplet_matrix = function(){
      
      if (self$verbose) message("... creating data.table for reindexing")  
      ID2STR <- data.table(id = unique(self$stat[["a_id"]]))
      ID2STR[ , str := as.nativeEnc(cl_id2str(corpus = self$partition@corpus, p_attribute = self$p_attribute, id = ID2STR[["id"]]), from = getEncoding(self$corpus))]
      setkeyv(ID2STR, cols = "id")
      setorderv(ID2STR, cols = "id")
      ID2STR[, "id_new" := 1L:nrow(ID2STR), with = TRUE]
      setkeyv(self$stat, "a_id")
      
      if (self$verbose) message("... id2str for a")
      coocCount2 <- self$stat[ID2STR]
      data.table::setnames(coocCount2, old = c("str", "id_new"), new = c("a_token", "a_new_key"))
      setkeyv(coocCount2, "b_id")
      if (self$verbose) message("... id2str for b")
      coocCount3 <- coocCount2[ID2STR]
      rm(coocCount2)
      setnames(coocCount3, old = c("str", "id_new"), new = c("b_token", "b_new_key"))
      if (self$verbose) message("... preparing simple_triplet_matrix")
      retval <- slam::simple_triplet_matrix(
        i = coocCount3[["a_new_key"]],
        j = coocCount3[["b_new_key"]],
        v = coocCount3[["ab_count"]],
        dimnames = list(ID2STR[["str"]], ID2STR[["str"]])
      )
      retval
    },
    
    ll = function(){
      
      self$method <- c(self$method, "ll")
      
      if (is.null(self$stat)){
        self$count()
        self$trim(action = "drop", by.id = TRUE)
      }
      
      if (self$verbose) message("... adding window size")
      
      setkeyv(self$window_sizes, "a_id")
      setkeyv(self$stat, "a_id")
      DT <- self$window_sizes[self$stat]
      
      if (length(self$p_attribute) == 1L){
        
        DT[, "a" := as.nativeEnc(cl_id2str(corpus = self$partition@corpus, p_attribute = self$p_attribute, id = DT[["a_id"]]), from = registry_get_encoding(self$corpus))]
        DT[, "b" := as.nativeEnc(cl_id2str(corpus = self$partition@corpus, p_attribute = self$p_attribute, id = DT[["b_id"]]), from = registry_get_encoding(self$corpus))]
        DT[, "a_id" := NULL][, "b_id" := NULL]
        setkeyv(self$partition@stat, self$p_attribute)
        setkeyv(DT, cols = "a")
        DT2 <- self$partition@stat[DT]
        
        rm(DT); gc()
        setnames(DT2, old = c(self$p_attribute, "count", paste(self$p_attribute, "id", sep = "_")), new = c("a", "a_count", paste("a", self$p_attribute, "id", sep = "_")))
        
        setkeyv(DT2, cols = "b")
        
        self$stat <- self$partition@stat[DT2]
        rm(DT2); gc()
        
        setnames(self$stat, old = c(self$p_attribute, "count", paste(self$p_attribute, "id", sep = "_")), new = c("b", "b_count", paste("b", self$p_attribute, "id", sep = "_")))
        setnames(self$stat, old = c("a", "b"), new = c(paste("a", self$p_attribute, sep = "_"), paste("b", self$p_attribute, sep = "_")))
        
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
      
      
      if (self$verbose) message('... g2-Test')
      
      exp_total <- self$stat[["b_count"]] / self$partition@size
      count_ref <- self$stat[["b_count"]] - self$stat[["ab_count"]]
      count_ref <- ifelse(count_ref < 0L, 0L, count_ref)
      self$stat[, "exp_coi" := self$stat[["size_window"]] * exp_total]
      self$stat[, "exp_ref" := (self$partition@size - self$stat[["size_window"]]) * exp_total]
      
      A <- self$stat[["ab_count"]] / self$stat[["exp_coi"]]
      B <- self$stat[["b_count"]] - self$stat[["exp_coi"]]
      C <- B / self$stat[["exp_ref"]]
      D <- ifelse(C > 0, log(C), log(0.0000001))
      E <- B * D
      ll_value <- 2 * (self$stat[["ab_count"]] * log(A) + E)
      
      direction <- ifelse(self$stat[["ab_count"]] < self$stat[["exp_coi"]], -1L, 1L)
      self$stat[, "ll" := ll_value * direction]
      
      setorderv(self$stat, cols = "ll", order = -1)
      self$stat[, "rank_ll" := 1L:nrow(self$stat)]
      
      invisible(self)
      
    },
    
    get_features = function(reference, included = FALSE, method = "ll"){
      
      if (!identical(self$p_attribute, reference$p_attribute)) {
        warning("BEWARE: cooccurrences objects are not based on the same p_attribute!")
      }
      
      if (self$verbose) message("... preparing tabs for matching")
      keys <- unlist(lapply(c("a", "b"), function(ab) paste(ab, self$p_attribute, sep = "_"))) 
      setkeyv(self$stat, keys)
      setkeyv(reference$stat, keys)
      MATCH <- reference$stat[self$stat]
      
      # remove columns not needed
      setnames(MATCH, old = c("ab_count", "i.ab_count"), new = c("count_ref", "count_coi"))
      colsToKeep <- c(keys, "count_ref", "count_coi")
      colsToDrop <- colnames(MATCH)[!colnames(MATCH) %in% colsToKeep]
      for (drop in colsToDrop) MATCH[, eval(drop) := NULL, with = TRUE]
      if (included) MATCH[, "count_ref" := MATCH[["count_ref"]] - MATCH[["count_coi"]] ]
      
      compObject <- new(
        "features",
        included = FALSE, corpus = self$corpus, size_coi = self$partition@size,
        size_ref = if (included) reference$partition@size - self$partition@size else reference$partition@size,
        p_attribute = self$p_attribute,
        stat = MATCH
      )
      
      for (how in method){
        if (self$verbose) message("... statistical test: ", how)
        compObject <- do.call(how, args = list(.Object = compObject))
      }
      return(compObject@stat)
    },
    
    
    select_features = function(reference, included = FALSE, method = "ll", n = 250){s
      DT <- self$get_features(reference = reference, included = included, method = method, verbose = self$verbose)
      keys <- unlist(lapply(c("a", "b"), function(what) paste(what, self$p_attribute, sep = "_")))
      rowsToKeep <- c(keys, "rank_ll")
      DT <- DT[, rowsToKeep, with = FALSE]
      DT[, keep := ifelse(rank_ll <= n, TRUE, FALSE)]
      DT[, rank_ll := NULL]
      DT2 <- DT[DT[["keep"]] == TRUE]
      
      setkeyv(self$stat, keys)
      setkeyv(DT2, keys)
      Y <- self$stat[DT2]
      self$stat <- Y
      invisible(self)
    },
    
    minimize = function(){
      DT <- copy(self$stat)
      aColsStr <- paste("a", self$p_attribute, sep = "_")
      bColsStr <- paste("b", self$p_attribute, sep = "_")
      KEY <- data.table(
        i = 1L:nrow(DT),
        aKey = apply(DT, 1L, function(x) paste(x[aColsStr], collapse = "//")),
        bKey = apply(DT, 1L, function(x) paste(x[bColsStr], collapse = "//"))
      )
      DT[, "order" := KEY[, order(c(.SD[["aKey"]][1], .SD[["bKey"]][1]))[1], by = "i"][["V1"]]]
      setkey(DT, "order")
      aToB <- DT[list(1)]
      setkeyv(aToB, cols = c(aColsStr, bColsStr))
      bToA <- DT[list(2)]
      setnames(bToA, old = c(aColsStr, bColsStr), new = c(bColsStr, aColsStr))
      setkeyv(bToA, cols = c(aColsStr, bColsStr))
      merger <- merge(aToB, bToA, all.x = FALSE, all.y = TRUE)
      FIN <- merger[, c(aColsStr, bColsStr, "ab_count.x", "ll.x", "ll.y", "a_count.x", "b_count.x"), with = FALSE]
      setnames(
        FIN,
        c("ab_count.x", "ll.x", "ll.y", "a_count.x", "b_count.x"),
        c("ab_count", "ab_ll", "ba_ll", "a_count", "b_count")
      )
      setcolorder(FIN, c(aColsStr, bColsStr, "ab_count", "a_count", "b_count", "ab_ll", "ba_ll"))
      setkeyv(FIN, cols = c(aColsStr, bColsStr))
      self$minimized <- TRUE
      self$stat <- FIN
      invisible(self)
    },
    
    subset = function(...){
      self$stat <- subset(self$stat, ...)
      invisible(self)
    },
    
    as.sparseMatrix = function(x, col){
      uniqueTerms <- unique(c(self$stat[,"node"], self$stat[,"cooccurrence"]))
      keyVector <- setNames(c(1:length(uniqueTerms)), uniqueTerms)
      splittedTab <- split(x = self$stat[,c(col, "cooccurrence")], f = self$stat[,"node"])
      
      bag <- list()
      i <- unname(unlist(lapply(names(splittedTab), function(n) rep(keyVector[n], times=nrow(splittedTab[[n]]))))) #nodes
      j <- unname(unlist(lapply(splittedTab, function(tab) keyVector[tab[,"cooccurrence"]]))) # cooccurrences
      x <- unname(unlist(lapply(splittedTab, function(tab) tab[,col]))) # values
      
      retval <- sparseMatrix(
        i = i, j = j, x = x, 
        dims = c(length(uniqueTerms), length(uniqueTerms)),
        dimnames = list(names(keyVector), names(keyVector)),
        giveCsparse = TRUE
      )   
      return(retval)
    },
    
    as.igraph = function(edge_attributes = c("ll", "ab_count", "rank_ll"), vertex_attributes = "count", as.undirected = TRUE){
      
      if (!requireNamespace("igraph", quietly = TRUE))
        stop("Package 'igraph' is required for as.igraph()-method, but not yet installed.")
      
      if (!all(edge_attributes %in% colnames(self$stat)))
        warning("edgeAttribute supplied is not available")
      
      tab <- as.data.frame(self$stat)
      aColsStr <- paste("a", self$p_attribute, sep = "_")
      bColsStr <- paste("b", self$p_attribute, sep = "_")
      if (length(self$p_attribute) == 1L){
        tab[["node"]] <- tab[[aColsStr]]
        tab[["collocate"]] <- tab[[bColsStr]]
      } else {
        tab[["node"]] <- apply(tab, 1, function(x) paste(x[aColsStr], collapse = "//"))
        tab[["collocate"]] <- apply(tab, 1, function(x) paste(x[bColsStr], collapse="//"))
      }
      g <- igraph::graph_from_data_frame(tab[, c("node", "collocate", edge_attributes)])
      if ("count" %in% vertex_attributes){
        TF <- data.table::copy(self$partition@stat) # this will be a data.frame
        if (length(self$p_attribute) == 1){
          TF[, "key" := TF[[self$p_attribute]] ]
        } else{
          TF[, "key" := apply(TF, 1, function(row) paste(row[self$p_attribute], collapse = "//"))]
        }
        setkey(TF, key)
        tfVector <- TF[names(igraph::V(g))][["count"]]
        igraph::V(g)$count <- tfVector
        igraph::V(g)$freq <- round((tfVector / self$partition@size) * 100000, 3)
      }
      if (as.undirected) g <- igraph::as.undirected(g, edge.attr.comb = "concat")
      g <- igraph::delete_vertices(g, igraph::V(g)[name == "\u0084"])
      g <- igraph::delete_vertices(g, igraph::V(g)[name == "\u0093"])
      g
    }
    
  )
)

setOldClass("Cooccurrences")

#' @rdname cooccurrences
setMethod("cooccurrences", "Cooccurrences", function(.Object, query){
  y <- new(
    "cooccurrences",
    corpus = .Object$corpus,
    p_attribute = .Object$p_attribute,
    encoding = .Object$partition@encoding,
    query = query,
    partition = .Object$partition,
    size_partition = size(.Object$partition),
    left = .Object$window,
    right = .Object$window,
    size = sum(.Object$window_sizes),
    boundary = character(),
    cpos = data.table(),
    call = character(),
    stat = subset(.Object$stat, .Object$stat[[paste("a", .Object$p_attribute, sep = "_")]] == query),
    method = .Object$method,
    included = FALSE,
    size_ref = size(.Object$partition) - sum(.Object$window_sizes),
    size_coi = sum(.Object$window_sizes)
  )
  y@stat[, "a_word_id" := NULL][, "b_word_id" := NULL][, "size_window" := NULL][, "a_word" := NULL][, "a_count" := NULL]
  setnames(
    y@stat,
    old = c("ab_count", "b_count", "exp_coi", "exp_ref", "b_word"),
    new = c("count_window", "count_partition", "exp_window", "exp_partition", "word")
  )
  setorderv(y@stat, cols = y@method[1], order = -1L)
  y@stat[[paste("rank", y@method[1], sep = "_")]] <- 1L:nrow(y@stat)
  y
})

