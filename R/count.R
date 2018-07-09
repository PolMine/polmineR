#' @include polmineR.R textstat.R partition.R context.R partition_bundle.R corpus.R S4classes.R
NULL




#' Get counts.
#' 
#' Count all tokens, or number of occurrences of a query (CQP syntax may be
#' used), or matches for the query.
#'
#' If .Object is a \code{partiton_bundle}, the data.table returned will have the
#' queries in the columns, and as many rows as there are in the
#' \code{partition_bundle}.
#'
#' If .Object is a length-one character vector and query is NULL, the count is
#' performed for the whole partition.
#'
#' If \code{breakdown} is \code{TRUE} and one query is supplied, the function
#' returns a frequency breakdown of the results of the query. If several queries
#' are supplied, frequencies for the individual queries are retrieved.
#' 
#' @seealso  For a metadata-based breakdown of counts
#' (i.e. tabulation by s-attributes), see \code{dispersion}.
#' 
#' @param .Object A \code{partition} or \code{partition_bundle}, or a length-one
#'   character vector providing the name of a corpus.
#' @param query A character vector (one or multiple terms), CQP syntax can be
#'   used.
#' @param cqp Either logical (\code{TRUE} if query is a CQP query), or a
#'   function to check whether query is a CQP query or not (defaults to is.query
#'   auxiliary function).
#' @param p_attribute The p-attribute(s) to use.
#' @param corpus The name of a CWB corpus.
#' @param decode Logical, whether to turn token ids into decoded strings (only
#'   if query is NULL).
#' @param sort Logical, whether to sort table with counts (in stat slot).
#' @param mc Logical, whether to use multicore (defaults to \code{FALSE}).
#' @param verbose Logical, whether to be verbose.
#' @param freq Logical, if \code{FALSE}, counts will be reported, if TRUE,
#'   (relative) frequencies are added to table.
#' @param breakdown Logical, whether to report number of occurrences for
#'   different matches for a query.
#' @param total Defaults to \code{FALSE}, if \code{TRUE}, the total value of
#'   counts (column named 'TOTAL') will be amended to the \code{data.table} that
#'   is returned.
#' @param progress Logical, whether to show progress bar.
#' @param ... Further arguments.
#' @return A \code{data.table} if argument query is used, a \code{count}-object,
#'   if query is \code{NULL} and \code{.Object} is a character vector (referring 
#'   to a corpus) or a \code{partition}, a \code{count_bundle}-object, if \code{.Object}
#'   is a \code{partition_bundle}.
#' @exportMethod count
#' @docType methods
#' @rdname count-method
#' @name count
#' @aliases count-method
#' @seealso count
#' @references 
#' Baker, Paul (2006): \emph{Using Corpora in Discourse Analysis}. London: continuum, p. 47-69 (ch. 3).
#' @examples
#' use("polmineR")
#' debates <- partition("GERMAPARLMINI", date = ".*", regex=TRUE)
#' count(debates, query = "Arbeit") # get frequencies for one token
#' count(debates, c("Arbeit", "Freizeit", "Zukunft")) # get frequencies for multiple tokens
#'   
#' count("GERMAPARLMINI", query = c("Migration", "Integration"), p_attribute = "word")
#' 
#' debates <- partition_bundle(
#'   "GERMAPARLMINI", s_attribute = "date", values = NULL,
#'   regex = TRUE, mc = FALSE, verbose = FALSE
#' )
#' y <- count(debates, query = "Arbeit", p_attribute = "word")
#' y <- count(debates, query = c("Arbeit", "Migration", "Zukunft"), p_attribute = "word")
#'   
#' count("GERMAPARLMINI", '"Integration.*"', breakdown = TRUE)
#' 
#' P <- partition("GERMAPARLMINI", date = "2009-11-11")
#' count(P, '"Integration.*"', breakdown = TRUE)
setGeneric("count", function(.Object, ...){standardGeneric("count")})

#' @rdname count-method
setMethod("count", "partition", function(
  .Object, query = NULL, cqp = is.cqp, breakdown = FALSE,
  decode = TRUE, p_attribute = getOption("polmineR.p_attribute"),
  mc = getOption("polmineR.cores"), verbose = TRUE, progress = FALSE,
  ...
){
  
  if ("pAttribute" %in% names(list(...))) p_attribute <- list(...)[["pAttribute"]]
  
  stopifnot( is.logical(breakdown) == TRUE)
  if (!is.null(query)){
    if (progress) verbose <- FALSE
    if (breakdown){
      dts <- lapply(
        query,
        function(x){
          cposHits <- cpos(.Object = .Object, query = x, cqp = cqp, p_attribute = p_attribute)
          if (is.null(cposHits)) return( NULL )
          hitsString <- apply(
            cposHits, 1,
            function(x) paste(CQI$cpos2str(.Object@corpus, p_attribute, x[1]:x[2]), collapse = ' ')
          )
          result <- table(hitsString)
          dt <- data.table(query = x, match = names(result), count = as.vector(unname(result)))
          if (nrow(dt) > 0){
            Encoding(dt[["match"]]) <- .Object@encoding
            dt[["match"]] <- as.nativeEnc(dt[["match"]], from = .Object@encoding)
            setorderv(dt, cols = "count", order = -1L)
            dt[["share"]] <- round(dt[["count"]] / sum(dt[["count"]]) * 100, 2)
          } else {
            dt <- NULL
          }
          dt
        }
      )
      return( rbindlist(dts) )
    } else if (breakdown == FALSE){
      .getNumberOfHits <- function(query, partition, cqp, p_attribute, ...) {
        .message("processing query", query, verbose = verbose)
        cposResult <- cpos(.Object = .Object, query = query, cqp = cqp, p_attribute = p_attribute, verbose = FALSE)
        if (is.null(cposResult)) return( 0 ) else return( nrow(cposResult) )
      }
      no <- as.integer(blapply(
        as.list(query),
        f = .getNumberOfHits,
        partition = .Object, cqp = cqp, p_attribute = p_attribute,
        mc = mc, verbose = verbose, progress = progress
      ))
      return( data.table(query = query, count = no, freq = no/.Object@size) )
    }
  } else {
    pAttr_id <- paste(p_attribute, "id", sep = "_")
    if (length(p_attribute) == 1){
      countMatrix <- RcppCWB::region_matrix_to_count_matrix(
        corpus = .Object@corpus, p_attribute = p_attribute,
        matrix = .Object@cpos
      )
      TF <- as.data.table(countMatrix)
      setnames(TF, old = c("V1", "V2"), new = c(pAttr_id, "count"))
    } else {
      cpos <- unlist(apply(.Object@cpos, 1, function(x) x[1]:x[2]))
      idList <- lapply(p_attribute, function(p) CQI$cpos2id(.Object@corpus, p, cpos))
      names(idList) <- paste(p_attribute, "id", sep = "_")
      ID <- as.data.table(idList)
      setkeyv(ID, cols = names(idList))
      TF <- ID[, .N, by = c(eval(names(idList))), with = TRUE]
      setnames(TF, "N", "count")
    }
    if (decode){
      dummy <- lapply(
        1L:length(p_attribute),
        function(i){
          str <- as.nativeEnc(CQI$id2str(.Object@corpus, p_attribute[i], TF[[pAttr_id[i]]]), from = .Object@encoding)
          TF[, eval(p_attribute[i]) := str , with = TRUE] 
        })
      setcolorder(TF, neworder = c(p_attribute, pAttr_id, "count"))
    } else {
      setcolorder(TF, neworder = c(pAttr_id, "count"))
    }
    y <- new(
      Class = "count",
      corpus = .Object@corpus,
      p_attribute = p_attribute,
      encoding = .Object@encoding,
      stat = TF,
      name = character(),
      size = .Object@size
    )
    return(y)
  }
})


#' @rdname count-method
#' @docType methods
setMethod("count", "partition_bundle", function(.Object, query = NULL, cqp = FALSE, p_attribute = NULL, freq = FALSE, total = TRUE, mc = FALSE, progress = TRUE, verbose = FALSE, ...){
  
  if ("pAttribute" %in% names(list(...))) p_attribute <- list(...)[["pAttribute"]]

  if (!is.null(query)){
    .message("getting hits for query/queries", verbose = verbose)
    DT <- hits(.Object, query = query, cqp = cqp, p_attribute = p_attribute, mc = mc, progress = progress, verbose = verbose)@stat
    .message("rearranging table", verbose = verbose)
    DT_cast <- dcast.data.table(DT, partition~query, value.var = "count", fill = 0)
    
    # remove counts that are not in one of the partitions
    noPartition <- which(is.na(DT_cast[["partition"]]) == TRUE)
    if (length(noPartition) > 0) DT_cast <- DT_cast[-noPartition]
    
    # add rows for partitions withous hits (all 0)
    missingPartitions <- names(.Object)[which(!names(.Object) %in% DT_cast[[1]])]
    if (length(missingPartitions) > 0){
      queryColnames <- colnames(DT_cast)[2L:ncol(DT_cast)]
      DTnewList <- c(
        list(partition = missingPartitions),
        lapply(setNames(queryColnames, queryColnames), function(Q) rep(0L, times = length(missingPartitions)))
      )
      DTnomatch <- data.table(data.frame(DTnewList, stringsAsFactors = FALSE))
      DT_cast <- rbindlist(list(DT_cast, DTnomatch))
    }
    
    # add columns for quits without hits (all 0)
    missingQueries <- query[!query %in% colnames(DT_cast)[2L:ncol(DT_cast)]]
    if (length(missingQueries) > 0){
      for (q in missingQueries){
        DT_cast[, eval(q) := rep(0L, times = nrow(DT_cast)), with = TRUE]
      }
    }
    
    if (total){
      .message("adding total number of hits (col 'TOTAL', verbose = verbose)")
      DT_cast[, "TOTAL" := rowSums(DT_cast[, 2L:ncol(DT_cast), with = FALSE]), with = TRUE]
    }
    return(DT_cast)
  } else {
    corpus <- corpus(.Object)
    if (length(corpus) > 1) stop("partitions in partition_bundle must be derived from the same corpus")
    if (verbose) message("... unfolding corpus positions")
    cpos_list <- lapply(
      .Object@objects,
      function(x) data.table(name = x@name, cpos = cpos(x@cpos))
    )
    DT <- rbindlist(cpos_list)
    rm(cpos_list)
    if (verbose) message(sprintf("... adding ids for p-attribute '%s'", p_attribute))
    DT[["id"]] <- CQI$cpos2id(corpus = corpus, p_attribute = p_attribute, cpos = DT[["cpos"]])
    if (verbose) message("... performing count")
    CNT <- DT[,.N, by = c("name", "id")]
    rm(DT)
    setnames(CNT, old = "N", new = "count")
    if (verbose) message("... adding decoded p-attribute")
    CNT[[p_attribute]] <- CQI$id2str(corpus = corpus, p_attribute = p_attribute, id = CNT[["id"]])
    if (verbose) message("... creating bundle of count objects")
    CNT_list <- split(CNT, by = "name")
    rm(CNT)
    y <- lapply(
      1L:length(CNT_list),
      function(i){
        new(
          "count",
          corpus = corpus,
          encoding = .Object@objects[[i]]@encoding,
          p_attribute = p_attribute,
          stat = CNT_list[[i]],
          name = names(CNT_list)[[i]],
          size = size(.Object@objects[[i]])
        )
      }
    )
    names(y) <- names(.Object)
    y2 <- new(
      Class = "count_bundle",
      objects = y,
      p_attribute = p_attribute,
      corpus = corpus,
      encoding = unique(unlist(lapply(as.list(.Object), function(x) encoding(x))))
    )
    return( y2 )
  }
})

#' @rdname count-method
setMethod("count", "character", function(.Object, query = NULL, cqp = is.cqp, p_attribute = getOption("polmineR.p_attribute"), breakdown = FALSE, sort = FALSE, decode = TRUE, verbose = TRUE, ...){

  if ("pAttribute" %in% names(list(...))) p_attribute <- list(...)[["pAttribute"]]
  
  stopifnot(.Object %in% CQI$list_corpora())
  if (is.null(query)){
    if (length(p_attribute) == 1){
      TF <- data.table(count = RcppCWB::get_count_vector(corpus = .Object, p_attribute = p_attribute))
      TF[, "id" := 0:(nrow(TF) - 1), with = TRUE]
      setnames(TF, old = "id", new = paste(p_attribute, "id", sep = "_"))
      if (decode == FALSE){
        setkeyv(TF, paste(p_attribute, "id", sep = "_"))
        setcolorder(TF, c(paste(p_attribute, "id", sep = "_"), "count"))
      } else {
        TF[, "token" := as.nativeEnc(CQI$id2str(.Object, p_attribute, 0:(nrow(TF) - 1)), from = registry_get_encoding(.Object)), with = TRUE]
        Encoding(TF[["token"]]) <- "unknown"
        setnames(TF, old = "token", new = p_attribute)
        setkeyv(TF, p_attribute)
        setcolorder(TF, c(p_attribute, paste(p_attribute, "id", sep = "_"), "count"))
        if (sort) setorderv(TF, cols = p_attribute)
      }
    } else {
      
      tokenStreamDT <- as.data.table(
        li <- lapply(
          setNames(p_attribute, paste(p_attribute, "id", sep = "_")),
          function(pAttr){
            .message("getting token stream for p-attribute: ", pAttr, verbose = verbose)
            CQI$cpos2id(.Object, pAttr, 0L:(size(.Object) - 1L))
          }
        )
      )
      .message("counting", verbose = verbose)
      TF <- tokenStreamDT[, .N, by = c(eval(colnames(tokenStreamDT)))]
      setnames(TF, old = "N", new = "count")
      if (decode){
        for (pAttr in p_attribute){
          .message("decode p-attribute: ", pAttr, verbose = verbose)
          TF[, eval(pAttr) := as.nativeEnc(CQI$id2str(.Object, pAttr, TF[[paste(pAttr, "id", sep = "_")]]), from = registry_get_encoding(.Object)), with = TRUE]
        }
        setcolorder(TF, c(p_attribute, paste(p_attribute, "id", sep = "_"), "count"))
      }
    }
    y <- new(
      Class = "count",
      corpus = .Object,
      p_attribute = p_attribute,
      encoding = registry_get_encoding(.Object),
      stat = TF,
      name = character(),
      size = size(.Object)
    )
    return(y)
  } else {
    stopifnot(.Object %in% CQI$list_corpora())
    total <- CQI$attribute_size(.Object, p_attribute, type = "p")
    if (class(cqp) == "function") cqp <- cqp(query)
    if (length(cqp) > 1) stop("length of cqp is larger than 1, it needs to be 1")
    if (cqp == FALSE){
      query <- as.corpusEnc(query, corpusEnc = registry_get_encoding(.Object))
      count <- sapply(
        query,
        function(query){
          query_id <- CQI$str2id(.Object, p_attribute, query)
          # if there is no id for query, query_id will be -5
          if (query_id >= 0) CQI$id2freq(.Object, p_attribute, query_id) else 0
        }
      )
      return(data.table(query = query, count = count, freq = count / total))
    } else if (cqp){
      if (breakdown == FALSE){
        count <- sapply(
          query,
          function(query){
            cpos_matrix <- cpos(.Object, query, cqp = cqp, p_attribute = p_attribute, encoding = registry_get_encoding(.Object))
            if (!is.null(cpos_matrix)){
              return( nrow(cpos_matrix) )
            } else {
              return( 0 )
            }
            
          })
        return( data.table(query = query, count = count, freq = count / total) )
      } else {
        C <- Corpus$new(.Object)
        C$p_attribute <- p_attribute
        retval <- count(.Object = C$as.partition(), query = query, cqp = cqp, p_attribute = p_attribute, breakdown = TRUE)
        return( retval )
      }
    }
  }
})

#' @rdname context-class
setMethod("count", "context", function(.Object) .Object@count )

#' @rdname count-method
setMethod("count", "vector", function(.Object, corpus, p_attribute, ...){
  
  if ("pAttribute" %in% names(list(...))) p_attribute <- list(...)[["pAttribute"]]

  ids <- CQI$cpos2id(corpus, p_attribute, .Object)
  count <- tabulate(ids)
  TF <- data.table(
    id = 0:length(count),
    count = c(length(which(ids == 0)), count)
  )
  setkey(TF, "id")
  setnames(TF, "id", paste(p_attribute, "id", sep = "_"))
  TF[count > 0]
})

#' @rdname count-method
setMethod("count", "Corpus", function(.Object, query = NULL, p_attribute){
  count(.Object$as.partition(), query = query, p_attribute = p_attribute)
})


#' @exportMethod hist
#' @rdname count_class
setMethod("hist", "count", function(x, ...) hist(x@stat[,"count"], ...) )
