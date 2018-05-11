#' Get counts.
#' 
#' Count all tokens, or number of occurrences of a query (CQP syntax may be
#' used), or matches for the query.
#' 
#' If .Object is a \code{partitonBundle}, the data.table returned will have the
#' queries in the columns, and as many rows as there are in the partitionBundle.
#' 
#' If .Object is a character vector (length 1) and query is NULL, the count is
#' performed for the whole partition. If \code{breakdown} is \code{TRUE} and one
#' query is supplied, the function returns a frequency breakdown of the results
#' of the query. If several queries are supplied, frequencies for the individual
#' queries are retrieved.
#' 
#' @seealso  For a metadata-based breakdown of counts
#' (i.e. tabulation by s-attributes), see \code{"dispersion"}.
#' 
#' @param .Object a \code{"partition"} or \code{"partitionBundle"} object, or a
#'   character vector (length 1) providing the name of a corpus
#' @param query a character vector (one or multiple terms to be looked up), CQP
#'   syntax can be used.
#' @param cqp either logical (TRUE if query is a CQP query), or a
#'   function to check whether query is a CQP query or not (defaults to is.query
#'   auxiliary function)
#' @param pAttribute the p-attribute(s) to use
#' @param corpus name of CWB corpus
#' @param decode logical, whether to add rownames (only if query is NULL)
#' @param sort logical, whether to sort stat
#' @param mc logical, whether to use multicore (defaults to FALSE)
#' @param verbose logical, whether to be verbose
#' @param freq logical, if FALSE, counts will be reported, if TRUE, frequencies
#' @param breakdown logical, whether to count occurrences for different matches for a query
#' @param total defaults to FALSE, if TRUE, the added value of counts (column:
#'   TOTAL) will be amended to the data.table that is returned
#' @param progress logical, whether to show progress
#' @param ... further parameters
#' @return a \code{"data.table"}
#' @exportMethod count
#' @docType methods
#' @rdname count-method
#' @name count
#' @aliases count-method
#' @seealso count
#' @references 
#' Baker, Paul (2006): \emph{Using Corpora in Discourse Analysis}. London: continuum, p. 47-69 (ch. 3).
#' @examples
#'   use("polmineR")
#'   debates <- partition("GERMAPARLMINI", date = ".*", regex=TRUE)
#'   count(debates, query = "Arbeit") # get frequencies for one token
#'   count(debates, c("Arbeit", "Freizeit", "Zukunft")) # get frequencies for multiple tokens
#'   
#'   count("GERMAPARLMINI", query = c("Migration", "Integration"), pAttribute = "word")
#' 
#'   debates <- partitionBundle(
#'     "GERMAPARLMINI", sAttribute = "date", values = NULL,
#'     regex = TRUE, mc = FALSE, verbose = FALSE
#'   )
#'   y <- count(debates, query = "Arbeit", pAttribute = "word")
#'   y <- count(debates, query = c("Arbeit", "Migration", "Zukunft"), pAttribute = "word")
#'   
#'   count("GERMAPARLMINI", '"Integration.*"', breakdown = TRUE)
#' 
#'   P <- partition("GERMAPARLMINI", date = "2009-11-11")
#'   count(P, '"Integration.*"', breakdown = TRUE)
setGeneric("count", function(.Object, ...){standardGeneric("count")})

#' @rdname count-method
setMethod("count", "partition", function(
  .Object, query = NULL, cqp = is.cqp, breakdown = FALSE,
  decode = TRUE, pAttribute = getOption("polmineR.pAttribute"),
  mc = getOption("polmineR.cores"), verbose = TRUE, progress = FALSE
  ){
  stopifnot( is.logical(breakdown) == TRUE)
  if (!is.null(query)){
    if (progress) verbose <- FALSE
    if (breakdown == TRUE){
      dts <- lapply(
        query,
        function(x){
          cposHits <- cpos(.Object = .Object, query = x, cqp = cqp, pAttribute = pAttribute)
          if (is.null(cposHits)) return( NULL )
          hitsString <- apply(
            cposHits, 1,
            function(x) paste(CQI$cpos2str(.Object@corpus, pAttribute, x[1]:x[2]), collapse = ' ')
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
      .getNumberOfHits <- function(query, partition, cqp, pAttribute, ...) {
        .message("processing query", query, verbose = verbose)
        cposResult <- cpos(.Object = .Object, query = query, cqp = cqp, pAttribute = pAttribute, verbose = FALSE)
        if (is.null(cposResult)) return( 0 ) else return( nrow(cposResult) )
      }
      no <- as.integer(blapply(
        as.list(query),
        f = .getNumberOfHits,
        partition = .Object, cqp = cqp, pAttribute = pAttribute,
        mc = mc, verbose = verbose, progress = progress
      ))
      return( data.table(query = query, count = no, freq = no/.Object@size) )
    }
  } else {
    pAttr_id <- paste(pAttribute, "id", sep = "_")
    if (length(pAttribute) == 1){
      if (class(CQI)[1] == "CQI.RcppCWB"){
        .message("using RcppCWB", verbose = verbose)
        countMatrix <- RcppCWB::region_matrix_to_count_matrix(
          corpus = .Object@corpus, p_attribute = pAttribute,
          matrix = .Object@cpos
          )
        TF <- as.data.table(countMatrix)
        setnames(TF, old = c("V1", "V2"), new = c(pAttr_id, "count"))
      } else {
        cpos <- unlist(apply(.Object@cpos, 1, function(x) x[1]:x[2]))
        TF <- count(cpos, .Object@corpus, pAttribute)
      }
    } 
    #   else {
    #   .message("using rcqp", verbose = verbose)
    #   cpos <- unlist(apply(.Object@cpos, 1, function(x) x[1]:x[2]))
    #   idList <- lapply(pAttribute, function(p) CQI$cpos2id(.Object@corpus, p, cpos))
    #   names(idList) <- paste(pAttribute, "id", sep = "_")
    #   ID <- as.data.table(idList)
    #   setkeyv(ID, cols = names(idList))
    #   TF <- ID[, .N, by = c(eval(names(idList))), with = TRUE]
    #   setnames(TF, "N", "count")
    # }
    if (decode){
      dummy <- lapply(
        1:length(pAttribute),
        function(i){
          str <- as.nativeEnc(CQI$id2str(.Object@corpus, pAttribute[i], TF[[pAttr_id[i]]]), from = .Object@encoding)
          TF[, eval(pAttribute[i]) := str , with = TRUE] 
        })
      setcolorder(TF, neworder = c(pAttribute, pAttr_id, "count"))
    } else {
      setcolorder(TF, neworder = c(pAttr_id, "count"))
    }
    return(TF)
  }
})


#' @rdname count-method
#' @docType methods
setMethod("count", "partitionBundle", function(.Object, query, cqp = FALSE, pAttribute = NULL, freq = FALSE, total = TRUE, mc = FALSE, progress = TRUE, verbose = FALSE){
  .message("getting hits for query/queries", verbose = verbose)
  DT <- hits(.Object, query = query, cqp = cqp, pAttribute = pAttribute, mc = mc, progress = progress, verbose = verbose)@stat
  .message("rearranging table", verbose = verbose)
  DT_cast <- dcast.data.table(DT, partition~query, value.var = "count", fill = 0)
  
  # remove counts that are not in one of the partitions
  noPartition <- which(is.na(DT_cast[["partition"]]) == TRUE)
  if (length(noPartition) > 0) DT_cast <- DT_cast[-noPartition]

  # add rows for partitions withous hits (all 0)
  missingPartitions <- names(.Object)[which(!names(.Object) %in% DT_cast[[1]])]
  if (length(missingPartitions) > 0){
    queryColnames <- colnames(DT_cast)[2:ncol(DT_cast)]
    DTnewList <- c(
      list(partition = missingPartitions),
      lapply(setNames(queryColnames, queryColnames), function(Q) rep(0, times = length(missingPartitions)))
    )
    DTnomatch <- data.table(data.frame(DTnewList))
    DT_cast <- rbindlist(list(DT_cast, DTnomatch))
  }
  
  # add columns for quits without hits (all 0)
  missingQueries <- query[!query %in% colnames(DT_cast)[2:ncol(DT_cast)]]
  if (length(missingQueries) > 0){
    for (q in missingQueries){
      DT_cast[, eval(q) := rep(0, times = nrow(DT_cast)), with = TRUE]
    }
  }
  
  if (total){
    .message("adding total number of hits (col 'TOTAL', verbose = verbose)")
    DT_cast[, "TOTAL" := rowSums(DT_cast[, 2:ncol(DT_cast), with = FALSE]), with = TRUE]
  }
  DT_cast
})

#' @rdname count-method
setMethod("count", "character", function(.Object, query = NULL, cqp = is.cqp, pAttribute = getOption("polmineR.pAttribute"), breakdown = FALSE, sort = FALSE, decode = TRUE, verbose = TRUE){
  stopifnot(.Object %in% CQI$list_corpora())
  if (is.null(query)){
    if (length(pAttribute) == 1){
      TF <- data.table(count = RcppCWB::get_count_vector(corpus = .Object, p_attribute = pAttribute))
      TF[, "id" := 0:(nrow(TF) - 1), with = TRUE]
      setnames(TF, old = "id", new = paste(pAttribute, "id", sep = "_"))
      if (decode == FALSE){
        setkeyv(TF, paste(pAttribute, "id", sep = "_"))
        setcolorder(TF, c(paste(pAttribute, "id", sep = "_"), "count"))
      } else {
        TF[, "token" := as.nativeEnc(CQI$id2str(.Object, pAttribute, 0:(nrow(TF) - 1)), from = getEncoding(.Object)), with = TRUE]
        Encoding(TF[["token"]]) <- "unknown"
        setnames(TF, old = "token", new = pAttribute)
        setkeyv(TF, pAttribute)
        setcolorder(TF, c(pAttribute, paste(pAttribute, "id", sep = "_"), "count"))
        if (sort) setorderv(TF, cols = pAttribute)
      }
      return(TF)
    } else {
      
      tokenStreamDT <- as.data.table(
        li <- lapply(
          setNames(pAttribute, paste(pAttribute, "id", sep = "_")),
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
        for (pAttr in pAttribute){
          .message("decode p-attribute: ", pAttr, verbose = verbose)
          TF[, eval(pAttr) := as.nativeEnc(CQI$id2str(.Object, pAttr, TF[[paste(pAttr, "id", sep = "_")]]), from = getEncoding(.Object)), with = TRUE]
        }
        setcolorder(TF, c(pAttribute, paste(pAttribute, "id", sep = "_"), "count"))
      }
      return(TF)
    }
  } else {
    stopifnot(.Object %in% CQI$list_corpora())
    total <- CQI$attribute_size(.Object, pAttribute, type = "p")
    if (class(cqp) == "function") cqp <- cqp(query)
    if (length(cqp) > 1) stop("length of cqp is larger than 1, it needs to be 1")
    if (cqp == FALSE){
      query <- as.corpusEnc(query, corpusEnc = getEncoding(.Object))
      count <- sapply(
        query,
        function(query){
          query_id <- CQI$str2id(.Object, pAttribute, query)
          # if there is no id for query, query_id will be -5
          if (query_id >= 0) CQI$id2freq(.Object, pAttribute, query_id) else 0
        }
      )
      return(data.table(query = query, count = count, freq = count / total))
    } else if (cqp == TRUE){
      if (breakdown == FALSE){
        count <- sapply(
          query,
          function(query){
            cpos_matrix <- cpos(.Object, query, cqp = cqp, pAttribute = pAttribute, encoding = getEncoding(.Object))
            if (!is.null(cpos_matrix)){
              return( nrow(cpos_matrix) )
            } else {
              return( 0 )
            }
            
          })
        return( data.table(query = query, count = count, freq = count / total) )
      } else {
        C <- Corpus$new(.Object)
        C$pAttribute <- pAttribute
        retval <- count(.Object = C$as.partition(), query = query, cqp = cqp, pAttribute = pAttribute, breakdown = TRUE)
        return( retval )
      }
    }
  }
})

#' @rdname context-class
setMethod("count", "context", function(.Object) {
  .Object@count
})

#' @rdname count-method
setMethod("count", "vector", function(.Object, corpus, pAttribute){
  ids <- CQI$cpos2id(corpus, pAttribute, .Object)
  count <- tabulate(ids)
  TF <- data.table(
    id = 0:length(count),
    count = c(length(which(ids == 0)), count)
  )
  setkey(TF, "id")
  setnames(TF, "id", paste(pAttribute, "id", sep = "_"))
  TF[count > 0]
})

#' @rdname count-method
setMethod("count", "Corpus", function(.Object, query = NULL, pAttribute){
  count(.Object$as.partition(), query = query, pAttribute = pAttribute)
})
