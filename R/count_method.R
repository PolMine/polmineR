#' @include dispersion_class.R
NULL

#' Get counts.
#' 
#' Count all tokens, or number of occurrences of a query (CQP syntax may be used).
#' 
#' If .Object is a partitonBundle, the data.table returned will have the queries
#' in the columns, and as many rows as there are in the partitionBundle.
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
#' @param id2str logical, whether to add rownames (only if query is NULL)
#' @param sort logical, whether to sort stat
#' @param mc logical, whether to use multicore (defaults to FALSE)
#' @param verbose logical, whether to be verbose
#' @param freq logical, if FALSE, counts will be reported, if TRUE, frequencies
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
#' @examples
#' \dontrun{
#'   use("polmineR.sampleCorpus")
#'   debates <- partition("PLPRBTTXT", list(text_id=".*"), regex=TRUE)
#'   x <- count(debates, "Arbeit") # get frequencies for one token
#'   x <- count(debates, c("Arbeit", "Freizeit", "Zukunft")) # get frequencies for multiple tokens
#'   x <- count("PLPRBTTXT", query = c("Migration", "Integration"), "word")
#' 
#'   debates <- partitionBundle(
#'     "PLPRBTTXT", sAttribute = "text_date", values = NULL,
#'     regex = TRUE, mc = FALSE, verbose = FALSE
#'   )
#' }
setGeneric("count", function(.Object, ...){standardGeneric("count")})

#' @rdname count-method
setMethod("count", "partition", function(
  .Object, query = NULL, cqp = is.cqp, 
  id2str = TRUE, pAttribute = getOption("polmineR.pAttribute"),
  mc = getOption("polmineR.cores"), verbose = TRUE, progress = FALSE
  ){
  if (!is.null(query)){
    if (progress) verbose <- FALSE
    .getNumberOfHits <- function(query, partition, cqp, pAttribute, ...) {
      if (verbose) message("... processing query ", query)
      cposResult <- cpos(.Object = .Object, query = query, cqp = cqp, pAttribute = pAttribute, verbose = FALSE)
      ifelse(is.null(cposResult), 0, nrow(cposResult))
    }
    no <- as.integer(blapply(
      as.list(query),
      f = .getNumberOfHits,
      partition = .Object, cqp = cqp, pAttribute = pAttribute,
      mc = mc, verbose = verbose, progress = progress
    ))
    data.table(query = query, count = no, freq = no/.Object@size)
  } else {
    pAttr_id <- paste(pAttribute, "id", sep = "_")
    if (length(pAttribute) == 1){
      if (requireNamespace("polmineR.Rcpp", quietly = TRUE) && (getOption("polmineR.Rcpp") == TRUE)){
        countMatrix <- polmineR.Rcpp::regionsToCountMatrix(
          corpus = .Object@corpus, pAttribute = pAttribute,
          matrix = .Object@cpos
          )
        TF <- as.data.table(countMatrix)
        setnames(TF, old = c("V1", "V2"), new = c(pAttr_id, "count"))
      } else {
        cpos <- unlist(apply(.Object@cpos, 1, function(x) x[1]:x[2]))
        TF <- count(cpos, .Object@corpus, pAttribute)
      }
    } else {
      cpos <- unlist(apply(.Object@cpos, 1, function(x) x[1]:x[2]))
      idList <- lapply(pAttribute, function(p) CQI$cpos2id(.Object@corpus, p, cpos))
      names(idList) <- paste(pAttribute, "id", sep = "_")
      ID <- as.data.table(idList)
      setkeyv(ID, cols = names(idList))
      TF <- ID[, .N, by = c(eval(names(idList))), with = TRUE]
      setnames(TF, "V1", "count")
    }
    if (id2str){
      dummy <- lapply(
        c(1:length(pAttribute)),
        function(i){
          str <- as.utf8(CQI$id2str(.Object@corpus, pAttribute[i], TF[[pAttr_id[i]]]), from = .Object@encoding)
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
setMethod("count", "partitionBundle", function(.Object, query, pAttribute = NULL, freq = FALSE, total = TRUE, mc = FALSE, progress = TRUE, verbose = FALSE){
  if (verbose) message("... getting hits for query/queries")
  DT <- hits(.Object, query = query, pAttribute = pAttribute, mc = mc, progress = progress, verbose = verbose)@dt
  if (verbose) message("... rearranging table")
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
    if (verbose) message("... adding total number of hits (col 'TOTAL')")
    DT_cast[, "TOTAL" := rowSums(DT_cast[, 2:ncol(DT_cast), with = FALSE]), with = TRUE]
  }
  DT_cast
})

#' @rdname count-method
setMethod("count", "character", function(.Object, query = NULL, cqp = is.cqp, pAttribute = getOption("polmineR.pAttribute"), sort = FALSE, id2str = TRUE, verbose = TRUE){
  if (is.null(query)){
    if (requireNamespace("polmineR.Rcpp", quietly = TRUE) && getOption("polmineR.Rcpp") == TRUE){
      if (verbose) message("... using polmineR.Rcpp for counting")
      TF <- data.table(count = polmineR.Rcpp::getCountVector(corpus = .Object, pAttribute = pAttribute))
      TF[, "id" := 0:(nrow(TF) - 1), with = TRUE]
      setnames(TF, old = "id", new = paste(pAttribute, "id", sep = "_"))
      if (id2str == FALSE){
        setkeyv(TF, paste(pAttribute, "id", sep = "_"))
        setcolorder(TF, c(paste(pAttribute, "id", sep = "_"), "count"))
      } else {
        TF[, "token" := as.utf8(CQI$id2str(.Object, pAttribute, 0:(nrow(TF) - 1))), with = TRUE]
        Encoding(TF[["token"]]) <- "unknown"
        setnames(TF, old = "token", new = pAttribute)
        setkeyv(TF, pAttribute)
        setcolorder(TF, c(pAttribute, paste(pAttribute, "id", sep = "_"), "count"))
        if (sort) setorderv(TF, cols = pAttribute)
      }
      return(TF)
    } else if (getOption("polmineR.cwb-lexdecode")){
      # cwb-lexdecode will be significantly faster than using rcqp
      if (verbose) message("... using cwb-lexdecode for counting")
      cmd <- paste(c("cwb-lexdecode", "-f", "-n", "-P", pAttribute, .Object), collapse = " ")
      lexdecodeResult <- system(cmd, intern = TRUE)
      Encoding(lexdecodeResult) <- getEncoding(.Object)
      lexdecodeList <- strsplit(lexdecodeResult, "\t")
      TF <- data.table(
        token = sapply(lexdecodeList, function(x) x[3]),
        id = sapply(lexdecodeList, function(x) x[1]),
        count = as.integer(sapply(lexdecodeList, function(x) x[2]))
      )
      Encoding(TF[["token"]]) <- "unknown"
      colnames(TF) <- c(pAttribute, paste(pAttribute, "id", sep = "_"), "count")
      setkeyv(TF, pAttribute)
      if (sort) setorderv(TF, cols = pAttribute)
      return(TF)
    } else {
      TF <- count(0:(size(.Object) - 1), .Object, pAttribute = pAttribute)
      if (id2str){
        TF[, "token" := CQI$id2str(.Object, pAttribute, TF[[paste(pAttribute, "id", sep = "_")]]), with = TRUE]
        setnames(TF, old = "token", new = pAttribute)
        setcolorder(TF, c(pAttribute, paste(pAttribute, "id", sep = "_"), "count"))
      }
    }
  } else {
    stopifnot(.Object %in% CQI$list_corpora())
    total <- CQI$attribute_size(.Object, pAttribute, type = "p")
    if (class(cqp) == "function") cqp <- cqp(query)
    if (cqp == FALSE){
      count <- sapply(
        query,
        function(query)
          CQI$id2freq(
            .Object,
            pAttribute,
            CQI$str2id(.Object, pAttribute, query)
          )
      )
      freq <- count/total
      return(data.table(query = query, count = count, freq = freq))
    } else if (cqp == TRUE){
    count <- sapply(
      query,
      function(query){
        cpos_matrix <- cpos(.Object, query, cqp = cqp, pAttribute = pAttribute, encoding = getEncoding(.Object))
        if (!is.null(cpos_matrix)){
          return( nrow(cpos_matrix) )
        } else {
          return( 0 )
        }
        
      }
      )
    freq <- count/total
    return(data.table(query = query, count = count, freq = freq))
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
    id = c(0:length(count)),
    count = c(length(which(ids == 0)), count)
  )
  setkey(TF, "id")
  setnames(TF, "id", paste(pAttribute, "id", sep = "_"))
  TF[count > 0]
})


#' @rdname dispersion-class
setMethod("count", "dispersion", function(.Object) .Object@count)
