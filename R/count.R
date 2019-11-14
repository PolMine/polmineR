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
#' @param check A \code{logical} value, whether to check validity of CQP query
#'   using \code{check_cqp_query}.
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
#' @param ... Further arguments. If \code{.Object} is a \code{remote_corpus}
#'   object, the three dots (\code{...}) are used to pass arguments. Hence, it is
#'   necessary to state the names of all arguments to be passed explicity.
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
  .Object, query = NULL, cqp = is.cqp, check = TRUE, breakdown = FALSE,
  decode = TRUE, p_attribute = getOption("polmineR.p_attribute"),
  mc = getOption("polmineR.cores"), verbose = TRUE, progress = FALSE,
  ...
  )
  callNextMethod()
)


#' @rdname count-method
setMethod("count", "subcorpus", function(
  .Object, query = NULL, cqp = is.cqp, check = TRUE, breakdown = FALSE,
  decode = TRUE, p_attribute = getOption("polmineR.p_attribute"),
  mc = getOption("polmineR.cores"), verbose = TRUE, progress = FALSE,
  ...
)
  callNextMethod()
)



#' @rdname count-method
setMethod("count", "slice", function(
  .Object, query = NULL, cqp = is.cqp, check = TRUE, breakdown = FALSE,
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
          cposHits <- cpos(.Object = .Object, query = x, cqp = cqp, check = check, p_attribute = p_attribute)
          if (is.null(cposHits)) return( NULL )
          hitsString <- apply(
            cposHits, 1,
            function(x)
              paste(
                cl_cpos2str(corpus = .Object@corpus, p_attribute = p_attribute, cpos = x[1]:x[2], registry = registry()),
                collapse = ' '
              )
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
      .fn <- function(query, obj, cqp, p_attribute, ...) {
        .message("processing query", query, verbose = verbose)
        cposResult <- cpos(.Object = obj, query = query, cqp = cqp, check = check, p_attribute = p_attribute, verbose = FALSE)
        if (is.null(cposResult)) return( 0 ) else return( nrow(cposResult) )
      }
      no <- as.integer(unlist(blapply(
        as.list(query),
        f = .fn,
        obj = .Object, cqp = cqp, p_attribute = p_attribute,
        mc = mc, verbose = verbose, progress = progress
      )))
      return( data.table(query = query, count = no, freq = no/.Object@size) )
    }
  } else {
    pAttr_id <- paste(p_attribute, "id", sep = "_")
    if (length(p_attribute) == 1L){
      countMatrix <- RcppCWB::region_matrix_to_count_matrix(
        corpus = .Object@corpus, p_attribute = p_attribute,
        matrix = .Object@cpos
      )
      TF <- data.table::as.data.table(countMatrix)
      setnames(TF, old = c("V1", "V2"), new = c(pAttr_id, "count"))
    } else {
      cpos <- unlist(apply(.Object@cpos, 1, function(x) x[1]:x[2]))
      idList <- lapply(p_attribute, function(p) cl_cpos2id(corpus = .Object@corpus, p_attribute = p, cpos = cpos, registry = registry()))
      names(idList) <- paste(p_attribute, "id", sep = "_")
      ID <- data.table::as.data.table(idList)
      setkeyv(ID, cols = names(idList))
      TF <- ID[, .N, by = c(eval(names(idList))), with = TRUE]
      setnames(TF, "N", "count")
    }
    if (decode){
      dummy <- lapply(
        1L:length(p_attribute),
        function(i){
          str <- cl_id2str(corpus = .Object@corpus, p_attribute = p_attribute[i], id = TF[[pAttr_id[i]]], registry = registry())
          TF[, eval(p_attribute[i]) := as.nativeEnc(str, from = .Object@encoding) , with = TRUE] 
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
    DT_cast <- dcast.data.table(DT, partition ~ query, value.var = "count", fill = 0)
    
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
    
    if (total) DT_cast[, "TOTAL" := rowSums(DT_cast[, 2L:ncol(DT_cast), with = FALSE]), with = TRUE]
    return(DT_cast)
  } else {
    
    corpus <- get_corpus(.Object)
    if (length(corpus) > 1L) stop("partitions in partition_bundle must be derived from the same corpus")
    
    if (verbose) message("... creating data.table with corpus positions")
    cpos_dt <- data.table(do.call(rbind, lapply(.Object@objects, slot, name = "cpos")))
    cpos_dt[, "name" := do.call(
      c,
      lapply(seq_along(.Object@objects), function(i) rep(x = names(.Object@objects)[[i]], times = nrow(.Object@objects[[i]]@cpos)))
      )]
    DT <- cpos_dt[, {do.call(c, lapply(1L:nrow(.SD), function(i) .SD[[1]][i]:.SD[[2]][i]))}, by = "name"]
    setnames(DT, old = "V1", new = "cpos")
    rm(cpos_dt)
    
    if (verbose) message(sprintf("... adding ids for p-attribute '%s'", p_attribute))
    DT[, "id" :=  cl_cpos2id(corpus = corpus, p_attribute = p_attribute, cpos = DT[["cpos"]], registry = registry())]
    if (verbose) message("... performing count")
    CNT <- DT[,.N, by = c("name", "id")]
    rm(DT)
    setnames(CNT, old = "N", new = "count")
    
    if (verbose) message("... adding decoded p-attribute")
    str_raw <- cl_id2str(corpus = corpus, p_attribute = p_attribute, id = CNT[["id"]], registry = registry())
    enc <- .Object@objects[[1]]@encoding
    CNT[, eval(p_attribute) := if (localeToCharset()[1] == enc) str_raw else as.nativeEnc(str_raw, from = enc)]
    rm(str_raw)
    
    if (verbose) message("... creating bundle of count objects")
    CNT_list <- split(CNT, by = "name")
    rm(CNT)
    y <- new(Class = "count_bundle", p_attribute = p_attribute, corpus = corpus, encoding = enc)
    .fn <- function(i){
      new(
        "count",
        corpus = corpus,
        encoding = .Object@objects[[i]]@encoding,
        p_attribute = p_attribute,
        stat = CNT_list[[i]],
        name = .Object@objects[[ CNT_list[[i]][["name"]][[1]] ]]@name,
        size = size(.Object@objects[[i]])
      )
    }
    y@objects <- if (progress) pblapply(seq_along(CNT_list), .fn) else lapply(seq_along(CNT_list), .fn)
    names(y@objects) <- names(.Object)
    return( y )
  }
})

#' @rdname count-method
setMethod("count", "corpus", function(.Object, query = NULL, cqp = is.cqp, check = TRUE, p_attribute = getOption("polmineR.p_attribute"), breakdown = FALSE, sort = FALSE, decode = TRUE, verbose = TRUE, ...){

  if ("pAttribute" %in% names(list(...))) p_attribute <- list(...)[["pAttribute"]]
  
  if (is.null(query)){
    if (length(p_attribute) == 1L){
      cnt_file <- file.path(.Object@data_dir, sprintf("%s.corpus.cnt", p_attribute)
      )
      if (file.exists(cnt_file)){
        cnt <- readBin(con = cnt_file, what = integer(), size = 4L, n = file.info(cnt_file)$size, endian = "big")
        TF <- data.table(count = cnt)
      } else {
        TF <- data.table(
          count = RcppCWB::get_count_vector(corpus = .Object@corpus, p_attribute = p_attribute)
          )
      }
      p_attr_col_id <- paste(p_attribute, "id", sep = "_")
      TF[, (p_attr_col_id) := 0L:(nrow(TF) - 1L)]

      if (!decode){
        setkeyv(TF, p_attr_col_id)
        setcolorder(TF, c(p_attr_col_id, "count"))
      } else {
        # somewhat surprisingly, cl_id2str a good deal faster than reading the
        # lexicon file directly using readBin a follows:
        # 
        # lexicon_file <- file.path(.Object@data_dir, paste(p_attribute, "lexicon", sep = "."))
        # lexicon_file_size <- file.info(lexicon_file)[["size"]]
        # tokens <- readBin(con = lexicon_file, what = character(), n = lexicon_file_size)
        # 
        # Using a Rcpp implementation for reading in the lexion file might be a
        # fast solution, see https://gist.github.com/hadley/6353939
        tokens <- cl_id2str(
          corpus = .Object@corpus,
          p_attribute = p_attribute,
          id = TF[[p_attr_col_id]],
          registry = registry()
        )
        TF[, "token" := as.nativeEnc(tokens, from = encoding(.Object)), with = TRUE] # recode is slow
        Encoding(TF[["token"]]) <- "unknown"
        setnames(TF, old = "token", new = p_attribute)
        setkeyv(TF, p_attribute) # slow
        setcolorder(TF, c(p_attribute, p_attr_col_id, "count"))
        if (sort) setorderv(TF, cols = p_attribute)
      }
    } else {
      
      tokenStreamDT <- data.table::as.data.table(
        li <- lapply(
          setNames(p_attribute, paste(p_attribute, "id", sep = "_")),
          function(p_attr){
            .message("getting token stream for p-attribute: ", p_attr, verbose = verbose)
            cl_cpos2id(corpus = .Object@corpus, p_attribute = p_attr, 0L:(size(.Object) - 1L), registry = registry())
          }
        )
      )
      .message("counting", verbose = verbose)
      TF <- tokenStreamDT[, .N, by = c(eval(colnames(tokenStreamDT)))]
      setnames(TF, old = "N", new = "count")
      if (decode){
        for (pAttr in p_attribute){
          .message("decode p-attribute: ", pAttr, verbose = verbose)
          TF[, eval(pAttr) := as.nativeEnc(cl_id2str(corpus = .Object@corpus, p_attribute = pAttr, id = TF[[paste(pAttr, "id", sep = "_")]], registry = registry()), from = encoding(.Object)), with = TRUE]
        }
        setcolorder(TF, c(p_attribute, paste(p_attribute, "id", sep = "_"), "count"))
      }
    }
    y <- new(
      Class = "count",
      corpus = .Object@corpus,
      p_attribute = p_attribute,
      encoding = encoding(.Object),
      stat = TF,
      name = character(),
      size = size(.Object)
    )
    return(y)
  } else {
    total <- cl_attribute_size(corpus = .Object@corpus, attribute = p_attribute, attribute_type = "p", registry = registry())
    if (class(cqp) == "function") cqp <- cqp(query)
    if (length(cqp) > 1) stop("length of cqp is larger than 1, it needs to be 1")
    if (cqp == FALSE){
      query <- as.corpusEnc(query, corpusEnc = encoding(.Object))
      count <- sapply(
        query,
        function(query){
          query_id <- cl_str2id(corpus = .Object@corpus, p_attribute = p_attribute, str = query, registry = registry())
          # if there is no id for query, query_id will be -5
          if (query_id >= 0) cl_id2freq(corpus = .Object@corpus, p_attribute = p_attribute, id = query_id, registry = registry()) else 0
        }
      )
      return(data.table(query = query, count = count, freq = count / total))
    } else if (cqp){
      if (!breakdown){
        count <- sapply(
          query,
          function(query){
            cpos_matrix <- cpos(.Object@corpus, query, cqp = cqp, check = check, p_attribute = p_attribute)
            if (!is.null(cpos_matrix)){
              return( nrow(cpos_matrix) )
            } else {
              return( 0 )
            }
            
          })
        return( data.table(query = query, count = count, freq = count / total) )
      } else {
        C <- Corpus$new(.Object@corpus)
        C$p_attribute <- p_attribute
        retval <- count(.Object = C$as.partition(), query = query, cqp = cqp, p_attribute = p_attribute, breakdown = TRUE)
        return( retval )
      }
    }
  }
})


#' @rdname count-method
setMethod("count", "character", function(.Object, query = NULL, cqp = is.cqp, check = TRUE, p_attribute = getOption("polmineR.p_attribute"), breakdown = FALSE, sort = FALSE, decode = TRUE, verbose = TRUE, ...){
  count(
    .Object = corpus(.Object),
    query = query,
    cqp = cqp,
    check = check,
    p_attribute = p_attribute,
    breakdown = breakdown,
    sort = sort,
    decode = decode,
    verbose = verbose,
    ...
  )
})


#' @rdname context-class
setMethod("count", "context", function(.Object) .Object@count )

#' @rdname count-method
setMethod("count", "vector", function(.Object, corpus, p_attribute, ...){
  
  if ("pAttribute" %in% names(list(...))) p_attribute <- list(...)[["pAttribute"]]

  ids <- cl_cpos2id(corpus = corpus, p_attribute = p_attribute, cpos = .Object, registry = registry())
  count <- tabulate(ids)
  TF <- data.table(
    id = 0L:length(count),
    count = c(length(which(ids == 0)), count)
  )
  setkeyv(TF, cols = "id")
  setnames(TF, "id", paste(p_attribute, "id", sep = "_"))
  TF[count > 0]
})


#' @rdname kwic-class
#' @exportMethod count
#' @details Applying the \code{count}-method on a \code{kwic} object will return
#'   a \code{count} object with the evaluation of the left and right context of
#'   the match.
#' @examples
#' # use count-method on kwic object
#' coi <- kwic("REUTERS", query = "oil") %>%
#'   count(p_attribute = "word")
#' 
#' # features vs cooccurrences-method (identical results)
#' ref <- count("REUTERS", p_attribute = "word") %>%
#'   subset(word != "oil")
#' slot(ref, "size") <- slot(ref, "size") - count("REUTERS", "oil")[["count"]]
#' y_features <- features(coi, ref, method = "ll", included = TRUE)
#' y_cooc <- cooccurrences("REUTERS", query = "oil")
#' 
setMethod("count", "kwic", function(.Object, p_attribute = "word"){
  cnt <- .Object@cpos[.Object@cpos[["direction"]] != 0][, .N, by = p_attribute]
  setnames(cnt, old = "N", new = "count")
  new(
    "count",
    stat = cnt,
    corpus = .Object@corpus,
    encoding = .Object@encoding,
    p_attribute = p_attribute,
    name = .Object@name,
    size = sum(cnt[["count"]])
  )
})

#' @exportMethod hist
#' @rdname count_class
setMethod("hist", "count", function(x, ...) hist(x@stat[,"count"], ...) )


#' @rdname count-method
setMethod("count", "remote_corpus", function(.Object, ...){
  ocpu_exec(
    fn = "count",
    server = .Object@server,
    user = .Object@user,
    password = .Object@password,
    do.call = FALSE,
    .Object = .Object@corpus,
    ...
  )
})


#' @rdname count-method
setMethod("count", "remote_subcorpus", function(.Object, ...){
  ocpu_exec(fn = "count", server = .Object@server, do.call = FALSE, .Object = as(.Object, "subcorpus"), ...)
})

