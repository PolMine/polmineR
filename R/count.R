#' @include polmineR.R textstat.R partition.R context.R partition_bundle.R corpus.R S4classes.R
NULL



#' Get counts.
#' 
#' Count all tokens, or number of occurrences of a query (CQP syntax may be
#' used), or matches for the query.
#'
#' If \code{.Object} is a \code{partiton_bundle}, the \code{data.table} returned will
#' have the queries in the columns, and as many rows as there are in the
#' \code{partition_bundle}.
#'
#' If \code{.Object} is a length-one \code{character} vector and \code{query} is
#' \code{NULL}, the count is performed for the whole partition.
#'
#' If \code{breakdown} is \code{TRUE} and one query is supplied, the function
#' returns a frequency breakdown of the results of the query. If several queries
#' are supplied, frequencies for the individual queries are retrieved.
#' 
#' Multiple queries can be used for argument \code{query}. Some care may be
#' necessary when summing up the counts for the individual queries. When the
#' CQP syntax is used, different queries may yield the same match result, so that
#' the sum of all individual query matches may overestimate the true number of
#' unique matches. In the case of overlapping matches, a warning message is
#' issued. Collapsing multiple CQP queries into a single query (separating the
#' individual queries by "|" and wrapping everything in round brackets) solves
#' this problem.
#' 
#' @seealso  For a metadata-based breakdown of counts (i.e. tabulation by
#'   s-attributes), see \code{\link{dispersion}}. The \code{\link{hits}} is the
#'   worker behind the \code{dispersion} method and offers a similar, yet more
#'   low-level functionality as compared to the \code{count} method. Using the
#'   \code{\link{hits}} method may be useful to obtain the data required for
#'   flexible cross-tabulations.
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
#' @param phrases A \code{phrases} object. If provided, the denoted regions will
#'   be concatenated as phrases.
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
#' @references 
#' Baker, Paul (2006): \emph{Using Corpora in Discourse Analysis}. London: continuum, p. 47-69 (ch. 3).
#' @examples
#' use("polmineR")
#' use(pkg = "RcppCWB", corpus = "REUTERS")
#' 
#' debates <- partition("GERMAPARLMINI", date = ".*", regex=TRUE)
#' count(debates, query = "Arbeit") # get frequencies for one token
#' count(debates, c("Arbeit", "Freizeit", "Zukunft")) # get frequencies for multiple tokens
#'   
#' count("GERMAPARLMINI", query = c("Migration", "Integration"), p_attribute = "word")
#' 
#' debates <- partition_bundle(
#'   "GERMAPARLMINI", s_attribute = "date", values = NULL,
#'   mc = FALSE, verbose = FALSE
#' )
#' y <- count(debates, query = "Arbeit", p_attribute = "word")
#' y <- count(debates, query = c("Arbeit", "Migration", "Zukunft"), p_attribute = "word")
#'   
#' count("GERMAPARLMINI", '"Integration.*"', breakdown = TRUE)
#' 
#' P <- partition("GERMAPARLMINI", date = "2009-11-11")
#' count(P, '"Integration.*"', breakdown = TRUE)
#' 
#' sc <- corpus("GERMAPARLMINI") %>% subset(party == "SPD")
#' phr <- cpos(sc, query = '"Deutsche.*" "Bundestag.*"', cqp = TRUE) %>%
#'   as.phrases(corpus = "GERMAPARLMINI", enc = "latin1")
#' cnt <- count(sc, phrases = phr, p_attribute = "word")
#' 
#' # Multiple queries and overlapping query matches. The first count 
#' # operation will issue a warning that matches overlap, see the second 
#' # example for a solution.
#' corpus("REUTERS") %>%
#'   count(query = c('".*oil"', '"turmoil"'), cqp = TRUE)
#' corpus("REUTERS") %>% 
#'   count(query = '"(.*oil|turmoil)"', cqp =TRUE)
setGeneric("count", function(.Object, ...){standardGeneric("count")})

#' @rdname count-method
setMethod("count", "partition", function(.Object, query = NULL, cqp = is.cqp, check = TRUE, breakdown = FALSE,
  decode = TRUE, p_attribute = getOption("polmineR.p_attribute"),
  mc = getOption("polmineR.cores"), verbose = TRUE, progress = FALSE,
  phrases = NULL,
  ...
  )
  count(
    .Object = as(.Object, "subcorpus"), query = query, cqp = cqp, check = check, breakdown = breakdown,
    decode = decode, p_attribute = p_attribute,
    mc = mc, verbose = verbose, progress = progress,
    ...
    
  )
)


#' @rdname count-method
setMethod("count", "subcorpus", function(
  .Object, query = NULL, cqp = is.cqp, check = TRUE, breakdown = FALSE,
  decode = TRUE, p_attribute = getOption("polmineR.p_attribute"),
  mc = getOption("polmineR.cores"), verbose = TRUE, progress = FALSE,
  phrases = NULL,
  ...
){
  if ("pAttribute" %in% names(list(...))) p_attribute <- list(...)[["pAttribute"]]
  
  stopifnot(isTRUE(is.logical(breakdown)))
  
  if (!is.null(query)){
    if (progress) verbose <- FALSE
    if (is.function(cqp)) cqp <- cqp(query)
    if (length(cqp) > 1L) stop("length of cqp is larger than 1, it needs to be 1")
    if (isTRUE(cqp)){
      if (breakdown){
        dts <- lapply(
          query,
          function(x){
            region_matrix <- cpos(.Object = .Object, query = x, cqp = cqp, check = check, p_attribute = p_attribute)
            if (is.null(region_matrix)) return(NULL)
            token <- get_token_stream(cpos(region_matrix), corpus = .Object@corpus, p_attribute = p_attribute, encoding = .Object@encoding)
            ids <- unlist(Map(rep, 1:nrow(region_matrix), region_matrix[,2] - region_matrix[,1] + 1))
            matches_cnt <- table(sapply(split(token, ids), paste, collapse = " "))
            dt <- data.table(query = x, match = names(matches_cnt), count = as.vector(unname(matches_cnt)))
            if (nrow(dt) == 0L) return(NULL)
            setorderv(dt, cols = "count", order = -1L)
            dt[, "share" := round(dt[["count"]] / sum(dt[["count"]]) * 100, 2)]
            dt
          }
        )
        return( rbindlist(dts) )
      } else if (isFALSE(breakdown)){
        callNextMethod()
      }
    } else if (isFALSE(cqp)){
      count_vec <- sapply(
        query,
        function(x){
          region_matrix <- cpos(.Object = .Object, query = x, cqp = cqp, check = check, p_attribute = p_attribute)
          if (is.null(region_matrix)) 0L else nrow(region_matrix)
        }
      )
      dt <- data.table(query = query, match = query, count = count_vec, freq = count_vec / size(.Object))
      return( dt )
    }
  } else {
    p_attr_id <- paste(p_attribute, "id", sep = "_")
    if (length(p_attribute) == 1L){
      if (is.null(phrases)){
        count_matrix <- RcppCWB::region_matrix_to_count_matrix(
          corpus = .Object@corpus, registry = .Object@registry_dir,
          p_attribute = p_attribute, matrix = .Object@cpos
        )
        TF <- data.table::as.data.table(count_matrix)
        setnames(TF, old = c("V1", "V2"), new = c(p_attr_id, "count"))
      } else {
        token_table <- decode(.Object, p_attributes = p_attribute, s_attributes = character(), verbose = FALSE)
        token_table_min <- concatenate_phrases(token_table, phrases = phrases, col = p_attribute)
        TF <- token_table_min[, .N, by = p_attribute]
        setnames(TF, old = "N", new = "count")
        decode <- FALSE
      }
    } else {
      cpos <- cpos(.Object@cpos)
      id_list <- lapply(
        p_attribute,
        function(p) cpos2id(x = .Object, p_attribute = p, cpos = cpos)
      )
      names(id_list) <- paste(p_attribute, "id", sep = "_")
      ID <- data.table::as.data.table(id_list)
      setkeyv(ID, cols = names(id_list))
      TF <- ID[, .N, by = c(eval(names(id_list))), with = TRUE]
      setnames(TF, "N", "count")
    }
    if (decode){
      dummy <- lapply(
        seq_along(p_attribute),
        function(i){
          str <- cl_id2str(
            corpus = .Object@corpus, registry = .Object@registry_dir,
            p_attribute = p_attribute[i], id = TF[[p_attr_id[i]]]
          )
          TF[, eval(p_attribute[i]) := as.nativeEnc(str, from = .Object@encoding) , with = TRUE] 
        })
      setcolorder(TF, neworder = c(p_attribute, p_attr_id, "count"))
    } else {
      # Reordering is conditional because if phrases have been generated, not all colnames
      # are necessarily present
      if (all(p_attr_id %in% colnames(TF))) setcolorder(TF, neworder = c(p_attr_id, "count"))
    }
    y <- new(
      Class = "count",
      corpus = .Object@corpus,
      registry_dir = .Object@registry_dir,
      data_dir = .Object@data_dir,
      info_file = .Object@info_file,
      template = .Object@template,
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
setMethod("count", "partition_bundle", function(.Object, query = NULL, cqp = FALSE, p_attribute = NULL, phrases = NULL, freq = FALSE, total = TRUE, mc = FALSE, progress = FALSE, verbose = FALSE, ...){
  
  if ("pAttribute" %in% names(list(...))) p_attribute <- list(...)[["pAttribute"]]

  if (!is.null(query)){
    .message("getting hits for query/queries", verbose = verbose)
    DT <- hits(.Object, query = query, cqp = cqp, p_attribute = p_attribute, mc = mc, progress = progress, verbose = verbose)@stat
    .message("rearranging table", verbose = verbose)
    if (nrow(DT) > 0L){
      DT_cast <- dcast.data.table(DT, partition ~ query, value.var = "count", fill = 0)
    } else {
      # If there are no matches, we generate a data.table directly that will be 
      # filled later on.
      DT_cast <- data.table(partition = names(.Object))
    }
    
    
    # remove counts that are not in one of the partitions
    no_partition <- which(is.na(DT_cast[["partition"]]) == TRUE)
    if (length(no_partition) > 0L) DT_cast <- DT_cast[-no_partition]
    
    # add rows for partitions withous hits (all 0)
    missing_partitions <- names(.Object)[which(!names(.Object) %in% DT_cast[[1]])]
    if (length(missing_partitions) > 0L){
      queryColnames <- colnames(DT_cast)[2L:ncol(DT_cast)]
      DTnewList <- c(
        list(partition = missing_partitions),
        lapply(
          setNames(queryColnames, queryColnames),
          function(Q) rep(0L, times = length(missing_partitions))
        )
      )
      DTnomatch <- as.data.table(DTnewList)
      DT_cast <- rbindlist(list(DT_cast, DTnomatch))
    }
    
    # add columns for quits without hits (all 0)
    missing_queries <- query[!query %in% colnames(DT_cast)[2L:ncol(DT_cast)]]
    if (length(missing_queries) > 0L){
      # It is not an unlikely scenario that there are no matches for many queries. But
      # data.table objects envisage less than 10000 columns to be added in a bulk
      # action. Therefore we check whether there 10000 or more columns to be 
      # added. If yes, we break up the job into chunks.
      if (length(missing_queries < 10000L)){
        DT_cast[, (missing_queries) := 0L]
      } else {
        qlist <- split(
          missing_queries,
          ceiling(1L:length(missing_queries) / 9999L)
        )
        lapply(qlist, function(queries) DT_cast[, (queries) := 0L])
      }
    }
    
    if (total) DT_cast[, "TOTAL" := rowSums(DT_cast[, 2L:ncol(DT_cast), with = FALSE]), with = TRUE]
    return(DT_cast)
  } else {
    
    enc <- encoding(.Object)
    corpus <- get_corpus(.Object)
    if (length(corpus) > 1L) stop("The objects in the bundle must be derived from the same corpus.")
    
    if (verbose) message("... creating data.table with corpus positions")
    DT <- data.table(
      cpos = cpos(do.call(rbind, lapply(.Object@objects, slot, "cpos"))),
      name_id = do.call(c, Map(rep, 1:length(.Object@objects), unname(sapply(.Object@objects, slot, "size"))))
    )

    if (is.null(phrases)){
      if (verbose) message(sprintf("... adding ids for p-attribute '%s'", p_attribute))
      DT[, "id" :=  cl_cpos2id(
        corpus = corpus, registry = corpus_registry_dir(corpus),
        p_attribute = p_attribute, cpos = DT[["cpos"]]
        )]
      
      if (verbose) message("... performing count")
      CNT <- DT[,.N, by = c("name_id", "id")]
      rm(DT)
      setnames(CNT, old = "N", new = "count")
      
      if (verbose) message("... adding decoded p-attribute")
      str_raw <- cl_id2str(
        corpus = corpus, registry = corpus_registry_dir(corpus),
        p_attribute = p_attribute, id = CNT[["id"]]
      )
      
      CNT[, eval(p_attribute) := if (encoding() == enc) str_raw else as.nativeEnc(str_raw, from = enc)]
      rm(str_raw)
    } else {
      if (verbose) message("... adding tokens")
      DT[, eval(p_attribute) := get_token_stream(
        DT[["cpos"]],
        corpus = corpus, p_attribute = p_attribute,
        encoding = .Object@objects[[1]]@encoding
        )]
      if (verbose) message("... generating phrases")
      DT_min <- concatenate_phrases(DT, phrases = phrases, col = p_attribute) # in utils.R
      if (verbose) message("... counting")
      CNT <- DT_min[, .N, by = c("name_id", p_attribute)]
      setnames(CNT, old = "N", new = "count")
    }
    
    if (verbose) message("... creating bundle of count objects")
    CNT_list <- split(CNT, by = "name_id")
    rm(CNT)
    
    y <- as(as(.Object, "corpus"), "count_bundle")

    prototype <- as(as(.Object, "corpus"), "count")
    .fn <- function(i){
      cnt <- prototype
      cnt@p_attribute <- p_attribute
      cnt@stat <- CNT_list[[i]][, "name_id" := NULL]
      cnt@name <- .Object@objects[[i]]@name
      cnt@size <- size(.Object@objects[[i]])
      cnt
    }
    
    y@objects <- if (progress)
      pblapply(seq_along(CNT_list), .fn)
    else
      lapply(seq_along(CNT_list), .fn)
    
    names(y@objects) <- names(.Object)
    
    return( y )
  }
})


#' @rdname count-method
#' @export
setMethod("count", "subcorpus_bundle", function(.Object, query = NULL, cqp = FALSE, p_attribute = NULL, phrases = NULL, freq = FALSE, total = TRUE, mc = FALSE, progress = TRUE, verbose = FALSE, ...){
  callNextMethod()
})


#' @rdname count-method
setMethod("count", "corpus", function(.Object, query = NULL, cqp = is.cqp, check = TRUE, p_attribute = getOption("polmineR.p_attribute"), breakdown = FALSE, sort = FALSE, decode = TRUE, verbose = TRUE, ...){
  
  if ("pAttribute" %in% names(list(...))) p_attribute <- list(...)[["pAttribute"]]
  
  if (is.null(query)){
    if (length(p_attribute) == 1L){
      cnt_file <- fs::path(.Object@data_dir, sprintf("%s.corpus.cnt", p_attribute)
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
        # lexicon_file <- path(.Object@data_dir, paste(p_attribute, "lexicon", sep = "."))
        # lexicon_file_size <- file.info(lexicon_file)[["size"]]
        # tokens <- readBin(con = lexicon_file, what = character(), n = lexicon_file_size)
        # 
        # Using a Rcpp implementation for reading in the lexion file might be a
        # fast solution, see https://gist.github.com/hadley/6353939
        tokens <- cl_id2str(
          corpus = .Object@corpus, registry = .Object@registry_dir,
          p_attribute = p_attribute, id = TF[[p_attr_col_id]]
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
            cpos2id(
              x = .Object, p_attribute = p_attr,
              cpos = 0L:(size(.Object) - 1L)
            )
          }
        )
      )
      .message("counting", verbose = verbose)
      TF <- tokenStreamDT[, .N, by = c(eval(colnames(tokenStreamDT)))]
      setnames(TF, old = "N", new = "count")
      if (decode){
        for (p_attr in p_attribute){
          .message("decode p-attribute: ", p_attr, verbose = verbose)
          strings <- cl_id2str(
            corpus = .Object@corpus, registry = .Object@registry_dir,
            p_attribute = p_attr, id = TF[[paste(p_attr, "id", sep = "_")]]
          ) 
          TF[, (p_attr) := as.nativeEnc(strings, from = encoding(.Object))]
        }
        setcolorder(TF, c(p_attribute, paste(p_attribute, "id", sep = "_"), "count"))
      }
    }
    y <- as(.Object, "count")
    
    y@p_attribute <- p_attribute
    y@stat <- TF
    y@name <- character()
    y@size <- size(.Object)
    
    return(y)
  } else {
    if (is.function(cqp)) cqp <- cqp(query)
    if (length(cqp) > 1) stop("length of cqp is larger than 1, it needs to be 1")
    if (isFALSE(cqp)){
      query <- as.corpusEnc(query, corpusEnc = encoding(.Object))
      count <- sapply(
        query,
        function(query){
          query_id <- cl_str2id(
            corpus = .Object@corpus, registry = .Object@registry_dir,
            p_attribute = p_attribute, str = query
          )
          # if there is no id for query, query_id will be -5
          if (query_id >= 0L){
            cl_id2freq(
              corpus = .Object@corpus, registry = .Object@registry_dir,
              p_attribute = p_attribute, id = query_id
            )
          } else {
            0L
          }
        }
      )
      return(data.table(query = query, count = count, freq = count / size(.Object)))
    } else if (cqp){
      if (isFALSE(breakdown)){
        region_matrices <- lapply(
          query,
          function(query) cpos(.Object, query = query, cqp = cqp, check = check, p_attribute = p_attribute, verbose = FALSE)
        )
        # If any corpus position in the region matrices occurrs more than once, there 
        # is an overlap of matches obtained for queries. A warning shall prevent that 
        # users sum up query matches and unknowingly overestimate the total number of 
        # query matches.
        if (any(table(unlist(lapply(region_matrices, cpos))) > 1L)){
          warning(
            "The CQP queries processed result in at least one overlapping query. ",
            "Summing up the counts for the individual query matches may result in an ",
            "overestimation of the total number of hits. To avoid this, ",
            "consider collapsing multiple CQP queries into one single query.")
        }
        counts <- sapply(region_matrices, function(m) if (!is.null(m)) nrow(m) else 0L)
        dt <- data.table(query = query, count = counts, freq = counts / size(.Object))
        return(dt)
      } else {
        # To avoid implementing the count()-method with breakdown = TRUE, we rely
        # on the method as it is implemented for the subcorpus class (which will
        # call the method for the slice class. There is an additional check whether
        # hits are within the regions defined by the subcorpus, but this extra 
        # cost is minimal.
        retval <- count(as(.Object, "subcorpus"), query = query, cqp = cqp, p_attribute = p_attribute, breakdown = TRUE)
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

  ids <- cl_cpos2id(
    corpus = corpus, registry = corpus_registry_dir(corpus),
    p_attribute = p_attribute, cpos = .Object
  )
  count <- tabulate(ids)
  TF <- data.table(
    id = 0L:length(count),
    count = c(length(which(ids == 0L)), count)
  )
  setkeyv(TF, cols = "id")
  setnames(TF, "id", paste(p_attribute, "id", sep = "_"))
  TF[count > 0L]
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
    registry_dir = .Object@registry_dir,
    data_dir = .Object@data_dir,
    info_file = .Object@info_file,
    template = .Object@template,
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
  ocpu_exec(fn = "count", corpus = .Object@corpus, server = .Object@server, restricted = .Object@restricted, .Object = as(.Object, "corpus"), ...)
})


#' @rdname count-method
setMethod("count", "remote_subcorpus", function(.Object, ...){
  ocpu_exec(fn = "count", corpus = .Object@corpus, server = .Object@server, restricted = .Object@restricted, .Object = as(.Object, "subcorpus"), ...)
})

