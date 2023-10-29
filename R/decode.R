#' @name decode-method
#' @aliases decode
#' @rdname decode
setGeneric("decode", function(.Object, ...) standardGeneric("decode"))

setOldClass("Annotation")
setOldClass("AnnotatedPlainTextDocument")


setAs(from = "corpus", to = "Annotation", def = function(from){
  
  # Implemented only for class 'corpus', 'subcorpus'-class will inherit from it.
  
  if (!requireNamespace(package = "NLP", quietly = TRUE))
    stop("Package 'NLP' required but not available")

  word <- get_token_stream(
    from@cpos,
    corpus = from@corpus,
    p_attribute = "word",
    encoding = from@encoding
  )
  
  if (!"pos" %in% p_attributes(from)) stop("p-attribute not available")
  pos <- get_token_stream(
    from@cpos,
    corpus = from@corpus,
    p_attribute = "pos",
    encoding = from@encoding
  )
  ws_after <- c(ifelse(pos %in% c("$.", "$,"), FALSE, TRUE)[-1], FALSE)
  word_with_ws <- paste(word, ifelse(ws_after, " ", ""), sep = "")
  s <- paste(word_with_ws, collapse = "")
  word_length <- sapply(word, nchar)
  left_offset <- c(
    1L,
    (cumsum(sapply(word_with_ws, nchar)) + 1L)[1L:(length(word) - 1L)]
  )
  names(left_offset) <- word
  right_offset <- left_offset + word_length - 1L
  names(right_offset) <- word
  cpos <- ranges_to_cpos(from@cpos)
  w <- NLP::Annotation(
    id = cpos,
    rep.int("word", length(cpos)),
    start = left_offset,
    end = right_offset
  )

  right_offset <- left_offset + word_length
  names(right_offset) <- word # repeats 
  m <- matrix(data = c(left_offset, right_offset), ncol = 2, byrow = FALSE)
  f <- cut(
    x = 1L:length(pos),
    breaks = unique(c(1L, grep("\\$\\.", pos), length(pos))), 
    include.lowest = TRUE
  )
  chunks <- split(x = m, f = f)
  sentence_left <- sapply(chunks, min)
  sentence_right <- sapply(chunks, max) - 1L
  s <- NLP::Annotation(
    id = 1L:length(sentence_left),
    rep.int("sentence", length(sentence_left)),
    start = sentence_left,
    end = sentence_right
  )
  c(w, s)
})


#' @importFrom RcppCWB region_matrix_to_struc_matrix
as.AnnotatedPlainTextDocument <- function(x, p_attributes = NULL, s_attributes = NULL, mw = NULL, stoplist = NULL, verbose = TRUE){
  # is required to be a subcorpus object
  
  stopifnot(
    inherits(x, "subcorpus"),
    length(mw) == length(unique(mw))
  )

  if (!requireNamespace(package = "NLP", quietly = TRUE))
    stop("Package 'NLP' required but not available.")
  
  if (verbose) cli_alert_info("decode p-attributes")
  p_attrs <- if (is.null(p_attributes)) p_attributes(x) else p_attributes
  ts <- decode(
    x,
    to = "data.table",
    p_attribute = p_attrs,
    s_attributes = character(),
    verbose = verbose
  )
  
  if (!is.null(stoplist)) ts <- ts[!ts[["word"]] %in% stoplist]
  
  if (verbose) cli_progress_step("generate plain text string")
  ws_after <- c(!grepl("^[.,;:!?]$", ts[["word"]])[2L:nrow(ts)], FALSE)
  word_with_ws <- paste(ts[["word"]], ifelse(ws_after, " ", ""), sep = "")
  s <- stringi::stri_c(word_with_ws, collapse = "")
  
  if (verbose) cli_progress_step("generate token-level annotation")
  left_offset <- c(1L, (cumsum(nchar(word_with_ws)) + 1L)[1L:(nrow(ts) - 1L)])
  right_offset <- left_offset + nchar(ts[["word"]]) - 1L
  w <- NLP::Annotation(
    id = ts[["cpos"]],
    rep.int("word", nrow(ts)),
    start = left_offset,
    end = right_offset,
    features = lapply(
      split(ts[, p_attrs, with = FALSE], f = 1L:nrow(ts)),
      as.data.frame
    )
  )
  
  mw_annotations <- lapply(
    mw,
    function(s_attr){
      if (verbose)
        cli_progress_step(
          "generate annotation for s-attribute {.val {s_attr}}"
        )
      
      struc_matrix <- RcppCWB::region_matrix_to_struc_matrix(
        corpus = x@corpus,
        registry = x@registry_dir,
        region_matrix = x@cpos,
        s_attribute = s_attr
      )
      strucs <- ranges_to_cpos(struc_matrix)
      strucs_min <- unique(strucs[strucs >= 0L])
      if (length(strucs_min) == 0L) return(NULL)
      
      regions <- get_region_matrix(
        corpus = x@corpus,
        s_attribute = s_attr,
        strucs = strucs_min,
        registry = x@registry_dir
      )
      
      str <- RcppCWB::cl_struc2str(
        corpus = x@corpus,
        registry = x@registry_dir,
        s_attribute = s_attr,
        struc = strucs_min
      )
      Encoding(str) <- x@encoding
      str <- as.nativeEnc(x = str, from = x@encoding)
      
      data <- lapply(
        1:nrow(regions),
        function(i){
          matching <- which(w$id %in% regions[i,1]:regions[i,2])
          
          # If the region covers tokens that have been dropped due to the 
          # stoplist, no matches - omit these cases.
          if (length(matching) > 0L){
            txt <- stri_sub(
              str = s,
              from = w[min(matching)]$start,
              to = w[max(matching)]$end
            )
            list(
              kind = str[i],
              text = txt,
              constituents = regions[i,1]:regions[i,2],
              start = w[min(matching)]$start,
              end = w[max(matching)]$end
            )
          } else {
            NULL
          }
        }
      )
      
      nomatch <- sapply(data, length) == 0
      if (any(nomatch)) data <- data[-which(nomatch)]
      
      NLP::Annotation(
        id = seq_along(data),
        type = rep.int(
          if (is.null(names(mw))) s_attr else names(mw)[which(mw == s_attr)],
          length(data)
        ),
        start = unname(unlist(lapply(data, `[[`, "start"))),
        end = unname(unlist(lapply(data, `[[`, "end"))),
        features = lapply(data, `[`, c("kind", "text", "constituents"))
      )
    }
  )
  
  if (verbose) cli_progress_step("retrieve metadata")
  meta <- lapply(
    setNames(s_attributes, s_attributes),
    function(s_attr){
      struc <- cl_cpos2struc(
        corpus = x@corpus,
        registry = x@registry_dir,
        s_attribute = s_attr,
        cpos = x@cpos[1,1]
      )
      value <- cl_struc2str(
        corpus = x@corpus,
        registry = x@registry_dir,
        s_attribute = s_attr,
        struc = struc
      )
      Encoding(value) <- x@encoding
      as.nativeEnc(x = value, from = x@encoding)
    }
  )
  
  if (verbose) cli_progress_step("instantiate AnnotatedPlainTextDocument")
  NLP::AnnotatedPlainTextDocument(
    s = s,
    a = c(w, do.call(c, mw_annotations)),
    meta = meta
  )
}


#' Decode corpus or subcorpus.
#' 
#' Decode `corpus` or `subcorpus` and return class specified by argument `to`.
#'
#' The primary purpose of the method is type conversion. By obtaining the corpus
#' or subcorpus in the format specified by the argument `to`, the data can be
#' processed with tools that do not rely on the Corpus Workbench (CWB).
#' Supported output formats are `data.table` (which can be converted to a
#' `data.frame` or `tibble` easily) or an `Annotation` object as defined in the
#' package 'NLP'. Another purpose of decoding the corpus can be to rework it,
#' and to re-import it into the CWB (e.g. using the 'cwbtools'-package).
#'
#' An earlier version of the method included an option to decode a single
#' s-attribute, which is not supported any more. See the `s_attribute_decode()`
#' function of the package RcppCWB.
#'
#' @return The return value will correspond to the class specified by argument
#'   `to`.
#' 
#' @param .Object The `corpus` or `subcorpus` to decode.
#' @param to The class of the returned object, stated as a length-one
#'   `character` vector.
#' @param s_attributes The structural attributes to decode. If `NULL` (default),
#'   all structural attributes will be decoded.
#' @param p_attributes The positional attributes to decode. If `NULL` (default),
#'   all positional attributes will be decoded.
#' @param stoplist A `character` vector with terms to be dropped.
#' @param decode A `logical` value, whether to decode token ids and struc ids to
#'   character strings. If `FALSE`, the values of columns for p- and
#'   s-attributes will be `integer` vectors. If `TRUE` (default), the respective
#'   columns are `character` vectors.
#' @param mw A `character` vector with s-attributes with multiword expressions.
#'   Used only if `to` is 'AnnotatedPlainTextDocument'.
#' @param ... Further arguments.
#' @param verbose A `logical` value, whether to output progess messages.
#' @exportMethod decode
#' @importFrom RcppCWB get_region_matrix
#' @seealso To decode a structural attribute, you can use the
#'   \code{\link{s_attributes}}-method, setting argument \code{unique} as
#'   \code{FALSE} and \code{\link[RcppCWB]{s_attribute_decode}}. See
#'   \code{\link{as.VCorpus}} to decode a \code{partition_bundle} object,
#'   returning a \code{VCorpus} object.
#' @examples
#' use("polmineR")
#' use(pkg = "RcppCWB", corpus = "REUTERS")
#' 
#' # Decode corpus as data.table
#' dt <- decode("REUTERS", to = "data.table")
#' 
#' # Decode corpus selectively
#' dt <- decode(
#'   "REUTERS",
#'   to = "data.table",
#'   p_attributes = "word",
#'   s_attributes = "id"
#' )
#' 
#' # Decode a subcorpus
#' dt <- corpus("REUTERS") %>%
#'   subset(id %in% c("127", "144")) %>%
#'   decode(s_attributes = "id", to = "data.table")
#' 
#' # Decode partition
#' dt <- partition("REUTERS", places = "kuwait", regex = TRUE) %>%
#'   decode(to = "data.table")
#' 
#' # Previous versions of polmineR offered an option to decode a single
#' # s-attribute. This is how you could proceed to get a table with metadata.
#' dt <- partition("REUTERS", places = "kuwait", regex = TRUE) %>% 
#'   decode(s_attribute = "id", decode = FALSE, to = "data.table")
#' dt[, "word" := NULL]
#' dt[,{list(cpos_left = min(.SD[["cpos"]]), cpos_right = max(.SD[["cpos"]]))}, by = "id"]
#' 
#' # Decode subcorpus as Annotation object
#' \dontrun{
#' if (requireNamespace("NLP")){
#'   library(NLP)
#'   p <- corpus("GERMAPARLMINI") %>%
#'     subset(date == "2009-11-10" & speaker == "Angela Dorothea Merkel")
#'   s <- as(p, "String")
#'   a <- as(p, "Annotation")
#'   
#'   # The beauty of having this NLP Annotation object is that you can now use 
#'   # the different annotators of the openNLP package. Here, just a short scenario
#'   # how you can have a look at the tokenized words and the sentences.
#' 
#'   words <- s[a[a$type == "word"]]
#'   sentences <- s[a[a$type == "sentence"]] # does not yet work perfectly for plenary protocols 
#'   
#'   doc <- decode(p, to = "AnnotatedPlainTextDocument")
#' }
#' }
#' @rdname decode
#' @importFrom cli cli_progress_step
setMethod("decode", "corpus", function(.Object, to = c("data.table", "Annotation", "AnnotatedPlainTextDocument"), p_attributes = NULL, s_attributes = NULL, mw = NULL, stoplist = NULL, decode = TRUE, verbose = TRUE){
  
  if (length(to) != 1L)
    stop("`decode()` - argument 'to' required to have length 1")
  if (!to %in% c("data.table", "Annotation", "AnnotatedPlainTextDocument")){
    cli_alert_danger(
      "Argument 'to' of `decode()` required to be 'data.table' or 'Annotation'."
    )
    stop()
  }
  
  if (to == "data.table"){

    if (is.null(p_attributes)) p_attributes <- p_attributes(.Object)
    if (!all(p_attributes %in% p_attributes(.Object)))
      stop("Not all p_attributes provided are available.")

    p_attribute_list <- lapply(
      setNames(p_attributes, p_attributes),
      function(p_attr){
        if (verbose)
          cli_progress_step(sprintf("decoding p-attribute: %s", p_attr))
        get_token_stream(.Object, p_attribute = p_attr, decode = decode)
      }
    )

    max_cpos <- size(.Object) - 1L
    
    if (is.null(s_attributes)) s_attributes <- s_attributes(.Object)
    if (length(s_attributes) > 0L){
      if (!all(s_attributes %in% s_attributes(.Object)))
        stop("Not all s_attributes provided are available.")
    }
    
    if (length(s_attributes) > 0L){
      s_attribute_list <- lapply(
        setNames(s_attributes, s_attributes),
        function(s_attr){
          if (verbose)
            cli_progress_step(sprintf("decoding s-attribute: %s", s_attr))
          
          struc <- cl_cpos2struc(
            corpus = .Object@corpus,
            registry = .Object@registry_dir,
            s_attribute = s_attr,
            cpos = 0L:max_cpos
          )
          
          if (decode && s_attr_has_values(s_attr, x = .Object)){
            str <- cl_struc2str(
              corpus = .Object@corpus,
              registry = .Object@registry_dir,
              s_attribute = s_attr,
              struc = struc
            )
            Encoding(str) <- encoding(.Object)
            return(as.nativeEnc(str, from = encoding(.Object)))
          } else {
            return(struc)
          }
        }
      )
    } else {
      s_attribute_list <- list()
    }

    if (verbose) cli_progress_step("assembling data.table")
    combined_list <- c(
      list(cpos = 0L:max_cpos),
      p_attribute_list,
      s_attribute_list
    )
    y <- data.table::as.data.table(combined_list)
    
  } else if (to == "Annotation"){
    y <- as(.Object, "Annotation")
  } else if (to == "AnnotatedPlainTextDocument"){
    y <- as.AnnotatedPlainTextDocument(
      x = as(.Object, "subcorpus"), # fn requires cpos slot
      p_attributes = p_attributes,
      s_attributes = s_attributes,
      mw = mw,
      stoplist = stoplist,
      verbose = verbose
    )
  }
  
  y
})


#' @exportMethod decode
#' @rdname decode
setMethod("decode", "character", function(.Object, to = c("data.table", "Annotation"), s_attributes = NULL, p_attributes = NULL, decode = TRUE, verbose = TRUE){
  decode(corpus(.Object), to = to, s_attributes = s_attributes, p_attributes = p_attributes, decode = decode, verbose = verbose)
})





#' @exportMethod decode
#' @rdname decode
setMethod("decode", "slice", function(.Object, to = c("data.table", "Annotation", "AnnotatedPlainTextDocument"), s_attributes = NULL, p_attributes = NULL, mw = NULL, stoplist = NULL, decode = TRUE, verbose = TRUE){
  
  if (to == "data.table"){
    
    if (is.null(p_attributes)) p_attributes <- p_attributes(.Object)
    if (!all(p_attributes %in% p_attributes(.Object)))
      stop("Not all p_attributes provided are available.")
    
    y <- data.table(cpos = ranges_to_cpos(.Object@cpos))
    setkeyv(y, cols = "cpos") # col cpos used repeatedly to  merge s-attributes

    for (p_attr in p_attributes){
      if (verbose) cli_progress_step("decoding p_attribute {.val {p_attr}}")
      y[, (p_attr) := get_token_stream(.Object, p_attribute = p_attr, decode = decode)]
      if (verbose) cli_progress_done()
    }
    
    if (is.null(s_attributes)){
      if (verbose)
        cli_alert_info(
          "argument `s_attributes` missing - decode all s-attributes"
          )
      s_attributes <- s_attributes(.Object)
    }
    
    if (length(s_attributes) > 0L){
      if (!all(s_attributes %in% s_attributes(.Object)))
        stop("Not all s_attributes provided are available.")

      for (i in 1L:length(s_attributes)){
        if (verbose)
          cli_progress_step("decoding s_attribute {.val  {s_attributes[i]}}")
        
        # Tested: strucs, regions and corpus positions will be identical if
        # i > 2 and current s-attribute is sibling of previous sibling. But
        # the cost of learning whether s-attributes are siblings consumes the 
        # performance gain, so we get strucs, regions and cpos anew.
        struc_matrix <- RcppCWB::region_matrix_to_struc_matrix(
          corpus = .Object@corpus,
          s_attribute = s_attributes[i],
          registry = .Object@registry_dir,
          region_matrix = .Object@cpos
        )
        strucs <- RcppCWB::ranges_to_cpos(struc_matrix)
        if (length(strucs) == 0L) next
        
        regions <- RcppCWB::get_region_matrix(
          corpus = .Object@corpus,
          registry = .Object@registry_dir,
          s_attribute = s_attributes[i],
          strucs = strucs
        )
        cpos <- RcppCWB::ranges_to_cpos(regions)
        
        decode <- decode && s_attr_has_values(s_attributes[i], x = .Object)
        if (decode){
          str <- cl_struc2str(
            corpus = .Object@corpus,
            registry = .Object@registry_dir,
            s_attribute = s_attributes[i],
            struc = strucs
          )
          Encoding(str) <- encoding(.Object)
        }
        
        values <- data.table(
          cpos = cpos,
          attr = do.call(
            c,
            mapply(
              rep,
              x = if (decode) str else strucs,
              times = (regions[,2] - regions[,1]) + 1,
              SIMPLIFY = FALSE
            )
          )
        )
        setnames(values, old = "attr", new = s_attributes[i])

        y <- values[y, on = "cpos"]
        if (verbose) cli_progress_done()
      }
      
      setcolorder(y, neworder = c("cpos", p_attributes, s_attributes))
    }
  } else if (to == "Annotation"){
    y <- as(.Object, "Annotation")
  } else if (to == "AnnotatedPlainTextDocument"){
    return(callNextMethod())
  } else {
    stop("not implemented")
  }
  y
})

#' @rdname decode
setMethod("decode", "partition", function(.Object, to = "data.table", s_attributes = NULL, p_attributes = NULL, decode = TRUE, verbose = TRUE){
  callNextMethod()
})


#' @exportMethod decode
#' @rdname decode
setMethod("decode", "subcorpus", function(.Object, to = c("data.table", "Annotation", "AnnotatedPlainTextDocument"), s_attributes = NULL, p_attributes = NULL, mw = NULL, stoplist = NULL, decode = TRUE, verbose = TRUE){
  
  if (to == "AnnotatedPlainTextDocument"){
    return(
      as.AnnotatedPlainTextDocument(
        x = .Object,
        p_attributes = p_attributes,
        s_attributes = s_attributes,
        mw = mw,
        stoplist = stoplist,
        verbose = verbose
      )
    )
  }
  
  callNextMethod()
})


#' @details If \code{.Object} is an \code{integer} vector, it is assumed to be a
#'   vector of integer ids of p-attributes. The \code{decode}-method will
#'   translate token ids to string values as efficiently as possible. The
#'   approach taken will depend on the corpus size and the share of the corpus
#'   that is to be decoded. To decode a large number of integer ids, it is more
#'   efficient to read the lexicon file from the data directory directly and to
#'   index the lexicon with the ids rather than relying on
#'   \code{RcppCWB::cl_id2str}. The internal decision rule is to use the lexicon
#'   file when the corpus is larger than 10 000 000 million tokens and more than
#'   5 percent of the corpus are to be decoded. The encoding of the
#'   \code{character} vector that is returned will be the coding of the locale
#'   (usually ISO-8859-1 on Windows, and UTF-8 on macOS and Linux machines).
#' @param boost A length-one \code{logical} value, whether to speed up decoding
#'   a long vector of token ids by directly by reading in the lexion file from
#'   the data directory of a corpus. If \code{NULL} (default), the internal
#'   decision rule is that \code{boost} will be \code{TRUE} if the corpus is
#'   larger than 10 000 000 million tokens and more than 5 percent of the corpus
#'   are to be decoded.
#' @param corpus A CWB indexed corpus, either a length-one \code{character} vector,
#'   or a \code{corpus} object.
#' @exportMethod decode
#' @rdname decode
#' @examples
#'  
#' # decode vector of token ids
#' y <- decode(0:20, corpus = "GERMAPARLMINI", p_attributes = "word")
#' @importFrom stringi stri_encode
setMethod("decode", "integer", function(.Object, corpus, p_attributes, boost = NULL){
  stopifnot(
    length(corpus) == 1L,
    length(p_attributes) == 1L,
    is.character(p_attributes)
  )
  corpus <- if (is.character(corpus)) corpus(corpus) else corpus
  if (!inherits(corpus, "corpus"))
    stop("Argument 'corpus' is required to be a corpus object.")
  
  if (is.null(boost)){
    boost <- if (corpus@size > 10000000L && length(cpos) >= (corpus@size * 0.05)) TRUE else FALSE
  }
  
  if (isTRUE(boost)){
    lexfile <- fs::path(corpus@data_dir, sprintf("%s.lexicon", p_attributes))
    lexicon <- readBin(con = lexfile, what = character(), n = file.info(lexfile)$size)
    Encoding(lexicon) <- corpus@encoding
    if (!identical(corpus@encoding, encoding())){
      # lexicon <- stringi::stri_encode(lexicon, from = corpus@encoding, to = encoding()) # as.locale
      lexicon <- iconv(lexicon, from = corpus@encoding, to = encoding())
      Encoding(lexicon) <- encoding()
    }
    y <- lexicon[.Object + 1L]
  } else if (isFALSE(boost)){
    y <- RcppCWB::cl_id2str(
      corpus = corpus@corpus, registry = corpus@registry_dir,
      p_attribute = p_attributes, id = .Object
    )
    Encoding(y) <- corpus@encoding
    if (!identical(corpus@encoding, encoding())){
      # y <- stringi::stri_encode(y, from = corpus@encoding, to = encoding())
      y <- iconv(y, from = corpus@encoding, to = encoding())
      Encoding(y) <- encoding()
    }
  }
  y
})


#' @details The \code{decode}-method for \code{data.table} objects will decode
#'   token ids (column '`p-attribute`_id'), adding the corresponding string as a
#'   new column. If a column "cpos" with corpus positions is present, ids are
#'   derived for the corpus positions given first. If the \code{data.table}
#'   neither has a column "cpos" nor columns with token ids (i.e. colummn name
#'   ending with "_id"), the input \code{data.table} is returned unchanged. Note
#'   that columns are added to the \code{data.table} in an in-place operation to
#'   handle memory parsimoniously.
#' @examples 
#' dt <- data.table::data.table(cpos = cpos("GERMAPARLMINI", query = "Liebe")[,1])
#' decode(dt, corpus = "GERMAPARLMINI", p_attributes = c("word", "pos"))
#' y <- dt[, .N, by = c("word", "pos")]
#' @rdname decode
setMethod("decode", "data.table", function(.Object, corpus, p_attributes){
  corpus <- if (is.character(corpus)) corpus(corpus) else corpus
  if (!inherits(corpus, "corpus")) stop("Argument 'corpus' is required to be a corpus object.")
  if (isFALSE(all(p_attributes %in% p_attributes(corpus)))){
    stop("At least one p-attribute is not available.")
  }
  
  if ("cpos" %in% colnames(.Object)){
    for (p_attr in p_attributes){
      p_attr_id <- paste(p_attr, "id", sep = "_")
      ids <- cpos2id(corpus, p_attribute = p_attr, cpos = .Object[["cpos"]])
      .Object[, (p_attr_id) := ids]
    }
  }
  
  for (p_attr in p_attributes){
    p_attr_id <- paste(p_attr, "id", sep = "_")
    if (p_attr_id %in% colnames(.Object)){
      .Object[, (p_attr) := decode(.Object[[p_attr_id]], corpus = corpus, p_attributes = p_attr)]
    }
  }
  
  .Object
})
