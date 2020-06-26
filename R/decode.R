#' @rdname decode
setGeneric("decode", function(.Object, ...) standardGeneric("decode"))

setOldClass("Annotation")


setAs(from = "corpus", to = "Annotation", def = function(from){
  
  # Implemented only for class 'corpus', 'subcorpus'-class will inherit from it.
  
  if (!requireNamespace(package = "NLP", quietly = TRUE))
    stop("Package 'NLP' required but not available")

  word <- get_token_stream(from@cpos, corpus = from@corpus, p_attribute = "word", encoding = from@encoding)
  pos <- get_token_stream(from@cpos, corpus = from@corpus, p_attribute = "pos", encoding = from@encoding)
  whitespace_after <- c(ifelse(pos %in% c("$.", "$,"), FALSE, TRUE)[2L:length(pos)], FALSE)
  word_with_whitespace <- paste(word, ifelse(whitespace_after, " ", ""), sep = "")
  s <- paste(word_with_whitespace, collapse = "")
  word_length <- sapply(word, nchar)
  left_offset <- c(1L, (cumsum(sapply(word_with_whitespace, nchar)) + 1L)[1L:(length(word) - 1L)] )
  names(left_offset) <- word
  right_offset <- left_offset + word_length - 1L
  names(right_offset) <- word
  cpos <- cpos(from@cpos)
  w <- NLP::Annotation(
    id = cpos,
    rep.int("word", length(cpos)),
    start = left_offset,
    end = right_offset
  )

  right_offset <- left_offset + word_length
  names(right_offset) <- word
  m <- matrix(data = c(left_offset, right_offset), ncol = 2, byrow = FALSE)
  f <- cut(x = 1L:length(pos), breaks = unique(c(1L, grep("\\$\\.", pos), length(pos))), include.lowest = TRUE)
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



#' Decode corpus or subcorpus.
#' 
#' Decode \code{corpus} or \code{subcorpus} and return class specified by
#' argument \code{to}.
#'
#' The primary purpose of the method is type conversion. By obtaining the corpus
#' or subcorpus in the format specified by the argument \code{to}, the data can
#' be processed with tools that do not rely on the Corpus Workbench (CWB).
#' Supported output formats are \code{data.table} (which can be converted to a
#' \code{data.frame} or \code{tibble} easily) or an \code{Annotation} object as
#' defined in the package \code{NLP}. Another purpose of decoding the corpus can
#' be to rework it, and to re-import it into the CWB (e.g. using the
#' \code{cwbtools}-package).
#'
#' An earlier version of the method included an option to decode a single
#' s-attribute, which is not supported any more. See the
#' \code{s_attribute_decode} function of the package RcppCWB.
#'
#' @return The return value will correspond to the class specified by argument
#'   \code{to}.
#' 
#' @param .Object The \code{corpus} or \code{subcorpus} to decode.
#' @param to The class of the returned object, stated as a length-one
#'   \code{character} vector.
#' @param s_attributes The structural attributes to decode. If \code{NULL}
#'   (default), all structural attributes will be decoded.
#' @param p_attributes The positional attributes to decode. If \code{NULL}
#'   (default), all positional attributes will be decoded.
#' @param decode A \code{logical} value, whether to decode token ids and struc
#'   ids to character strings. If \code{FALSE}, the values of columns for p- and
#'   s-attributes will be \code{integer} vectors. If \code{TRUE} (default), the
#'   respective columns are \code{character} vectors.
#' @param ... Further arguments.
#' @param verbose A \code{logical} value, whether to output progess messages.
#' @exportMethod decode
#' @importFrom RcppCWB get_region_matrix
#' @seealso To decode a structural attribute, you can use the
#'   \code{\link{s_attributes}}-method, setting argument \code{unique} as
#'   \code{FALSE} and \code{\link[RcppCWB]{s_attribute_decode}}. See
#'   \code{\link{as.VCorpus}} to decode a \code{partition_bundle} object,
#'   returning a \code{VCorpus} object.
#' @examples
#' use("polmineR")
#' 
#' # Decode corpus as data.table
#' dt <- decode("GERMAPARLMINI", to = "data.table")
#' 
#' # Decode corpus selectively
#' dt <- decode("GERMAPARLMINI", to = "data.table", p_attributes = "word", s_attributes = "party")
#' 
#' # Decode a subcorpus
#' sc <- subset(corpus("GERMAPARLMINI"), speaker == "Angela Dorothea Merkel")
#' dt <- decode(sc, to = "data.table")
#' 
#' # Decode subcorpus selectively
#' dt <- decode(sc, to = "data.table", p_attributes = "word", s_attributes = "party")
#' 
#' # Decode partition
#' P <- partition("REUTERS", places = "kuwait", regex = TRUE)
#' dt <- decode(P)
#' 
#' # Previous versions of polmineR offered an option to decode a single
#' # s-attribute. This is how you could proceed to get a table with metadata.
#' dt <- decode(P, s_attribute = "id", decode = FALSE)
#' dt[, "word" := NULL]
#' dt[,{list(cpos_left = min(.SD[["cpos"]]), cpos_right = max(.SD[["cpos"]]))}, by = "id"]
#' 
#' # Decode subcorpus as Annotation object
#' \dontrun{
#' if (requireNamespace("NLP")){
#'   library(NLP)
#'   p <- subset(corpus("GERMAPARLMINI"), date == "2009-11-10" & speaker == "Angela Dorothea Merkel")
#'   s <- as(p, "String")
#'   a <- as(p, "Annotation")
#'   
#'   # The beauty of having this NLP Annotation object is that you can now use 
#'   # the different annotators of the openNLP package. Here, just a short scenario
#'   # how you can have a look at the tokenized words and the sentences.
#' 
#'   words <- s[a[a$type == "word"]]
#'   sentences <- s[a[a$type == "sentence"]] # does not yet work perfectly for plenary protocols 
#' }
#' }
#' @rdname decode
setMethod("decode", "corpus", function(.Object, to = c("data.table", "Annotation"), p_attributes = NULL, s_attributes = NULL, decode = TRUE, verbose = TRUE){
  if (to == "data.table"){

    if (is.null(p_attributes)) p_attributes <- p_attributes(.Object)
    if (!all(p_attributes %in% p_attributes(.Object))) stop("Not all p_attributes provided are available.")
    

    p_attribute_list <- lapply(
      setNames(p_attributes, p_attributes),
      function(p_attr){
        if (verbose) message("decoding p-attribute:", p_attr)
        get_token_stream(.Object, p_attribute = p_attr, decode = decode)
      }
    )

    max_cpos <- size(.Object) - 1L
    
    if (is.null(s_attributes)) s_attributes <- s_attributes(.Object)
    if (length(s_attributes) > 0L){
      if (!all(s_attributes %in% s_attributes(.Object))) stop("Not all s_attributes provided are available.")
    }
    
    if (length(s_attributes) > 0L){
      s_attribute_list <- lapply(
        setNames(s_attributes, s_attributes),
        function(s_attr){
          if (verbose) message("decoding s-attribute:", s_attr)
          corpus_id <- get_corpus(.Object)
          struc <- cl_cpos2struc(corpus = corpus_id, s_attribute = s_attr, cpos = 0L:max_cpos, registry = registry())
          if (decode){
            str <- cl_struc2str(corpus = corpus_id, s_attribute = s_attr, struc = struc, registry = registry())
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
    

    message("assembling data.table")
    combined_list <- c(
      list(cpos = 0L:max_cpos),
      p_attribute_list,
      s_attribute_list
    )
    y <- data.table::as.data.table(combined_list)
    
  } else if (to == "Annotation"){
    y <- as(.Object, "Annotation")
  } else {
    stop("The 'to'-argument of the decode()-method is required to be either 'data.table' or 'Annotation'.")
  }
  y
})


#' @exportMethod decode
#' @rdname decode
setMethod("decode", "character", function(.Object, to = c("data.table", "Annotation"), s_attributes = NULL, p_attributes = NULL, decode = TRUE, verbose = TRUE, ...){
  if (any(c("sAttribute", "s_attribute") %in% names(list(...)))){
    stop("Decoding an s_attribute is not supported any longer in the decode()-method of ",
         "the polmineR package. See s_attribute_decode in the RcppCWB package as a substitute.")
  }
  decode(corpus(.Object), to = to, s_attributes = s_attributes, p_attributes = p_attributes, decode = decode, verbose = verbose)
})





#' @exportMethod decode
#' @rdname decode
setMethod("decode", "slice", function(.Object, to = "data.table", s_attributes = NULL, p_attributes = NULL, decode = TRUE, verbose = TRUE){
  if (to == "data.table"){
    
    if (is.null(p_attributes)) p_attributes <- p_attributes(.Object)
    if (!all(p_attributes %in% p_attributes(.Object))) stop("Not all p_attributes provided are available.")
    
    y <- data.table(cpos = cpos(.Object@cpos))

    for (p_attr in p_attributes){
      if (verbose) message("... decoding p_attribute ", p_attr)
      y[, (p_attr) := get_token_stream(.Object, p_attribute = p_attr, decode = decode)]
    }
    
    if (is.null(s_attributes)) s_attributes <- s_attributes(.Object)
    if (length(s_attributes) > 0L){
      if (!all(s_attributes %in% s_attributes(.Object))) stop("Not all s_attributes provided are available.")
      strucs <- RcppCWB::cl_cpos2struc(corpus = .Object@corpus, s_attribute = s_attributes[1], cpos = .Object@cpos[,1])
      
      s_attr_dt <- data.table(
        RcppCWB::get_region_matrix(corpus = .Object@corpus, s_attribute = s_attributes[1], strucs = strucs, registry = registry())
      )
      setnames(s_attr_dt, old = c("V1", "V2"), new = c("cpos_left", "cpos_right"))
      s_attr_dt[, "struc" := strucs]
      
      for (s_attr in s_attributes){
        if (decode){
          if (verbose) message("... decoding s_attribute ", s_attr)
          str <- RcppCWB::cl_struc2str(corpus = .Object@corpus, s_attribute = s_attr, struc = strucs)
          Encoding(str) <- encoding(.Object)
          s_attr_dt[, (s_attr) := as.nativeEnc(str, from = encoding(.Object))]
        } else {
          s_attr_dt[, (s_attr) := strucs]
        }
      }
      
      unfold <- function(.SD){
        dt <- data.table(cpos = .SD[["cpos_left"]]:.SD[["cpos_right"]])
        for (s_attr in s_attributes){
          value <- .SD[[s_attr]]
          dt[, (s_attr) := value]
        }
        dt
      }
      s_attr_dt_ext <- s_attr_dt[, unfold(.SD), by = "struc"]
      y <- y[s_attr_dt_ext, on = "cpos"]
      setcolorder(y, neworder = c("cpos", p_attributes, s_attributes))
    }
  } else if (to == "Annotation"){
    y <- as(.Object, "Annotation")
  }
  y
})

#' @rdname decode
setMethod("decode", "partition", function(.Object, to = "data.table", s_attributes = NULL, p_attributes = NULL, decode = TRUE, verbose = TRUE){
  callNextMethod()
})


#' @exportMethod decode
#' @rdname decode
setMethod("decode", "subcorpus", function(.Object, to = "data.table", s_attributes = NULL, p_attributes = NULL, decode = TRUE, verbose = TRUE){
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
  if (!inherits(corpus, "corpus")) stop("Argument 'corpus' is required to be a corpus object.")
  
  if (is.null(boost)){
    boost <- if (corpus@size > 10000000L && length(cpos) >= (corpus@size * 0.05)) TRUE else FALSE
  }
  
  if (isTRUE(boost)){
    lexfile <- file.path(corpus@data_dir, sprintf("%s.lexicon", p_attributes), fsep = "/")
    lexicon <- readBin(con = lexfile, what = character(), n = file.info(lexfile)$size)
    Encoding(lexicon) <- corpus@encoding
    if (!identical(corpus@encoding, localeToCharset()[1])){
      # lexicon <- stringi::stri_encode(lexicon, from = corpus@encoding, to = localeToCharset()[1]) # as.locale
      lexicon <- iconv(lexicon, from = corpus@encoding, to = localeToCharset()[1])
      Encoding(lexicon) <- localeToCharset()[1]
    }
    y <- lexicon[.Object + 1L]
  } else if (isFALSE(boost)){
    y <- RcppCWB::cl_id2str(corpus = corpus@corpus, p_attribute = p_attributes, registry = registry(), id = .Object)
    Encoding(y) <- corpus@encoding
    if (!identical(corpus@encoding, localeToCharset()[1])){
      # y <- stringi::stri_encode(y, from = corpus@encoding, to = localeToCharset()[1])
      y <- iconv(y, from = corpus@encoding, to = localeToCharset()[1])
      Encoding(y) <- localeToCharset()[1]
    }
  }
  y
})
