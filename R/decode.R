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
  cpos <- unlist(apply(from@cpos, 1, function(x) x[1]:x[2]))
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
#' Decode \code{corpus} or \code{subcorpus} and return class specified by argument \code{to}.
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
#' s-attribute, which is not supported any more. See the \code{s_attribute_decode}
#' function of the package RcppCWB.
#' 
#' @return The return value will correspond to the class specified by argument \code{to}. 
#' 
#' @param .Object The \code{corpus} or \code{subcorpus} to decode.
#' @param to The class of the returned object, stated as a length-one
#'   \code{character} vector.
#' @param s_attributes The structural attributes to decode. If \code{NULL} (default), all
#'   structural attributes will be decoded.
#' @param p_attributes The positional attributes to decode. If \code{NULL} (default), all
#'   positional attributes will be decoded.
#' @param ... Further arguments.
#' @exportMethod decode
#' @importFrom RcppCWB get_region_matrix
#' @rdname decode
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
#' dt[, "word" := NULL]
#' dt[,
#'   {list(cpos_left = min(.SD[["cpos"]]), cpos_right = max(.SD[["cpos"]]), id = unique(.SD[["id"]]))},
#'   by = "struc"
#'   ]
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
setMethod("decode", "corpus", function(.Object, to = c("data.table", "Annotation"), p_attributes = NULL, s_attributes = NULL){
  if (to == "data.table"){

    if (is.null(p_attributes)) p_attributes <- p_attributes(.Object)
    if (!all(p_attributes %in% p_attributes(.Object))) stop("Not all p_attributes provided are available.")
    
    if (is.null(s_attributes)) s_attributes <- s_attributes(.Object)
    if (!all(s_attributes %in% s_attributes(.Object))) stop("Not all s_attributes provided are available.")

    p_attribute_list <- lapply(
      setNames(p_attributes, p_attributes),
      function(p_attr){
        message("decoding p-attribute:", p_attr)
        get_token_stream(.Object, p_attribute = p_attr)
      }
    )

    max_cpos <- size(.Object) - 1L
    s_attribute_list <- lapply(
      setNames(s_attributes, s_attributes),
      function(s_attr){
        message("decoding s-attribute:", s_attr)
        corpus_id <- get_corpus(.Object)
        struc <- cl_cpos2struc(corpus = corpus_id, s_attribute = s_attr, cpos = 0L:max_cpos, registry = registry())
        str <- cl_struc2str(corpus = corpus_id, s_attribute = s_attr, struc = struc, registry = registry())
        Encoding(str) <- encoding(.Object)
        as.nativeEnc(str, from = encoding(.Object))
      }
    )

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
setMethod("decode", "character", function(.Object, to = c("data.table", "Annotation"), s_attributes = NULL, p_attributes = NULL, ...){
  if (any(c("sAttribute", "s_attribute") %in% names(list(...)))){
    stop("Decoding an s_attribute is not supported any longer in the decode()-method of ",
         "the polmineR package. See s_attribute_decode in the RcppCWB package as a substitute.")
  }
  decode(corpus(.Object), to = to, s_attributes = s_attributes, p_attributes = p_attributes)
})





#' @exportMethod decode
#' @rdname decode
setMethod("decode", "slice", function(.Object, to = "data.table", s_attributes = NULL, p_attributes = NULL){
  if (to == "data.table"){
    
    if (is.null(p_attributes)) p_attributes <- p_attributes(.Object)
    if (!all(p_attributes %in% p_attributes(.Object))) stop("Not all p_attributes provided are available.")
    
    if (is.null(s_attributes)) s_attributes <- s_attributes(.Object)
    if (!all(s_attributes %in% s_attributes(.Object))) stop("Not all s_attributes provided are available.")

    ts <- lapply(
      setNames(p_attributes, p_attributes),
      function(p_attr){
        message("... decoding p_attribute ", p_attr)
        get_token_stream(.Object, p_attribute = p_attr)
      }
    )
    p_attr_dt <- as.data.table(ts)
    p_attr_dt[, "cpos" := unlist(apply(.Object@cpos, 1, function(row) row[1]:row[2]))]
    
    strucs <- RcppCWB::cl_cpos2struc(corpus = .Object@corpus, s_attribute = s_attributes[1], cpos = .Object@cpos[,1])
    
    dts <- lapply(
      strucs,
      function(struc){
        region <- RcppCWB::cl_struc2cpos(corpus = .Object@corpus, s_attribute = s_attributes[1], struc = struc)
        data.table(struc = struc, cpos_left = region[1], cpos_right = region[2])
      }
    )
    regions <- rbindlist(dts)
    
    s_attr_values <- lapply(
      setNames(s_attributes, s_attributes),
      function(s_attr){
        message("... decoding s_attribute ", s_attr)
        str <- RcppCWB::cl_struc2str(corpus = .Object@corpus, s_attribute = s_attr, struc = strucs)
        Encoding(str) <- encoding(.Object)
        as.nativeEnc(str, from = encoding(.Object))
      }
    )
    dt <- as.data.table(s_attr_values)
    
    s_attr_dt <- cbind(regions, dt)

    unfold <- function(.SD){
      dt <- data.table(cpos = .SD[["cpos_left"]]:.SD[["cpos_right"]])
      for (s_attr in s_attributes) dt[[s_attr]] <- rep(.SD[[s_attr]], times = nrow(dt))
      dt
    }
    s_attr_dt_ext <- s_attr_dt[, unfold(.SD), by = "struc"]
    
    setkeyv(p_attr_dt, cols = "cpos")
    setkeyv(s_attr_dt_ext, cols = "cpos")
    y <- p_attr_dt[s_attr_dt_ext]
    
  } else if (to == "Annotation"){
    y <- as(.Object, "Annotation")
  }
  y
})

#' @rdname decode
setMethod("decode", "partition", function(.Object, to = "data.table", s_attributes = NULL, p_attributes = NULL){
  callNextMethod()
})


#' @exportMethod decode
#' @rdname decode
setMethod("decode", "subcorpus", function(.Object, to = "data.table", s_attributes = NULL, p_attributes = NULL){
  callNextMethod()
})

