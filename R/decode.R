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


setAs(from = "character", to = "data.table", def = function(from){
  
  p_attribute_list <- lapply(
    p_attributes(from),
    function(x){
      message("decoding p-attribute:", x)
      tokens <- get_token_stream(from, p_attribute = x)
      Encoding(tokens) <- registry_get_encoding(from)
      as.nativeEnc(tokens, from = registry_get_encoding(from))
    }
  )
  names(p_attribute_list) <- p_attributes(from)
  
  max_cpos <- cl_attribute_size(corpus = from, attribute = "word", attribute_type = "p", registry = registry()) - 1L
  s_attribute_list <- lapply(
    s_attributes(from),
    function(x){
      message("decoding s-attribute:", x)
      struc <- cl_cpos2struc(corpus = from, s_attribute = x, cpos = 0L:max_cpos, registry = registry())
      str <- cl_struc2str(corpus = from, s_attribute = x, struc = struc, registry = registry())
      Encoding(str) <- registry_get_encoding(from)
      as.nativeEnc(str, from = registry_get_encoding(from))
    }
  )
  names(s_attribute_list) <- s_attributes(from)
  
  message("assembling data.table")
  combinedList <- c(
    list(cpos = 0L:max_cpos),
    p_attribute_list,
    s_attribute_list
  )
  data.table::as.data.table(combinedList)
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
#' # Decode a subcorpus
#' sc <- subset(corpus("GERMAPARLMINI"), speaker == "Angela Dorothea Merkel")
#' dt <- decode(sc, to = "data.table")
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
setMethod("decode", "character", function(.Object, to = c("data.table", "Annotation"), ...){
  
  if (any(c("sAttribute", "s_attribute") %in% names(list(...)))){
    stop("Decoding an s_attribute is not supported any longer in the decode()-method of ",
         "the polmineR package. See s_attribute_decode in the RcppCWB package as a substitute.")
  }
  
  stopifnot(
    length(.Object) == 1L, # cannot process more than one corpus
    .Object %in% .list_corpora() # ensure that corpus is available
  )
  
  as(.Object, to)
  
})


setAs(from = "subcorpus", to = "data.table", def = function(from){
  ts <- lapply(
    setNames(p_attributes(from), p_attributes(from)),
    function(p_attr){
      message("... decoding p_attribute ", p_attr)
      get_token_stream(from, p_attribute = p_attr)
    }
  )
  p_attr_dt <- as.data.table(ts)
  p_attr_dt[, "cpos" := unlist(apply(from@cpos, 1, function(row) row[1]:row[2]))]
  
  s_attrs <- s_attributes(from)
  strucs <- RcppCWB::cl_cpos2struc(corpus = from@corpus, s_attribute = s_attrs[1], cpos = from@cpos[,1])
  
  dts <- lapply(
    strucs,
    function(struc){
      region <- RcppCWB::cl_struc2cpos(corpus = from@corpus, s_attribute = s_attrs[1], struc = struc)
      data.table(struc = struc, cpos_left = region[1], cpos_right = region[2])
    }
  )
  regions <- rbindlist(dts)
  
  s_attr_values <- lapply(
    setNames(s_attrs, s_attrs),
    function(s_attr) RcppCWB::cl_struc2str(corpus = from@corpus, s_attribute = s_attr, struc = strucs)
  )
  dt <- as.data.table(s_attr_values)
  
  s_attr_dt <- cbind(regions, dt)
  
  
  unfold <- function(.SD){
    dt <- data.table(cpos = .SD[["cpos_left"]]:.SD[["cpos_right"]])
    for (s_attr in s_attrs) dt[[s_attr]] <- rep(.SD[[s_attr]], times = nrow(dt))
    dt
  }
  s_attr_dt_ext <- s_attr_dt[, unfold(.SD), by = "struc"]
  
  setkeyv(p_attr_dt, cols = "cpos")
  setkeyv(s_attr_dt_ext, cols = "cpos")
  p_attr_dt[s_attr_dt_ext]
})


#' @exportMethod decode
#' @rdname decode
setMethod("decode", "slice", function(.Object, to = "data.table"){
  as(.Object, to)
})

#' @rdname decode
setMethod("decode", "partition", function(.Object, to = "data.table"){
  as(as(.Object, "subcorpus"), to)
})

