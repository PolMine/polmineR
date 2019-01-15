#' @rdname decode
setGeneric("decode", function(.Object, ...) standardGeneric("decode"))


#' Decode structural attribute, partition or corpus.
#' 
#' Function that can be applied on a corpus or a \code{partition}. The returned
#' \code{data.table} can be coerced to a tibble easily and processed according
#' to tidytext approaches.
#' 
#' If a \code{s_attribute} is a character vector providing one or several
#' structural attributes, the return value is a \code{data.table} with the left
#' and right corpus positions in the first and second columns ("cpos_left" and
#' "cpos_right"). Values of further columns are the decoded s-attributes. The
#' name of the s-attribute is the column name. An error is thrown if the lengths
#' of structural attributes differ (i.e. if there is a nested data structure).
#'
#' If \code{s_attribute} is NULL, the token stream is decoded for all positional
#' attributes that are present. Structural attributes are reported in additional
#' columns. Decoding the entire corpus may be useful to make a transition to
#' processing data following the 'tidy' approach, or to manipulate the corpus
#' data and to re-encode the corpus.
#' @param .Object The corpus or partition to decode (character vector).
#' @param verbose Logical value, whether to output messages.
#' @param s_attribute The s-attribute to decode.
#' @param ... Further arguments.
#' @return The return value is a \code{data.table}. 
#' @rdname decode
#' @examples
#' \dontrun{
#' use("polmineR")
#' 
#' # Scenario 1: Decode one or two s-attributes
#' dt <- decode("REUTERS", s_attribute = "id")
#' dt <- decode("REUTERS", s_attribute = c("topics_cat", "places"))
#' 
#' # Scenario 2: Decode entire corpus
#' dt <- decode("REUTERS")
#' 
#' # Scenario 3: Decode partition
#' p <- partition("REUTERS", places = "kuwait", regex = TRUE)
#' dt <- decode(p)
#' 
#' # Scenario 4: Decode partition_bundle
#' pb <- partition_bundle("REUTERS", s_attribute = "id")
#' dts <- lapply(as.list(pb), decode)
#' dts <- lapply(names(dts), function(n) dts[[n]][, speech_id := n])
#' dt <- data.table::rbindlist(dts)
#' }
#' @exportMethod decode
#' @importFrom data.table fread
#' @importFrom RcppCWB get_region_matrix
decode <- function(.Object, s_attribute = NULL, verbose = TRUE, ...){
  
  if ("sAttribute" %in% names(list(...))) s_attribute <- list(...)[["sAttribute"]]
  
  is.corpus <- function(x) if (is.character(x)) if (x %in% CQI$list_corpora()) TRUE else FALSE else FALSE
  if (!(is.partition(.Object) || is.corpus(.Object))){
    stop(".Object needs to be a partition, or an available corpus.")
  }
  
  corpus_enc <- if (is.corpus(.Object)) registry_get_encoding(.Object) else .Object@encoding
  
  if (!is.null(s_attribute)){
    
    stopifnot(s_attribute %in% s_attributes(.Object)) # s-attribute needs to be available 
    
    if (is.corpus(.Object)){
      regions <- get_region_matrix(
        .Object, s_attribute = s_attribute[1],
        strucs = 0L:(CQI$attribute_size(.Object, s_attribute[1]) - 1L),
        registry = Sys.getenv("CORPUS_REGISTRY")
      )
      y <- data.table(regions)
    } else if (is.partition(.Object)){
      y <- as.data.table(.Object@cpos)
    }
    colnames(y) <- c("cpos_left", "cpos_right")

    if (s_attribute[1] %in% colnames(y)) s_attribute <- s_attribute[-1]
    for (s_attr in s_attribute){
      .message("decoding s-attribute:", s_attr)
      s_attr_decoded <- s_attributes(.Object, s_attribute = s_attr, unique = FALSE)
      if (length(s_attr_decoded) != nrow(y)){
        stop(
          "s-attribute", s_attr, " has ", length(s_attr_decoded), " values, but s-s_attribute ",
          s_attr, " has only ", nrow(y), " - decode will only work for flat XML with strucs with identical length"
        )
      }
      Encoding(s_attr_decoded) <- corpus_enc
      s_attr_decoded <- as.nativeEnc(s_attr_decoded, from = corpus_enc)
      y[, eval(s_attr) := s_attr_decoded]
    }
    return( y )
    
  } else {
    
    if (is.corpus(.Object)){
      max_cpos <- CQI$attribute_size(.Object, "word", type = "p") - 1L
    } else {
      cpos_vector <- as.vector(unlist(apply(.Object@cpos, 1, function(r) r[1]:r[2])))
    }
    
    p_attr_list <- lapply(
      p_attributes(.Object),
      function(p_attr){
        .message("decoding p-attribute:", p_attr, verbose = verbose)
        tokens <- get_token_stream(.Object, p_attribute = p_attr)
      }
    )
    names(p_attr_list) <- p_attributes(.Object)
    
    s_attr_list <- lapply(
      s_attributes(.Object),
      function(s_attr){
        .message("decoding s-attribute:", s_attr, verbose = verbose)
        struc <- CQI$cpos2struc(
          if (is.corpus(.Object)) .Object else .Object@corpus,
          s_attribute = s_attr,
          cpos = if (is.corpus(.Object)) 0L:max_cpos else cpos_vector
        )
        str <- CQI$struc2str(if (is.corpus(.Object)) .Object else .Object@corpus, s_attribute = s_attr, struc)
        Encoding(str) <- corpus_enc
        as.nativeEnc(str, from = corpus_enc)
      }
    )
    names(s_attr_list) <- s_attributes(.Object)

    .message("assembling data.table", verbose = verbose)
    lists <- c(
      list(cpos = if (is.corpus(.Object)) 0L:max_cpos else cpos_vector),
      p_attr_list,
      s_attr_list
    )
    y <- data.table::as.data.table(lists)
  }
  y
}


