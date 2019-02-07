#' @rdname decode
setGeneric("decode", function(.Object, ...) standardGeneric("decode"))


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
  
  max_cpos <- CQI$attribute_size(from, "word", type = "p") - 1L
  s_attribute_list <- lapply(
    s_attributes(from),
    function(x){
      message("decoding s-attribute:", x)
      struc <- CQI$cpos2struc(from, x, 0L:max_cpos)
      str <- CQI$struc2str(from, x, struc)
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

#' Decode Structural Attribute or Entire Corpus.
#' 
#' If a \code{s_attribute} is a character vector providing one or several structural attributes,
#' the return value is a \code{data.table} with the left and right corpus positions in the first
#' and second columns ("cpos_left" and "cpos_right"). Values of further columns are the decoded
#' s-attributes. The name of the s-attribute is the column name. An error is thrown if the
#' lengths of structural attributes differ (i.e. if there is a nested data structure).
#' 
#' If \code{s_attribute} is NULL, the token stream is decoded for all positional attributes that
#' are present. Structural attributes are reported in additional columns. Decoding the entire
#' corpus may be useful to make a transition to processing data following the 'tidy' approach,
#' or to manipulate the corpus data and to re-encode the corpus.
#' 
#' The return value is a \code{data.table}. 
#' 
#' @param .Object the corpus to decode (character vector)
#' @param verbose logical
#' @param s_attribute the s-attribute to decode
#' @param ... further parameters
#' @return a \code{data.table}
#' @rdname decode
#' @examples
#' use("polmineR")
#' 
#' # Decode corpus entirely
#' dt <- decode("GERMAPARLMINI")
#' @exportMethod decode
#' @importFrom data.table fread
#' @importFrom RcppCWB get_region_matrix
#' @seealso To decode a structural attribute, see \code{\link[RcppCWB]{s_attribute_decode}}.
setMethod("decode", "character", function(.Object, to = "data.table", ...){
  
  if (any(c("sAttribute", "s_attribute") %in% names(list(...)))){
    stop("Decoding an s_attribute is not supported any longer in the decode()-method of ",
         "the polmineR package. See s_attribute_decode in the RcppCWB package as a substitute.")
  }
  
  stopifnot(
    length(.Object) == 1L, # cannot process more than one corpus
    .Object %in% CQI$list_corpora() # make that corpus is available
  )
  
  as(.Object, to)
  
})


setAs(from = "partition", to = "data.table", def = function(from){
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
#' @examples
#' \dontrun{
#' P <- partition("REUTERS", places = "kuwait", regex = TRUE)
#' dt <- decode(P)
#' dt[, "word" := NULL]
#' dt[,{list(cpos_left = min(.SD[["cpos"]]), cpos_right = max(.SD[["cpos"]]), id = unique(.SD[["id"]]))}, by = "struc"]
#' }
setMethod("decode", "partition", function(.Object, to = "data.table"){
  as(.Object, to)
})
