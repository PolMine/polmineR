#' @rdname decode
setGeneric("decode", function(.Object, ...) standardGeneric("decode"))


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
#' # Scenario 1: Decode one or two s-attributes
#' dt <- decode("GERMAPARLMINI", s_attribute = "date")
#' dt <- decode("GERMAPARLMINI", s_attribute = c("date", "speaker"))
#' 
#' # Scenario 2: Decode corpus entirely
#' dt <- decode("GERMAPARLMINI")
#' @exportMethod decode
#' @importFrom data.table fread
#' @importFrom RcppCWB get_region_matrix
setMethod("decode", "character", function(.Object, s_attribute = NULL, verbose = TRUE, ...){
  
  if ("sAttribute" %in% names(list(...))) s_attribute <- list(...)[["sAttribute"]]
  
  stopifnot(
    length(.Object) == 1L, # cannot process more than one corpus
    .Object %in% CQI$list_corpora() # make that corpus is available
    ) 
  
  if (!is.null(s_attribute)){
    
    stopifnot(
      s_attribute %in% s_attributes(.Object) # s-attribute needs to be available
      ) 

    regions <- get_region_matrix(
      .Object, s_attribute = s_attribute[1],
      strucs = 0L:(CQI$attribute_size(.Object, s_attribute[1]) - 1L),
      registry = Sys.getenv("CORPUS_REGISTRY")
    )
    y <- data.table(regions)
    colnames(y) <- c("cpos_left", "cpos_right")
    
    if (s_attribute[1] %in% colnames(y)) s_attribute <- s_attribute[-1]
    for (sAttr in s_attribute){
      .message("decoding s-attribute: ", sAttr)
      sAttrMax <- CQI$attribute_size(.Object, sAttr)
      if (sAttrMax != nrow(y)) stop(
        "s-attribute", sAttr, " has ", sAttrMax, " values, but s-s_attribute ",
        sAttr, " has only ", nrow(y), " - decode will only work for flat XML with strucs with identical length"
      )
      y[[sAttr]] <- s_attributes(.Object, s_attribute = sAttr, unique = FALSE)
    }
    
    return( y )
    
  } else {
    
    maxCpos <- CQI$attribute_size(.Object, "word", type = "p") - 1L
    pAttributeList <- lapply(
      p_attributes(.Object),
      function(x){
        .message("decoding p-attribute:", x, verbose = verbose)
        tokens <- get_token_stream(.Object, p_attribute = x)
        Encoding(tokens) <- registry_get_encoding(.Object)
        as.nativeEnc(tokens, from = registry_get_encoding(.Object))
      }
    )
    names(pAttributeList) <- p_attributes(.Object)
    
    sAttributeList <- lapply(
      s_attributes(.Object),
      function(x){
        .message("decoding s-attribute:", x, verbose = verbose)
        struc <- CQI$cpos2struc(.Object, x, 0L:maxCpos)
        str <- CQI$struc2str(.Object, x, struc)
        Encoding(str) <- registry_get_encoding(.Object)
        as.nativeEnc(str, from = registry_get_encoding(.Object))
      }
    )
    names(sAttributeList) <- s_attributes(.Object)
    
    
    .message("assembling data.table", verbose = verbose)
    combinedList <- c(
      list(cpos = 0L:maxCpos),
      pAttributeList,
      sAttributeList
    )
    y <- data.table::as.data.table(combinedList)
    
  }
  y
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
setMethod("decode", "partition", function(.Object){
  ts <- lapply(
    setNames(p_attributes(.Object), p_attributes(.Object)),
    function(p_attr){
      message("... decoding p_attribute ", p_attr)
      get_token_stream(.Object, p_attribute = p_attr)
    }
  )
  p_attr_dt <- as.data.table(ts)
  p_attr_dt[, "cpos" := unlist(apply(.Object@cpos, 1, function(row) row[1]:row[2]))]
  
  s_attrs <- s_attributes(.Object)
  strucs <- RcppCWB::cl_cpos2struc(corpus = .Object@corpus, s_attribute = s_attrs[1], cpos = .Object@cpos[,1])
  
  dts <- lapply(
    strucs,
    function(struc){
      region <- RcppCWB::cl_struc2cpos(corpus = .Object@corpus, s_attribute = s_attrs[1], struc = struc)
      data.table(struc = struc, cpos_left = region[1], cpos_right = region[2])
    }
  )
  regions <- rbindlist(dts)
  
  s_attr_values <- lapply(
    setNames(s_attrs, s_attrs),
    function(s_attr) RcppCWB::cl_struc2str(corpus = .Object@corpus, s_attribute = s_attr, struc = strucs)
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
  y <- p_attr_dt[s_attr_dt_ext]
  y
})
