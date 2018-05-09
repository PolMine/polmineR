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
#' @param sAttribute the s-attribute to decode
#' @param ... further parameters
#' @return a \code{data.table}
#' @rdname decode
#' @examples
#' use("polmineR")
#' 
#' # Scenario 1: Decode one or two s-attributes
#' dt <- decode("GERMAPARLMINI", sAttribute = "date")
#' dt <- decode("GERMAPARLMINI", sAttribute = c("date", "speaker"))
#' 
#' # Scenario 2: Decode corpus entirely
#' dt <- decode("GERMAPARLMINI")
#' @exportMethod decode
#' @importFrom data.table fread
#' @importFrom RcppCWB get_region_matrix
setMethod("decode", "character", function(.Object, sAttribute = NULL, verbose = TRUE){
  
  stopifnot(
    length(.Object) == 1, # cannot process more than one corpus
    .Object %in% CQI$list_corpora() # make that corpus is available
    ) 
  
  if (!is.null(sAttribute)){
    
    stopifnot(
      sAttribute %in% sAttributes(.Object) # s-attribute needs to be available
      ) 

    regions <- get_region_matrix(
      .Object, s_attribute = sAttribute[1],
      strucs = 0L:(CQI$attribute_size(.Object, sAttribute[1]) - 1L),
      registry = Sys.getenv("CORPUS_REGISTRY")
    )
    y <- data.table(regions)
    colnames(y) <- c("cpos_left", "cpos_right")
    
    if (sAttribute[1] %in% colnames(y)) sAttribute <- sAttribute[-1]
    for (sAttr in sAttribute){
      .message("decoding s-attribute: ", sAttr)
      sAttrMax <- CQI$attribute_size(.Object, sAttr)
      if (sAttrMax != nrow(y)) stop(
        "s-attribute", sAttr, " has ", sAttrMax, " values, but s-sattribute ",
        sAttr, " has only ", nrow(y), " - decode will only work for flat XML with strucs with identical length"
      )
      y[[sAttr]] <- sAttributes(.Object, sAttribute = sAttr, unique = FALSE)
    }
    
    return( y )
    
  } else {
    
    maxCpos <- CQI$attribute_size(.Object, "word", type = "p") - 1L
    pAttributeList <- lapply(
      pAttributes(.Object),
      function(x){
        .message("decoding p-attribute:", x, verbose = verbose)
        tokens <- getTokenStream(.Object, pAttribute = x)
        Encoding(tokens) <- getEncoding(.Object)
        as.nativeEnc(tokens, from = getEncoding(.Object))
      }
    )
    names(pAttributeList) <- pAttributes(.Object)
    
    sAttributeList <- lapply(
      sAttributes(.Object),
      function(x){
        .message("decoding sAttribute:", x, verbose = verbose)
        struc <- CQI$cpos2struc(.Object, x, 0L:maxCpos)
        str <- CQI$struc2str(.Object, x, struc)
        Encoding(str) <- getEncoding(.Object)
        as.nativeEnc(str, from = getEncoding(.Object))
      }
    )
    names(sAttributeList) <- sAttributes(.Object)
    
    
    .message("assembling data.table", verbose = verbose)
    combinedList <- c(
      list(cpos = 0L:maxCpos),
      pAttributeList,
      sAttributeList
    )
    y <- as.data.table(combinedList)
    
  }
  y
})