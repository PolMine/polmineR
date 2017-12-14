#' @rdname decode
setGeneric("decode", function(.Object, ...) standardGeneric("decode"))


#' Decode corpus or s-attribute.
#' 
#' Decode either a corpus, or a s-attribute.
#' 
#' @param .Object the corpus to decode (character vector)
#' @param verbose logical
#' @param sAttribute the s-attribute to decode
#' @param ... further parameters
#' @return a \code{data.table}
#' @rdname decode
#' @examples
#' \dontrun{
#' use("polmineR.sampleCorpus")
#' 
#' # Scenario 1: Decode one s-attribute
#' dt <- decode("PLPRBTTXT", sAttribute = "text_date")
#' 
#' # Scenario 2: Decode corpus entirely
#' dt <- decode("PLPRBTTXT")
#' 
#' }
#' @exportMethod decode
#' @seealso \code{\link{encode}}
#' @importFrom data.table fread
setMethod("decode", "character", function(.Object, sAttribute = NULL, verbose = TRUE){
  
  stopifnot(.Object %in% CQI$list_corpora()) # ensure that corpus is available
  
  if (!is.null(sAttribute)){
    
    stopifnot(sAttribute %in% sAttributes(.Object)) 
    
    if (requireNamespace("polmineR.Rcpp", quietly = TRUE)){
      
      .message("using polmineR.Rcpp to get regions", verbose = verbose)
      regions <- polmineR.Rcpp::get_region_matrix(
        .Object, s_attribute = sAttribute[1],
        strucs = 0:(CQI$attribute_size(.Object, sAttribute[1]) - 1),
        registry = Sys.getenv("CORPUS_REGISTRY")
      )
      y <- data.table(regions)
      colnames(y) <- c("cpos_left", "cpos_right")
    } else if (getOption("polmineR.cwb-s-decode")){
      .message("run cwb-s-decode to get corpus positions", verbose = verbose)
      tmpfile <- tempfile()
      cmd <- c(
        "cwb-s-decode",
        "-r", Sys.getenv("CORPUS_REGISTRY"), .Object,
        "-S", sAttribute[1],
        ">", tmpfile
      )
      system(paste(cmd, collapse = " ", sep = " "), intern = FALSE)
      
      .message("read in result from tmpfile", verbose = verbose)
      y <- data.table::fread(input = tmpfile, sep = "\t", header = FALSE)
      
      .message("consolidate table", verbose = verbose)
      colnames(y) <- c("cpos_left", "cpos_right", sAttribute[1])
      y[, "cpos_left" := as.integer(y[["cpos_left"]])]
      y[, "cpos_right" := as.integer(y[["cpos_right"]])]
      Encoding(y[[sAttribute[1]]]) <- getEncoding(.Object)
      y[[ sAttribute[1] ]] <- as.nativeEnc(x = y[[sAttribute [1] ]], from = getEncoding(.Object))
      
    } else {
      .message("... getting corpus positions for strucs")
      regionList <- lapply(
        0:(CQI$attribute_size(.Object, sAttribute[1]) - 1),
        function(i) CQI$struc2cpos(.Object, sAttribute[1], i)
      )
      regions <- do.call(cbind, regionList)
      y <- data.table(t(regions))
      colnames(y) <- c("cpos_left", "cpos_right")
    }
    
    if (sAttribute[1] %in% colnames(y)) sAttribute <- sAttribute[-1]
    for (sAttr in sAttribute){
      .message("decoding s-attribute", sAttr)
      sAttrMax <- CQI$attribute_size(.Object, sAttr)
      if (sAttrMax != nrow(y)) stop(
        "s-attribute", sAttr, " has ", sAttrMax, " values, but s-sattribute ",
        sAttr, " has only ", nrow(y), " - decode will only work for flat XML with strucs with identical length"
      )
      y[[sAttr]] <- sAttributes(.Object, sAttribute = sAttr, unique = FALSE)
    }
    
    return( y )
    
  } else {
    
    maxCpos <- CQI$attribute_size(.Object, "word", type = "p") - 1
    pAttributeList <- lapply(
      pAttributes(.Object),
      function(x){
        .message("decoding pAttribute", x, verbose = verbose)
        tokens <- getTokenStream(.Object, pAttribute = x)
        Encoding(tokens) <- getEncoding(.Object)
        as.nativeEnc(tokens, from = getEncoding(.Object))
      }
    )
    names(pAttributeList) <- pAttributes(.Object)
    
    sAttributeList <- lapply(
      sAttributes(.Object),
      function(x){
        .message("decoding sAttribute ", x, verbose = verbose)
        struc <- CQI$cpos2struc(.Object, x, 0:maxCpos)
        str <- CQI$struc2str(.Object, x, struc)
        Encoding(str) <- getEncoding(.Object)
        as.nativeEnc(str, from = getEncoding(.Object))
      }
    )
    names(sAttributeList) <- sAttributes(.Object)
    
    
    .message("assembling data.table", verbose)
    combinedList <- c(
      list(cpos = 0:maxCpos),
      pAttributeList,
      sAttributeList
    )
    y <- as.data.table(combinedList)
    
  }
  y
})