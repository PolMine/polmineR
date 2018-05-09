setGeneric("sAttributes2cpos", function(.Object, ...) standardGeneric("sAttributes2cpos"))

#' Get corpus position matrix.
#' 
#' Based on the sAttributes as defined in the respective slot of the partition
#' object, a corpus position matrix is generated, i.e. the slot \code{cpos}
#' of the object is filled.
#' 
#' @param partition partition object with list of s-attributes
#' @param xml either "flat" or "nested"
#' @param regex logical
#' @return An augmented partition object, with cpos and strucs slots filled.
#' @noRd
setMethod("sAttributes2cpos", "partition", function(.Object, xml = "flat", regex = FALSE){
  stopifnot(xml %in% c("flat", "nested"))
  stopifnot(is.logical(regex))
  if (xml == "flat"){
    # The function works nicely - potentially, it can be optimized, but I have tried many things.
    # Interestingly, the for-loop is more effective than a vectorized version
    # an Rcpp-implementation of struc2str is not faster
    # potential for optimization: struc2str
    maxAttr <- CQI$attribute_size(.Object@corpus, .Object@sAttributeStrucs, type = "s")
    meta <- data.frame(struc = 0:(maxAttr - 1), select = rep(0, times = maxAttr))
    if (length(.Object@sAttributes) > 0) {
      for (sAttr in names(.Object@sAttributes)){
        meta[,2] <- as.vector(CQI$struc2str(.Object@corpus, sAttr, meta[,1]))
        Encoding(meta[,2]) <- .Object@encoding
        if (regex == FALSE) {
          meta <- meta[which(meta[,2] %in% .Object@sAttributes[[sAttr]]),]
        } else {
          lines <- lapply(.Object@sAttributes[[sAttr]], function(x) grep(x, meta[,2]))
          meta <- meta[unique(unlist(lines)),]
        }
      }
      if (nrow(meta) == 0) {
        warning(paste("no strucs found for the values provided for s-attribute", sAttr))
      }
    }
    if (nrow(meta) != 0){
      if (requireNamespace("RcppCWB", quietly = TRUE)){
        .Object@cpos <- RcppCWB::get_region_matrix(
          corpus = .Object@corpus, s_attribute = .Object@sAttributeStrucs, strucs = meta[,1]
          )
      } else {
        .Object@cpos <- matrix(
          data = unlist(lapply(meta[,1], function(x) CQI$struc2cpos(.Object@corpus, .Object@sAttributeStrucs, x))),
          ncol = 2, byrow = TRUE
        )
      }
      .Object@strucs <- as.numeric(meta[,1])
    } else {
      warning("returning a NULL object")
      .Object <- NULL    
    }
  } else if (xml == "nested"){
    sAttrNames <- rev(names(.Object@sAttributes))
    strucs <- 0:(CQI$attribute_size(.Object@corpus, sAttrNames[1], type = "s") - 1)
    sAttrValues <- CQI$struc2str(.Object@corpus, sAttrNames[1], strucs)
    Encoding(sAttrValues) <- .Object@encoding
    if (regex == FALSE) {
      strucs <- strucs[ which(sAttrValues %in% .Object@sAttributes[[ sAttrNames[1] ]]) ]
    } else {
      matchList <- lapply(.Object@sAttributes[[ sAttrNames[1] ]], function(x) grep(x, sAttrValues))
      strucs <- strucs[ unique(unlist(matchList)) ]
    }
    
    # turn strucs into cpos matrix, using RcppCWB, if available
    if (requireNamespace("RcppCWB", quietly = TRUE)){
      cpos <- RcppCWB::get_region_matrix(
        corpus = .Object@corpus, s_attribute = sAttrNames[1],
        registry = Sys.getenv("CORPUS_REGISTRY"), strucs = strucs
      )
    } else {
      cpos <- matrix(
        unlist(lapply(strucs, function(x) CQI$struc2cpos(.Object@corpus, sAttrNames[1], x))),
        byrow = TRUE, ncol = 2
      )
    }
    
    if (length(sAttrNames) > 1){
      for (i in 2:length(sAttrNames)){
        sAttrValues <- CQI$struc2str(.Object@corpus, sAttrNames[i], CQI$cpos2struc(.Object@corpus, sAttrNames[i], cpos[,1]))
        Encoding(sAttrValues) <- .Object@encoding
        if (regex == FALSE) {
          hits <- which(sAttrValues %in% .Object@sAttributes[[ sAttrNames[i] ]])
        } else if (regex == TRUE) {
          hits <- unique(unlist(lapply(.Object@sAttributes[[ sAttrNames[i] ]], function(x) grep(x, sAttrValues))))
        }
        cpos <- cpos[hits,]
        strucs <- strucs[hits]
      }
    }
    .Object@strucs <- strucs
    .Object@cpos <- cpos
  }
  .Object
})

