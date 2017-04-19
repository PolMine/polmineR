setGeneric("sAttributes2cpos", function(.Object, ...) standardGeneric("sAttributes2cpos"))

#' Get corpus positions based on s-attributes.
#' 
#' Based on the sAttributes as defined in the respective slot of the partition
#' object, corpus positions (matrix in the cpos slot) is generated.
#' 
#' @param partition the rudimentary partition object
#' @param xml either "flat" or "nested"
#' @param regex logical
#' @return An augmented partition object, with cpos and strucs slots filled.
#' @noRd
setMethod("sAttributes2cpos", "partition", function(.Object, xml, regex){
  if (xml == "flat"){
    # The function works nicely - potentially, it can be optimized, but I have tried many things.
    # Interestingly, the for-loop is more effective than a vectorized version
    # an Rcpp-implementation of struc2str is not faster
    # potential for optimization: struc2str
    maxAttr <- CQI$attribute_size(.Object@corpus, .Object@sAttributeStrucs, type = "s")
    meta <- data.frame(struc = c(0:(maxAttr-1)), select = rep(0, times = maxAttr))
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
      if (requireNamespace("polmineR.Rcpp", quietly = TRUE) && getOption("polmineR.Rcpp") == TRUE){
        .Object@cpos <- polmineR.Rcpp::getRegionMatrix(.Object@corpus, .Object@sAttributeStrucs, meta[,1])
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
    sAttr <- vapply(
      names(.Object@sAttributes),
      USE.NAMES = TRUE, FUN.VALUE = "character",
      function(x) paste(.Object@corpus, '.', x, sep = '')
    )
    sAttr <- rev(sAttr)
    strucs <- c(0:(CQI$attribute_size(.Object@corpus, names(.Object@sAttributes)[1], type = "s")-1))
    metaVector <- CQI$struc2str(.Object@corpus, names(.Object@sAttributes)[1], strucs)
    Encoding(metaVector) <- .Object@encoding
    if (regex == FALSE) {
      strucs <- strucs[which(metaVector %in% .Object@sAttributes[[length(.Object@sAttributes)]])]
    } else {
      positions <- lapply(.Object@sAttributes[[length(.Object@sAttributes)]], function(x) grep(x, metaVector))
      strucs <- strucs[unique(unlist(positions))]
    }
    cpos <- matrix(
      unlist(lapply(strucs, function(x) CQI$struc2cpos(.Object@corpus, names(.Object@sAttributes)[1], x))),
      byrow=TRUE, ncol=2
    )
    if (length(sAttr) > 1){
      for (i in c(2:length(sAttr))){
        meta <- CQI$struc2str(.Object@corpus, names(.Object@sAttributes)[i], CQI$cpos2struc(.Object@corpus, names(.Object@sAttributes)[i], cpos[,1]))
        Encoding(meta) <- .Object@encoding
        if (regex == FALSE) {
          hits <- which(meta %in% .Object@sAttributes[[names(sAttr)[i]]])
        } else if (regex == TRUE) {
          hits <- unique(unlist(lapply(.Object@sAttributes[[names(sAttr)[i]]], function(x) grep(x, meta))))
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

