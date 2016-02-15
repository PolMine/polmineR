setGeneric("sAttributes2cpos", function(.Object, ...) standardGeneric("sAttributes2cpos"))

#' Get the cpos for a partition based on sattributes
#' 
#' Augment the partition object
#' 
#' @param partition the rudimentary partition object
#' @return an augmented partition object (includes now cpos and strucs)
#' @noRd
setMethod("sAttributes2cpos", "partition", function(.Object, xml, regex){
  if (xml == "flat"){
    # The function works nicely - potentially, it can be optimized, but I have tried many things.
    # Interestingly, the for-loop is more effective than a vectorized version
    root <- paste(.Object@corpus, '.', .Object@sAttributeStrucs, sep='')
    meta <- data.frame(struc=c(0:(cqi_attribute_size(root)-1)), select=rep(0, times=cqi_attribute_size(root)))
    if (length(.Object@sAttributes) > 0) {
      for (s in names(.Object@sAttributes)){
        sattr <- paste(.Object@corpus, ".", s, sep="")
        meta[,2] <- as.vector(cqi_struc2str(sattr, meta[,1]))
        Encoding(meta[,2]) <- .Object@encoding
        if (regex==FALSE) {
          meta <- meta[which(meta[,2] %in% .Object@sAttributes[[s]]),]
        } else {
          lines <- lapply(.Object@sAttributes[[s]], function(x) grep(x, meta[,2]))
          meta <- meta[unique(unlist(lines)),]
        }
      }
      if (nrow(meta) == 0) {
        warning(paste("no strucs found for the values provided for s-attribute", s))
      }
    }
    if (nrow(meta) != 0){
      .Object@cpos <- matrix(
        data=unlist(lapply(meta[,1], function(x)cqi_struc2cpos(root, x))),
        ncol=2, byrow=TRUE
      )
      .Object@strucs <- as.numeric(meta[,1])
    } else {
      warning("returning a NULL object")
      .Object <- NULL    
    }
  } else if (xml == "nested"){
    sAttr <- vapply(
      names(.Object@sAttributes),
      USE.NAMES=TRUE, FUN.VALUE="character",
      function(x) paste(.Object@corpus, '.', x, sep='')
    )
    sAttr <- rev(sAttr)
    strucs <- c(0:(cqi_attribute_size(sAttr[1])-1))
    metaVector <- cqi_struc2str(sAttr[1], strucs)
    Encoding(metaVector) <- .Object@encoding
    if (regex == FALSE) {
      strucs <- strucs[which(metaVector %in% .Object@sAttributes[[length(.Object@sAttributes)]])]
    } else {
      positions <- lapply(.Object@sAttributes[[length(.Object@sAttributes)]], function(x) grep(x, metaVector))
      strucs <- strucs[unique(unlist(positions))]
    }
    cpos <- matrix(
      unlist(lapply(strucs, function(x) cqi_struc2cpos(sAttr[1], x))),
      byrow=TRUE, ncol=2
    )
    if (length(sAttr) > 1){
      for (i in c(2:length(sAttr))){
        meta <- cqi_struc2str(sAttr[i], cqi_cpos2struc(sAttr[i], cpos[,1]))
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

