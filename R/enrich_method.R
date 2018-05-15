#' @include partition_class.R partitionBundle_class.R context_class.R contextBundle_class.R
#' @include features_class.R
NULL

#' Enrich an object.
#' 
#' Methods to enrich objects with additional (statistical) information. The methods are documented
#' with the classes to which they adhere. See the references in the \code{seealso}-section.
#' @param .Object a partition, partitionBundle or comp object
#' @param ... further parameters
#' @aliases enrich enrich-method
#' @docType methods
#' @rdname enrich-method
#' @seealso The enrich method is defined for the following classes:
#' "partition", (see \code{\link{partition_class}}),
#' "partitionBundle" (see \code{\link{partitionBundle-class}}),
#' "kwic" (see \code{\link{kwic-class}}), and
#' "context" (see \code{\link{context-class}}). See the linked documentation
#' to learn how the enrich method can be applied to respective objects.
setGeneric("enrich", function(.Object, ...){standardGeneric("enrich")})

#' @param size logical
#' @param mc logical or, if numeric, providing the number of cores
#' @param decode logical
#' @exportMethod enrich
#' @docType methods
#' @rdname partition_class
setMethod("enrich", "partition", function(.Object, size = FALSE, pAttribute = NULL, decode = TRUE, verbose = TRUE, mc = FALSE, ...){
  if (size) .Object@size <- size(.Object)
  if (!is.null(pAttribute)) {
    stopifnot(is.character(pAttribute) == TRUE, length(pAttribute) <= 2, all(pAttribute %in% pAttributes(.Object)))
    .message('getting counts for p-attribute(s):', paste(pAttribute, collapse = ", "), verbose = verbose)  
    .Object@stat <- count(.Object = .Object, pAttribute = pAttribute, decode = decode, mc = mc, verbose = verbose)
    .Object@pAttribute <- pAttribute
  }
  .Object
})

#' @param mc logical or, if numeric, providing the number of cores
#' @param progress logical
#' @param verbose logical
#' @exportMethod enrich
#' @docType methods
#' @rdname partitionBundle-class
setMethod("enrich", "partitionBundle", function(.Object, mc = FALSE, progress = TRUE, verbose = FALSE, ...){
  blapply(x = .Object, f = enrich, mc = mc, progress = progress, verbose = verbose, ...)  
})


#' @rdname kwic-class
setMethod("enrich", "kwic", function(.Object, meta = NULL, table = FALSE){
  if (length(meta) > 0){
    metainformation <- lapply(
      meta,
      function(metadat){
        cposToGet <- .Object@cpos[which(.Object@cpos[["position"]] == 0)][, .SD[1], by = "hit_no", with = TRUE][["cpos"]]
        strucs <- CQI$cpos2struc(.Object@corpus, metadat, cposToGet)
        as.nativeEnc(CQI$struc2str(.Object@corpus, metadat, strucs), from = .Object@encoding)
      }
    )
    metainformation <- data.frame(metainformation, stringsAsFactors = FALSE)
    colnames(metainformation) <- meta
    .Object@table <- data.frame(metainformation, .Object@table)
    .Object@metadata <- c(meta, .Object@metadata)
  }
  
  if (table){
    if (nrow(.Object@cpos) > 0){
      .paste <- function(.SD) paste(.SD[["word"]], collapse = " ")
      DT2 <- .Object@cpos[, .paste(.SD), by = c("hit_no", "direction"), with = TRUE]
      tab <- dcast(data = DT2, formula = hit_no ~ direction, value.var = "V1")
      setnames(tab, old = c("-1", "0", "1"), new = c("left", "node", "right"))
    } else {
      tab <- data.table(hit_no = integer(), left = character(), node = character(), right = character())
    }
    .Object@table <- as.data.frame(tab)
  }
  
  .Object
})

#' @details The \code{enrich}-method can be used to add additional information to the \code{data.table}
#' in the "cpos"-slot of a \code{context}-object.
#' 
#' @exportMethod enrich
#' @docType methods
#' @rdname context-class
#' @param sAttribute s-attribute(s) to add to data.table in cpos-slot
#' @param pAttribute p-attribute(s) to add to data.table in cpos-slot
#' @param decode logical, whether to convert integer ids to expressive strings
#' @param verbose logical, whether to be talkative
setMethod("enrich", "context", function(.Object, sAttribute = NULL, pAttribute = NULL, decode = FALSE, verbose = TRUE){
  if (!is.null(sAttribute)){
    # check that all s-attributes are available
    .message("checking that all s-attributes are available", verbose = verbose)
    stopifnot( all(sAttribute %in% CQI$attributes(.Object@corpus, type = "s")) )
    
    for (sAttr in sAttribute){
      .message("get struc for s-attribute:", sAttr, verbose = verbose)
      strucs <- CQI$cpos2struc(.Object@corpus, sAttr, .Object@cpos[["cpos"]])
      if (decode == FALSE){
        colname_struc <- paste(sAttr, "int", sep = "_")
        if (colname_struc %in% colnames(.Object@cpos)){
          .message("already present, skipping assignment of column:", colname_struc, verbose = verbose)
        } else {
          .Object@cpos[[colname_struc]] <- strucs
        }
      } else {
        if (sAttr %in% colnames(.Object@cpos)){
          .message("already present, skipping assignment of column:", sAttr, verbose = verbose)
        } else {
          .message("get string for s-attribute:", sAttr, verbose = verbose)
          strings <- CQI$struc2str(.Object@corpus, sAttr, strucs)
          .Object@cpos[[sAttr]] <- as.nativeEnc(strings, from = .Object@encoding)
        }
      }
    }
  }
  if (!is.null(pAttribute)){
    # check that all p-attributes are available
    .message("checking that all p-attributes are available", verbose = verbose)
    stopifnot( all(pAttribute %in% CQI$attributes(.Object@corpus, type = "p")) )
    
    # add ids
    for (pAttr in pAttribute){
      colname <- paste(pAttr, "id", sep = "_")
      if (colname %in% colnames(.Object@cpos)){
        .message("already present - skip getting ids for p-attribute:", pAttr, verbose = verbose)
      } else {
        .message("getting token id for p-attribute:", pAttr, verbose = verbose)
        ids <- CQI$cpos2id(.Object@corpus, pAttr, .Object@cpos[["cpos"]])
        .message("assigning to data.table", verbose = verbose)
        .Object@cpos[[colname]] <- ids
      }
    }
    
    # add 
    if (decode){
      for (pAttr in pAttribute){
        if (pAttr %in% colnames(.Object@cpos)){
          .message("already present - skip getting strings for p-attribute:", pAttr, verbose = verbose)
        } else {
          .message("decode p-attribute:", pAttr, verbose = verbose)
          decoded <- CQI$id2str(.Object@corpus, pAttr, .Object@cpos[[paste(pAttr, "id", sep = "_")]])
          .Object@cpos[[pAttr]] <- as.nativeEnc(decoded, from = .Object@encoding)
          .Object@cpos[[paste(pAttr, "id", sep = "_")]] <- NULL
        }
      }
    }
    
  }
  .Object
})