#' @include partition.R partition_bundle.R context.R S4classes.R
#' @include features.R
NULL

#' Enrich an object.
#' 
#' Methods to enrich objects with additional (statistical) information. The methods are documented
#' with the classes to which they adhere. See the references in the \code{seealso}-section.
#' @param .Object a \code{partition}, \code{partition_bundle} or comp object
#' @param ... further parameters
#' @aliases enrich enrich-method
#' @docType methods
#' @rdname enrich-method
#' @seealso The enrich method is defined for the following classes:
#' "partition", (see \code{\link{partition_class}}),
#' "partition_bundle" (see \code{\link{partition_bundle-class}}),
#' "kwic" (see \code{\link{kwic-class}}), and
#' "context" (see \code{\link{context-class}}). See the linked documentation
#' to learn how the enrich method can be applied to respective objects.
setGeneric("enrich", function(.Object, ...){standardGeneric("enrich")})

#' @details The \code{enrich}-method will add a count of tokens defined by argument
#' \code{p_attribute} to slot \code{stat} of the \code{partition} object.
#' @param mc \code{logical} or, if numeric, providing the number of cores
#' @param decode \code{logical} value, whether to decode token ids into strings when performing count
#' @exportMethod enrich
#' @docType methods
#' @rdname partition_class
setMethod("enrich", "partition", function(.Object, p_attribute = NULL, decode = TRUE, verbose = TRUE, mc = FALSE, ...){
  if ("pAttribute" %in% names(list(...))) p_attribute <- list(...)[["pAttribute"]]
  
  if (!is.null(p_attribute)) {
    stopifnot(is.character(p_attribute) == TRUE, length(p_attribute) <= 2, all(p_attribute %in% p_attributes(.Object)))
    .message('getting counts for p-attribute(s):', paste(p_attribute, collapse = ", "), verbose = verbose)  
    .Object@stat <- count(.Object = .Object, p_attribute = p_attribute, decode = decode, mc = mc, verbose = verbose)@stat
    .Object@p_attribute <- p_attribute
  }
  .Object
})

#' @param mc logical or, if numeric, providing the number of cores
#' @param progress logical
#' @param verbose logical
#' @exportMethod enrich
#' @docType methods
#' @rdname partition_bundle-class
setMethod("enrich", "partition_bundle", function(.Object, mc = FALSE, progress = TRUE, verbose = FALSE, ...){
  blapply(x = .Object, f = enrich, mc = mc, progress = progress, verbose = verbose, ...)  
})


#' @details The \code{enrich} method is used to generate the actual output for
#' the kwic method. If param \code{table} is \code{TRUE}, corpus positions will
#' be turned into a data.frame with the concordance lines. If param \code{s_attributes}
#' is a character vector with s-attributes, the respective s-attributes will be
#' added as columns to the table with concordance lines.
#' @rdname kwic-class
setMethod("enrich", "kwic", function(.Object, s_attributes = NULL, table = FALSE, ...){
  
  if ("meta" %in% names(list(...))) s_attributes <- list(...)[["meta"]]
  
  if (length(s_attributes) > 0L){
    metainformation <- lapply(
      s_attributes,
      function(metadat){
        cposToGet <- .Object@cpos[which(.Object@cpos[["position"]] == 0)][, .SD[1], by = "hit_no", with = TRUE][["cpos"]]
        strucs <- CQI$cpos2struc(.Object@corpus, metadat, cposToGet)
        strucs_invalid <- which(strucs < 0)
        if (length(strucs_invalid) > 0) strucs[strucs_invalid] <- 0
        struc_values <- CQI$struc2str(.Object@corpus, metadat, strucs)
        if (length(strucs_invalid) > 0) struc_values[strucs_invalid] <- ""
        as.nativeEnc(struc_values, from = .Object@encoding)
      }
    )
    metainformation <- data.frame(metainformation, stringsAsFactors = FALSE)
    colnames(metainformation) <- s_attributes
    .Object@table <- data.frame(metainformation, .Object@table)
    .Object@metadata <- c(s_attributes, .Object@metadata)
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
#' @param s_attribute s-attribute(s) to add to data.table in cpos-slot
#' @param p_attribute p-attribute(s) to add to data.table in cpos-slot
#' @param decode logical, whether to convert integer ids to expressive strings
#' @param verbose logical, whether to be talkative
setMethod("enrich", "context", function(.Object, s_attribute = NULL, p_attribute = NULL, decode = FALSE, verbose = TRUE, ...){
  
  if ("pAttribute" %in% names(list(...))) p_attribute <- list(...)[["pAttribute"]]
  if ("sAttribute" %in% names(list(...))) s_attribute <- list(...)[["sAttribute"]]
  
  if (!is.null(s_attribute)){
    # check that all s-attributes are available
    .message("checking that all s-attributes are available", verbose = verbose)
    stopifnot( all(s_attribute %in% CQI$attributes(.Object@corpus, type = "s")) )
    
    for (sAttr in s_attribute){
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
  if (!is.null(p_attribute)){
    # check that all p-attributes are available
    .message("checking that all p-attributes are available", verbose = verbose)
    stopifnot( all(p_attribute %in% CQI$attributes(.Object@corpus, type = "p")) )
    
    # add ids
    for (pAttr in p_attribute){
      colname <- paste(pAttr, "id", sep = "_")
      if (colname %in% colnames(.Object@cpos)){
        .message("already present - skip getting ids for p-attribute:", pAttr, verbose = verbose)
      } else {
        .message("getting token id for p-attribute:", pAttr, verbose = verbose)
        ids <- CQI$cpos2id(.Object@corpus, pAttr, .Object@cpos[["cpos"]])
        .Object@cpos[[colname]] <- ids
      }
    }
    
    # add 
    if (decode){
      for (pAttr in p_attribute){
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