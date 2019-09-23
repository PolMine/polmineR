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
#'   the kwic method. If param \code{table} is \code{TRUE}, corpus positions
#'   will be turned into a \code{data.frame} with the concordance lines. If
#'   param \code{s_attributes} is a character vector with s-attributes, the
#'   respective s-attributes will be added as columns to the table with
#'   concordance lines.
#' @rdname kwic-class
#' @examples
#' # enrich kwic object
#' i <- corpus("GERMAPARLMINI") %>%
#'   kwic(query = "Integration") %>%
#'   enrich(s_attributes = c("date", "speaker", "party"))
setMethod("enrich", "kwic", function(.Object, s_attributes = NULL, extra = NULL, table = FALSE, ...){
  
  if ("meta" %in% names(list(...))) s_attributes <- list(...)[["meta"]]
  
  if (length(s_attributes) > 0L){
    metainformation <- lapply(
      setNames(s_attributes, s_attributes),
      function(s_attr){
        cpos_to_get <- .Object@cpos[which(.Object@cpos[["position"]] == 0)][, .SD[1], by = "match_id", with = TRUE][["cpos"]]
        strucs <- cl_cpos2struc(corpus = .Object@corpus, s_attribute = s_attr, cpos = cpos_to_get, registry = registry())
        strucs_invalid <- which(strucs < 0L)
        if (length(strucs_invalid) > 0L) strucs[strucs_invalid] <- 0L
        struc_values <- cl_struc2str(corpus = .Object@corpus, s_attribute = s_attr, struc = strucs, registry = registry())
        if (length(strucs_invalid) > 0) struc_values[strucs_invalid] <- ""
        as.nativeEnc(struc_values, from = .Object@encoding)
      }
    )
    meta_dt <- data.table(data.frame(metainformation, stringsAsFactors = FALSE))
    .Object@stat <- data.table(meta_dt, .Object@stat)
    .Object@metadata <- c(s_attributes, .Object@metadata)
  }
  
  if (!is.null(extra)){
    table <- TRUE # it will be necessary to regenerate the table
    stopifnot(is.integer(extra) || is.numeric(extra))
    if (is.numeric(extra)) extra <- as.integer(extra)
    .fn <- function(.SD){
      cpos_min <- min(.SD[["cpos"]])
      cpos_max <- max(.SD[["cpos"]])
      position_min <- min(.SD[["position"]])
      position_max <- max(.SD[["position"]])
      hit <- unique(.SD[["match_id"]])
      rbindlist(
        list(
          extra_left = data.table(
            match_id = hit,
            cpos = (cpos_min - extra):(cpos_min - 1L),
            position = (position_min - extra):(position_min - 1L),
            word_id = NA,
            word = NA,
            direction = -2L
          ),
          kwic = .SD,
          extra_right = data.table(
            match_id = hit,
            cpos = (cpos_max + 1L):(cpos_max + extra),
            position = (position_max + 1L):(position_max + extra),
            word_id = NA,
            word = NA,
            direction = +2L
          )
        )
      )
    }
    dt <- .Object@cpos[, .fn(.SD), by = "match_id", .SDcols = 1L:ncol(.Object@cpos)]
    corpus_size <- RcppCWB::cl_attribute_size(
      corpus = .Object@corpus,
      attribute = "word",
      attribute_type = "p",
      registry = registry()
    )
    .Object@cpos <- dt[cpos >= 0L][cpos <= (corpus_size - 1L)]
    word_id_na <- is.na(.Object@cpos[["word_id"]])
    word_id_na_index <- which(word_id_na)
    ids_na <- RcppCWB::cl_cpos2id(
      corpus = .Object@corpus,
      p_attribute = "word",
      registry = registry(),
      cpos = .Object@cpos[["cpos"]][word_id_na]
    )
    str_na <- RcppCWB::cl_id2str(
      corpus = .Object@corpus,
      p_attribute = "word",
      registry = registry(),
      id = ids_na
    )
    str_na <- as.nativeEnc(str_na, from = .Object@encoding)
    .Object@cpos[word_id_na_index, "word_id" := ids_na]
    .Object@cpos[word_id_na_index, "word" := str_na]
  }
  
  if (table){
    if (nrow(.Object@cpos) > 0L){
      .fn <- function(.SD) paste(.SD[[.Object@p_attribute]], collapse = " ")
      table_ext <- .Object@cpos[, .fn(.SD), by = c("match_id", "direction"), with = TRUE]
      .Object@stat <- dcast(data = table_ext, formula = match_id ~ direction, value.var = "V1")
      setnames(.Object@stat, old = "0", new = "node")
      
      # columns are renamed one at a time to cover the special case when either the 
      # left or the right context are (deliberately) empty
      
      if ("-2" %in% colnames(.Object@stat)) setnames(.Object@stat, old = "-1", new = "left_extra")
      if ("-1" %in% colnames(.Object@stat)) setnames(.Object@stat, old = "-1", new = "left")
      if ("1" %in% colnames(.Object@stat)) setnames(.Object@stat, old = "1", new = "right")
      if ("2" %in% colnames(.Object@stat)) setnames(.Object@stat, old = "-1", new = "right_extra")

    } else {
      .Object@stat <- data.table(match_id = integer(), left = character(), node = character(), right = character())
    }
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
    stopifnot( all(s_attribute %in% registry_get_s_attributes(.Object@corpus)) )
    
    for (s_attr in s_attribute){
      .message("get struc for s-attribute:", s_attr, verbose = verbose)
      strucs <- cl_cpos2struc(corpus = .Object@corpus, s_attribute = s_attr, cpos = .Object@cpos[["cpos"]], registry = registry())
      if (decode == FALSE){
        colname_struc <- paste(s_attr, "int", sep = "_")
        if (colname_struc %in% colnames(.Object@cpos)){
          .message("already present, skipping assignment of column:", colname_struc, verbose = verbose)
        } else {
          .Object@cpos[[colname_struc]] <- strucs
        }
      } else {
        if (s_attr %in% colnames(.Object@cpos)){
          .message("already present, skipping assignment of column:", s_attr, verbose = verbose)
        } else {
          .message("get string for s-attribute:", s_attr, verbose = verbose)
          strings <- cl_struc2str(corpus = .Object@corpus, s_attribute = s_attr, struc = strucs, registry = registry())
          .Object@cpos[[s_attr]] <- as.nativeEnc(strings, from = .Object@encoding)
        }
      }
    }
  }
  if (!is.null(p_attribute)){
    # check that all p-attributes are available
    .message("checking that all p-attributes are available", verbose = verbose)
    stopifnot( all(p_attribute %in% registry_get_p_attributes(.Object@corpus)) )
    
    # add ids
    for (pAttr in p_attribute){
      colname <- paste(pAttr, "id", sep = "_")
      if (colname %in% colnames(.Object@cpos)){
        .message("already present - skip getting ids for p-attribute:", pAttr, verbose = verbose)
      } else {
        .message("getting token id for p-attribute:", pAttr, verbose = verbose)
        ids <- cl_cpos2id(corpus = .Object@corpus, p_attribute = pAttr, cpos = .Object@cpos[["cpos"]],  registry = registry())
        .Object@cpos[[colname]] <- ids
      }
    }
    
    # add 
    if (decode){
      for (p_attr in p_attribute){
        if (p_attr %in% colnames(.Object@cpos)){
          .message("already present - skip getting strings for p-attribute:", p_attr, verbose = verbose)
        } else {
          .message("decode p-attribute:", p_attr, verbose = verbose)
          p_attr_id <- paste(p_attr, "id", sep = "_")
          decoded <- cl_id2str(
            corpus = .Object@corpus,
            p_attribute = p_attr,
            id = .Object@cpos[[p_attr_id]],
            registry = registry()
          )
          .Object@cpos[, (p_attr) := as.nativeEnc(decoded, from = .Object@encoding)]
          .Object@cpos[, (p_attr_id) := NULL]
        }
      }
    }
    
  }
  .Object
})