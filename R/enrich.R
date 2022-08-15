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
    stopifnot(
      is.character(p_attribute),
      length(p_attribute) <= 2,
      all(p_attribute %in% p_attributes(.Object))
    )
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
  
  if (!is.null(extra)){
    table <- TRUE # it will be necessary to regenerate the table
    stopifnot(is.integer(extra) || is.numeric(extra))
    if (is.numeric(extra)) extra <- as.integer(extra)
    .fn_left <- function(.SD){
      cpos_min <- min(.SD[["cpos"]])
      position_min <- min(.SD[["position"]])
      hit <- .SD[["match_id"]][1]
      list(
        cpos = (cpos_min - extra):(cpos_min - 1L),
        position = (position_min - extra):(position_min - 1L),
        direction = -2L
      )
    }
    .fn_right <- function(.SD){
      cpos_max <- max(.SD[["cpos"]])
      position_max <- max(.SD[["position"]])
      hit <- .SD[["match_id"]][1]
      list(
        cpos = (cpos_max + 1L):(cpos_max + extra),
        position = (position_max + 1L):(position_max + extra),
        direction = 2L
      )
    }
    dt_left <- .Object@cpos[, .fn_left(.SD), by = "match_id", .SDcols = 1L:ncol(.Object@cpos)]
    dt_right <- .Object@cpos[, .fn_right(.SD), by = "match_id", .SDcols = 1L:ncol(.Object@cpos)]
    dt <- rbindlist(list(.Object@cpos, dt_left, dt_right), use.names = TRUE, fill = TRUE)
    setkeyv(x = dt, cols = c("match_id", "cpos"))
    setorderv(x = dt, cols = "cpos")
    
    corpus_size <- RcppCWB::cl_attribute_size(
      corpus = .Object@corpus, registry = .Object@registry_dir,
      attribute = p_attributes(.Object), attribute_type = "p"
      
    )
    .Object@cpos <- dt[cpos >= 0L][cpos <= (corpus_size - 1L)]
    
    token_id <- paste(p_attributes(.Object), "id", sep = "_")
    word_id_na <- is.na(.Object@cpos[[token_id]])
    word_id_na_index <- which(word_id_na)
    ids_na <- cpos2id(
      .Object, p_attribute = p_attributes(.Object),
      cpos = .Object@cpos[["cpos"]][word_id_na]
    )
    str_na <- RcppCWB::cl_id2str(
      corpus = .Object@corpus, registry = .Object@registry_dir,
      p_attribute = p_attributes(.Object), id = ids_na
    )
    str_na <- as.nativeEnc(str_na, from = .Object@encoding)
    .Object@cpos[word_id_na_index, (token_id) := ids_na]
    .Object@cpos[word_id_na_index, (p_attributes(.Object)) := str_na]
  }
  
  if (table){
    if (nrow(.Object@cpos) > 0L){
      .fn <- function(.SD) paste(.SD[[.Object@p_attribute[1]]], collapse = " ")
      table_ext <- .Object@cpos[, .fn(.SD), by = c("match_id", "direction"), with = TRUE]
      .Object@stat <- dcast(data = table_ext, formula = match_id ~ direction, value.var = "V1")
      setnames(.Object@stat, old = "0", new = "node")
      
      # columns are renamed one at a time to cover the special case when either the 
      # left or the right context are (deliberately) empty
      
      if ("-2" %in% colnames(.Object@stat)) setnames(.Object@stat, old = "-2", new = "left_extra")
      if ("-1" %in% colnames(.Object@stat)) setnames(.Object@stat, old = "-1", new = "left")
      if ("1" %in% colnames(.Object@stat)) setnames(.Object@stat, old = "1", new = "right")
      if ("2" %in% colnames(.Object@stat)) setnames(.Object@stat, old = "2", new = "right_extra")

    } else {
      .Object@stat <- data.table(match_id = integer(), left = character(), node = character(), right = character())
    }
  }
  
  if (length(s_attributes) > 0L){
    .Object@metadata <- unique(c(s_attributes, .Object@metadata))
    for (s_attr in .Object@metadata){
      if (!s_attr %in% colnames(.Object@stat)){
        cpos_to_get <- .Object@cpos[which(.Object@cpos[["position"]] == 0)][, .SD[1], by = "match_id", with = TRUE][["cpos"]]
        strucs <- cl_cpos2struc(
          corpus = .Object@corpus, registry = .Object@registry_dir,
          s_attribute = s_attr, cpos = cpos_to_get
        )
        strucs_invalid <- which(strucs < 0L)
        if (length(strucs_invalid) > 0L) strucs[strucs_invalid] <- 0L
        struc_values <- cl_struc2str(
          corpus = .Object@corpus, registry = .Object@registry_dir,
          s_attribute = s_attr, struc = strucs
        )
        if (length(strucs_invalid) > 0L) struc_values[strucs_invalid] <- ""
        .Object@stat[, (s_attr) := as.nativeEnc(struc_values, from = .Object@encoding)]
      }
    }
    setcolorder(x = .Object@stat, neworder = c(
      .Object@metadata,
      if ("left_extra" %in% colnames(.Object@stat)) "left_extra" else NULL,
      if ("left" %in% colnames(.Object@stat)) "left" else NULL,
      "node",
      if ("right" %in% colnames(.Object@stat)) "right" else NULL,
      if ("right_extra" %in% colnames(.Object@stat)) "right_extra" else NULL
      )
    )
  }
  .Object
})

#' @details The `enrich()`-method can be used to add additional information to
#'   the `data.table` in the `cpos`-slot of a `context`-object.
#' 
#' @exportMethod enrich
#' @docType methods
#' @rdname context-class
#' @param s_attribute The s-attribute(s) to add to `data.table` in slot `cpos`.
#' @param p_attribute The p-attribute(s) to add to `data.table` in slot `cpos`.
#' @param decode A `logical` value, whether to convert integer ids to expressive
#'   strings.
#' @param stat A `logical` value, whether to generate / update slot `stat` from
#'   the `cpos` table.
#' @param verbose A `logical`, whether to be talkative.
#' @importFrom RcppCWB corpus_p_attributes
setMethod("enrich", "context", function(.Object, s_attribute = NULL, p_attribute = NULL, decode = FALSE, stat = FALSE, verbose = TRUE, ...){
  
  if ("pAttribute" %in% names(list(...))){
    lifecycle::deprecate_warn(
      when = "0.8.7", 
      what = "enrich(pAttribute)",
      with = "enrich(p_attribute)"
    )
    p_attribute <- list(...)[["pAttribute"]]
  }
  
  if ("sAttribute" %in% names(list(...))){
    lifecycle::deprecate_warn(
      when = "0.8.7", 
      what = "enrich(sAttribute)",
      with = "enrich(s_attribute)"
    )
    s_attribute <- list(...)[["pAttribute"]]
  }
  
  if (!is.null(s_attribute)){
    # check that all s-attributes are available
    .message("checking that all s-attributes are available", verbose = verbose)
    stopifnot(
      all(s_attribute %in% corpus_s_attributes(corpus = .Object@corpus, registry = .Object@registry_dir))
    )
    
    for (s_attr in s_attribute){
      .message("get struc for s-attribute:", s_attr, verbose = verbose)
      strucs <- cl_cpos2struc(
        corpus = .Object@corpus, registry = .Object@registry_dir,
        s_attribute = s_attr, cpos = .Object@cpos[["cpos"]]
      )
      if (decode == FALSE){
        colname_struc <- paste(s_attr, "int", sep = "_")
        if (colname_struc %in% colnames(.Object@cpos)){
          .message("already present, skipping assignment of column:", colname_struc, verbose = verbose)
        } else {
          .Object@cpos[, (colname_struc) := strucs]
        }
      } else {
        if (s_attr %in% colnames(.Object@cpos)){
          .message("already present, skipping assignment of column:", s_attr, verbose = verbose)
        } else {
          .message("get string for s-attribute:", s_attr, verbose = verbose)
          strings <- cl_struc2str(
            corpus = .Object@corpus, registry = .Object@registry_dir,
            s_attribute = s_attr, struc = strucs
          )
          .Object@cpos[, (s_attr) := as.nativeEnc(strings, from = .Object@encoding)]
        }
      }
    }
  }
  
  if (!is.null(p_attribute)){
    # check that all p-attributes are available
    .message("checking that all p-attributes are available", verbose = verbose)
    stopifnot(
      all(p_attribute %in% corpus_p_attributes(.Object@corpus, registry = .Object@registry_dir))
    )
    
    # add ids and decode if requested
    for (p_attr in p_attribute){
      colname <- paste(p_attr, "id", sep = "_")
      if (colname %in% colnames(.Object@cpos)){
        .message("already present - skip getting ids for p-attribute:", p_attr, verbose = verbose)
      } else {
        .message("getting token id for p-attribute:", p_attr, verbose = verbose)
        ids <- cpos2id(
          x = .Object, p_attribute = p_attr, cpos = .Object@cpos[["cpos"]]
        )
        .Object@cpos[, (colname) := ids]
      }
      
      if (decode){
        if (p_attr %in% colnames(.Object@cpos)){
          .message("already present - skip getting strings for p-attribute:", p_attr, verbose = verbose)
        } else {
          .message("decode p-attribute:", p_attr, verbose = verbose)
          p_attr_id <- paste(p_attr, "id", sep = "_")
          decoded <- id2str(
            x = .Object, p_attribute = p_attr, id = .Object@cpos[[p_attr_id]]
          )
          native <- as.nativeEnc(decoded, from = .Object@encoding)
          .Object@cpos <- .Object@cpos[, "word" := native]
          # .Object@cpos[, (p_attr_id) := NULL]
        }
      }
    }
  }
  
  if (isTRUE(stat)){
    msg <- sprintf(
      "%s count statistics for slot cpos",
      if (nrow(.Object@cpos) == 0L) "generate" else "update"
    )
    .message(msg, verbose = verbose)
    
    p_attr_id <- paste(.Object@p_attribute, "id", sep = "_")
    setkeyv(.Object@cpos, p_attr_id)
    cpos_min <- .Object@cpos[which(.Object@cpos[["position"]] != 0)]
    .Object@stat <- cpos_min[, .N, by = eval(p_attr_id), with = TRUE]
    setnames(.Object@stat, "N", "count_coi")
    
    for (i in seq_along(.Object@p_attribute)){
      new_col <- id2str(
        x = .Object,
        p_attribute = .Object@p_attribute[i],
        id = .Object@stat[[p_attr_id[i]]]
      )
      new_col_native <- as.nativeEnc(new_col, from = .Object@encoding)
      .Object@stat[, eval(.Object@p_attribute[i]) := new_col_native]
    }
    
  }
  
  .Object
})

#' @export
#' @rdname all-cooccurrences-class
#' @details The `enrich()`-method will add columns 'a_count' and 'b_count' to
#'   the `data.table` in the 'stat' slot of the `Cooccurrences` object. If the
#'   count for the subcorpus/partition from which the cooccurrences are derived
#'   is not yet present, the count is performed first.
setMethod("enrich", "Cooccurrences", function(.Object){
  
  cnt <- if (nrow(.Object@partition@stat) > 0L){
    .Object@partition@stat
  } else {
    count(.Object@partition, p_attribute = .Object@p_attribute, decode = FALSE)@stat
  }
  
  setkeyv(cnt, paste(.Object@p_attribute, "id", sep = "_"))
  
  setkeyv(.Object@stat, cols = "a_id")
  .Object@stat[, "a_count" := cnt[.Object@stat][["count"]] ]
  
  setkeyv(.Object@stat, cols = "b_id")
  .Object@stat[, "b_count" := cnt[.Object@stat][["count"]] ]
  invisible(.Object)
})