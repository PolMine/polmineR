#' @exportMethod format
#' @rdname textstat-class
#' @details The \code{format()}-method returns a pretty-printed and minimized
#'   version of the \code{data.table} in the \code{stat}-slot of the
#'   \code{textstat}-object: It will round all numeric columns to the number of decimal
#'   numbers specified by \code{digits}, and drop all columns with token ids. The 
#'   return value is a \code{data.table}.
#' @importFrom data.table copy
setMethod("format", "textstat", function(x, digits = 2L){
  dt <- copy(x@stat) # create copy, to avoid confusion resulting from in-place modification
  round(dt, digits = digits) # this is an in-place operation
  id_columns <- grep("_id", colnames(dt)) # get columns with token ids
  for (i in id_columns) dt[[i]] <- NULL # remove token id columns
  dt
})

#' @param digits Integer indicating the number of decimal places (round) or
#'   significant digits (signif) to be used.
#' @rdname cooccurrences-class
#' @exportMethod format
setMethod("format", "cooccurrences", function(x, digits = 2L){
  dt <- copy(x@stat)
  round(dt, digits = digits)
  
  if ("count_partition" %in% colnames(dt))
    attr(dt[["count_partition"]], "label") <- "observed in partition"
  if ("count_window" %in% colnames(dt))
    attr(dt[["count_window"]], "label") <- "observed in window"
  if ("exp_window" %in% colnames(dt))
    attr(dt[["exp_window"]], "label") <- "expected in window"
  if ("exp_partition" %in% colnames(dt))
    attr(dt[["exp_partition"]], "label") <- "expected in partition"
  if ("ll" %in% colnames(dt)) attr(dt[["ll"]], "label") <- "log likelihood"
  if ("rank_ll" %in% colnames(dt)) attr(dt[["rank_ll"]], "label") <- "rank"
  
  dt
})


#' @param x A \code{features} object.
#' @param digits Integer indicating the number of decimal places (round) or
#'   significant digits (signif) to be used.
#' @rdname features-class
setMethod("format", "features", function(x, digits = 2L){
  dt <- copy(x@stat)
  round(dt, digits = digits)
  cols_to_keep <- c(
    paste("rank", x@method, sep = "_"),
    x@p_attribute,
    "count_coi", "count_ref", "exp_coi",
    x@method
  )
  dt[, cols_to_keep, with = FALSE]
})

