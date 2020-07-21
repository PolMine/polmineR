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
  if (is(dt)[1] == "data.table"){
    round(dt, digits = digits) # this is an in-place operation
    id_columns <- grep("_id", colnames(dt)) # get columns with token ids
    for (i in id_columns) dt[[i]] <- NULL # remove token id columns
  } else {
    stop("No data.table in slot 'stat' of the object - cannot show output.")
  }
  dt
})

#' @param digits Integer indicating the number of decimal places (round) or
#'   significant digits (signif) to be used.
#' @rdname cooccurrences-class
#' @exportMethod format
setMethod("format", "cooccurrences", function(x, digits = 2L){
  dt <- copy(x@stat)
  round(dt, digits = digits)
  
  if ("count_ref" %in% colnames(dt))
    attr(dt[["count_ref"]], "label") <- "observed in ref"
  if ("count_coi" %in% colnames(dt))
    attr(dt[["count_coi"]], "label") <- "observed in coi"
  if ("exp_coi" %in% colnames(dt))
    attr(dt[["exp_coi"]], "label") <- "expected in coi"
  if ("exp_ref" %in% colnames(dt))
    attr(dt[["exp_ref"]], "label") <- "expected in ref"
  if ("ll" %in% colnames(dt)) attr(dt[["ll"]], "label") <- "log likelihood"
  if ("rank_ll" %in% colnames(dt)) attr(dt[["rank_ll"]], "label") <- "rank"
  if ("count_partition" %in% colnames(dt)) dt[, "count_partition" := NULL]
  for (x in grep("_id", colnames(dt), value = TRUE)) dt[[x]] <- NULL
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


#' @rdname kwic-class
#' @param node_color If not \code{NULL}, the html color of the node. If
#'   supplied, the node will be wrapped in respective html tags.
#' @param extra_color If extra context has been generated using \code{enrich},
#'   the html color of the additional output (defaults to 'grey').
#' @param align A \code{logical} value for preparing kwic output. If
#'   \code{TRUE}, whether the content of the columns 'left', 'node' and 'right'
#'   will be wrapped in html div elements that will align the output right,
#'   centered and left, respectively.
#' @param lineview A \code{logical} value, whether to concatenate left context,
#'   node and right context when preparing kwic output.
#' @details The \code{format}-method will return a \code{data.table} that can
#'   serve as input for rendering a \code{htmlwidget}, for instance using
#'   \code{DT::datatable} or \code{rhandsontable::rhandsontable}. It will
#'   include html tags, so ensure that the rendering engine does not obfuscate
#'   the html.
#'   
setMethod("format", "kwic", function(x, node_color = "blue", align = TRUE, extra_color = "grey", lineview = getOption("polmineR.lineview")){
  if (lineview) align <- FALSE
  y <- copy(x@stat)[, "match_id" := NULL]
  
  if ("left_extra" %in% colnames(y)){
    if (lineview) y[, "left" := sprintf("<u>%s</u>", y[["left"]])]
    y[, "left" := sprintf("<font color='%s'>%s</font> %s", extra_color, y[["left_extra"]], y[["left"]])]
    y[, "left_extra" := NULL]
  }
  if (align) y[, "left" := sprintf("<div align='right'>%s</div>", y[["left"]])]
  
  if (length(node_color) == 1L){
    stopifnot(is.character(node_color))
    y[, "node" := sprintf("<font color='%s'>%s</font>", node_color, y[["node"]])]
  }
  if (align) y[, "node" := sprintf("<div align='center'>%s</div>", y[["node"]])]
  
  if ("right_extra" %in% colnames(y)){
    if (lineview) y[, "right" := sprintf("<u>%s</u>", y[["right"]])]
    y[, "right" := sprintf("%s <font color='%s'>%s</font>", y[["right"]], extra_color, y[["right_extra"]])]
    if (align) y[, "right" := sprintf("<div align='left'>%s</div>", y[["right"]])]
    y[, "right_extra" := NULL]
  }
  
  if (lineview){
    y[, "concordance" := apply(y, 1, function(x) paste(x[c("left", "node", "right")], collapse = " "))]
    for (column in c("left", "node", "right")) y[, (column) := NULL]
    setcolorder(y, neworder = c(x@metadata, "concordance", x@annotation_cols))
  }
  
  y
})