#' @include partition.R S4classes.R
NULL

#' @details The \code{as.regions}-method coerces objects to a \code{regions}-object.
#' @param ... Further arguments.
#' @rdname regions_class
#' @exportMethod as.regions
setGeneric("as.regions", function(x, ...) standardGeneric("as.regions"))



setAs(from = "partition", to = "regions", function(from, to){
  new(
    "regions",
    cpos = from@cpos,
    encoding = from@encoding,
    corpus = from@corpus
    )
})


setAs(from = "regions", to = "partition", function(from, to){
  new(
    "partition",
    cpos = from@cpos,
    encoding = from@encoding,
    corpus = from@corpus,
    stat = data.table()
  )
})

#' @rdname partition_class
setMethod("as.regions", "partition", function(x) as(x, "regions"))

#' @param node A logical value, whether to include the node (i.e. query matches) in the region matrix
#' generated when creating a \code{partition} from a \code{context}-object.
#' @rdname context-class
setMethod("as.regions", "context", function(x, node = TRUE){
  DT <- copy(x@cpos)
  setkeyv(x = DT, cols = c("hit_no", "cpos"))
  .cpos_left_right <- function(.SD) list(cpos_left = min(.SD[["cpos"]]), cpos_right = max(.SD[["cpos"]]))
  DT_list <- list(left = subset(DT, DT[["position"]] < 0), right = subset(DT, DT[["position"]] > 0))
  if (node) DT_list[["node"]] <- subset(DT, DT[["position"]] == 0)
  DT_regions <- rbindlist(lapply(DT_list, function(x) x[, .cpos_left_right(.SD), by = "hit_no"]))
  setorderv(DT_regions, cols = "hit_no")
  new(
    Class = "regions",
    cpos = as.matrix(DT_regions[, "hit_no" := NULL]),
    corpus = x@corpus,
    encoding = x@encoding
  )
})




#' @rdname regions_class
#' @exportMethod as.data.table
setMethod("as.data.table", "regions", function(x, values = NULL){
  dt <- as.data.table(x@cpos)
  if (!is.null(values)){
    stopifnot(length(values) == nrow(dt) || length(values) == 1)
    dt[[3]] <- values
  }
  dt
})