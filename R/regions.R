#' @include partition.R S4classes.R
NULL


setAs(from = "partition", to = "regions", function(from, to){
  new(
    "regions",
    cpos = from@cpos,
    encoding = from@encoding,
    corpus = from@corpus
    )
})

#' @rdname regions_class
#' @export as.regions
as.regions <- function(x){
  stopifnot("partition" %in% is(x))
  as(x, "regions")
}

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