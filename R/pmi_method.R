#' @include textstat_class.R features_class.R context_class.R
NULL

#' @rdname textstatistics
setGeneric("pmi", function(.Object){standardGeneric("pmi")})

#' @rdname textstatistics
setMethod("pmi", "context", function(.Object){
  .Object@stat[, "pmi" := log2((.Object@stat[["count_window"]]/.Object@partitionSize)/((.Object@count/.Object@partitionSize)*(.Object@stat[["count_partition"]]/.Object@partitionSize)))]
  .Object <- sort(.Object, by="pmi")
  .Object@stat[, "rank_pmi" := c(1:nrow(.Object@stat))]
  .Object@method <- c(.Object@method, "pmi")
  return(.Object)
})
