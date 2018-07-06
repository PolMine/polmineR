#' @include textstat.R features.R context.R S4classes.R
NULL

#' @rdname textstatistics
setGeneric("pmi", function(.Object){standardGeneric("pmi")})

#' @rdname textstatistics
setMethod("pmi", "context", function(.Object){
  .Object@stat[, "pmi" := log2((.Object@stat[["count_window"]]/.Object@size_partition)/((.Object@count/.Object@size_partition)*(.Object@stat[["count_partition"]]/.Object@size_partition)))]
  .Object <- sort(.Object, by="pmi")
  .Object@stat[, "rank_pmi" := c(1:nrow(.Object@stat))]
  .Object@method <- c(.Object@method, "pmi")
  return(.Object)
})
