#' @include textstat.R features.R context.R S4classes.R
NULL

#' text statistics
#' 
#' @param .Object an object
#' @param ... further parameters
#' @exportMethod ll
#' @rdname textstatistics
setGeneric("ll", function(.Object, ...) standardGeneric("ll") )


#' @rdname textstatistics
setMethod("ll", "context", function(.Object){
  size_window <- .Object@size
  size_partition <- .Object@size_partition
  .Object@stat[, "exp_window" := size_window * (.Object@stat[["count_partition"]] / size_partition)]
  .Object@stat[, "exp_partition" := (size_partition - size_window) * (.Object@stat[["count_partition"]] / size_partition)]
  .Object@stat[, "ll" := 2 * (.Object@stat[["count_window"]] * log(.Object@stat[["count_window"]]/.Object@stat[["exp_window"]]) + (.Object@stat[["count_partition"]] - .Object@stat[["exp_window"]]) * log((.Object@stat[["count_partition"]] - .Object@stat[["exp_window"]])/.Object@stat[["exp_partition"]]))]
  direction <- ifelse(.Object@stat[["count_window"]] < .Object@stat[["exp_window"]], -1L, 1L)
  .Object@stat[, "ll" := ll * direction]
  .Object <- sort(.Object, by="ll")
  .Object@stat[, "rank_ll" := 1L:nrow(.Object@stat)]
  .Object@method <- c(.Object@method, "ll")
  .Object
})

#' @rdname textstatistics
setMethod("ll", "cooccurrences", function(.Object){
  .Object@stat[, "exp_window" := .Object@stat[["size_window"]] * (.Object@stat[["count_b"]] / .Object@size_partition)]
  .Object@stat[, "exp_partition" := (.Object@size_partition - .Object@stat[["size_window"]]) * (.Object@stat[["count_b"]] / .Object@size_partition)]
  .Object@stat[, "ll" := 2 * (.Object@stat[["count_ab"]] * log(.Object@stat[["count_ab"]]/.Object@stat[["exp_window"]]) + (.Object@stat[["count_b"]] - .Object@stat[["exp_window"]]) * log((.Object@stat[["count_b"]] - .Object@stat[["exp_window"]])/.Object@stat[["exp_partition"]]))]
  direction <- ifelse(.Object@stat[["count_ab"]] < .Object@stat[["exp_window"]], -1L, 1L)
  .Object@stat[, "ll" := ll * direction]
  .Object <- sort(.Object, by = "ll")
  .Object@stat[, "rank_ll" := 1L:nrow(.Object@stat)]
  .Object@method <- c(.Object@method, "ll")
  .Object
  return(.Object)
})

#' @rdname textstatistics
setMethod("ll", "features", function(.Object){
  exp_total <- (.Object@stat[["count_coi"]] + .Object@stat[["count_ref"]]) / (.Object@size_coi + .Object@size_ref)
  .Object@stat[, "exp_coi" := .Object@size_coi * exp_total]
  .Object@stat[, "exp_ref" := .Object@size_ref * exp_total]
  .Object@stat[, ll := 2 * (.Object@stat[["count_coi"]] * log(.Object@stat[["count_coi"]]/.Object@stat[["exp_coi"]]) + (.Object@stat[["count_ref"]] * log(.Object@stat[["count_ref"]]/.Object@stat[["exp_ref"]])))]
  direction <- ifelse(.Object@stat[["count_coi"]] < .Object@stat[["exp_coi"]], -1L, 1L)
  .Object@stat[, "ll" := ll * direction]
  .Object <- sort(.Object, by="ll")
  .Object@stat[, "rank_ll" := 1L:nrow(.Object@stat)]
  .Object@method <- c(.Object@method, "ll")
  return(.Object)
})

