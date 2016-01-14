#' @include textstat_class.R comp_class.R context_class.R
NULL

#' text statistics
#' 
#' @param .Object an object
#' @param ... further parameters
#' @exportMethod ll
#' @rdname textstatistics
setGeneric("ll", function(.Object, ...){standardGeneric("ll")})

# .llWorker <- function(.Object, size_a, size_b){
#   .Object@stat[, exp_a := size_a * count_b / size_b]
#   .Object@stat[, exp_b := (size_b - size_a) * count_b / size_b]
#   .Object@stat[, ll := 2 * (count_a * log(count_a/exp_a) + (count_b - exp_a) * log((count_b - exp_a)/exp_b))]
#   .Object <- sort(.Object, by="ll")
#   .Object@stat[, rank := c(1:nrow(.Object@stat))]
#   .Object@statisticalTest <- c(.Object@statisticalTest, "ll")
#   .Object
# }

#' @rdname textstatistics
setMethod("ll", "context", function(.Object){
  size_window <- .Object@size
  size_partition <- .Object@partitionSize
  .Object@stat[, "exp_window" := size_window * (.Object@stat[["count_partition"]] / size_partition)]
  .Object@stat[, "exp_partition" := (size_partition - size_window) * .Object@stat[["count_partition"]] / size_partition]
  .Object@stat[, "ll" := 2 * (.Object@stat[["count_window"]] * log(.Object@stat[["count_window"]]/.Object@stat[["exp_window"]]) + (.Object@stat[["count_partition"]] - .Object@stat[["exp_window"]]) * log((.Object@stat[["count_partition"]] - .Object@stat[["exp_window"]])/.Object@stat[["exp_partition"]]))]
  .Object@stat[, "ll" := ll * apply(.Object@stat, 1, function(x) ifelse(x["count_window"] > x["exp_window"], -1, 1))]
  .Object <- sort(.Object, by="ll")
  .Object@stat[, "rank_ll" := c(1:nrow(.Object@stat))]
  .Object@method <- c(.Object@method, "ll")
  .Object
})

#' @rdname textstatistics
setMethod("ll", "cooccurrences", function(.Object){
  .Object@stat[, "exp_window" := .Object@stat[["size_window"]] * (.Object@stat[["count_b"]] / .Object@partitionSize)]
  .Object@stat[, "exp_partition" := (.Object@partitionSize - .Object@stat[["size_window"]]) * (.Object@stat[["count_b"]] / .Object@partitionSize)]
  .Object@stat[, "ll" := 2 * (.Object@stat[["count_ab"]] * log(.Object@stat[["count_ab"]]/.Object@stat[["exp_window"]]) + (.Object@stat[["count_b"]] - .Object@stat[["exp_window"]]) * log((.Object@stat[["count_b"]] - .Object@stat[["exp_window"]])/.Object@stat[["exp_partition"]]))]
  direction <- apply(
    cbind(.Object@stat[["count_ab"]], .Object@stat[["exp_window"]]), 1, function(x) ifelse(x[1] < x[2], -1, 1)
  )
  .Object@stat[, "ll" := ll * direction]
  .Object <- sort(.Object, by="ll")
  .Object@stat[, "rank_ll" := c(1:nrow(.Object@stat))]
  .Object@method <- c(.Object@method, "ll")
  .Object
  return(.Object)
})

#' @rdname textstatistics
setMethod("ll", "comp", function(.Object){
  exp_total <- (.Object@stat[["count_coi"]] + .Object@stat[["count_ref"]]) / (.Object@sizeCoi + .Object@sizeRef)
  .Object@stat[, "exp_coi" := .Object@sizeCoi * exp_total]
  .Object@stat[, "exp_ref" := .Object@sizeRef * exp_total]
  .Object@stat[, ll := 2 * (.Object@stat[["count_coi"]] * log(.Object@stat[["count_coi"]]/.Object@stat[["exp_coi"]]) + (.Object@stat[["count_ref"]] * log(.Object@stat[["count_ref"]]/.Object@stat[["exp_ref"]])))]
  direction <- apply(
    cbind(.Object@stat[["count_coi"]], .Object@stat[["exp_coi"]]), 1, function(x) ifelse(x[1] < x[2], -1, 1)
    )
  .Object@stat[, "ll" := ll * direction]
  .Object <- sort(.Object, by="ll")
  .Object@stat[, "rank_ll" := c(1:nrow(.Object@stat))]
  .Object@method <- c(.Object@method, "ll")
  return(.Object)
})

