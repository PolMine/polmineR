#' @include textstat_class.R comp_class.R context_class.R
NULL

#' Log-likelihood-test 
#' 
#' @exportMethod ll
#' @param ids the numeric ids (integer) of the tokens
#' @param windowFreq term frequency of the tokens in corpus A
#' @param corpusFreq term frequency of the tokens in the corpus B
#' @param windows.total total size of the window 
#' @param corpus.total total size of the corpus
#' @return a Matrix with six columns
#' @rdname textstatistics
#' @name textstatistics
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

setMethod("ll", "context", function(.Object){
  size_window <- .Object@size
  size_partition <- .Object@partitionSize
  .Object@stat[, exp_window := size_window * (count_partition / size_partition)]
  .Object@stat[, exp_partition := (size_partition - size_window) * count_partition / size_partition]
  .Object@stat[, ll := 2 * (count_window * log(count_window/exp_window) + (count_partition - exp_window) * log((count_partition - exp_window)/exp_partition))]
  .Object@stat[, ll := ll * apply(.Object@stat, 1, function(x) ifelse(x["count_window"] > x["exp_windo"], -1, 1))]
  .Object <- sort(.Object, by="ll")
  .Object@stat[, rank_ll := c(1:nrow(.Object@stat))]
  .Object@method <- c(.Object@method, "ll")
  .Object
})

setMethod("ll", "cooccurrences", function(.Object){
  .Object@stat[, exp_window := size_window * (count_b / .Object@partitionSize)]
  .Object@stat[, exp_partition := (.Object@partitionSize - size_window) * (count_b / .Object@partitionSize)]
  .Object@stat[, ll := 2 * (count_ab * log(count_ab/exp_window) + (count_b - exp_window) * log((count_b - exp_window)/exp_partition))]
  direction <- apply(
    cbind(.Object@stat[["count_ab"]], .Object@stat[["exp_window"]]), 1, function(x) ifelse(x[1] < x[2], -1, 1)
  )
  .Object@stat[, ll := ll * direction]
  .Object <- sort(.Object, by="ll")
  .Object@stat[, rank_ll := c(1:nrow(.Object@stat))]
  .Object@method <- c(.Object@method, "ll")
  .Object
  return(.Object)
})

setMethod("ll", "comp", function(.Object){
  exp_total <- (.Object@stat[["count_coi"]] + .Object@stat[["count_ref"]]) / (.Object@sizeCoi + .Object@sizeRef)
  .Object@stat[, exp_coi := .Object@sizeCoi * exp_total]
  .Object@stat[, exp_ref := .Object@sizeRef * exp_total]
  .Object@stat[, ll := 2 * (count_coi * log(count_coi/exp_coi) + (count_ref * log(count_ref/exp_ref)))]
  direction <- apply(
    cbind(.Object@stat[["count_coi"]], .Object@stat[["exp_coi"]]), 1, function(x) ifelse(x[1] < x[2], -1, 1)
    )
  .Object@stat[, ll := ll * direction]
  .Object <- sort(.Object, by="ll")
  .Object@stat[, rank_ll := c(1:nrow(.Object@stat))]
  .Object@method <- c(.Object@method, "ll")
  return(.Object)
})


setMethod("ll", "compCooccurrences", function(.Object, partitionSize){
  mat <- .g2Statistic(
    ids=rep(0, times=nrow(.Object@stat)),
    windowFreq=.Object@stat[["x_ab_count"]],
    corpusFreq=.Object@stat[["y_ab_count"]],
    windows.total=.Object@sizeCoi,
    corpus.total=partitionSize
  )
  .Object@stat[, exp_coi := mat[,"exp_a"]]
  .Object@stat[, ll := mat[, "ll"]]
  .Object <- sort(.Object, by="ll")
  .Object@stat[, rank := c(1:nrow(.Object@stat))]
  return(.Object)
})
