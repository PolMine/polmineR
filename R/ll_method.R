#' @include textstat_class.R keyness_class.R context_class.R
NULL

#' Log-likelihood-test 
#' 
#' @exportMethod ll
#' #' @param ids the numeric ids (integer) of the tokens
#' @param windowFreq term frequency of the tokens in corpus A
#' @param corpusFreq term frequency of the tokens in the corpus B
#' @param windows.total total size of the window 
#' @param corpus.total total size of the corpus
#' @return a Matrix with six columns
#' @rdname textstatistics
#' @name textstatistics
setGeneric("ll", function(.Object, ...){standardGeneric("ll")})

.llWorker <- function(.Object, size_a, size_b){
  .Object@stat[, exp_a := size_a * count_b / size_b]
  .Object@stat[, exp_b := (size_b - size_a) * count_b / size_b]
  .Object@stat[, ll := 2 * (count_a * log(count_a/exp_a) + (count_b - exp_a) * log((count_b - exp_a)/exp_b))]
  .Object <- sort(.Object, by="ll")
  .Object@stat[, rank := c(1:nrow(.Object@stat))]
  .Object@statisticalTest <- c(.Object@statisticalTest, "ll")
  .Object
}

setMethod("ll", "context", function(.Object){
  size_window <- .Object@size
  size_partition <- .Object@partitionSize
  .Object@stat[, exp_window := size_window * (count_partition / size_partition)]
  .Object@stat[, exp_partition := (size_partition - size_window) * count_partition / size_partition]
  .Object@stat[, ll := 2 * (count_window * log(count_window/exp_window) + (count_partition - exp_window) * log((count_partition - exp_window)/exp_partition))]
  .Object <- sort(.Object, by="ll")
  .Object@stat[, rank_ll := c(1:nrow(.Object@stat))]
  .Object@statisticalTest <- c(.Object@statisticalTest, "ll")
  .Object
})

setMethod("ll", "keyness", function(.Object){
  
  mat <- .g2Statistic(
    ids=rep(0, times=nrow(.Object@stat)),
    windowFreq=.Object@stat$count_a,
    corpusFreq=.Object@stat$count_b,
    windows.total=.Object@sizeCoi,
    corpus.total=.Object@sizeRef
    )
  if (! "exp_a" %in% colnames(.Object@stat)) .Object@stat$exp_a <- mat[, "exp_a"]
  if (! "exp_b" %in% colnames(.Object@stat)) .Object@stat$expRef <- mat[, "exp_b"]
  .Object@stat$ll <- mat[,"ll"]
  .Object <- sort(.Object, by="ll")
  .Object@stat[, rank := c(1:nrow(.Object@stat))]
  .Object@statisticalTest <- c(.Object@statisticalTest, "ll")
  return(.Object)
})

setMethod("ll", "cooccurrences", function(.Object, partitionSize){
  mat <- .g2Statistic(
    ids=rep(0, times=nrow(.Object@stat)),
    windowFreq=.Object@stat[["ab_count"]],
    corpusFreq=.Object@stat[["b_count"]],
    windows.total=.Object@stat[["window_size"]],
    corpus.total=partitionSize
    )
  .Object@stat[, exp_coi := mat[,"exp_a"]]
  .Object@stat[, ll := mat[, "ll"]]
  .Object@method <- c(.Object@method, "ll")
  return(.Object)
})

setMethod("ll", "keynessCooccurrences", function(.Object, partitionSize){
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
