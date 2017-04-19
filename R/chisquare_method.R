#' @include textstat_class.R features_class.R context_class.R
NULL

#' perform chisquare-text
#' 
#' Perform Chisquare-Test based on a table with counts
#' 
#' This function deliberately uses a self-made chi-square test for performance
#' reason
#' 
#' @param .Object object
#' @param ... further parameters
#' @exportMethod chisquare
#' @return a table
#' @author Andreas Blaette
#' @rdname chisquare-method
setGeneric("chisquare", function(.Object, ...){standardGeneric("chisquare")})

#' @rdname chisquare-method
setMethod("chisquare", "textstat", function(.Object){
  size_coi <- .Object@sizeCoi
  size_ref <- .Object@sizeRef
  size_total <- size_coi + size_ref
  count_x_coi <- .Object@stat[["count_coi"]]
  count_x_ref <- .Object@stat[["count_ref"]]
  count_x_total <- count_x_coi + count_x_ref
  count_notx_coi <- size_coi - count_x_coi
  count_notx_ref <- size_ref - count_x_ref
  count_notx_total <- size_total - count_x_total
  options(digits=20)
  exp_x_coi <- (count_x_total/size_total) * size_coi
  exp_x_ref <- (count_x_total/size_total) * size_ref
  exp_notx_coi <- (count_notx_total/size_total) * size_coi
  exp_notx_ref <- (count_notx_total/size_total) * size_ref
  chi1 <- ((exp_x_coi - count_x_coi)**2) / exp_x_coi
  chi2 <- ((exp_x_ref - count_x_ref)**2) / exp_x_ref
  chi3 <- ((exp_notx_coi - count_notx_coi)**2) / exp_notx_coi
  chi4 <- ((exp_notx_ref - count_notx_ref)**2) / exp_notx_ref
  chi <- chi1 + chi2 + chi3 + chi4
  chi <- chi * apply(cbind(count_x_coi, exp_x_coi), 1, function(x) ifelse(x[1] > x[2], 1, -1))
  options(digits=7)
  .Object@stat[, "exp_coi" := exp_x_coi]
  .Object@stat[, "chisquare" := chi]
  .Object <- sort(.Object, by="chisquare")
  .Object@stat[, "rank_chisquare" := c(1:nrow(.Object@stat))]
  .Object@method <- c(.Object@method, "chisquare")
  return(.Object)
})


#' @rdname chisquare-method
setMethod("chisquare", "context", function(.Object){
  setnames(
    .Object@stat,
    old=c("count_window", "count_partition"),
    new=c("count_coi", "count_ref")
    )
  .Object@stat[, "count_ref" := .Object@stat[["count_ref"]] - .Object@stat[["count_coi"]] ]
  .Object <- callNextMethod()
  setnames(
    .Object@stat,
    old=c("count_coi", "count_ref", "exp_coi", "exp_ref"),
    new=c("count_window", "count_partition", "exp_window", "exp_partition")
    )
  # .Object@stat[, count_partition := count_partition + count_window]
  .Object
})