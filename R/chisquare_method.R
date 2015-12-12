#' @include textstat_class.R keyness_class.R context_class.R
NULL

setGeneric("chisquare", function(x, ...){standardGeneric("chisquare")})

#' perform chisquare-text
#' 
#' Perform Chisquare-Test based on a table with counts
#' 
#' This function deliberately uses a self-made chi-square test for performance
#' reason
#' 
#' @param ctab a matrix with ids in column 1 term frequencies in col 2, and
#' overall frequencies in col 3
#' @param included defaults to FALSE, YES if corpus of interest is included in
#' reference corpus
#' @param minFrequency minimum frequency for a token to be kept in matrix
#' @return a table
#' @author Andreas Blaette
#' @noRd
.chisquare <- function(ctab, sizeCoi, sizeRef, minFrequency) {
  ctab <- ctab[which(ctab[,"count_a"] > minFrequency),]
  o <- matrix(data=0, nrow=nrow(ctab), ncol=6)
  o[,1] <- ctab[,2]
  o[,2] <- ctab[,3]
  o[,3] <- o[,1] + o[,2]
  o[,4] <- sizeCoi - o[,1]
  o[,5] <- sizeRef - o[,2]
  o[,6] <- o[,4] + o[,5]
  sizeTotal <- sizeCoi + sizeRef
  e <- matrix(data=0, nrow=dim(o)[1], ncol=4)
  options(digits=20)
  e[,1] <- (o[,3]/sizeTotal) * sizeCoi
  e[,2] <- (o[,3]/sizeTotal) * sizeRef
  e[,3] <- (o[,6]/sizeTotal) * sizeCoi
  e[,4] <- (o[,6]/sizeTotal) * sizeRef
  chi <- ((e[,1]-o[,1])**2)/e[,1]+((e[,2]-o[,2])**2)/e[,2]+((e[,3]-o[,4])**2)/e[,3]+((e[,4]-o[,5])**2)/e[,4]
  chi <- chi*apply(cbind(o[,1], e[,1]), MARGIN=1, function(x){if (x[1]>x[2]){1} else {-1}})
  options(digits=7)
  ctab$exp_a <- e[,1]
  ctab$exp_b <- e[,2]
  ctab$chiSquare <- chi
  # retval <- cbind(ctab, exp_a=e[,1], exp_b=e[,2], chiSquare=chi)
  return(ctab)
}

setMethod("chisquare", "keyness", function(x){
  chiResult <- .chisquare(
    ctab=as.data.frame(x@stat[, c(colnames(x@stat)[1], "count_a", "countRef"), with=FALSE]),
    sizeCoi=x@sizeCoi,
    sizeRef=x@sizeRef,
    minFrequency=x@minFrequency
  )
  x@stat[, chiSquare:= chiResult[["chiSquare"]]]
  x@statisticalTest <- c(x@statisticalTest, "chiSquare")
  return(x)
})

setMethod("chisquare", "context", function(x){
  size_window <- x@size
  size_total <- x@partitionSize
  count_x_coi <- x@stat[["count_window"]]
  count_x_ref <- x@stat[["count_partition"]] - count_x_coi
  count_x_total <- x@stat[["count_partition"]]
  count_notx_coi <- x@size - x@stat[["count_window"]]
  count_notx_ref <- size_total - size_window - count_x_ref
  count_notx_total <- size_total - x@stat[["count_partition"]]
  options(digits=20)
  exp_x_coi <- (count_x_total/size_total) * size_window
  exp_x_ref <- (count_x_total/size_total) * (size_total - size_window)
  exp_notx_coi <- (count_notx_total/size_total) * size_window
  exp_notx_ref <- (count_notx_total/size_total) * (size_total - size_window)
  chi1 <- ((exp_x_coi - count_x_coi)**2) / exp_x_coi
  chi2 <- ((exp_x_ref - count_x_ref)**2) / exp_x_ref
  chi3 <- ((exp_notx_coi - count_notx_coi)**2) / exp_notx_coi
  chi4 <- ((exp_notx_ref - count_notx_ref)**2) / exp_notx_ref
  chi <- chi1 + chi2 + chi3 + chi4
  chi <- chi * apply(cbind(count_x_coi, exp_x_coi), 1, function(x) ifelse(x[1] > x[2], 1, -1))
  options(digits=7)
  x@stat[, exp_window := exp_x_coi]
  x@stat[, chisquare := chi]
  x <- sort(x, by="chisquare")
  x@stat[, rank_chisquare := c(1:nrow(x@stat))]
  x@statisticalTest <- c(x@statisticalTest, "chisquare")
  return(x)
})